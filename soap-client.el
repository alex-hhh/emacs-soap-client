;;;; soap.el -- Access SOAP web services from Emacs

;; Copyright (C) 2009  Alex Harsanyi <AlexHarsanyi@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Alexandru Harsanyi (AlexHarsanyi@gmail.com)
;; Created: December, 2009
;; Keywords: soap, web-services
;; Homepage: http://code.google.com/p/emacs-soap-client
;;
;; usefull links:
;;
;; http://www.w3.org/TR/wsdl

(require 'cl)
(require 'xml)
(require 'warnings)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'mm-decode)

(defsubst soap-warning (message &rest args)
  (display-warning 'soap-client (apply 'format message args) :warning))

(defgroup soap-client nil
  "Access SOAP web services from Emacs."
  :group 'tools)

;;;; Namespace aliases

;; XML documents with namespaces are dificult to parse because the names of
;; the nodes depend on what "xmlns" aliases have been defined in the document.
;; To work with such documents, we introduce a translation layer between a
;; "well known" namespace tag and the local namespace tag in the document
;; being parsed.

(defconst *soap-well-known-xmlns*
  '(("apachesoap" . "http://xml.apache.org/xml-soap")
    ("soapenc" . "http://schemas.xmlsoap.org/soap/encoding/")
    ("wsdl" . "http://schemas.xmlsoap.org/wsdl/")
    ("wsdlsoap" . "http://schemas.xmlsoap.org/wsdl/soap/")
    ("xsd" . "http://www.w3.org/2001/XMLSchema")
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("soap" . "http://schemas.xmlsoap.org/soap/envelope/"))
  "A list of well known xml namespaces and their aliases.")

(defvar *soap-local-xmlns* nil
  "A list of local namespace aliases.
This is a dynamically bound variable, controlled by
`soap-with-local-xmlns'.")

(defvar *soap-default-xmlns* nil
  "The default XML namespaces.
Names in this namespace will be unqualified.  This is a
dynamically bound variable, controlled by
`soap-with-local-xmlns'")

(defun soap-wk2l (well-known-name)
  "Return local variant of WELL-KNOWN-NAME.
This is done by looking up the namespace in the
`*soap-well-known-xmlns*' table and resolving the namespace to
the local name based on the current local translation tabble
`*soap-local-xmlns*'.  See also `soap-with-local-xmlns'."
  (let ((wk-name-1 (if (symbolp well-known-name) 
                       (symbol-name well-known-name) 
                       well-known-name)))
    (cond
      ((string-match "^\\(.*\\):\\(.*\\)$" wk-name-1)
       (let ((ns (match-string 1 wk-name-1))
             (name (match-string 2 wk-name-1)))
         (let ((namespace (cdr (assoc ns *soap-well-known-xmlns*))))
           (cond ((equal namespace *soap-default-xmlns*)
                  ;; Name is unqualified in the default namespace
                  (if (symbolp well-known-name)
                      (intern name)
                      name))
                 (t
                  (let* ((local-ns (car (rassoc namespace *soap-local-xmlns*)))
                         (local-name (concat local-ns ":" name)))
                    (if (symbolp well-known-name)
                        (intern local-name)
                        local-name)))))))
          (t well-known-name))))

(defun soap-extract-xmlns (node &optional xmlns-table)
  "Return an alias table for the xml NODE by extending XMLNS-TABLE."
  (let (xmlns default-ns)
    (dolist (a (xml-node-attributes node))
      (let ((name (symbol-name (car a)))
            (value (cdr a)))
        (cond ((string= name "xmlns")
               (setq default-ns value))
              ((string-match "^xmlns:\\(.*\\)$" name)
               (push (cons (match-string 1 name) value) xmlns)))))
    (unless (assoc "tns" xmlns)
      ;; a tns alias was not defined in this node.  See if the node
      ;; has a "targetNamespace" attribute and add an alias to this.
      ;; Note that we might override an existing tns alias in
      ;; XMLNS-TABLE, but that is intended.
      (let ((tns (xml-get-attribute-or-nil node 'targetNamespace)))
        (when tns
          (push (cons "tns" tns) xmlns))))
    (cons default-ns (append xmlns xmlns-table))))

(defmacro soap-with-local-xmlns (node &rest body)
  "Install a local alias table from NODE and execute BODY."
  (declare (debug (form &rest form)) (indent 1))
  (let ((xmlns (make-symbol "xmlns")))
    `(let ((,xmlns (soap-extract-xmlns ,node *soap-local-xmlns*)))
       (let ((*soap-default-xmlns* (car ,xmlns))
             (*soap-local-xmlns* (cdr ,xmlns)))
         ,@body))))


;;;; XML namespaces

;; An element in an XML namespace, "things" stored in soap-xml-namespaces will
;; be derived from this object.

(defstruct soap-element
  name
  ;; The "well-known" namespace tag for the element.  For example, while
  ;; parsing XML documents, we can have different tags for the XMLSchema
  ;; namespace, but internally all our XMLSchema elements will have the "xsd"
  ;; tag.
  namespace-tag)

(defun soap-element-fq-name (element)
  (concat (soap-element-namespace-tag element)
          ":" (soap-element-name element)))

;; a namespace link stores an alias for an object in once namespace to a
;; "target" object possibly in a different namespace

(defstruct (soap-namespace-link (:include soap-element))
  target)

;; A namespace is a collection of soap-element objects under a name (the name
;; of the namespace).

(defstruct soap-namespace
  (name nil :read-only t)               ; e.g "http://xml.apache.org/xml-soap"
  (elements (make-hash-table :test 'equal) :read-only t))

(defun soap-namespace-put (element ns)
  "Store ELEMENT in NS.
Multiple elements with the same name can be stored in a
namespace.  When retrieving the element you can specify a
discriminant predicate to `soap-namespace-get'"
  (let ((name (soap-element-name element)))
    (push element (gethash name (soap-namespace-elements ns)))))

(defun soap-namespace-put-link (name target ns &optional replace)
  "Store a link from NAME to TARGET in NS.
An error will be signaled if an element by the same name is
already present in NS, unless REPLACE is non nil.

TARGET can be either a SOAP-ELEMENT or a string denoting an
element name into another namespace.

If NAME is nil, an element with the same name as TARGET will be
added to the namespace."

  (unless (and name (not (equal name "")))
    (cond ((soap-element-p target)
           (setq name (soap-element-name target)))
          ((stringp target)
           (cond ((string-match "^\\(.*\\):\\(.*\\)$" target)
                  (setq name (match-string 2 target)))
                 (t
                  (setq name target))))))

  (assert name)
  (push (make-soap-namespace-link :name name :target target)
        (gethash name (soap-namespace-elements ns))))

(defun soap-namespace-get (name ns &optional discrimninant-predicate)
  "Retrieve an element with NAME from the namespace NS.
If multiple elements with the same name exist,
DISCRIMNINANT-PREDICATE is used to pick one of them.  This allows
storing elements of different types (like a message type and a
binding) but the same name."
  (let ((e (gethash name (soap-namespace-elements ns))))
    (cond (discrimninant-predicate (find-if discrimninant-predicate e))
          ((= (length e) 1) (car e))
          ((> (length e) 1)
           (error "Multiple elements named %s, discriminant needed" name))
          (t
           nil))))


;;;; WSDL documents
;;;;; WSDL document elements

(defstruct (soap-basic-type (:include soap-element))
  kind                              ; a symbol of: string, dateTime, long, int
  )

(defstruct soap-sequence-element
  name type nillable? multiple?)

(defstruct (soap-sequence-type (:include soap-element))
  parent                                ; OPTIONAL WSDL-TYPE name
  elements                              ; LIST of SOAP-SEQUCENCE-ELEMENT
  )

(defstruct (soap-array-type (:include soap-element))
  element-type                          ; WSDL-TYPE of the array elements
  )

(defstruct (soap-message (:include soap-element))
  parts                                 ; ALIST of NAME => WSDL-TYPE name
  )

(defstruct (soap-operation (:include soap-element))
  parameter-order
  input                                 ; (NAME . MESSAGE)
  output                                ; (NAME . MESSAGE)
  faults)                               ; a list of (NAME . MESSAGE)

(defstruct (soap-port-type (:include soap-element))
  operations)                           ; a namespace of operations

;; A bound operation is an operation which has a soap action and a use
;; method attached -- these are attached as part of a binding and we
;; can have different bindings for the same operations.
(defstruct soap-bound-operation
  operation                             ; SOAP-OPERATION
  soap-action                           ; value for SOAPAction HTTP header
  use                                   ; 'literal or 'encoded, see http://www.w3.org/TR/wsdl#_soap:body
  )

(defstruct (soap-binding (:include soap-element))
  port-type
  (operations (make-hash-table :test 'equal) :readonly t))

(defstruct (soap-port (:include soap-element))
  service-url
  binding)

(defun soap-default-xsd-types ()
  "Return a namespace containing some of the XMLSchema types."
  (let ((ns (make-soap-namespace :name "http://www.w3.org/2001/XMLSchema")))
    (dolist (type '("string" "dateTime" "boolean" "long" "int" "anyType"))
      (soap-namespace-put
       (make-soap-basic-type :name type :kind (intern type))
       ns))
    ns))

;;;;; The WSDL document

;; The WSDL data structure used for encoding/decoding SOAP messages
(defstruct soap-wsdl
  ports                                 ; a list of SOAP-PORT instances
  alias-table                           ; a list of namespace aliases
  namespaces                            ; a list of namespaces
  )

(defun soap-wsdl-add-alias (alias name wsdl)
  "Add a namespace alias to the WSDL document"
  (push (cons alias name) (soap-wsdl-alias-table wsdl)))

(defun soap-wsdl-add-namespace (ns wsdl)
  "Add NAMESPACE to the WSDL document.
If a namespace by this name already exists in WSDL, individual
elements will be added to it."
  (let ((existing (find (soap-namespace-name ns)
                        (soap-wsdl-namespaces wsdl)
                        :key 'soap-namespace-name
                        :test 'string=)))
    (if existing
        ;; Add elements from NS to EXISTING, replacing existing values.
        (maphash (lambda (key value)
                   (dolist (v value)
                     (soap-namespace-put v existing)))
                 (soap-namespace-elements ns))
        (push ns (soap-wsdl-namespaces wsdl)))))

(defun soap-wsdl-get (name wsdl &optional predicate use-local-alias-table)
  "Retrieve element NAME from the WSDL document.
If USE-LOCAL-ALIAS-TABLE is not nil, `*soap-local-xmlns*` will be
used to resolve the namespace alias."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (let ((alias-table (soap-wsdl-alias-table wsdl)))
    (when use-local-alias-table
      (setq alias-table (append *soap-local-xmlns* alias-table)))
    (cond ((string-match "^\\(.*\\):\\(.*\\)$" name)
           (let ((ns-alias (match-string 1 name))
                 (name (match-string 2 name)))
             (let ((namespace (cdr (assoc ns-alias alias-table))))
               (unless namespace
                 (error "Cannot find %s in wsdl" ns-alias))
               (let ((ns (find namespace
                               (soap-wsdl-namespaces wsdl)
                               :key 'soap-namespace-name
                               :test 'string=)))
                 (unless ns
                   (error "Unknown namespace: %s, refered by alias %s"
                          namespace ns-alias))
                 (let ((element (soap-namespace-get name ns 
                                                    (if predicate
                                                        (lambda (e)
                                                          (or (funcall 'soap-namespace-link-p e)
                                                              (funcall predicate e)))
                                                        nil))))
                   (if (soap-namespace-link-p element)
                       ;; NOTE: don't use the local alias table here
                       (soap-wsdl-get (soap-namespace-link-target element) wsdl predicate)
                       element))))))
          (t
           (error "Cannot resolve %s" name)))))

(defun soap-l2wsdl (l-name wsdl)
  "Convert L-NAME to a fully qualified name in this WSDL.
L-NAME is usually a node name in an XML document and this
function will return a fully qualified name, suitable for a
`soap-wsdl-get' call.  Note that only name resolving is done, the
actual element might not exist in this WSDL.

This function will only work inside the body of a
`soap-with-local-xmlns' macro."
  (let ((l-name-1 (if (symbolp l-name) (symbol-name l-name) l-name)))
    (let (ns name)
      (cond
        ((string-match "^\\(.*\\):\\(.*\\)$" l-name-1)
         (setq ns (match-string 1 l-name-1))
         (setq name (match-string 2 l-name-1)))
        (t
         (setq name l-name-1)))

      (let ((namespace 
             (cond (ns (cdr (assoc ns *soap-local-xmlns*)))
                   (t *soap-default-xmlns*))))

        (let ((nstag (car (rassoc namespace (soap-wsdl-alias-table wsdl)))))
          (unless nstag
            (error "Namespace %s not found in WSDL" namespace))

          (let ((wsdl-name (concat nstag ":" name)))
            (if (symbolp l-name)
                (intern wsdl-name)
                wsdl-name)))))))


;;;;; Resolving references for wsdl types

;; When the WSDL elements are created from the XML document, they refer to
;; each other by name.  For example, the ELEMENT-TYPE slot of an
;; SOAP-ARRAY-TYPE will contain the name of the element and the user would
;; have to call `soap-wsdl-get' to obtain the actual element.
;;
;; After the entire document is loaded, we resolve all these references to the
;; actual elements they refer to so that at runtime, we don't have to call
;; `soap-wsdl-get' each time we traverse an element tree.

(defun soap-resolve-references-for-element (element wsdl)
  "Resolve (inplace) references in ELEMENT using WSDL.
This is a generic function which invokes a specific function
depending on the element type.

If ELEMENT has no resolver function, it is silently ignored"
  (let ((resolver (get (aref element 0) 'soap-resolve-references)))
    (when resolver
      (funcall resolver element wsdl))))

(defun soap-resolve-references-for-sequence-type (type wsdl)
  (let ((parent (soap-sequence-type-parent type)))
    (when (stringp parent)
      (setf (soap-sequence-type-parent type)
            (soap-wsdl-get parent wsdl))))
  (dolist (element (soap-sequence-type-elements type))
    (let ((element-type (soap-sequence-element-type element)))
      (cond ((stringp element-type)
             (setf (soap-sequence-element-type element)
                   (soap-wsdl-get element-type wsdl)))
            ((soap-element-p element-type)
             ;; since the element already has a child element, it
             ;; could be an inline structure. we must resolve
             ;; references in it, because it might not be reached by
             ;; scaning the wsdl names.
             (soap-resolve-references-for-element element-type wsdl))))))

(defun soap-resolve-references-for-array-type (type wsdl)
  (let ((element-type (soap-array-type-element-type type)))
    (when (stringp element-type)
      (setf (soap-array-type-element-type type)
            (soap-wsdl-get element-type wsdl)))))

(defun soap-resolve-references-for-message (message wsdl)
  (let (resolved-parts)
    (dolist (part (soap-message-parts message))
      (let ((name (car part))
            (type (cdr part)))
        (when (stringp name)
          (setq name (intern name)))
        (when (stringp type)
          (setq type (soap-wsdl-get type wsdl)))
        (push (cons name type) resolved-parts)))
     (setf (soap-message-parts message) (nreverse resolved-parts))))

(defun soap-resolve-references-for-operation (operation wsdl)
  (let ((input (soap-operation-input operation)))
    (let ((name (car input))
          (message (cdr input)))
      (when (stringp message)
        (setf (soap-operation-input operation)
              (cons (intern name) (soap-wsdl-get message wsdl))))))

  (let ((output (soap-operation-output operation)))
    (let ((name (car output))
          (message (cdr output)))
      (when (stringp message)
        (setf (soap-operation-output operation)
              (cons (intern name) (soap-wsdl-get message wsdl))))))

  (let ((resolved-faults nil))
    (dolist (fault (soap-operation-faults operation))
      (let ((name (car fault))
            (message (cdr fault)))
      (if (stringp message)
          (push (cons (intern name) (soap-wsdl-get message wsdl))
                resolved-faults)
          (push fault resolved-faults))))
    (setf (soap-operation-faults operation) resolved-faults))

  (if (= (length (soap-operation-parameter-order operation)) 0)
      (setf (soap-operation-parameter-order operation)
            (mapcar 'car (soap-message-parts 
                          (cdr (soap-operation-input operation))))))

  (setf (soap-operation-parameter-order operation)
        (mapcar 'intern (soap-operation-parameter-order operation))))

(defun soap-resolve-references-for-binding (binding wsdl)
  (when (stringp (soap-binding-port-type binding))
    (setf (soap-binding-port-type binding)
          (soap-wsdl-get (soap-binding-port-type binding) wsdl 'soap-port-type-p)))

  (let ((port-ops (soap-port-type-operations (soap-binding-port-type binding))))
    (maphash (lambda (k v)
               (setf (soap-bound-operation-operation v)
                     (soap-namespace-get k port-ops 'soap-operation-p)))
             (soap-binding-operations binding))))

(defun soap-resolve-references-for-port (port wsdl)
  (when (stringp (soap-port-binding port))
    (setf (soap-port-binding port)
          (soap-wsdl-get (soap-port-binding port) wsdl 'soap-binding-p))))

;; Install resolvers for our types
(progn
  (put (aref (make-soap-sequence-type) 0) 'soap-resolve-references
       'soap-resolve-references-for-sequence-type)
  (put (aref (make-soap-array-type) 0) 'soap-resolve-references
       'soap-resolve-references-for-array-type)
  (put (aref (make-soap-message) 0) 'soap-resolve-references
       'soap-resolve-references-for-message)
  (put (aref (make-soap-operation) 0) 'soap-resolve-references
       'soap-resolve-references-for-operation)
  (put (aref (make-soap-binding) 0) 'soap-resolve-references
       'soap-resolve-references-for-binding)
  (put (aref (make-soap-port) 0) 'soap-resolve-references
       'soap-resolve-references-for-port))

(defun soap-wsdl-resolve-references (wsdl)
  (let ((nprocessed 0)
        (nstag-id 0)
        (alias-table (soap-wsdl-alias-table wsdl)))
    (dolist (ns (soap-wsdl-namespaces wsdl))
      (let ((nstag (car-safe (rassoc (soap-namespace-name ns) alias-table))))
        (unless nstag
          ;; If this namespace does not have an alias, create one for it.
          (catch 'done
            (while t
              (setq nstag (format "ns%d" (incf nstag-id)))
              (unless (assoc nstag alias-table)
                (soap-wsdl-add-alias nstag (soap-namespace-name ns) wsdl)
                (throw 'done t)))))

        (maphash (lambda (name element)
                   (cond ((soap-element-p element) ; skip links
                          (incf nprocessed)
                          (soap-resolve-references-for-element element wsdl)
                          (setf (soap-element-namespace-tag element) nstag))
                         ((listp element)
                          (dolist (e element)
                            (when (soap-element-p e)
                              (incf nprocessed)
                              (soap-resolve-references-for-element e wsdl)
                              (setf (soap-element-namespace-tag e) nstag))))))
                 (soap-namespace-elements ns))))

    (message "Processed %d" nprocessed))
    wsdl)

;;;;; Loading WSDL from XML documents

(defun soap-load-wsdl-from-url (url)
  (let ((url-request-method "GET")
        (url-package-name "soap-client.el")
        (url-package-version "1.0")
        (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
        (url-request-coding-system 'utf-8)
        (url-http-attempt-keepalives nil))
    (let ((buffer (url-retrieve-synchronously url)))
      (with-current-buffer buffer
        (declare (special url-http-response-status))
        (if (> url-http-response-status 299)
            (error "Error retrieving WSDL: %s" url-http-response-status))
        (let ((mime-part (mm-dissect-buffer t t)))
          (unless mime-part
            (error "Failed to decode response from server"))
          (unless (equal (car (mm-handle-type mime-part)) "text/xml")
            (error "Server response is not an XML document"))
          (with-temp-buffer 
            (mm-insert-part mime-part)
            (let ((wsdl-xml (car (xml-parse-region (point-min) (point-max)))))
              (prog1
                  (soap-parse-wsdl wsdl-xml)
                (kill-buffer buffer)))))))))

(defun soap-load-wsdl (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((xml (car (xml-parse-region (point-min) (point-max)))))
      (soap-parse-wsdl xml))))

(defun soap-parse-wsdl (node)
  (soap-with-local-xmlns node
    (assert (eq (xml-node-name node) (soap-wk2l 'wsdl:definitions)))

    (let ((wsdl (make-soap-wsdl)))

      ;; Add the local alias table to the wsdl document -- it will be used for
      ;; all types in this document even after we finish parsing it.
      (setf (soap-wsdl-alias-table wsdl) *soap-local-xmlns*)

      ;; Add the XSD types to the wsdl document
      (let ((ns (soap-default-xsd-types)))
        (soap-wsdl-add-namespace ns wsdl)
        (soap-wsdl-add-alias "xsd" (soap-namespace-name ns) wsdl))

      ;; Find all the 'xsd:schema nodes which are children of wsdl:types nodes
      ;; and build our type-library

      (let ((types (car (xml-get-children node (soap-wk2l 'wsdl:types)))))
        (dolist (node (xml-node-children types))
          ;; We cannot use (xml-get-children node (soap-wk2l 'xsd:schama))
          ;; because each node can install its own alias type so the schema
          ;; nodes might have a different prefix.
          (when (consp node)
            (soap-with-local-xmlns node
              (when (eq (xml-node-name node) (soap-wk2l 'xsd:schema))
                (soap-wsdl-add-namespace (soap-parse-schema node) wsdl))))))

      (let ((ns (make-soap-namespace
                 :name (xml-get-attribute node 'targetNamespace))))
        (dolist (node (xml-get-children node (soap-wk2l 'wsdl:message)))
          (soap-namespace-put (soap-parse-message node) ns))

        (dolist (node (xml-get-children node (soap-wk2l 'wsdl:portType)))
          (let ((port-type (soap-parse-port-type node)))
            (soap-namespace-put port-type ns)
            (soap-wsdl-add-namespace (soap-port-type-operations port-type) wsdl)))

        (dolist (node (xml-get-children node (soap-wk2l 'wsdl:binding)))
          (soap-namespace-put (soap-parse-binding node) ns))

        (dolist (node (xml-get-children node (soap-wk2l 'wsdl:service)))
          (dolist (node (xml-get-children node (soap-wk2l 'wsdl:port)))
            (let ((name (xml-get-attribute node 'name))
                  (binding (xml-get-attribute node 'binding))
                  (url (let ((n (car (xml-get-children node (soap-wk2l 'wsdlsoap:address)))))
                         (xml-get-attribute n 'location))))
              (let ((port (make-soap-port 
                           :name name :binding binding :service-url url)))
                (soap-namespace-put port ns)
                (push port (soap-wsdl-ports wsdl))))))

        (soap-wsdl-add-namespace ns wsdl))

      (soap-wsdl-resolve-references wsdl)

      wsdl)))

(defun soap-parse-schema (node)
  "Parse a schema NODE.
Return a SOAP-NAMESPACE containg the elements."
  (soap-with-local-xmlns node
    (assert (eq (xml-node-name node) (soap-wk2l 'xsd:schema)))
    (let ((ns (make-soap-namespace
               :name (xml-get-attribute node 'targetNamespace))))
      ;; NOTE: we only extract the complexTypes from the schema, we wouldn't
      ;; know how to handle basic types beyond the built in ones anyway.
      (dolist (node (xml-get-children node (soap-wk2l 'xsd:complexType)))
        (soap-namespace-put (soap-parse-complex-type node) ns))

      (dolist (node (xml-get-children node (soap-wk2l 'xsd:element)))
        (soap-namespace-put (soap-parse-schema-element node) ns))

      ns)))

(defun soap-parse-schema-element (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'xsd:element)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        type)
    ;; A schema element that contains an inline complex type --
    ;; construct the actual complex type for it.
    (let ((type-node (xml-get-children node (soap-wk2l 'xsd:complexType))))
      (when (> (length type-node) 0)
        (assert (= (length type-node) 1)) ; only one complex type definition per element
        (setq type (soap-parse-complex-type (car type-node)))))
    (setf (soap-element-name type) name)
    type))

(defun soap-parse-complex-type (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'xsd:complexType)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        ;; Use a dummy type for the complex type, it will be replaced
        ;; with the real type below, except when the complex type node
        ;; is empty...
        (type (make-soap-sequence-type :elements nil)))
    (dolist (c (xml-node-children node))
      (when (consp c)               ; skip string nodes, which are whitespace
        (let ((node-name (xml-node-name c)))
          (cond
            ((eq node-name (soap-wk2l 'xsd:sequence))
             (setq type (soap-parse-complex-type-sequence c)))
            ((eq node-name (soap-wk2l 'xsd:complexContent))
             (setq type (soap-parse-complex-type-complex-content c)))
            ((eq node-name (soap-wk2l 'xsd:attribute))
             ;; The name of this node comes from an attribute tag
             (let ((n (xml-get-attribute-or-nil c 'name)))
               (setq name n)))
            (t
             (error "Unknown node type %s" node-name))))))
    (setf (soap-element-name type) name)
    type))

(defun soap-parse-sequence (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'xsd:sequence)))
  (let (elements)
    (dolist (e (xml-get-children node (soap-wk2l 'xsd:element)))
      (let ((name (xml-get-attribute-or-nil e 'name))
            (type (xml-get-attribute-or-nil e 'type))
            (nillable? (or (equal (xml-get-attribute-or-nil e 'nillable) "true")
                           (let ((e (xml-get-attribute-or-nil e 'minOccurs)))
                             (and e (equal e "0")))))
            (multiple? (let ((e (xml-get-attribute-or-nil e 'maxOccurs)))
                         (and e (not (equal e "1"))))))
        (unless type
          ;; The node does not have a type, maybe it has a complexType
          ;; defined inline...
          (let ((type-node (xml-get-children e (soap-wk2l 'xsd:complexType))))
            (when (> (length type-node) 0)
              (assert (= (length type-node) 1)) ; only one complex type definition per element
              (setq type (soap-parse-complex-type (car type-node))))))
        (push (make-soap-sequence-element
               :name (intern name) :type type :nillable? nillable? :multiple? multiple?)
              elements)))
    (nreverse elements)))

(defun soap-parse-complex-type-sequence (node)
  (let ((elements (soap-parse-sequence node)))
    (make-soap-sequence-type :elements elements)))

(defun soap-parse-complex-type-complex-content (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'xsd:complexContent)))
  (let (array? parent elements)
    (let ((extension (car-safe (xml-get-children node (soap-wk2l 'xsd:extension))))
          (restriction (car-safe (xml-get-children node (soap-wk2l 'xsd:restriction)))))
      ;; a complex content node is either an extension or a restriction
      (cond (extension
             (setq parent (xml-get-attribute-or-nil extension 'base))
             (setq elements (soap-parse-sequence
                             (car (xml-get-children extension 'sequence)))))
            (restriction
             (let ((base (xml-get-attribute-or-nil restriction 'base)))
               ;; we only support restrictions on array types
               (assert (equal base "soapenc:Array")))
             (setq array? t)
             (let ((attribute (car (xml-get-children restriction (soap-wk2l 'xsd:attribute)))))
               (let ((array-type (xml-get-attribute-or-nil attribute (soap-wk2l 'wsdl:arrayType))))
                 (when (string-match "^\\(.*\\)\\[\\]$" array-type)
                   (setq parent (match-string 1 array-type))))))

            (t
             (error "Unknown complex type"))))
    (if array?
        (make-soap-array-type :element-type parent)
        (make-soap-sequence-type :parent parent :elements elements))))

(defun soap-parse-message (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'wsdl:message)))
  (let ((name (xml-get-attribute-or-nil node 'name))
        parts)
    (dolist (p (xml-get-children node (soap-wk2l 'wsdl:part)))
      (let ((name (xml-get-attribute-or-nil p 'name))
            (type (xml-get-attribute-or-nil p 'type))
            (element (xml-get-attribute-or-nil p 'element)))
        (push (cons name (or type element)) parts)))
    (make-soap-message :name name :parts (nreverse parts))))

(defun soap-parse-port-type (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'wsdl:portType)))
  (let ((ns (make-soap-namespace
             :name (concat "urn:" (xml-get-attribute node 'name)))))
    (dolist (node (xml-get-children node (soap-wk2l 'wsdl:operation)))
      (let ((o (soap-parse-operation node)))

        (let ((other-operation (soap-namespace-get (soap-element-name o) ns 'soap-operation-p)))
          (if other-operation
              ;; Unfortunately, the Confluence WSDL defines two operations
              ;; named "search" which differ only in parameter names...
              (soap-warning "Discarding duplicate operation: %s" (soap-element-name o))

              (progn
                (soap-namespace-put o ns)

                ;; link all messages from this namespace, as this namespace
                ;; will be used for decoding the response.
                (destructuring-bind (name . message) (soap-operation-input o)
                  (soap-namespace-put-link name message ns))

                (destructuring-bind (name . message) (soap-operation-output o)
                  (soap-namespace-put-link name message ns))

                (dolist (fault (soap-operation-faults o))
                  (destructuring-bind (name . message) fault
                    (soap-namespace-put-link name message ns 'replace)))

                )))))

    (make-soap-port-type :name (xml-get-attribute node 'name)
                        :operations ns)))

(defun soap-parse-operation (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'wsdl:operation)))
  (let ((name (xml-get-attribute node 'name))
        (parameter-order (split-string (xml-get-attribute node 'parameterOrder)))
        input output faults)
    (dolist (n (xml-node-children node))
      (when (consp n)                 ; skip string nodes which are whitespace
        (let ((node-name (xml-node-name n)))
          (cond
            ((eq node-name (soap-wk2l 'wsdl:input))
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (setq input (cons name message))))
            ((eq node-name (soap-wk2l 'wsdl:output))
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (setq output (cons name message))))
            ((eq node-name (soap-wk2l 'wsdl:fault))
             (let ((message (xml-get-attribute n 'message))
                   (name (xml-get-attribute n 'name)))
               (push (cons name message) faults)))))))
    (make-soap-operation
     :name name
     :parameter-order parameter-order
     :input input
     :output output
     :faults (nreverse faults))))

(defun soap-parse-binding (node)
  (assert (eq (xml-node-name node) (soap-wk2l 'wsdl:binding)))
  (let ((name (xml-get-attribute node 'name))
        (type (xml-get-attribute node 'type)))
    (let ((binding (make-soap-binding :name name :port-type type)))
      (dolist (wo (xml-get-children node (soap-wk2l 'wsdl:operation)))
        (let ((name (xml-get-attribute wo 'name))
              soap-action 
              use)
          (dolist (so (xml-get-children wo (soap-wk2l 'wsdlsoap:operation)))
            (setq soap-action (xml-get-attribute-or-nil so 'soapAction)))

          ;; Search a wsdlsoap:body node and find a "use" tag.  The
          ;; same use tag is assumed to be present for both input and
          ;; output types (alhtough the WDSL spec allows separate
          ;; "use"-s for each of them...

          (dolist (i (xml-get-children wo (soap-wk2l 'wsdl:input)))
            (dolist (b (xml-get-children i (soap-wk2l 'wsdlsoap:body)))
              (setq use (or use
                            (xml-get-attribute-or-nil b 'use)))))

          (unless use
            (dolist (i (xml-get-children wo (soap-wk2l 'wsdl:output)))
              (dolist (b (xml-get-children i (soap-wk2l 'wsdlsoap:body)))
                (setq use (or use
                              (xml-get-attribute-or-nil b 'use))))))

          (puthash name (make-soap-bound-operation :operation name
                                                   :soap-action soap-action
                                                   :use (and use (intern use)))
                   (soap-binding-operations binding))))
      binding)))

;;;;; Describe WSDL operations
;;;; SOAP type decoding

(defvar *soap-multi-refs*)
(defvar *soap-decoded-multi-refs*)

(defun soap-decode-type (type node)
  (let ((href (xml-get-attribute-or-nil node 'href)))
    (cond (href
           (catch 'done
             ;; NODE is actually a HREF, find the target and decode that.
             ;; Check first if we already decoded this multiref.

             (let ((decoded (cdr (assoc href *soap-decoded-multi-refs*))))
               (when decoded
                 (throw 'done decoded)))

             (string-match "^#\\(.*\\)$" href) ; TODO: check that it matched

             (let ((id (match-string 1 href)))
               (dolist (mr *soap-multi-refs*)
                 (let ((mrid (xml-get-attribute mr 'id)))
                   (when (equal id mrid)
                     ;; recurse here, in case there are multiple HREF's
                     (let ((decoded (soap-decode-type type mr)))
                       (push (cons href decoded) *soap-decoded-multi-refs*)
                       (throw 'done decoded)))))
               (error "Cannot find href %s" href))))
          (t
           (soap-with-local-xmlns node
             (if (equal (xml-get-attribute-or-nil node (soap-wk2l 'xsi:nil)) "true")
                 nil
                 (let ((decoder (get (aref type 0) 'soap-decoder)))
                   (funcall decoder type node))))))))

(defun soap-decode-basic-type (type node)
  (let ((contents (xml-node-children node)))
    (assert (<= (length contents) 1))
    (setq contents (car-safe contents))
    (if (null contents)
        nil
        (assert (stringp contents))
        (ecase (soap-basic-type-kind type)
          (string contents)
          (dateTime contents)              ; TODO: convert to a date time
          ((long int) (string-to-number contents))
          (boolean (string= (downcase contents) "true"))))))

(defun soap-decode-sequence-type (type node)
  (let ((result nil)
        (parent (soap-sequence-type-parent type)))
    (when parent
      (setq result (nreverse (soap-decode-type parent node))))
    (dolist (element (soap-sequence-type-elements type))
      (let ((instance-count 0)
            (e-name (soap-sequence-element-name element))
            (e-type (soap-sequence-element-type element)))
        (dolist (node (xml-get-children node e-name))
          (incf instance-count)
          (push (cons e-name (soap-decode-type e-type node)) result))
        ;; Do some sanity checking
        (cond ((and (= instance-count 0)
                    (not (soap-sequence-element-nillable? element)))
               (soap-warning "While decoding %s: missing non-nillable slot %s"
                             (soap-element-name type) e-name))
              ((and (> instance-count 1)
                    (not (soap-sequence-element-multiple? element)))
               (soap-warning "While decoding %s: multiple slots named %s"
                             (soap-element-name type) e-name)))))
    (nreverse result)))

(defun soap-decode-array-type (type node)
  "Arrays are decoded as lists.
This is because it is easier to work with list results in LISP."
  (let ((result nil)
        (element-type (soap-array-type-element-type type)))
    (dolist (node (xml-node-children node))
      (when (consp node)
        (push (soap-decode-type element-type node) result)))
    (nreverse result)))

(progn
  (put (aref (make-soap-basic-type) 0)
       'soap-decoder 'soap-decode-basic-type)
  (put (aref (make-soap-sequence-type) 0)
       'soap-decoder 'soap-decode-sequence-type)
  (put (aref (make-soap-array-type) 0)
       'soap-decoder 'soap-decode-array-type))

;;;; Soap Envelope parsing

(put 'soap-error
     'error-conditions
     '(error soap-error))
(put 'soap-error 'error-message "SOAP error")

(defun soap-parse-envelope (node operation wsdl)
  (soap-with-local-xmlns node
    (assert (eq (xml-node-name node) (soap-wk2l 'soap:Envelope)))
    (let ((body (car (xml-get-children node (soap-wk2l 'soap:Body)))))

      (let ((fault (car (xml-get-children body (soap-wk2l 'soap:Fault)))))
        (when fault
          (let ((fault-code (let ((n (car (xml-get-children fault 'faultcode))))
                              (car-safe (xml-node-children n))))
                (fault-string (let ((n (car (xml-get-children fault 'faultstring))))
                                (car-safe (xml-node-children n)))))
          (while t
            (signal 'soap-error (list fault-code fault-string))))))

      ;; First (non string) element of the body is the root node of he
      ;; response
      (let ((response (if (eq (soap-bound-operation-use operation) 'literal)
                          ;; For 'literal uses, the response is the actual body
                          body
                          ;; ...otherwise the first non string element
                          ;; of the body is the response
                          (catch 'found
                            (dolist (n (xml-node-children body))
                              (when (consp n)
                                (throw 'found n)))))))
        (soap-parse-response response operation wsdl body)))))

(defun soap-parse-response (response-node operation wsdl soap-body)
  (let* ((op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-output op))))

    (soap-with-local-xmlns response-node

      (when (eq use 'encoded)
        (let* ((received-message-name (soap-l2wsdl (xml-node-name response-node) wsdl))
               (received-message (soap-wsdl-get received-message-name wsdl 'soap-message-p)))
          (unless (eq received-message message)
            (error "Unexpected message: got %s, expecting %s"
                   received-message-name
                   (soap-element-name message)))))

      (let ((decoded-parts nil)
            (*soap-multi-refs* (xml-get-children soap-body 'multiRef))
            (*soap-decoded-multi-refs* nil))

        (dolist (part (soap-message-parts message))
          (let ((tag (car part))
                (type (cdr part))
                node)
            
            (cond ((eq use 'encoded)
                   (setq node (car (xml-get-children response-node tag))))
                  ((eq use 'literal)
                   (setq node
                         (catch 'found
                           (let ((fqname (intern (soap-element-fq-name type))))
                             (dolist (c (xml-node-children response-node))
                               (when (consp c)
                                 (soap-with-local-xmlns c
                                   (when (equal (soap-l2wsdl (xml-node-name c) wsdl) fqname)
                                     (throw 'found c))))))))))

            (assert node)
            (push (soap-decode-type type node) decoded-parts)))

        decoded-parts))))

;;;; SOAP type encoding

(defvar *soap-encoded-namespaces*)

(defun soap-encode-value (param-name type value)
  (let ((encoder (get (aref type 0) 'soap-encoder)))
    (assert encoder)
    (funcall encoder param-name type value))
  (push (soap-element-namespace-tag type) *soap-encoded-namespaces*))

(defun soap-encode-basic-type (param-name type value)
  (let ((xml-tag  (symbol-name param-name))
        (xsi-type (soap-element-fq-name type)))
    (insert "<" xml-tag " xsi:type=\"" xsi-type "\"")
    (if value
        (progn
          (insert ">")
          (cond ((eq (soap-basic-type-kind type) 'boolean)
                 (insert (if value "true" "false")))
                ((eq (soap-basic-type-kind type) 'dateTime)
                 (cond ((and (consp value) ; is there a time-value-p ?
                             (>= (length value) 2)
                             (numberp (nth 0 value))
                             (numberp (nth 1 value)))
                        ;; Value is a (current-time) style value, convert to a string
                        (insert (format-time-string "%Y-%m-%dT%H:%M:%S" value)))
                       (t
                        (insert value))))
                (t
                 (insert (url-insert-entities-in-string (format "%s" value))))))
        (insert " xsi:nil=\"true\">"))
    (insert "</" xml-tag ">\n")))

(defun soap-encode-sequence-type (param-name type value)
  (let ((xml-tag  (if (symbolp param-name) (symbol-name param-name) param-name))
        (xsi-type (soap-element-fq-name type)))
    (insert "<" xml-tag " xsi:type=\"" xsi-type "\"")
    (if value
        (progn
          (insert ">\n")
          (let ((parents (list type))
                (parent (soap-sequence-type-parent type)))

            (while parent
              (push parent parents)
              (setq parent (soap-sequence-type-parent parent)))
            
            (dolist (type parents)
              (dolist (element (soap-sequence-type-elements type))
                (let ((instance-count 0)
                      (e-name (soap-sequence-element-name element))
                      (e-type (soap-sequence-element-type element)))
                  (dolist (v value)
                    (when (equal (car v) e-name)
                      (incf instance-count)
                      (soap-encode-value e-name e-type (cdr v))))

                  ;; Do some sanity checking
                  (cond ((and (= instance-count 0)
                              (not (soap-sequence-element-nillable? element)))
                         (soap-warning "While encoding %s: missing non-nillable slot %s"
                                       (soap-element-name type) e-name))
                        ((and (> instance-count 1)
                              (not (soap-sequence-element-multiple? element)))
                         (soap-warning "While encoding %s: multiple slots named %s"
                                       (soap-element-name type) e-name))))))))
        (insert " xsi:nil=\"true\">"))
    (insert "</" xml-tag ">\n")))

(defun soap-encode-array-type (param-name type value)
  (unless (vectorp value)
    (error "soap-encode: %s(%s) expects a vector, got: %s" 
           param-name (soap-element-fq-name type) value))
  (let* ((xml-tag  (symbol-name param-name))
         (element-type (soap-array-type-element-type type))
         (array-type (concat (soap-element-fq-name element-type)
                             "[" (format "%s" (length value)) "]")))
    (insert "<" xml-tag
            " soapenc:arrayType=\"" array-type "\" "
            " xsi:type=\"soapenc:Array\">\n")
    (loop for i below (length value)
         do (soap-encode-value param-name element-type (aref value i)))
    (insert "</" xml-tag ">\n")))

(progn
  (put (aref (make-soap-basic-type) 0)
       'soap-encoder 'soap-encode-basic-type)
  (put (aref (make-soap-sequence-type) 0)
       'soap-encoder 'soap-encode-sequence-type)
  (put (aref (make-soap-array-type) 0)
       'soap-encoder 'soap-encode-array-type))

(defun soap-encode-body (operation parameters wsdl)
  (let* ((op (soap-bound-operation-operation operation))
         (use (soap-bound-operation-use operation))
         (message (cdr (soap-operation-input op)))
         (parameter-order (soap-operation-parameter-order op)))

    (unless (= (length parameter-order) (length parameters))
      (error "Wrong number of parameters for %s: expected %d, got %s"
             (soap-element-name op)
             (length parameter-order)
             (length parameters)))

    (insert "<soap:Body>\n")
    (when (eq use 'encoded)
      (push (soap-element-namespace-tag op) *soap-encoded-namespaces*)
      (insert "<" (soap-element-fq-name op) ">\n"))

    (let ((param-table (loop for formal in parameter-order
                          for value in parameters
                          collect (cons formal value))))
      (dolist (part (soap-message-parts message))
        (let* ((param-name (car part))
               (type (cdr part))
               (tag-name (if (eq use 'encoded) 
                             param-name
                             (soap-element-name type)))
               (value (cdr (assoc param-name param-table)))
               (start-pos (point)))
          (soap-encode-value tag-name type value)
          (when (eq use 'literal)
            ;; hack: add the xmlns attribute to the tag, the only way
            ;; ASP.NET web services recognize the namespace of the
            ;; element itself...
            (save-excursion
              (goto-char start-pos)
              (when (re-search-forward " ")
                (let* ((ns (soap-element-namespace-tag type))
                       (namespace (cdr (assoc ns (soap-wsdl-alias-table wsdl)))))
                  (when namespace
                    (insert "xmlns=\"" namespace "\" ")))))))))

    (when (eq use 'encoded)
      (insert "</" (soap-element-fq-name op) ">\n"))
    (insert "</soap:Body>\n")))

(defun soap-create-envelope (wsdl operation parameters)
  (with-temp-buffer
    (let ((*soap-encoded-namespaces* '("xsi" "soap" "soapenc"))
          (use (soap-bound-operation-use operation)))

      ;; Create the request body
      (soap-encode-body operation parameters wsdl)

      ;; Put the envelope around the body
      (goto-char (point-min))
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<soap:Envelope\n")
      (when (eq use 'encoded)
        (insert "    soapenc:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"\n"))
      (dolist (nstag (remove-duplicates *soap-encoded-namespaces* :test 'equal))
        (insert "    xmlns:" nstag "=\"")
        (let ((nsname (cdr (assoc nstag *soap-well-known-xmlns*))))
          (unless nsname
            (setq nsname (cdr (assoc nstag (soap-wsdl-alias-table wsdl)))))
          (insert nsname)
        (insert "\"\n")))
      (insert ">\n")
      (goto-char (point-max))
      (insert "</soap:Envelope>\n"))

    (buffer-string)))

;;;; invoking soap methods

(defcustom soap-debug nil
  "When t, enable some debugging facilities"
  :type 'boolean
  :group 'soap-client)

(defun soap-invoke (wsdl service operation-name &rest parameters)
  (let ((port (find service (soap-wsdl-ports wsdl) 
                    :key 'soap-element-name :test 'equal)))
    (unless port
      (error "Unknown SOAP service: %s" service))
    
    (let* ((binding (soap-port-binding port))
           (operation (gethash operation-name (soap-binding-operations binding))))
      (unless operation
        (error "No operation %s for SOAP service %s" operation-name service))
    
      (let ((url-request-method "POST")
            (url-package-name "esoap.el")
            (url-package-version "1.0")
            (url-http-version "1.0")
            (url-request-data (soap-create-envelope wsdl operation parameters))
            (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
            (url-request-coding-system 'utf-8)
            (url-http-attempt-keepalives t)
            (url-request-extra-headers (list
                                        (cons "SOAPAction" (soap-bound-operation-soap-action operation))
                                        (cons "Content-Type" "text/xml; charset=utf-8"))))
        (let ((buffer (url-retrieve-synchronously (soap-port-service-url port))))
          (condition-case err
              (with-current-buffer buffer
                (declare (special url-http-response-status))
                (if (null url-http-response-status)
                    (error "No HTTP response from server."))
                (if (and soap-debug (> url-http-response-status 299))
                    ;; This is a warning because some SOAP errors come
                    ;; back with a HTTP response 500 (internal server
                    ;; error)
                    (warn "Error in SOAP response: HTTP code %s" url-http-response-status))
                                 (when (> (buffer-size) 1000000)
                                   (soap-warning "Received large message: %s bytes" (buffer-size)))
                (let ((mime-part (mm-dissect-buffer t t)))
                  (unless mime-part
                    (error "Failed to decode response from server"))
                  (unless (equal (car (mm-handle-type mime-part)) "text/xml")
                    (error "Server response is not an XML document"))
                  (with-temp-buffer 
                    (mm-insert-part mime-part)
                    (let ((response (car (xml-parse-region (point-min) (point-max)))))
            (prog1
                (soap-parse-envelope response operation wsdl)
                        (kill-buffer buffer))))))
            (soap-error
             ;; Propagate soap-errors -- they are error replies of the
             ;; SOAP protocol and don't indicate a communication
             ;; problem or a bug in this code.
             (signal (car err) (cdr err)))
            (error 
             (when soap-debug
               (pop-to-buffer buffer))
             (error (error-message-string err)))))))))
  
(provide 'soap-client)


;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;;;+"
;;; End:

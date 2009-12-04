;;; jira2.el -- Provide connectivity to JIRA SOAP service

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
;; Keywords: soap, web-services, jira
;; Homepage: http://code.google.com/p/emacs-soap-client

;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; Jira References:
;;
;; http://confluence.atlassian.com/display/JIRA/Creating+a+SOAP+Client
;;
;; JavaDoc for the Jira SOAP service
;; http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html

(require 'soap-client)
(require 'cl)

(defgroup jira2 nil
  "Access JIRA from emacs."
  :group 'tools)

(defcustom jira2-wsdl-descriptor-url 
  "http://jira/rpc/soap/jirasoapservice-v2?wsdl"
  "The location for the WSDL descriptor for the JIRA service.
This is specific to your local JIRA installation.  The URL is
tipically:

  http://YOUR_INSTALLATION/rpc/soap/jirasoapservice-v2?wsdl

The default value works if JIRA is located at a hostname named
'jira'.")

(defvar jira2-token nil
  "JIRA token used for authentication")

(defvar jira2-wsdl nil)

(defun jira2-load-wsdl ()
  "Load the JIRA WSDL descriptor."
  (setq jira2-wsdl (soap-load-wsdl-from-url jira2-wsdl-descriptor-url)))

(defun jira2-login (username password)
  "Login into JIRA and store the authentication token in `jira2-token'"
  ;; NOTE that we cannot rely on `jira2-call' because `jira2-call' relies on
  ;; us ;-)
  (interactive
   (list (read-string 
          (format "JIRA Username [%s]: " user-login-name) nil nil user-login-name)
	 (read-passwd "JIRA Password: ")))
  (unless jira2-wsdl 
    (jira2-load-wsdl))
  (setq jira2-token 
        (car (soap-invoke jira2-wsdl "jirasoapservice-v2" "login" username password))))

(defun jira2-call (method &rest params)
  "Invoke the JIRA METHOD with supplied PARAMS.
This should be used for all JIRA inteface calls, as the method
ensures the user is logged in and invokes `soap-invoke' with the
correct service name and authentication token.

All JIRA inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter. For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `jira2-call', the call shoulbe be:

  (jira2-call \"getIssue\" KEY)
"
  (unless jira2-token
    (call-interactively 'jira2-login))
  (condition-case data
      (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
             method jira2-token params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq jira2-token nil)
     (call-interactively 'jira2-login)
     (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
            method jira2-token params))))


;;;; Some utility functions

(defun jira2-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (loop for element in data
     collect (cons (cdr (assoc key-field element))
		   (cdr (assoc value-field element)))))

(defun jira2-make-remote-field-values (fields)
  "Transform a (KEY . VALUE) list into a RemoteFieldValue structure.

Each (KEY . VALUE) pair is transformed into 
 ((id . KEY) (values . (VALUE)))

This method exists because Several JIRA methods require a
RemoteFieldValue list, but it is easier to work with ALISTS in
emacs-lisp"
  (let ((remote-field-values))

    ;; we accept an ALIST of field-name field-values parameter, but we need to
    ;; construct a structure that encodes as a RemoteFieldValue which is what
    ;; updateIssue wants
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (when (symbolp name)
          (setq name (symbol-name name)))
        ;; Value must be an "array" (for which soap-client accepts lists) even
        ;; if it is just one value
        (unless (vectorp value)
          (setq value (vector value)))
        (push `((id . ,name) (values . ,value)) 
              remote-field-values)))
    
    (apply 'vector (nreverse remote-field-values))))

;;;; Wrappers around JIRA methods

(defun jira2-get-issue (key)
  (car (jira2-call "getIssue" key)))

(defun jira2-get-comments (key)
  (car (jira2-call "getComments" key)))

(defun jira2-add-comment (key comment)
  (car (jira2-call "addComment" key `((body . ,comment)))))

(defun jira2-update-issue (key fields)
  (jira2-call "updateIssue" key (jira2-make-remote-field-values fields)))

(defun jira2-create-issue (fields)
  (jira2-call "createIssue" fields))

(defvar jira2-status-codes-cache nil)

(defun jira2-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-status-codes-cache
    (setq jira2-status-codes-cache
	  (jira2-make-assoc-list (car (jira2-call "getStatuses")) 'id 'name)))
  jira2-status-codes-cache)

(defvar jira2-issue-types-cache nil)

(defun jira2-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-issue-types-cache
    (setq jira2-issue-types-cache
	  (jira2-make-assoc-list (car (jira2-call "getIssueTypes")) 'id 'name)))
  jira2-issue-types-cache)

(defvar jira2-priority-codes-cache nil)

(defun jira2-get-priorities ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-priority-codes-cache
    (setq jira2-priority-codes-cache
	  (jira2-make-assoc-list (car (jira2-call "getPriorities")) 'id 'name)))
  jira2-priority-codes-cache)

(defvar jira2-resolution-code-cache nil)

(defun jira2-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-resolution-code-cache
    (setq jira2-resolution-code-cache
	  (jira2-make-assoc-list (car (jira2-call "getResolutions")) 'id 'name)))
  jira2-resolution-code-cache)

(defvar jira2-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun jira2-get-issue-regexp ()
  "Return a regexp that matches an issue name.
The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless jira2-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (car (jira2-call "getProjectsNoSchemes")))))
      (setq jira2-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  jira2-issue-regexp)

(provide 'jira2)


(require 'ert)      

(defvar *xs* (soap-make-xs-basic-types "http://www.w3.org/2001/XMLSchema" "xs"))

(defun soapt-parse-xml (text)
  (with-temp-buffer 
    (insert text)
    (car (xml-parse-region (point-min) (point-max)))))

(defun soapt-parse-xml-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (car (xml-parse-region (point-min) (point-max)))))

(ert-deftest soapt-encode-xs-basic-types ()
  (let ((type (soap-namespace-get "int" *xs*)))
    (let ((expected "<hello xsi:type=\"xs:int\">42</hello>\n")
          (produced (with-temp-buffer 
                      (soap-encode-xs-basic-type "hello" 42 type)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (should (equal expected produced))))

  (let ((type (soap-namespace-get "string" *xs*)))
    (let ((expected "<hello xsi:type=\"xs:string\">hello</hello>\n")
          (produced (with-temp-buffer 
                      (soap-encode-xs-basic-type "hello" "hello" type)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (should (equal expected produced))))

  (let ((type (soap-namespace-get "string" *xs*)))
    (should-error
     (with-temp-buffer 
       (soap-encode-xs-basic-type "hello" 42 type)
       (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest soapt-decode-xs-basic-types ()
  (let ((type (soap-namespace-get "int" *xs*))
        (node (soapt-parse-xml "<hello xsi:type=\"xs:int\">42</hello>\n")))
    (let ((expected 42)
          (produced (soap-decode-xs-basic-type type node)))
      (should (equal expected produced)))))

(defconst *xs-simple-type1* 
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"st1\">
    <xs:restriction base=\"xs:integer\">
      <xs:minInclusive value=\"0\"/>
      <xs:maxInclusive value=\"120\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type2* 
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"st2\">
    <xs:restriction base=\"xs:integer\">
      <xs:minExclusive value=\"0\"/>
      <xs:maxExclusive value=\"120\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type3*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"st3\">
    <xs:restriction base=\"xs:string\">
      <xs:enumeration value=\"Audi\"/>
      <xs:enumeration value=\"Golf\"/>
      <xs:enumeration value=\"BMW\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type4*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
    <xs:restriction base=\"xs:integer\">
      <xs:pattern value=\"[0-9][0-9][0-9][0-9][0-9]\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type5*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
    <xs:restriction base=\"xs:string\">
      <xs:length value=\"8\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type6*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
    <xs:restriction base=\"xs:string\">
      <xs:minLength value=\"5\"/>
      <xs:maxLength value=\"8\"/>
    </xs:restriction>
  </xs:simpleType>")

(defconst *xs-simple-type7*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">
    <xs:union memberTypes=\"sizebyno sizebystring\" />
  </xs:simpleType>")

(defconst *xs-simple-type8*
  "<xs:simpleType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"PropertyTagType\">
    <xs:union memberTypes =\"xs:unsignedShort\">
      <xs:simpleType id=\"HexPropertyTagType\">
        <xs:restriction base=\"xs:string\">
          <xs:pattern value=\"(0x|0X)[0-9A-Fa-f]{1,4}\"/>
        </xs:restriction>
      </xs:simpleType>
    </xs:union>
  </xs:simpleType>")

(ert-deftest soapt-make-simple-type ()
  (dolist (test-case 
            `(([cl-struct-soap-xs-simple-type "st1" nil nil nil "xs:integer" nil nil nil
                                              (0 . 120)] . ,*xs-simple-type1*)
              ([cl-struct-soap-xs-simple-type "st2" nil nil nil "xs:integer" nil nil nil
                                              (1 . 119)] . ,*xs-simple-type2*)
              ([cl-struct-soap-xs-simple-type "st3" nil nil nil "xs:string"
                               ("BMW" "Golf" "Audi")
                               nil nil nil] . ,*xs-simple-type3*)
              ([cl-struct-soap-xs-simple-type nil nil nil nil "xs:integer" nil
                                              "[0-9][0-9][0-9][0-9][0-9]" nil nil] . ,*xs-simple-type4*)

              ([cl-struct-soap-xs-simple-type nil nil nil nil "xs:string" nil nil
                               (8 . 8)
                               nil] . ,*xs-simple-type5*)
              ([cl-struct-soap-xs-simple-type nil nil nil nil "xs:string" nil nil
                               (5 . 8)
                               nil] . ,*xs-simple-type6*)
              ([cl-struct-soap-xs-simple-type nil nil nil nil
                               ("sizebyno" "sizebystring")
                               nil nil nil nil] . ,*xs-simple-type7*)
              ([cl-struct-soap-xs-simple-type "PropertyTagType" nil nil nil
                                              ([cl-struct-soap-xs-simple-type
                                              nil
                                              nil "HexPropertyTagType"
                                              nil "xs:string"
                                              nil "(0x|0X)[0-9A-Fa-f]{1,4}"
                                              nil nil]
                                               "xs:unsignedShort")
                                              nil nil nil nil] . ,*xs-simple-type8*)
              ))
    (should (equal (car test-case)
                   (let ((r (soapt-parse-xml (cdr test-case))))
                     (soap-with-local-xmlns r
                       (soap-xs-parse-simple-type r)))))))

(defconst *xs-element-def1*
  "<xs:element xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"age\">
  <xs:simpleType>
    <xs:restriction base=\"xs:integer\">
      <xs:minInclusive value=\"0\"/>
      <xs:maxInclusive value=\"120\"/>
    </xs:restriction>
  </xs:simpleType>
</xs:element> ")

(defconst *xs-element-def2*
  "<xs:element xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"age\" type=\"ageType\"/>")

(defconst *xs-element-def3*
  "<xs:element xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"shoesize\">
<xs:complexType>
    <xs:simpleContent>
      <xs:extension base=\"xs:integer\">
        <xs:attribute name=\"country\" type=\"xs:string\" />
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>
</xs:element>")

(ert-deftest soapt-make-element ()
  (dolist (test-case
            `(([cl-struct-soap-xs-element "age" nil nil
                                          [cl-struct-soap-xs-simple-type nil nil nil nil "xs:integer" nil nil nil
                                                                         (0 . 120)]
                                          nil nil nil nil] 
               . ,*xs-element-def1*)
              ([cl-struct-soap-xs-element "age" nil nil "ageType" nil nil nil nil]
               . ,*xs-element-def2*)
              ([cl-struct-soap-xs-element "shoesize" nil nil
                           [cl-struct-soap-xs-simple-type nil nil nil
                                                          ([cl-struct-soap-xs-attribute "country" nil "xs:string" nil])
                                                          "xs:integer" nil nil nil nil]
                           nil nil nil nil]
               . ,*xs-element-def3*)))
    (should (equal (car test-case)
                   (let ((r (soapt-parse-xml (cdr test-case))))
                     (soap-with-local-xmlns r
                       (soap-xs-parse-element r)))))))
    
(defconst *xs-complex-type1*
  "<xs:complexType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"shoesize\">
    <xs:simpleContent>
      <xs:extension base=\"xs:integer\">
        <xs:attribute name=\"country\" type=\"xs:string\" />
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>")

(defconst *xs-complex-type2*
  "<xs:complexType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" >
    <xs:sequence>
      <xs:element name=\"firstname\" type=\"xs:string\"/>
      <xs:element name=\"lastname\" type=\"xs:string\"/>
    </xs:sequence>
  </xs:complexType>
</xs:element>")

(defconst *xs-complex-type3*
  "<xs:complexType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" name=\"fullpersoninfo\">
  <xs:complexContent>
    <xs:extension base=\"personinfo\">
      <xs:sequence>
        <xs:element name=\"address\" type=\"xs:string\"/>
        <xs:element name=\"city\" type=\"xs:string\"/>
        <xs:element name=\"country\" type=\"xs:string\"/>
      </xs:sequence>
    </xs:extension>
  </xs:complexContent>
</xs:complexType>")

(defconst *xs-complex-type4*
  "<xs:complexType xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" 
                   xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"
                   xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\"
                   name=\"ArrayOf_tns1_RemoteUser\">
    <xs:complexContent>
     <xs:restriction base=\"soapenc:Array\">
      <xs:attribute ref=\"soapenc:arrayType\" wsdl:arrayType=\"tns1:RemoteUser[]\"/>
     </xs:restriction>
    </xs:complexContent>
   </xs:complexType>")

(ert-deftest soapt-make-complex-type ()
  (dolist (test-case
            `(([cl-struct-soap-xs-simple-type "shoesize" nil nil
                               ([cl-struct-soap-xs-attribute "country" nil "xs:string" nil])
                               "xs:integer" nil nil nil nil]
               . ,*xs-complex-type1*)
              ([cl-struct-soap-xs-complex-type nil nil nil nil sequence nil
                                ([cl-struct-soap-xs-element "lastname" nil nil "xs:string" nil nil nil nil]
                                 [cl-struct-soap-xs-element "firstname" nil nil "xs:string" nil nil nil nil])]
               . ,*xs-complex-type2*)
              ([cl-struct-soap-xs-complex-type "fullpersoninfo" nil nil nil sequence "personinfo"
                                ([cl-struct-soap-xs-element "country" nil nil "xs:string" nil nil nil nil]
                                 [cl-struct-soap-xs-element "city" nil nil "xs:string" nil nil nil nil]
                                 [cl-struct-soap-xs-element "address" nil nil "xs:string" nil nil nil nil])]
               . ,*xs-complex-type3*)
              ([cl-struct-soap-xs-complex-type "ArrayOf_tns1_RemoteUser" nil nil nil array "tns1:RemoteUser" nil]
               . ,*xs-complex-type4*)))
    (should (equal (car test-case)
                   (let ((r (soapt-parse-xml (cdr test-case))))
                     (soap-with-local-xmlns r
                       (soap-xs-parse-complex-type r)))))))



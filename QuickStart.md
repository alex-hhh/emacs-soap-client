The `jira2.el` is a working example of how to access a SOAP web service using `soap-client.el`.  Below is a brief description of the process involved

# Obtaining the souce code #

To obtain the source code for this package, you will need to checkout a clone of the Mercurial repository.  See the [source tab](http://code.google.com/p/emacs-soap-client/source/checkout) for more details.

# Locate and load the WSDL descriptor #

SOAP services use a WSDL descriptor for the format of the messages passed between the client and the server.

  * `soap-load-wsdl-from-url (URL)` will load a WSDL descriptor from URL and return the descriptor object
  * `soap-load-wsdl (FILE)` will load a WSDL descriptor from FILE and return the descriptor object

NOTE that the WSDL descriptor contains the address where SOAP requests need to be made.  In the JIRA example, you will not be able to download the WSDL descriptor from Atlassian and use it for your local JIRA installation.  You will need to use the WSDL from the local installation.

# Invoking SOAP methods #

`soap-client.el` provides the `soap-invoke` function for making SOAP calls:

```
  soap-invoke (WSDL SERVICE OPERATION-NAME &rest PARAMETERS)
```

The parameters are:

  * **WSDL** is the WSDL object as returned by `soap-load-wsdl-from-url` or `soap-load-wsdl`
  * **SERVICE** is the name of the SOAP service binding as defined in the WSDL descriptor.  Look for the `name` attribute of the `wsdl:port` node (which should be a child of the `wsdl:service` node.
  * **OPERATION-NAME** is the name of the SOAP operation to invoke.  Operations are defined under the `wsdl:portType` node in the WSDL descriptor
  * **PARAMETERS** represents the input parameters for the operation.  You will need to read the declaration for the input message corresponding to **OPERATION-NAME**

## Mapping of EMACS-LISP types to SOAP ##

When invoking SOAP methods, the input parameters and the returned value are LISP data types which must correspond to the data types described in the WSDL descriptor.  The following is the mapping used:

Basic WSDL types, **strings** and **numbers** are represented by strings and numbers in LISP.  Currently, a date type is also represented as a string.

Complex types (structures) are represented as association lists.  For example, a structure with members `a`, `b` and `c` is represented in LISP as:

```
'((a . VALUE-A)
  (b . VALUE-B)
  (c . VALUE-C))
```

Where the values can be any WSDL data type.

Array are represented differently when encoding and decoding:  When encoding a SOAP message (as input parameters to `soap-invoke`), arrays are represented as LISP vectors.  This is done so that the `soap-client.el` code can do more sanity checking.  When decoding a SOAP message (the returned value from `soap-invoke`) arrays are represented as lists.  You can create an vector from a listp list using this mechanism:

```
(apply 'vector my-list)
```
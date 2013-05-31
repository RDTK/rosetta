;;;; protocol.lisp --- Protocol provided by the model module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen  <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model)

;;; Name protocol

(defgeneric kind (thing)
  (:documentation
   "Return a keyword describing the kind of THING."))

(defgeneric name (thing)
  (:documentation
   "Return the name of THING."))

(defgeneric qname (thing)
  (:documentation
   "Return the fully qualified name of THING."))

(defgeneric qname/kind (thing)
  (:documentation
   "Return the fully qualified name of THING with components of the
form

  (NAME-COMPONENT . KIND)

where NAME-COMPONENT is a string and KIND is the `kind' of the object
corresponding to NAME-COMPONENT."))

;;; Printing qnames

(declaim (ftype (function (stream name &optional t t character)
                          (values name &rest nil))
                print-qname))

(defun print-qname (stream qname &optional colon? at? (separator #\.))
  "Print the relative or absolute qualified name QNAME (see type
`name') onto STREAM.

For relative names, a single SEPARATOR is printed in front of the
remaining printed representation.

COLON? controls the behavior in case QNAME is (:absolute). If COLON?
is non-nil, \"<root>\" is printed. Otherwise, the empty string is
printed.

SEPARATOR is printed to separate name components."
  (declare (ignore at?))

  (if *print-readably*
      (print qname stream)
      (let+ ((format (format nil "~~:[~~;~C~~]~~{~~A~~^~:*~C~~}" separator))
             ((anchor &rest components) qname)
             (relative? (eq anchor :relative)))
        (cond
          ((or relative? (not (length= 1 qname)))
           (format stream format relative? components))
          (colon?
           (format stream "<root>")))))
  qname)

(declaim (ftype (function (stream name-expression &optional t t character)
                          (values name-expression &rest nil))
                print-qname-expression))

(defun print-name-expression (stream expression
                              &optional
                              colon? at? (separator #\.))
  "Print the name expression (see type `name-expression') EXPRESSION
onto STREAM."
  (if *print-readably*
      (print expression stream)
      (etypecase expression
        (name
         (print-qname stream expression colon? at? separator))
        ((cons (eql or))
         (let ((*print-circle* nil))
           (pprint-logical-block (stream expression)
             (iter (for element in (rest expression))
                   (unless (first-iteration-p)
                     (format stream " ~:_or "))
                   (print-qname stream element colon? at? separator)))))))
  expression)

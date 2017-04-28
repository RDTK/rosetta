;;;; protocol.lisp --- Protocol provided by the model module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
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

;;; Parent protocol

(defgeneric parent (thing)
  (:documentation
   "Assuming the data type THING is contained in a composite data type,
    return that data type. Otherwise return nil.

    Note: this method does not reflect super/subtype relations like
    integer/uint32, but composition relations like structure/field or
    tuple/item.

    See: `ancestors', `root'."))

(defgeneric ancestors (thing
                       &key
                       include-self?)
  (:documentation
   "Return the list of transitive `parent's of THING.

    INCLUDE-SELF? controls whether THING is included at the beginning
    of the returned list.

    See: `parent', `root'."))

(defgeneric root (thing)
  (:documentation
   "Return the ancestor of THING which does not have a parent (the
    \"root\").

    See: `parent', `ancestors'."))

;; Default behavior

(defmethod parent ((thing t))
  ;; Default behavior is to not return a parent.
  nil)

(defmethod ancestors ((thing t)
                      &key
                      (include-self? t))
  (let ((from-parents (when-let ((parent (parent thing)))
                        (ancestors parent))))
    (if include-self? (cons thing from-parents) from-parents)))

(defmethod root ((thing t))
  (if-let ((parent (parent thing)))
    (root parent)
    thing))

;;; Builder protocol
;;;
;;; As a general convention throughout the protocol, the value nil can
;;; be used to indicate a node which should be ignored. For example,
;;; the default behavior of `add-child' is to ignore nil and return
;;; the unmodified parent.

(defgeneric find-node (builder kind
                       &rest args &key
                       if-does-not-exist
                       &allow-other-keys)
  (:documentation
   "Use BUILDER to find and return the node described by KIND and
    ARGS.

    IF-DOES-NOT-EXIST determines the behavior in case the requested
    node does not exist.

    When a requested node cannot be found and IF-DOES-NOT-EXIST is a
    function, the function is called with a `use-value' restart
    established."))

(defgeneric make-node (builder kind
                       &rest args &key &allow-other-keys)
  (:documentation
   "Use BUILDER to create and return a node described by KIND and
    ARGS."))

(defgeneric add-child (builder parent child)
  (:documentation
   "Use BUILDER to add CHILD to PARENT. Return the modified PARENT."))

;; Default behavior

(defmethod find-node :around ((builder t) (kind t)
                              &key
                              qname
                              (if-does-not-exist #'error)
                              &allow-other-keys)
  ;; Default behavior in case a requested node cannot be found.
  (or (call-next-method)
      (etypecase if-does-not-exist
        (null
         nil)
        (function
         (restart-case
             (funcall if-does-not-exist
                      (make-condition 'no-such-child
                                      :container builder
                                      :key       (list kind qname))) ; TODO condition
           (use-value (value)
             value))))))

(defmethod add-child ((builder t) (parent t) (child (eql nil)))
  parent)

(defmethod find-node ((builder (eql nil)) (kind t) &key)
  t)

(defmethod make-node ((builder (eql nil)) (kind t) &key)
  nil)

(defmethod add-child ((builder (eql nil)) (parent t) (child t))
  nil)

;;; Builder class family

(service-provider:define-service builder
  (:documentation
   "Providers of this service implement the builder protocol."))

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

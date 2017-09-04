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

;;; Composite protocol

(defgeneric composite? (thing)
  (:documentation
   "Return non-nil when THING is composed of other things."))

(defgeneric contents (container kind)
  (:documentation
   "Return a sequence of the elements in CONTAINER which are of kind
    KIND.

    If KIND is t, the returned sequence consists of all elements
    contained in CONTAINER."))

(defgeneric contents/plist (container)
  (:documentation
   "Return a plist of kinds and elements for the elements of
    CONTAINER."))

(defgeneric lookup (container kind key
                    &key
                    if-does-not-exist
                    if-exists)
  (:documentation
   "Retrieve the element of kind KIND identified by KEY,
    i.e. associated to the (KIND KEY) pair, within CONTAINER.

    IF-DOES-NOT-EXIST controls whether an error should be signaled
    if (KIND KEY) does not designate a element within CONTAINER. The
    following values are allowed:

      a function

        Make a `no-such-child' error and call IF-DOES-NOT-EXIST with
        it as the sole argument.

      nil

        nil is returned.

    IF-EXISTS is accepted for parity with the `setf' method and
    ignored."))

(defgeneric (setf lookup) (new-value container kind key
                           &key
                           if-does-not-exist
                           if-exists)
  (:documentation
   "Associate NEW-VALUE with the (KIND KEY) pair in CONTAINER.

    IF-DOES-NOT-EXIST is accepted for parity with the `lookup' method
    and ignored.

    IF-EXISTS controls the behavior in case something is already
    associated with (KIND KEY) in CONTAINER. The following values are
    allowed:

      :KEEP

        Do not modify CONTAINER and return the existing value.

      :SUPERSEDE

        Replace the existing value with NEW-VALUE.

      a function

        Make a `duplicate-child-key' error and call IF-EXISTS with it as
        the sole argument."))

(defmethod composite? ((thing t))
  nil)

(defmethod contents ((container t) (kind t))
  ;; Default behavior is to not return any contents.
  '())

(defmethod lookup ((container t)
                   (kind      t)
                   (key       t)
                   &key &allow-other-keys)
  ;; Default behavior is to not return a result.
  nil)

(defmethod lookup ((container t)
                   (kind      t)
                   (key       list)
                   &key &allow-other-keys)
  (cond
    ;;
    ((and (typep key 'name/absolute) (root container))
     (lookup (root container) kind (cons :relative (rest key))
             :if-does-not-exist nil))

    ;; If KEY is not a relative name, we cannot do anything with it =>
    ;; call next method (which is probably the default behavior of
    ;; just returning nil).
    ((not (typep key 'name/relative))
     (call-next-method))

    ;; A relative name without components refers to the context object
    ;; itself => return CONTAINER.
    ((length= 1 key)
     container)

    ;; A relative name with a single component => we can perform a
    ;; direct lookup in CONTAINER.
    ((length= 2 key)
     (lookup container kind (second key)
             :if-does-not-exist nil))

    ;; A relative name with more than one component => lookup first
    ;; name component and recur on the result and remaining
    ;; components.
    (t
     (when-let ((parent (lookup container t (second key)
                                :if-does-not-exist nil)))
       (lookup parent kind (cons :relative (nthcdr 2 key))
               :if-does-not-exist nil)))))

(defmethod lookup :around ((container t)
                           (kind      t)
                           (key       t)
                           &key
                           (if-does-not-exist #'error)
                           &allow-other-keys)
  (let+ (((&flet handle-does-not-exist (&optional condition)
            (declare (ignore condition))
            (etypecase if-does-not-exist
              (null
               (return-from lookup nil))
              (function
               (restart-case
                   (funcall if-does-not-exist
                            (make-condition 'no-such-child
                                            :container container
                                            :key       (list kind key)))
                 (use-value (value)
                   :report (lambda (stream)
                             (format stream "~@<Use a given value that ~
                                             should be used in place ~
                                             of the missing ~
                                             value.~@:>"))
                   :interactive (lambda ()
                                  (format *query-io* "Value (evaluated): ")
                                  (finish-output *query-io*)
                                  (list (eval (read *query-io*))))
                   value)
                 (store-value (value)
                   :report (lambda (stream)
                             (format stream "~@<Store a value to be ~
                                             used in place of the ~
                                             missing value.~@:>"))
                   :interactive (lambda ()
                                  (format *query-io* "Replacement value (evaluated): ")
                                  (finish-output *query-io*)
                                  (list (eval (read *query-io*))))
                   (setf (lookup container kind key) value))))))))
    (or (handler-bind
            (((or simple-error no-such-child) #'handle-does-not-exist))
          (call-next-method))
        (handle-does-not-exist))))

(defmethod (setf lookup) :around ((new-value  t)
                                  (container  t)
                                  (kind       t)
                                  (key        t)
                                  &rest args
                                  &key
                                  (if-exists #'error)
                                  &allow-other-keys)
  (when-let ((existing (apply #'lookup container kind key
                              :if-does-not-exist nil
                              (remove-from-plist
                               args :if-exists :if-does-not-exist))))
    (etypecase if-exists
      ((eql :keep)
       (return-from lookup existing))
      ((eql :supersede))
      (function
       (restart-case
           (funcall if-exists
                    (make-condition 'duplicate-child-key
                                    :container container
                                    :key       (list kind key)))
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Replace the existing value ~S ~
                                     with ~S.~@:>"
                             existing new-value))
           (declare (ignore condition)))
         (keep ()
           :report (lambda (stream)
                     (format stream "~@<Keep the existing value ~
                                     ~S.~@:>"
                             existing))
           (return-from lookup existing))))))

  (call-next-method))

(defgeneric query (container kind key)
  (:documentation
   "Retrieve the element of kind KIND identified by KEY,
    i.e. associated to the (KIND KEY) pair, within CONTAINER.

    KIND can be a symbol or a list of the form

      (OR ALTERNATIVE1 ALTERNATIVE2 ...)

    KEY can be usually be a `cl:string', a `name/relative' or a
    `name/absolute'. In addition, KEY can be a list of the form

      (OR ALTERNATIVE1 ALTERNATIVE2 ...)"))

(defmethod query ((container t)
                  (kind      t)
                  (key       t))
  (lookup container kind key :if-does-not-exist nil))

(defmethod query ((container t)
                  (kind      t)
                  (key       list))
  (if (eq (first key) 'or)
      (some (curry #'query container kind) (rest key))
      (call-next-method)))

(defmethod query ((container t)
                  (kind      list)
                  (key       t))
  (some (lambda (kind) (query container kind key)) kind))

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

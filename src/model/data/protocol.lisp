;;;; protocol.lisp --- Protocol for data types.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

;;; Documentation protocol

(defgeneric documentation1 (thing)
  (:documentation
   "Return the documentation string associated to THING."))

(intern "DATA-TYPE")

(defmethod documentation ((thing t) (type (eql 'data-type)))
  "Return documentation for data type THING when asked for
documentation of type 'data-type."
  (documentation1 thing))

;;; Composite data type protocol

(defgeneric contents (container kind)
  (:documentation
   "Return a sequence of the elements in CONTAINER which are of kind
KIND.

If KIND is t, the returned sequence consists of all elements contained
in CONTAINER."))

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

    Make a `no-such-child' error and call IF-DOES-NOT-EXIST with it as
    the sole argument.

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

IF-DOES-NOT-EXIST is accepted for parity with the `lookup' method and
ignored.

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

(defmethod contents ((container t) (kind t))
  "Default behavior is to not return any contents."
  nil)

(defmethod lookup ((container t)
                   (kind      t)
                   (key       t)
                   &key &allow-other-keys)
  "Default behavior is to not return a result."
  nil)

(defmethod lookup ((container t)
                   (kind      t)
                   (key       list)
                   &key &allow-other-keys)
  (cond
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
                                            :type container
                                            :key  (list kind key)))
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
                                  &key
                                  (if-exists #'error)
                                  &allow-other-keys)
  (when-let ((existing (lookup container kind key
                               :if-does-not-exist nil)))
    (etypecase if-exists
      ((eql :keep)
       (return-from lookup existing))
      ((eql :supersede))
      (function
       (restart-case
           (funcall if-exists
                    (make-condition 'duplicate-child-key
                                    :type container
                                    :key  (list kind key)))
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

INCLUDE-SELF? controls whether THING is included at the beginning of
the returned list.

See: `parent', `root'."))

(defgeneric root (thing)
  (:documentation
   "Return the ancestor of THING which does not have a parent (the
\"root\").

See: `parent', `ancestors'."))

(defgeneric composite? (type)
  (:documentation
   "Return non-nil when the data type TYPE is in some way composed of
other types."))

(defmethod composite? ((type t))
  nil)

;;; Typed protocol

(defgeneric type1 (thing)
  (:documentation
   "Return a type instance representing the type of THING."))

;;; Value validation protocol

(defgeneric validate-value (type value
                            &key
                            if-invalid)
  (:documentation
   "Check whether VALUE is valid for TYPE.

 Return non-nil, if VALUE is valid for TYPE. If VALUE is invalid for
TYPE (depending on IF-INVALID), return two values: nil and a
`value-invalid-for-type' condition.

IF-INVALID controls the behavior in case VALUE is invalid for
TYPE. Valid values are nil or a function which can be called with a
condition object (of type `valid-invalid-for-type'). If IF-INVALID is
a function, a `cl:continue' restart is established around the call."))

(defmethod validate-value :around ((type t) (value t)
                                   &key
                                   (if-invalid #'error))
  (let+ (((&flet make-error (&optional cause)
            (apply #'make-condition 'value-invalid-for-type
                   :type  type
                   :value value
                   (when cause
                     (list :cause cause)))))
         ((&flet handle-invalid (&optional cause)
            (etypecase if-invalid
              (null
               (return-from validate-value (values nil (make-error cause))))
              (function
               (restart-case
                   (funcall if-invalid (make-error cause))
                 (continue ()
                   :report (lambda (stream)
                             (format stream "~@<Ignore the ~
                                             incompatibility.~@:>"))
                   t)))))))
    (or (handler-bind
            (((or simple-error value-invalid-for-type) #'handle-invalid))
          (call-next-method))
        (handle-invalid))))

(defmethod validate-value ((type t) (value t)
                           &key &allow-other-keys)
  nil)

;;; Dependency protocol

(defgeneric direct-dependencies (thing)
  (:method-combination append)
  (:documentation
   "Return a duplicate-free list of things on which THING directly
depends. For example, a structure depends on the types of its fields
and an array type depends on its element type."))

(defgeneric dependencies (thing
                          &key
                          include-self?
                          blacklist)
  (:documentation
   "Return a duplicate-free list of things on which THING (directly or
indirectly) depends. The set of dependencies is determined as the
transitive closure with respect to data-type dependencies. For
example, a structure depends on the types of its fields and an array
type depends on its element type. Cyclic relations (when THING depends
on itself through intermediate dependencies) can be processed.

INCLUDE-SELF? controls whether THING should be included in the
returned set.

BLACKLIST can be (), a function of one argument, a sequence of
blacklisted objects or a list or the form

  (or BLACKLIST1 BLACKLIST2 ...)"))

;; Default behavior

(defmethod direct-dependencies append ((thing t))
  '())

(defmethod direct-dependencies :around ((thing t))
  (remove thing (remove-duplicates (call-next-method) :test #'eq)
          :test #'eq))

(defmethod dependencies ((thing t)
                         &key
                         (include-self? t)
                         blacklist)
  (let+ ((seen (make-hash-table :test #'eq))
         ((&labels blacklisted? (thing blacklist)
            (etypecase blacklist
              (null
               nil)
              (function
               (funcall blacklist thing))
              ((cons (eql or))
               (mapcar (curry #'blacklisted? thing) (rest blacklist)))
              (sequence
               (find thing blacklist)))))
         ((&labels do-thing (thing)
            (cond
              ((blacklisted? thing blacklist)
               (return-from do-thing))
              ((gethash thing seen)
               (return-from do-thing))
              (t
               (setf (gethash thing seen) t)))
            (cons thing (mappend #'do-thing (direct-dependencies thing))))))
    ;; If requested, remove THING from an arbitrary position. It can
    ;; end up there because of cycles.
    (if include-self?
        (do-thing thing)
        (remove thing (do-thing thing)))))

;;; Fundamental type protocol

(defgeneric fundamental? (type)
  (:documentation
   "Return non-nil when TYPE is a fundamental data type."))

(defgeneric category (type)
  (:documentation
   "Return a symbol indicating the category of the fundamental data
type TYPE. Categories include :bool, :integer, :float, :string,
etc."))

(defgeneric width (type)
  (:documentation
   "Return the width in bits required to store values of data type
TYPE, if it is a fixed-width data type."))

(defgeneric signed? (type)
  (:documentation
   "Return non-nil if the integer data type TYPE is signed, that is
allows negative values."))

(defgeneric encoding (type)
  (:documentation
   "Return of string data type TYPE."))

(defmethod fundamental? ((type t))
  (eq (kind type) :fundamental))

;;; Field protocol for structure-like data types

(defgeneric optional? (field)
  (:documentation
   "Return non-nil if FIELD does not have to be present in
realizations of its containing data type."))

(defmethod optional? ((field t))
  nil)

;;; Array protocol

(defgeneric element-type (array)
  (:documentation
   "Return the type object corresponding to the element type of
ARRAY."))

(defgeneric index-type (array)
  (:documentation
   "Return the type object corresponding to the index type of
ARRAY."))

(defgeneric fixed-size? (type)
  (:documentation
   "Return non-nil if the array type described by TYPE has a fixed
number of elements."))

;;; Singleton value protocol
;;;
;;; This protocol is provided by types whose extension is a single
;;; object.

(defgeneric value (singleton)
  (:documentation
   "Return the value of SINGLETON."))

;;; Forward reference protocol

(defgeneric upgrade! (instance other)
  (:documentation
   "Upgrade the forward reference INSTANCE to OTHER by changing its
class to the class of OTHER and copying all slot values from OTHER to
INSTANCE."))

;;; Builder protocol

(defgeneric find-node (builder kind
                       &rest args
                       &key
                       if-does-not-exist
                       &allow-other-keys)
  (:documentation
   "Use BUILDER to find and return the node described by KIND and
ARGS.

IF-DOES-NOT-EXIST determines the behavior in case the requested node
does not exist.

When a requested node cannot be found and IF-DOES-NOT-EXIST is a
function, the function is called with a `use-value' restart
established."))

(defgeneric make-node (builder kind
                       &rest args
                       &key &allow-other-keys)
  (:documentation
   "Use BUILDER to create and return a node described by KIND and
ARGS."))

(defgeneric add-child (builder parent child)
  (:documentation
   "Use BUILDER to add CHILD to PARENT. Return the modified PARENT."))

(defmethod find-node :around ((builder t) (kind t)
                              &key
                              qname
                              (if-does-not-exist #'error)
                              &allow-other-keys)
  "Default behavior in case a requested node cannot be found."
  (or (call-next-method)
      (etypecase if-does-not-exist
        (null
         nil)
        (function
         (restart-case
             (funcall if-does-not-exist
                      (make-condition 'no-such-child
                                      :type builder
                                      :key  (list kind qname))) ; TODO condition
           (use-value (value)
             value))))))

;;; Builder class family

(intern "BUILDER") ;; (documentation 'builder 'dynamic-classes:class-family)

(dynamic-classes:define-findable-class-family builder
  "This class family consists of classes whose instances implement the
builder protocol.")

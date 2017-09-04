;;;; protocol.lisp --- Protocol for data types.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
            (return-from validate-value
              (etypecase if-invalid
                (null
                 (values nil (make-error cause)))
                (function
                 (restart-case
                     (funcall if-invalid (make-error cause))
                   (continue ()
                     :report (lambda (stream)
                               (format stream "~@<Ignore the ~
                                               incompatibility.~@:>"))
                     t))))))))
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
               (some (curry #'blacklisted? thing) (rest blacklist)))
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

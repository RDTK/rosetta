;;;; conditions.lisp --- Conditions used in the model.data module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

(define-condition data-type-error (error)
  ((type :initarg  :type
         :reader   data-type-error-type
         :documentation
         "Stores the data type object for which the error occurred."))
  (:documentation
   "This error condition class is intended to used as a superclass for
type-related error condition classes."))

;;; Value-validation errors

(define-condition value-invalid-for-type (data-type-error
                                          chainable-condition)
  ((value :initarg  :value
          :reader   data-type-error-value
          :documentation
          "Stores the invalid value."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The value ~S is invalid for type ~
~A~/more-conditions::maybe-print-cause/~@:>"
             (data-type-error-value condition)
             (data-type-error-type  condition)
             condition)))
  (:documentation
   "This error is signaled when an attempt is made to use a value that
is invalid for a given data type."))

(defun value-invalid-for-type (type value &optional cause)
  "Signal a `value-invalid-for-type' error for TYPE and VALUE and
optionally CAUSE."
  (apply #'error 'value-invalid-for-type
         :type  type
         :value value
         (when cause
           (list :cause cause))))

;;; Relation errors

(define-condition child-error (data-type-error)
  ((key :initarg  :key
        :reader   data-type-error-key
        :documentation
        ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Child ~S caused an error within type ~S.~@:>"
             (data-type-error-key  condition)
             (data-type-error-type condition))))
  (:documentation
   "This class serves as a superclass for errors related to composite
types and their children."))

(define-condition chainable-child-error (child-error
                                         chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Child ~S caused an error within type ~
~S~/more-conditions::maybe-print-cause/~@:>"
             (data-type-error-key  condition)
             (data-type-error-type condition)
             condition)))
  (:documentation
   "This error is signaled when a child-related error is caused by
some other condition. It can also serve as a superclass for more
specific child-related errors."))

(defun chainable-child-error (type child &optional cause)
  "Signal a `chainable-child-error' for TYPE and CHILD which has been
caused by CAUSE."
  (apply #'error 'chainable-child-error
         :type type
         :key  child
         (when cause
           (list :cause cause))))

(define-condition simple-child-error (child-error
                                      simple-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Error for child ~S in type ~A: ~?~@:>"
             (data-type-error-key               condition)
             (data-type-error-type              condition)
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition))))
  (:documentation
   "This condition can be used to signal child-related error which
include a simple description of the error."))

(defun simple-child-error (type child format-control
                           &rest format-arguments)
  "Signal a `simple-child-error' for TYPE and CHILD with a report
generated from FORMAT-CONTROL and FORMAT-ARGUMENTS."
  (error 'simple-child-error
         :type             type
         :key              child
         :format-control   format-control
         :format-arguments format-arguments))

(define-condition no-such-child (child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The requested child type ~S could not be found ~
within the type ~S.~@:>"
             (data-type-error-key  condition)
             (data-type-error-type condition))))
  (:documentation
   "This error is signaled when a requested child type cannot be found
within the specified container type."))

(defun no-such-child (type key)
  "Signal a `no-such-child' error for TYPE and KEY."
  (error 'no-such-child :type type :key key))

(define-condition duplicate-child-key (child-error)
  ((existing :initarg  :existing
             :reader   data-type-error-existing
             :initform nil
             :documentation
             ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The child key ~S is already in use~@[ by child ~
~A~] within type ~S.~@:>"
             (data-type-error-key      condition)
             (data-type-error-existing condition)
             (data-type-error-type     condition))))
  (:documentation
   "This error is signaled when a attempt is made to add child to a
composite structure using a key that which is already in use."))

(defun duplicate-child-key (type key &optional existing)
  "Signal a `duplicate-child-key' error for TYPE, KEY and optionally
EXISTING."
  (apply #'error 'duplicate-child-key
         :type type
         :key  key
         (when existing
           (list :existing existing))))

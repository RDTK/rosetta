;;;; conditions.lisp --- Conditions used in the model.data module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

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

(define-condition no-such-child (child-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The requested child type ~S could not be found ~
                     within the type ~S.~@:>"
             (child-condition-key           condition)
             (container-condition-container condition))))
  (:documentation
   "This error is signaled when a requested child type cannot be found
within the specified container type."))

(defun no-such-child (type key)
  "Signal a `no-such-child' error for TYPE and KEY."
  (error 'no-such-child :container type :key key))

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
             (child-condition-key           condition)
             (data-type-error-existing      condition)
             (container-condition-container condition))))
  (:documentation
   "This error is signaled when a attempt is made to add child to a
composite structure using a key that which is already in use."))

(defun duplicate-child-key (type key &optional existing)
  "Signal a `duplicate-child-key' error for TYPE, KEY and optionally
EXISTING."
  (apply #'error 'duplicate-child-key
         :container type
         :key       key
         (when existing
           (list :existing existing))))

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

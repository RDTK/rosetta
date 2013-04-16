;;;; conditions.lisp --- Conditions used by the model.serialization module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.serialization)

(define-condition type-invalid-for-mechanism (error
                                              chainable-condition)
  ((mechanism :initarg  :mechanism
              :reader   mechanism
              :documentation
              "Stores the mechanism for which the data type was
invalid.")
   (type      :initarg  :type
              :reader   type1
              :documentation
              "Stores the type that was invalid for the mechanism."))
  (:default-initargs
   :mechanism (missing-required-initarg 'type-invalid-for-mechanism :mechanism)
   :type      (missing-required-initarg 'type-invalid-for-mechanism :type))
  (:report
   (lambda (condition stream)
     (format stream "~@<Type ~A is invalid for mechanism ~
~A~/more-conditions::maybe-print-cause/~@:>"
             (type1     condition)
             (mechanism condition)
             condition)))
  (:documentation
   "This error is signaled when an attempt is made to use a data-type
in conjunction with a mechanism for which the data-type is not
suitable."))

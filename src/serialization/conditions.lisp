;;;; conditions.lisp --- Conditions used in the serialization module of rosetta.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.serialization)

(define-condition serialization-error (error)
  ((mechanism   :initarg  :mechanism
                :type     standard-object
                :reader   serialization-error-mechanism
                :documentation
                "The serialization mechanism that was used in the
failed serialization of deserialization operation.")
   (source      :initarg  :source
                :reader   serialization-error-source
                :documentation
                "The source of the failed serialization or
deserialization operation.")
   (destination :initarg  :destination
                :reader   serialization-error-destination
                :documentation
                "The destination of the failed serialization or
deserialization operation."))
  (:report
   (lambda (condition stream)
     (format stream "~@<An error occurred when serializing ~A to ~A ~
using mechanism ~A.~@:>"
             (serialization-error-source      condition)
             (serialization-error-destination condition)
             (serialization-error-mechanism   condition))))
  (:documentation
   "Errors of this type and subtypes when a serialization or
deserialization operation fails."))

(define-condition pack-error (serialization-error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an error
occurs during packing \(serialization)."))

(define-condition unpack-error (serialization-error)
  ()
  (:documentation
   "Conditions of this type and subtypes are signaled when an error
occurs during unpacking \(deserialization)."))

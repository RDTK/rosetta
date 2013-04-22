;;;; protocol.lisp --- Protocol functions for serialization model elements.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.serialization)

;;; Mechanism protocol

(defgeneric name (mechanism)
  (:documentation
   "Return the name of the serialization mechanism MECHANISM."))

(defgeneric wire-type (mechanism)
  (:documentation
   "Return the wire-type of the serialization mechanism MECHANISM. The
returned object models a type."))

(defgeneric offset-type (mechanism)
  (:documentation
   "Return the type used by the serialization mechanism MECHANISM to
represent offsets in binary buffers. The returned object models a
type."))

(defgeneric length-type (mechanism)
  (:documentation
   "Return the type used by the serialization mechanism MECHANISM to
represent lengths of arrays. The returned object models a type."))

(defgeneric endian-for (mechanism type)
  (:documentation
   "Return an `endian-designator' designating the endian that should
be used on the wire for the combination of MECHANISM and TYPE."))

(defgeneric validate-type (mechanism type
                           &key
                           if-invalid)
  (:documentation
   "Check whether TYPE can be processed by MECHANISM.

Return non-nil, if TYPE can be processed by MECHANISM. If TYPE is
invalid for MECHANISM (depending on IF-INVALID), return two values:
nil and potentially a condition describing the cause.

IF-INVALID controls the behavior in case TYPE cannot be processed by
MECHANISM. Valid values are nil or a function which can be called with
a condition object (of type `type-invalid-for-mechanism'). If
IF-INVALID is a function, a `cl:continue' restart is established
around the call."))

(defmethod name ((mechanism t))
  (class-name (class-of mechanism)))

(defmethod endian-for ((mechanism t) (type t))
  "Default behavior consists in using little endian."
  :little-endian)

(declaim (special *seen*))

(defvar *seen* nil
  "During `validate-type' calls, stores a `hash-table' which is used
to detect and process recursive types.")

(defmethod validate-type :around ((mechanism t)
                                  (type      t)
                                  &key
                                  (if-invalid #'error))
  (let+ ((*seen* (or *seen* (make-hash-table :test #'eq)))
         ((&values seen? found?) (gethash type *seen*))
         ((&flet make-error (&optional cause)
            (apply #'make-condition 'type-invalid-for-mechanism
                   :mechanism  mechanism
                   :type       type
                   (when cause
                     (list :cause cause)))))
         ((&flet handle-invalid (&optional cause)
            (etypecase if-invalid
              (null
               (return-from validate-type (values nil (make-error cause))))
              (function
               (restart-case
                   (funcall if-invalid (make-error cause))
                 (continue ()
                   :report (lambda (stream)
                             (format stream "~@<Ignore the incompatibility.~@:>"))
                   t)))))))

    ;; If we already processed TYPE or are currently processing TYPE
    ;; in recursive VALIDATE-TYPE calls, return the result which has
    ;; been stored by the previous/outer call. For the outer call,
    ;; this ensures that recursive appearances of TYPE do not lead to
    ;; validation failure.
    (when found?
      (return-from validate-type seen?))
    (setf (gethash type *seen*) t)

    ;; Call the next method which either returns a generalized Boolean
    ;; or signals an error.
    (or (handler-bind
            (((or simple-error type-invalid-for-mechanism)
               #'handle-invalid))
          (call-next-method))
        (handle-invalid))))

(defmethod validate-type ((mechanism t)
                          (type      t)
                          &key &allow-other-keys)
  nil)

(defmethod validate-type ((mechanism t)
                          (type      composite-mixin)
                          &key &allow-other-keys)
  (every (curry #'validate-type mechanism) (contents type t)))

(defmethod validate-type ((mechanism t)
                          (type      typed-mixin)
                          &key &allow-other-keys)
  (validate-type mechanism (rs.m.d:type1 type)))

(defmethod validate-type ((mechanism t)
                          (type      array-mixin)
                          &key &allow-other-keys)
  (validate-type mechanism (element-type type)))

;;; Mechanisms

(intern "MECHANISM") ; for (documentation MECHANISM 'rosetta.model.serialization:mechanism)

(dynamic-classes:define-findable-class-family mechanism
    "This family consists of serialization mechanism classes. Each
serialization mechanism class represents a serialization mechanism
with respect to its principal properties. Instances may in addition
store specific parameters of a serialization mechanism \(like
indentation of output for an XML-based mechanism).")

(defmethod documentation ((thing symbol) (type (eql 'mechanism)))
  "Obtain documentation of type MECHANISM from the mechanism class
designated by THING."
  (documentation (find-mechanism-class thing) t))

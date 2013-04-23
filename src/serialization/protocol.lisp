;;;; protocol.lisp --- Serialization protocol of the rosetta system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.serialization)

;;; Serialization and deserialization protocol

(defgeneric packed-size (mechanism object &key &allow-other-keys)
  (:documentation
   "Compute the number of octets that are required to represent OBJECT
when represented in the serialization format of MECHANISM."))

(defgeneric pack (mechanism source destination &key &allow-other-keys)
  (:documentation
   "Convert SOURCE to a representation in the serialization format of
MECHANISM and store it in DESTINATION.
Two values are returned: the number of emitted octets or nil and
DESTINATION."))

(defgeneric pack* (mechanism source &key &allow-other-keys)
  (:documentation
   "Convenience function for `pack' that does not take destination
argument and tries to figure out suitable default destination. The
created destination is returned."))

(defgeneric unpack (mechanism source destination &key &allow-other-keys)
  (:documentation
   "Decode the object that is stored in SOURCE in the serialization
format of MECHANISM into DESTINATION.
Two values are returned: the modified DESTINATION (or a new object)
and the number of consumed octets or nil."))

;;; Default behavior

(defmethod pack* ((mechanism t) (source t)
                  &rest args &key)
  "Default behavior is to use a nil destination and return the created
destination instead of the amount of produced data."
  (nth-value 1 (apply #'pack mechanism source nil args)))

;;; Partial deserializtion protocol

(defgeneric location (mechanism source schema query
                      &key &allow-other-keys)
  (:documentation
   "Find and return the location in SOURCE, which is assumed to be
encoded in the serialization format of MECHANISM, at which the first
thing matches QUERY which describes a part of SCHEMA."))

(defgeneric extract (mechanism source schema query destination
                     &key &allow-other-keys)
  (:documentation
   "In SOURCE, which is assumed to be encoded in the serialization
format of MECHANISM, find the first thing matching QUERY which
describes a part of SCHEMA and store it in DESTINATION."))

;;; Mechanism class lookup

(macrolet
    ((define-mechanism-lookup (method args)
       (let ((args/typed (mapcar #'list args (circular-list t))))
         `(progn
            (defmethod ,method ((mechanism list) ,@args/typed
                                &rest rest-args &key)
              (let+ (((mechanism-name &rest mechanism-args) mechanism)
                     (mechanism-class    (find-mechanism-class mechanism-name))
                     (mechanism-instance
                      (apply #'make-instance mechanism-class mechanism-args)))
                (apply #',method mechanism-instance ,@args rest-args)))

            (defmethod ,method ((mechanism symbol) ,@args/typed
                                &rest rest-args &key)
              (apply #',method (list mechanism) ,@args rest-args))))))

  (define-mechanism-lookup packed-size (source))
  (define-mechanism-lookup pack        (source destination))
  (define-mechanism-lookup pack*       (source))
  (define-mechanism-lookup unpack      (source destination))
  (define-mechanism-lookup location    (source schema query))
  (define-mechanism-lookup extract     (source schema query destination)))

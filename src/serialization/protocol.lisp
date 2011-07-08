;;; protocol.lisp --- Serialization protocol of the cl-rosetta system.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rosetta.serialization)


;;; Mechanisms
;;

(intern "MECHANISM") ;; for (documentation :MECHANISM 'rosetta.backend:mechanism)

(dynamic-classes:define-findable-class-family mechanism
    "This family consists of serialization mechanism classes. Each
serialization mechanism class represents a serialization mechanism
with respect to its principal properties. Instances may in addition
store specific parameters of a serialization mechanism \(like
indentation of output for an XML-based mechanism).")

(defmethod documentation ((thing symbol) (type (eql 'mechanism)))
  "Obtain documentation of type TARGET from the target class
designated by THING."
  (documentation (find-mechanism-class thing) t))


;;; Serialization and deserialization protocol
;;

(defgeneric packed-size (mechanism object &key &allow-other-keys)
  (:documentation
   "Compute the number of octets that are required to represent OBJECT
when represented in the serialization format of MECHANISM."))

(defgeneric pack (mechanism source destination &key &allow-other-keys)
  (:documentation
   "Convert SOURCE to a representation in the serialization format of
MECHANISM and store it in DESTINATION.
Two values are returned: the number of emitted octets and
DESTINATION."))

(defgeneric pack* (mechanism source &key &allow-other-keys)
  (:documentation
   "Convenience function for `pack' that does not take destination
argument and tries to figure out suitable default destination. The
created destination is returned as the first value."))

(defgeneric unpack (mechanism source destination &key &allow-other-keys)
  (:documentation
   "Decode the object that is stored in SOURCE in the serialization
format of MECHANISM into DESTINATION.
Two values are returned: the modified DESTINATION (or a new object)
and the number of consumed octets."))


;;; Mechanism class lookup
;;


(macrolet
    ((define-mechanism-lookup (method args)
       (let ((args/typed
	      (map 'list #'list
		   args (make-list (length args) :initial-element t))))
	 `(progn
	    (defmethod ,method ((mechanism list) ,@args/typed
				&rest rest-args
				&key)
	      (bind (((mechanism-name &rest mechanism-args) mechanism)
		     (mechanism-class    (find-mechanism-class mechanism-name))
		     (mechanism-instance
		      (apply #'make-instance mechanism-class mechanism-args)))
		(apply #',method mechanism-instance ,@args rest-args)))

	    (defmethod ,method ((mechanism symbol) ,@args/typed
				&rest rest-args
				&key)
	      (apply #',method (list mechanism) ,@args rest-args))))))

  (define-mechanism-lookup packed-size (source))
  (define-mechanism-lookup pack        (source destination))
  (define-mechanism-lookup pack*       (source))
  (define-mechanism-lookup unpack      (source destination)))


;;; Default behavior
;;

(defmethod pack* ((mechanism t) (source t)
		  &rest args
		  &key)
  "Default behavior is to use a nil destination and return the created
destination instead of the amount of produced data."
  (nth-value 1 (apply #'pack mechanism source nil args)))

(defmethod unpack ((mechanism t) (source t) (destination class)
		   &rest args &key)
  "Make an instance of the class DESTINATION and load SOURCE into the
instance."
  (apply #'unpack mechanism source (make-instance destination) args))

(defmethod unpack ((mechanism t) (source t) (destination symbol)
		   &rest args &key)
  "Make an instance of the class designated by DESTINATION and load
SOURCE into the instance."
  (apply #'unpack mechanism source (find-class destination) args))


;;; Partial deserializtion protocol
;;

(defgeneric location (mechanism source schema part
		      &key &allow-other-keys)
  (:documentation
   "Find and return the location in SOURCE at which the first instance
of the PART of SCHEMA occurs."))

(defgeneric extract (mechanism source schema part
		     &key &allow-other-keys)
  (:documentation
   "Extract and return the value which PART of SCHEMA has in SOURCE."))

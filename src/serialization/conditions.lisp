;;; conditions.lisp --- Conditions used in the serialization module of cl-rosetta.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rosetta.serialization)

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

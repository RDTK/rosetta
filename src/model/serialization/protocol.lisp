;;; protocol.lisp --- Protocol functions for serialization model elements.
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

(cl:in-package :rosetta.model.serialization)


;;; Mechanism protocol
;;

(defgeneric name (mechanism)
  (:documentation
   "Return the name of the serialization mechanism MECHANISM."))

(defgeneric wire-type (mechanism)
  (:documentation
   "Return the wire-type of the serialization mechanism MECHANISM. The
returned object models a type."))

(defgeneric validate-type (mechanism type
			   &key
			   if-invalid)
  (:documentation
   "Check whether the type represented by TYPE can be processed by the
serialization mechanism represented by MECHANISM. Return non-nil, if
TYPE can be processed by MECHANISM.

IF-INVALID controls the behavior in case TYPE cannot be processed by
MECHANISM. Valid values are nil or a function which can be called with
a condition object."))

(defmethod name ((mechanism t))
  (class-name (class-of mechanism)))

(defmethod validate-type :around ((mechanism t)
				  (type      t)
				  &key
				  (if-invalid #'error))
  (or (call-next-method)
      (etypecase if-invalid
	(null
	 nil)
	(function
	 (restart-case
	     (funcall if-invalid
		      (make-condition
		       'simple-error
		       :format-control   "~@<Type ~A is invalid for ~
mechanism ~A.~@:>"
		       :format-arguments (list type mechanism)))
	   (continue ()
	     :report (lambda (stream)
		       (format stream "~@<Ignore the incompatibility ~
and try to continue.~@:>"))
	     t))))))

(defmethod validate-type ((mechanism t)
			  (type      t)
			  &key &allow-other-keys)
  nil)

(defmethod validate-type ((mechanism t)
			  (type      composite-mixin)
			  &key &allow-other-keys)
  (every (curry #'validate-type mechanism)
	 (contents type t)))

(defmethod validate-type ((mechanism t)
			  (type      typed-mixin)
			  &key &allow-other-keys)
  (validate-type mechanism (type1 type)))


;;; Mechanisms
;;

(intern "MECHANISM") ;; for (documentation :MECHANISM 'rosetta.serialization:mechanism)

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

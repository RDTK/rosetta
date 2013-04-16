;;;; mixins.lisp --- Mixin classes used in the model.serialization module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.serialization)


;;; `wire-type-mixin' mixin class
;;

(defclass wire-type-mixin ()
  ((wire-type :initarg  :wire-type
	      :reader   wire-type
	      :documentation
	      "Stores the wire-type to/from the which the
corresponding mechanism serializes/deserializes."))
  (:default-initargs
   :wire-type (missing-required-initarg 'wire-type-mixin :wire-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
wire-type slot describing the wire-type to/from which the
corresponding mechanism serializes/deserializes."))


;;; `offset-type-mixin' mixin class
;;

(defclass offset-type-mixin ()
  ((offset-type :initarg  :offset-type
		:reader   offset-type
		:documentation
		"Stores the type of offsets in binary buffers used by
the corresponding serialization mechanism."))
  (:default-initargs
   :offset-type (missing-required-initarg 'offset-type-mixin :offset-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
offset-type slot describing the type of offset in binary buffers used
by the corresponding serialization mechanism."))


;;; `length-type-mixin' mixin class
;;

(defclass length-type-mixin ()
  ((length-type :initarg  :length-type
		:reader   length-type
		:documentation
		"Stores the array length type used by the
corresponding serialization mechanism."))
  (:default-initargs
   :length-type (missing-required-initarg 'length-type-mixin :length-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
length-type slot describing the array length type used by the
corresponding serialization mechanism."))


;;; `constant-endian-mixin' mixin class
;;

(defclass constant-endian-mixin ()
  ((endian :initarg  :endian
	   :type     endian-designator
	   :reader   endian
	   :documentation
	   "Stores the fixed endian used by the mechanism."))
  (:default-initargs
   :endian (missing-required-initarg 'constant-endian-mixin :endian))
  (:documentation
   "This class is intended to be mixed into mechanism classes which
use one fixed endian on the wire."))

(defmethod endian-for ((mechanism constant-endian-mixin)
		       (type      t))
  (endian mechanism))

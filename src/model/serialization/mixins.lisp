;;; mixins.lisp --- Mixin classes used in the model.serialization module.
;;
;; Copyright (C) 2012 Jan Moringen
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

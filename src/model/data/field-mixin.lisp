;;; field-mixin.lisp --- A mixin class for field-like classes.
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

(cl:in-package :rosetta.model.data)

(defclass field-mixin (named-mixin)
  ((name :accessor field-name)
   (type :initarg  :type
	 :accessor field-type
	 :documentation
	 "Stores the type of values of the field."))
  (:default-initargs
   :type (missing-required-initarg 'field-mixin :type))
  (:documentation
   "This class is intended to be mixed into data type classes that
represent fields of structure-like composite data types. Note that
instances of subclasses of this class do not generally represent data
types."))

(defmethod print-items append ((object field-mixin))
  "Try to enforce a meaningful order of the name and type print
items."
  (list (list :name (field-name object) "~S")
	(list :type (data-type-name (field-type object)) " : ~A")))

;;; protocol.lisp --- Protocol for data types.
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


;;; Data type protocol
;;

(defgeneric data-type-name (type)
  (:documentation
   "Return the name of the data-type TYPE."))

(defgeneric data-type-documentation (type)
  (:documentation
   "Return the documentation string associated to the data type
TYPE."))


;;; Default behavior
;;

(intern "DATA-TYPE")

(defmethod documentation ((thing t) (type (eql 'data-type)))
  "Return documentation for data type THING when asked for
documentation of type 'data-type."
  (data-type-documentation thing))


;;; Composite data type protocol
;;

(defgeneric data-type-composite? (type)
  (:documentation
   "Return non-nil when the data type TYPE is in some way composed of
other types."))

(defgeneric data-type-parent (type)
  (:documentation
   "Assuming the data type TYPE is contained in a composite data type,
return that data type. Otherwise return nil.

Note: this method does not reflect super/subtype relations like
integer/uint32, but composition relations like structure/field or
tuple/item."))

(defgeneric composite-children (type)
  (:documentation
   "Return a sequence of the child data types of the composite data
type TYPE. "))

(defgeneric composite-child (type key
			     &key
			     error?)
  (:documentation
   "Retrieve the child identified by KEY of the composite data type
TYPE.
ERROR? controls whether an error should be signaled if KEY does not
designate a child within TYPE. If ERROR? is non-nil and the requested
child does not exist, an error of type `no-such-child' is signaled. If
ERROR? is nil, nil is returned in that case."))

(defmethod composite-child :around ((type t) (key t)
				    &key
				    (error? t))
  (or (call-next-method)
      (when error?
	(error 'no-such-child
	       :key  key
	       :type type))))


;;; Field protocol for structure-like data types
;;

(defgeneric field-name (field)
  (:documentation
   "Return the name of FIELD. The returned name uniquely identifies
the field within its containing type."))

(defgeneric field-type (field)
  (:documentation
   "Return the data type of FIELD."))

(defgeneric field-optional? (field)
  (:documentation
   "Return non-nil if FIELD does not have to be present in
realizations of its containing data type."))

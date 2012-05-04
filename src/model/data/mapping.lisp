;;; mapping.lisp --- Associate wire-schema to given data-holder.
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

(cl:in-package :rosetta.model.data)

(defclass mapping (print-items-mixin)
  ((data-holder :initarg  :data-holder
		:reader   data-holder
		:documentation
		"Stores the structure type which describes the
data-holder part of the mapping.")
   (wire-schema :initarg  :wire-schema
		:reader   wire-schema
		:documentation
		"Stores the structure type which describes the
wire-schema part of the mapping.")
   (rules       :initarg  :rules
		:type     list
		:accessor rules
		:initform nil
		:documentation
		"Stores the rules which associate field of the
data-holder and wire-schema structure types."))
  (:default-initargs
   :data-holder (missing-required-initarg 'mapping     :data-holder)
   :wire-schema (missing-required-initarg 'wire-schema :wire-schema))
  (:documentation
   "instances of this class associate an arbitrary wire-schema (which
is described as a structure type) to a data-holder (which is described
as a structure type as well) by means of a set of rules.

Rules associates the values fields of the data-holder and wire-schema
via simple expressions."))

(defmethod data-type-name ((type mapping))
  (data-type-name (data-holder type)))

(defmethod print-items append ((object mapping))
  (let+ (((&accessors-r/o data-holder wire-schema rules) object))
    (list (list :data-holder (data-type-name data-holder))
	  (list :wire-schema (data-type-name wire-schema)  " -> ~A")
	  (list :num-items   (length rules)                " (~D)"))))

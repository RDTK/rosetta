;;; structure-mixin.lisp --- Mixin class for structure-like data types.
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

(in-package :rosetta.model.data)

(defclass structure-mixin (named-mixin
			   composite-mixin)
  ((fields :type     hash-table
	   :accessor %structure-fields
	   :documentation
	   "Stores a mapping of field names to field objects."))
  (:documentation
   "This class is intended to be mixed into composite data types that
consist of a collection of named fields."))

(defmethod shared-initialize :after ((instance   structure-mixin)
                                     (slot-names t)
                                     &key
				     fields)
  (setf (slot-value instance 'fields)
	(cond
	  ((or (null fields)
	       (and (typep fields 'sequence) (emptyp fields)))
	   (make-hash-table))
	  ;; plist
	  ((and (listp fields) (keywordp (first fields)))
	   (plist-hash-table fields))
	  ;; sequence of named child instances
	  ((and (typep fields 'sequence)
		(not (emptyp fields))
		(typep (elt fields 0) 'field-mixin))
	   (alist-hash-table (map 'list #'list
				  (map 'list #'data-type-name fields)
				  fields)))
	  ;; hash-table of child instances
	  ((hash-table-p fields)
	   fields))))

(defmethod composite-children ((type structure-mixin))
  "Return the fields of TYPE."
  (hash-table-values (%structure-fields type)))

(defmethod composite-child ((type structure-mixin)
			    (name string))
  "Return the field named NAME in TYPE."
  (or (nth-value 0 (gethash name (%structure-fields type)))
      (error "No such field: ~S" name)))

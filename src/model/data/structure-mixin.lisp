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

(defclass structure-mixin (composite-mixin)
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
  (check-type fields (or sequence hash-table)
	      "a plist of names and types, a list of named types or a hash-table.")

  (setf (slot-value instance 'fields)
	(cond
	  ((or (null fields)
	       (and (typep fields 'sequence) (emptyp fields)))
	   (make-hash-table :test #'equal))
	  ;; plist
	  ((and (listp fields) (stringp (first fields)))
	   (plist-hash-table fields  :test #'equal))
	  ;; sequence of named child instances
	  ((and (typep fields 'sequence)
		(not (emptyp fields))
		(typep (elt fields 0) 'field-mixin))
	   (alist-hash-table (map 'list #'cons
				  (map 'list #'data-type-name fields)
				  fields)
			     :test #'equal))
	  ;; hash-table of child instances
	  ((hash-table-p fields)
	   fields)

	  (t
	   (error 'type-error
		  :datum         fields
		  :expected-type '(or sequence hash-table))))))

(defmethod composite-children ((type structure-mixin))
  "Return the fields of TYPE."
  (hash-table-values (%structure-fields type)))

(defmethod composite-child ((type structure-mixin)
			    (name string)
			    &key &allow-other-keys)
  "Return the field named NAME in TYPE."
  (nth-value 0 (gethash name (%structure-fields type))))

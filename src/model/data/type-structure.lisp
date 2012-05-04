;;; type-structure.lisp ---
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


;;; `base-field' class
;;


(defclass base-field (named-component-mixin
		      documentation-mixin
		      print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))


;;; `base-structure' class
;;

(defclass base-structure (named-mixin
			  parented-mixin
			  ordered-mixin
			  structure-mixin
			  toplevel-mixin
			  documentation-mixin
			  print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   base-structure)
                                     (slot-names t)
                                     &key
				     (fields nil fields-supplied?))
  ;; etypecase?
  (cond
    ;; nil
    ((not fields-supplied?))

    ;; plist
    ((and (listp fields) (stringp (first fields)))
     (setf (%children instance)
	   (iter (for (name field) on fields :by #'cddr)
		 (collect field))))

    ;; sequence of named child instances
    ((and (typep fields 'sequence)
	  (or (emptyp fields)
	      (typep (elt fields 0) 'field-mixin))) ;;; TODO(jmoringe, 2012-04-12):
     (setf (%children instance) (coerce fields 'list)))

    (t
     (error 'type-error
	    :datum         fields
	    :expected-type 'sequence))))

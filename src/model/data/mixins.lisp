;;; mixins.lisp --- Mixins class for the model.data module.
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


;;; `documentation-mixin' mixin class
;;

(defclass documentation-mixin ()
  ((documentation :initarg  :documentation
		  :type     (or null string)
		  :accessor documentation1
		  :initform nil
		  :documentation
		  "Stores documentation associated to the type."))
  (:documentation
   "This class is intended to be mixed into data type classes to which
documentation can be associated."))


;;; `named-mixin' mixin class
;;

(defclass named-mixin ()
  ((name :initarg  :name
	 :type     string
	 :reader   data-type-name
	 :reader   name
	 :documentation
	 "Stores the name of the data type."))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name))
  (:documentation
   "This class is intended to be mixed into data type classes
instances of which represent named data types."))

(defmethod print-items append ((object named-mixin))
  (list (list :name (name object) "~S ")))


;;; `typed-mixin' mixin class
;;

(defclass typed-mixin ()
  ((type :initarg  :type
	 :accessor type1
	 :documentation
	 "Stores the type of values of the field."))
  (:default-initargs
   :type (missing-required-initarg 'typed-mixin :type))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod print-items append ((object typed-mixin))
  "Try to enforce a meaningful order of the name and type print
items."
  (list (list :type (name (type1 object)) " : ~A")))


;;; `parented-mixin' mixin class
;;

(defclass parented-mixin ()
  ((parent :initarg  :parent
	   :accessor parent
	   :initform nil
	   :documentation
	   "Stores the parent of the type."))
  (:documentation
   "This class is intended to be mixed into classes that have an
associated parent object."))

(defmethod root ((type parented-mixin))
  (if-let ((parent (parent type)))
    (root parent)
    type))

(defmethod ancestors ((type parented-mixin)
		      &key
		      (include-self? t))
  (let ((from-parents (when-let ((parent (parent type)))
			(ancestors parent))))
    (if include-self? (cons type from-parents) from-parents)))

(defmethod qname ((type parented-mixin))
  (if-let ((parent (parent type)))
    (append (qname parent) (list (name type)))
    (list :absolute (name type))))

;; Hint for generic builders

(defmethod add-child :after ((builder t)
			     (parent  t)
			     (child   parented-mixin))
  (setf (parent child) parent))


;;; `composite-mixin' mixin class
;;

(defclass composite-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into classes that represent
composite data types."))

(defmethod print-items append ((object composite-mixin))
  (list (list :num-items (length (composite-children object)) "(~D)")))


;;; `ordered-mixin' mixin class
;;

(defclass ordered-mixin (composite-mixin)
  ((children :initarg  :children
	     :type     list
	     :accessor %children
	     :reader   composite-children
	     :initform nil
	     :documentation
	     ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod composite-child ((type ordered-mixin)
			    (name integer)
			    &key &allow-other-keys)
  "Return the child at index NAME in TYPE."
  (nth name (%children type)))

(defmethod (setf composite-child) :after ((new-value t)
					  (type      ordered-mixin)
					  (name      t)
					  &key &allow-other-keys)
  (appendf (%children type) (list new-value)))


;;; `named-component-mixin' mixin class 
;;

(defclass named-component-mixin (named-mixin
				 typed-mixin
				 parented-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into data type classes that
represent fields of structure-like composite data types. Note that
instances of subclasses of this class do not generally represent data
types."))


;;; `field-mixin' mixin class
;;

(defclass field-mixin (named-component-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))


;;; `structure-mixin' mixin class
;;

(defclass structure-mixin (composite-mixin)
  ((fields :type     hash-table
	   :accessor %fields
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

  (setf (%fields instance)
	(cond
	  ((or (null fields)
	       (and (typep fields 'sequence) (emptyp fields)))
	   (make-hash-table :test #'equal))
	  ;; plist
	  ((and (listp fields) (stringp (first fields)))
	   (plist-hash-table fields :test #'equal))
	  ;; sequence of named child instances
	  ((and (typep fields 'sequence)
		(not (emptyp fields))
		(compute-applicable-methods
		 #'data-type-name (list (elt fields 0))))
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
  (hash-table-values (%fields type)))

(defmethod lookup ((container structure-mixin)
		   (kind      (eql :field))
		   (name      string)
		   &key &allow-other-keys)
  "Return the field named NAME in TYPE."
  (values (gethash name (%fields container))))

(defmethod (setf lookup) ((new-value t)
			  (container structure-mixin)
			  (kind      (eql :field))
			  (name      string)
			  &key &allow-other-keys)
  (setf (gethash name (%fields container)) new-value))

;; obsolete
(defmethod composite-child ((type structure-mixin)
			    (name string)
			    &key &allow-other-keys)
  "Return the field named NAME in TYPE."
  (values (gethash name (%fields type))))

(defmethod (setf composite-child) ((new-value t)
				   (type      structure-mixin)
				   (name      string)
				   &key &allow-other-keys)
  (setf (gethash name (%fields type)) new-value))

;; Hint for generic builders

(defmethod add-child ((builder t)
		      (parent  structure-mixin)
		      (child   named-mixin))
  (setf (composite-child parent (data-type-name child)) child)
  (if (next-method-p)
      (call-next-method)
      parent))


;;; `array-mixin' mixin class
;;

(defclass array-mixin (composite-mixin)
  ((element-type :initarg  :element-type
		 :reader   array-element-type1 ;;; TODO(jmoringe, 2012-04-17):
		 :reader   element-type
		 :documentation
		 "")
   (index-type   :initarg  :index-type
		 :reader   array-index-type ;;; TODO(jmoringe, 2012-04-17):
		 :reader   index-type
		 :documentation
		 ""))
  (:default-initargs
   :element-type (missing-required-initarg 'array-mixin :element-type)
   :index-type   (missing-required-initarg 'array-mixin :index-type))
  (:documentation
   "This class is intended to be mixed into type classes which
represent array-like types which consist of an index type and an
element type."))

(defmethod composite-children ((type array-mixin))
  (list (array-element-type1 type)
	(array-index-type    type)))

(defmethod fixed-size? ((type array-mixin))
  (let+ (((&accessors-r/o index-type) type))
    (or (typep index-type 'singleton)
	(and (typep index-type 'list) ;;; TODO(jmoringe, 2012-04-24): tuple
	     (every (of-type 'singleton) index-type)))))

(defmethod print-items append ((type array-mixin))
  (list (list :element-type (data-type-name (element-type type)))
	(list :index-type   (mapcar #'(lambda (index)
					(if (typep index 'singleton)
					    (value index)
					    '*))
				    (ensure-list (index-type type)))
	      "[~{~A~^, ~}]") ;;; TODO(jmoringe, 2012-03-28):
	(list :num-items    "")))


;;; `toplevel-mixin' mixin class
;;

(defclass toplevel-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

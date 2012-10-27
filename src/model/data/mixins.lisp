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


;;;
;;

(defclass composite-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into classes that represent
composite data types."))

(defmacro define-composite-mixin
    (name
     &key
     (class-name       (format-symbol *package* "~A-MIXIN" name))
     (kind             (make-keyword name))
     (kind-specializer (typecase kind
			 (keyword `(eql ,kind))
			 (t       kind)))
     (slot-name        (format-symbol *package* "~A" name))
     (accessor-name    (format-symbol *package* "%~A" slot-name))
     (key-type         'string)
     (key-class        key-type)
     (key-form         (typecase kind
			 (keyword (lambda (kind-var key-var)
				    (declare (ignore kind-var))
				    key-var))
			 (t       (lambda (kind-var key-var)
				    `(cons ,kind-var ,key-var))))))
  "Define a class named NAME which implements to composite
protocol (i.e `contents' and `lookup')."
  `(progn
     (defclass ,class-name (composite-mixin)
       ((,slot-name :type     hash-table
		    :accessor ,accessor-name
		    :initform (make-hash-table :test #'equal)
		    :documentation
		    ,(format nil "Stores the contents of kind ~A of ~
the container."
			     kind)))
       (:documentation

	,(format nil "This class is intended to be mixed into classes ~
which implement the composite protocol for kind ~A."
		 kind)))

     (defmethod contents ((container ,class-name)
			  (kind      ,kind-specializer))
       (hash-table-values (,accessor-name container)))

     (defmethod contents ((container ,class-name)
			  (kind      (eql t)))
       ,(typecase kind
	  (keyword
	   `(nconc (when (next-method-p)
		     (call-next-method))
		   (contents container ,kind)))
	  (t
	   `(call-next-method))))

     (defmethod lookup ((container ,class-name)
			(kind      ,kind-specializer)
			(key       ,key-class)
			&key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
	   `((check-type key ,key-type)))

       (or (when (next-method-p)
	     (call-next-method))
	   (values (gethash ,(funcall key-form 'kind 'key)
			    (,accessor-name container)))))

     (defmethod (setf lookup) ((new-value t)
			       (container ,class-name)
			       (kind      ,kind-specializer)
			       (key       ,key-class)
			       &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
	   `((check-type key ,key-type)))

       (setf (gethash ,(funcall key-form 'kind 'key)
		      (,accessor-name container))
	     new-value))

     (defmethod print-items append ((object ,class-name))
       `((,',(make-keyword name)
	  ,(hash-table-count (,accessor-name object))
	  ,',(format nil " (~C ~~D)" (aref (string name) 0)))))

     ',class-name))

(define-composite-mixin nested
    :class-name nesting-mixin)

(define-composite-mixin nested
    :class-name container/absolute-mixin
    :kind       symbol
    :key-type   name/absolute
    :key-class  list)

(define-composite-mixin nested
    :class-name container/relative-mixin
    :kind       symbol)


;;; `ordered-mixin' mixin class
;;

(defclass ordered-mixin ()
  ((children :initarg  :children
	     :type     list
	     :accessor %children
	     :initform nil
	     :documentation
	     ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod contents ((container ordered-mixin)
		     (kind      (eql :nested)))
  (%children container))

(defmethod (setf lookup) :after ((new-value t)
				 (type      ordered-mixin)
				 (kind      t)
				 (name      t)
				 &key &allow-other-keys)
  (let+ (((&accessors (children %children)) type))
    (unless (member new-value children)
      (appendf children (list new-value)))))


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

(defmethod kind ((type field-mixin))
  :field)


;;; `structure-mixin' mixin class
;;

(define-composite-mixin fields
    :kind :field)

(defclass structure-mixin (nesting-mixin
			   fields-mixin)
  ()
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

(defmethod kind ((type structure-mixin))
  :structure)

;; Hint for generic builders

(defmethod add-child ((builder t)
		      (parent  structure-mixin)
		      (child   field-mixin))
  (setf (lookup parent :field (name child)) child)
  (if (next-method-p)
      (call-next-method)
      parent))


;;; `array-mixin' mixin class
;;

(defclass array-mixin ()
  ((element-type :initarg  :element-type
		 :reader   element-type
		 :documentation
		 "")
   (index-type   :initarg  :index-type
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

(defmethod kind ((type array-mixin))
  :array)

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
	      "[~{~A~^, ~}]"))) ;;; TODO(jmoringe, 2012-03-28):


;;; `toplevel-mixin' mixin class
;;

(defclass toplevel-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

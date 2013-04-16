;;;; type-enum.lisp --- Representation of basic enum types.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)


;;; `base-enum-value' class
;;

(defclass enum-value (named-mixin ;; named-component-mixin
		      singleton ;;; TODO(jmoringe, 2012-05-03): singleton-mixin
		      parented-mixin
		      documentation-mixin
		      print-items-mixin)
  ()
  (:default-initargs
   :type +uint32+)
  (:documentation
   "Instances of this class represent enumeration values consisting of
a name and an associated, usually numeric, value."))


;;; `enum' class
;;

(define-composite-mixin value
    :class-name values-mixin)

(defclass enum (named-mixin
		parented-mixin
		typed-mixin
		ordered-mixin
		values-mixin
		toplevel-mixin
		documentation-mixin
		print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent enum data types, that is a named
collection of value objects each associated a name and a, usually
numeric, value.

One example of a class representing such values is `enum-value'."))

(defmethod shared-initialize :after ((instance   enum)
                                     (slot-names t)
                                     &key
				     (value-class 'enum-value)
				     values)
  (etypecase values
    ;; Values are provided as a "plist" with (or string keyword) keys
    ;; and values.
    ((cons (or string keyword) cons)
     (iter (for (name value-and-initargs) on values :by #'cddr)
	   (setf (lookup instance :value (string name))
		 (apply #'make-instance value-class
			:name  (string name)
			:type  (type1 instance)
			:value (ensure-list value-and-initargs)))))
    ;; Values are provided as a sequence of `enum-value' objects.
    (sequence
     (iter (for value each values)
	   (setf (lookup instance :value (name value)) value)))))

(defmethod kind ((type enum))
  :enum)

(defmethod lookup ((container enum)
		   (kind      (eql :value))
		   (key       integer)
		   &key &allow-other-keys)
  (find key (contents container :value)
	:test #'=
	:key  #'value))

(defmethod (setf lookup) :before ((new-value t)
				  (container enum)
				  (kind      (eql :value))
				  (key       string)
				  &key &allow-other-keys)
  ;; Make sure that we can obtain a numeric value from
  ;; NEW-VALUE. Otherwise, reject it.
  (unless (compute-applicable-methods #'value (list new-value))
    (simple-child-error container new-value
			"~@<Supplied value ~A for ~A does not ~
specialize the ~S method.~@:>"
			new-value container 'value))

  (let ((value (value new-value)))
    ;; Make sure we do not already have the numeric value associated
    ;; to NEW-VALUE.
    (when-let ((existing (find value (contents container :value)
			       :key #'value)))
      (duplicate-child-key container (list :value value) existing))

    ;; Finally make sure the numeric value associated to NEW-VALUE is
    ;; valid for the numeric type used by CONTAINER.
    (let+ (((&values valid? cause)
	    (validate-value (type1 container) value :if-invalid nil)))
      (unless valid?
	(chainable-child-error container key cause)))))

(defmethod validate-value ((type enum) (value t)
			   &key &allow-other-keys)
  (unless (lookup type :value (string value) :if-does-not-exist nil)
    (let ((values (mapcar #'name (contents type :value))))
      (error "~@<~S is not one of the valid values of ~A. ~[There are ~
no valid values~;The only valid value is ~:;Valid values are ~]~{~S~^, ~
~}.~@:>"
	     value type (length values) values)))
  t)

;; Hint for generic builders

(defmethod add-child ((builder t)
		      (parent  enum)
		      (child   named-mixin))
  (setf (lookup parent :value (name child)) child)
  (if (next-method-p)
      (call-next-method)
      parent))

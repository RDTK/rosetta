;;;; mixins.lisp --- Mixins class for the model.data module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

;;; `documentation-mixin' mixin class

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

;; Hint for generic builders

(defmethod add-child ((builder t)
                      (parent  documentation-mixin)
                      (child   string))
  (setf (documentation1 parent) child)
  parent)

;;; `named-mixin' mixin class

(defclass named-mixin ()
  ((name :initarg  :name
         :type     string
         :reader   name
         :documentation
         "Stores the name of the data type."))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name))
  (:documentation
   "This class is intended to be mixed into data type classes
instances of which represent named data types."))

(defmethod print-items append ((object named-mixin))
  `((:name ,(name object) "~S")))

;;; `typed-mixin' mixin class

(defclass typed-mixin ()
  ((type :initarg  :type
         :accessor type1
         :documentation
         "Stores the type of values of the field."))
  (:default-initargs
   :type (missing-required-initarg 'typed-mixin :type))
  (:documentation
   "This class is intended to be mixed into data type classes which
refer to another type such as a field of a structure."))

(defmethod validate-value ((type typed-mixin) (value t)
                           &key &allow-other-keys)
  (validate-value (type1 type) value))

(defmethod direct-dependencies append ((thing typed-mixin))
  (list (type1 thing)))

(defmethod print-items append ((object typed-mixin))
  "Try to enforce a meaningful order of the name and type print
items."
  `((:type ,(print-items (type1 object))
           ": ~/print-items:format-print-items/"
           ((:after :name)))))

;;; `composite-mixin' (for backward compatibility)

(defclass composite-mixin (mapping-composite-mixin)
  ())

(defmacro define-composite-mixin
    (name
     &rest args &key
     class-name
     kind
     kind-specializer
     slot-name
     accessor-name
     key-type
     key-class
     relation-args?
     make-key-form
     key-func/any-kind
     set-parent?)
  #.(documentation 'define-mapping-composite-mixin 'function)
  (declare (ignore class-name kind kind-specializer slot-name
                   accessor-name key-type key-class relation-args?
                   make-key-form key-func/any-kind set-parent?))
  `(define-mapping-composite-mixin ,name ,@args))

;;; `nested-mixin'

(define-composite-mixin nested
  :class-name nesting-mixin)

(defmethod shared-initialize :after ((instance   nesting-mixin)
                                     (slot-names t)
                                     &key
                                     (nested nil nested-supplied?))
  (when nested-supplied?
    (iter (for thing in-sequence nested)
          (setf (lookup instance :nested (name thing)) thing))))

(defmethod direct-dependencies append ((thing nesting-mixin))
  (contents thing :nested))

(define-composite-mixin nested
  :class-name  container/absolute-mixin
  :kind        symbol
  :key-type    name/absolute
  :key-class   list
  :set-parent? nil)

(define-composite-mixin nested
  :class-name container/relative-mixin
  :kind       symbol)

;;; `ordered-mixin' mixin class

(defclass ordered-mixin ()
  ((children :initarg  :children
             :type     list
             :accessor %children
             :initform nil
             :documentation
             "Stores an ordered set of references to \"child\"
types. "))
  (:documentation
   "This class is intended to be mixed into data type classes which
maintain an ordered set of references to \"child\" data types such as
structures or tuples."))

(defmethod contents :around ((container ordered-mixin)
                             (kind      t))
  (sort (copy-seq (call-next-method)) #'<
        :key (rcurry #'position (%children container))))

(defmethod (setf lookup) :after ((new-value t)
                                 (type      ordered-mixin)
                                 (kind      t)
                                 (name      t)
                                 &key &allow-other-keys)
  (let+ (((&accessors (children %children)) type))
    (unless (member new-value children)
      (appendf children (list new-value)))))

;;; `named-component-mixin' mixin class

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

(defclass field-mixin (named-component-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into classes which represent
concepts similar to fields in a structure."))

(defmethod kind ((type field-mixin))
  :field)

;;; `structure-mixin' mixin class

(define-composite-mixin fields
  :kind :field)

(defmethod (setf lookup) :before ((new-value t)
                                  (container fields-mixin)
                                  (kind      (eql :field))
                                  (key       t)
                                  &key &allow-other-keys)
  (when (let ((dependencies
                (dependencies
                 new-value :blacklist `(or ,#'optional?
                                           ,(lambda (thing)
                                              (eq (kind thing) :array))))))
          (member container dependencies :test #'eq))
    (simple-child-error container key
                        "~@<Field ~A (transitively) contains a ~
                         mandatory instantiation of its desired ~
                         containing structure type ~A. Adding this ~
                         field would make ~:*~A impossible to ~
                         instantiate.~@:>"
                        new-value container)))

(defmethod direct-dependencies append ((thing fields-mixin))
  (mappend #'direct-dependencies (contents thing :field)))

(defclass structure-mixin (nesting-mixin
                           fields-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into composite data types that
consist of a collection of named fields."))

(defmethod shared-initialize :after ((instance   structure-mixin)
                                     (slot-names t)
                                     &key
                                     (field-class 'base-field)
                                     fields)
  (etypecase fields
    ;; Fields are provided as a "plist" with (or string keyword) keys
    ;; and type values.
    ((cons (or string keyword) cons)
     (iter (for (name type-and-initargs) on fields :by #'cddr)
           (setf (lookup instance :field (string name))
                 (apply #'make-instance field-class
                        :name  (string name)
                        :type  (ensure-list type-and-initargs)))))
    ;; Fields are provided as a sequence of `field-mixin' objects.
    (sequence
     (iter (for field in-sequence fields)
           (check-type field field-mixin)
           (setf (lookup instance :field (name field)) field)))))

(defmethod kind ((type structure-mixin))
  :structure)

;; Hints for generic builders

(defmethod add-child ((builder t)
                      (parent  structure-mixin)
                      (child   field-mixin))
  (setf (lookup parent :field (name child)) child)
  (if (next-method-p)
      (call-next-method)
      parent))

(defclass toplevel-mixin () ()) ;; forward

(defmethod add-child ((builder t)
                      (parent  structure-mixin)
                      (child   toplevel-mixin))
  (setf (lookup parent :nested (name child)) child)
  (if (next-method-p)
      (call-next-method)
      parent))

;;; `array-mixin' mixin class

(defclass array-mixin ()
  ((element-type :initarg  :element-type
                 :reader   element-type
                 :documentation
                 "Stores the type of the elements of the array.")
   (index-type   :initarg  :index-type
                 :reader   index-type
                 :documentation
                 "Stores the type of indices of the array. Usually,
the index type is either an integer type of a tuple type of integer
types."))
  (:default-initargs
   :element-type (missing-required-initarg 'array-mixin :element-type)
   :index-type   (missing-required-initarg 'array-mixin :index-type))
  (:documentation
   "This class is intended to be mixed into type classes which
represent array-like types which consist of an index type and an
element type."))

(defmethod kind ((type array-mixin))
  :array)

(defmethod direct-dependencies append ((thing array-mixin))
  (list (element-type thing) (index-type thing)))

(defmethod fixed-size? ((type array-mixin))
  (let+ (((&accessors-r/o index-type) type))
    (or (typep index-type 'singleton)
        (and (typep index-type 'list) ; TODO(jmoringe, 2012-04-24): tuple
             (every (of-type 'singleton) index-type)))))

(defmethod print-items append ((object array-mixin))
  `((:element-type ,(name (element-type object)))
    (:index-type   ,(mapcar (lambda (index)
                              (if (typep index 'singleton)
                                  (value index)
                                  '*))
                            (ensure-list (index-type object)))
                   "[~{~A~^, ~}]" ; TODO(jmoringe, 2012-03-28):
                   ((:after :element-type)))))

;;; `toplevel-mixin' mixin class

(defclass toplevel-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

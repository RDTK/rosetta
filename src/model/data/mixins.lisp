;;;; mixins.lisp --- Mixins class for the model.data module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

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
  (list (list :name (name object) "~S ")))

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
  (list (list :type (print-items (type1 object)) " : ~/rosetta::format-print-items/")))

;;; `parented-mixin' mixin class

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

(defclass composite-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into classes that represent
composite data types."))

(defmacro define-composite-mixin
    (name
     &key
     (class-name        (format-symbol *package* "~A-MIXIN" name))
     (kind              (make-keyword name))
     (kind-specializer  (typecase kind
                          (keyword `(eql ,kind))
                          (t       kind)))
     (slot-name         (format-symbol *package* "~A" name))
     (accessor-name     (format-symbol *package* "%~A" slot-name))
     (key-type          'string)
     (key-class         key-type)
     (make-key-form     (typecase kind
                          (keyword (lambda (kind-var key-var)
                                     (declare (ignore kind-var))
                                     key-var))
                          (t       (lambda (kind-var key-var)
                                     `(cons ,kind-var ,key-var)))))
     (key-func/any-kind (typecase kind
                          (keyword 'identity)
                          (t       'rest))))
  "Define a class named NAME which implements to composite
protocol (i.e `contents' and `lookup').

MAKE-KEY-FORM is a function of two arguments, the name of the variable
holding KIND and the name of the variable holding KEY, which returns a
form which constructs a composite key using the two names.

KEY-FUNC/ANY-KIND is applied to stored keys when `lookup' is called
with KIND `t'. The returned value is compared to the KEY of the
`lookup' call."
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
           (values (gethash ,(funcall make-key-form 'kind 'key)
                            (,accessor-name container)))))

     (defmethod lookup ((container ,class-name)
                        (kind      (eql t))
                        (key       ,key-class)
                        &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
           `((check-type key ,key-type)))

       (or (when (next-method-p)
             (call-next-method))
           (cdr (find key (hash-table-alist (,accessor-name container))
                      :test #'equal
                      :key  (compose #',key-func/any-kind #'car)))))

     (defmethod (setf lookup) ((new-value t)
                               (container ,class-name)
                               (kind      ,kind-specializer)
                               (key       ,key-class)
                               &key &allow-other-keys)
       ,@(when (and key-type (not (eq key-type key-class)))
           `((check-type key ,key-type)))

       (setf (gethash ,(funcall make-key-form 'kind 'key)
                      (,accessor-name container))
             new-value))

     (defmethod print-items append ((object ,class-name))
       `((,',(make-keyword name)
          ,(hash-table-count (,accessor-name object))
          ,',(format nil " (~C ~~D)" (aref (string name) 0)))))

     ',class-name))

(define-composite-mixin nested
    :class-name nesting-mixin)

(defmethod direct-dependencies append ((thing nesting-mixin))
  (contents thing :nested))

(define-composite-mixin nested
    :class-name container/absolute-mixin
    :kind       symbol
    :key-type   name/absolute
    :key-class  list)

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
     (iter (for field each fields)
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

(defmethod direct-dependencies append ((thing array-mixin))
  (list (element-type thing) (index-type thing)))

(defmethod fixed-size? ((type array-mixin))
  (let+ (((&accessors-r/o index-type) type))
    (or (typep index-type 'singleton)
        (and (typep index-type 'list) ; TODO(jmoringe, 2012-04-24): tuple
             (every (of-type 'singleton) index-type)))))

(defmethod print-items append ((type array-mixin))
  (list (list :element-type (name (element-type type)))
        (list :index-type   (mapcar #'(lambda (index)
                                        (if (typep index 'singleton)
                                            (value index)
                                            '*))
                                    (ensure-list (index-type type)))
              "[~{~A~^, ~}]"))) ; TODO(jmoringe, 2012-03-28):

;;; `toplevel-mixin' mixin class

(defclass toplevel-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

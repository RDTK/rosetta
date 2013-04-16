;;;; target-class.lisp --- Generate Lisp classes data type definitions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend)

;;; Evaluate generated code

(defmethod emit/context ((node     t)
                         (target   code-generating-target-mixin)
                         (language language-lisp/compiled))
  (let+ (((&accessors-r/o optimization-settings) target)
         (code `(locally
                    ,@(when optimization-settings
                        `((declare (optimize ,@optimization-settings))))
                  ,(generate node target :lisp))))
    (handler-bind
        (#+sbcl (sb-c::redefinition-warning #'muffle-warning)
         (error #'(lambda (condition)
                    (error "~@<Failed to compile code~2%~S~2%Caused by:~%~A~@:>"
                           code condition))))
      (eval code))))

;;; Generic stuff

(defmethod emit/context :around ((node     named-mixin)
                                 (target   t)
                                 (language language-lisp))
  (let+ ((name (normalize-name (name node) :transform #'string-upcase))
         ((&env (package (find-package :cl-user)) ; TODO(jmoringe, 2012-12-07): package
                (:name (intern name package)))))
    (call-next-method)))

(defmethod emit :after ((node     documentation-mixin)
                        (target   target-class)
                        (language language-lisp))
  (when-let ((name          (context-get *context* :name :default nil))
             (documentation (documentation1 node)))
    (setf (documentation name 'type) documentation)))

(defmethod emit :after ((node     documentation-mixin)
                        (target   method-target-mixin)
                        (language language-lisp))
  (when-let ((name          (context-get *context* :name :default nil))
             (documentation (documentation1 node)))
    (setf (documentation name 'function) documentation)))

(defmethod emit :after ((node     t)
                        (target   code-generating-target-mixin)
                        (language language-lisp))
  (when-let ((name (context-get *context* :name :default nil)))
    (export name (symbol-package name))))
;; TODO(jmoringe): config in target; could use a predicate (funcall export node)

(defmethod emit ((node     named-mixin)
                 (target   target-reference)
                 (language language-lisp))
  (let+ (((&env-r/o name)))
    name))

;;; Fundamental types

(defmethod emit ((node     fundamental-type-mixin)
                 (target   target-class)
                 (language language-lisp))
  (ecase (category node)
    (:bool
     'boolean)
    (:integer
     `(,(if (signed? node) 'signed-byte 'unsigned-byte) ,(width node)))
    (:float
     (ecase (width node)
       (32 'single-float)
       (64 'double-float)))
    (:string
     'string)
    (:bytes
     'nibbles:octet-vector)))

(defmethod emit ((node     fundamental-type-mixin)
                 (target   target-reference)
                 (language language-lisp))
  (generate node :class language))

(macrolet
    ((define-instantiate-method (type value)
       `(defmethod emit ((node     ,type)
                         (target   target-instantiate)
                         (language t))
          (let+ (((&plist-r/o (value :value ,value))
                  (target-initargs target))
                 (value (coerce value (generate node :reference language))))
            (validate-value node value)
            value))))

  (define-instantiate-method type-bool     nil)
  (define-instantiate-method type-integer* 0)
  (define-instantiate-method type-float32  0.0f0)
  (define-instantiate-method type-float64  0.0d0)
  (define-instantiate-method type-string*  ""))

(defmethod emit ((node     type-octet-vector)
                 (target   target-instantiate)
                 (language language-lisp))
  (getf (target-initargs target) :value `(nibbles:octet-vector)))

;;; Singleton type

(defmethod emit ((node     singleton)
                 (target   target-class)
                 (language language-lisp))
  `(eql ,(value node)))

(defmethod emit ((node     singleton)
                 (target   target-reference)
                 (language language-lisp))
  (generate node :class language))

(defmethod emit ((node     singleton)
                 (target   target-instantiate)
                 (language language-lisp))
  (let+ (((&plist-r/o (value :value (value node))) (target-initargs target))
         (value (generate (type1 node)
                          `(:instantiate :initargs (:value ,value))
                          language)))
    (validate-value node value)
    value))

;;; Enum types

(defmethod emit/context ((node     enum)
                         (target   t)
                         (language language-lisp))
  "Emit an enum definition for NODE."
  (let+ (((&env-r/o package name))
         ((&env (:name-name (format-symbol package "~A-NAME" name))
                (:code-name (format-symbol package "~A-CODE" name)))))
    (call-next-method)))

(defmethod emit ((node     enum)
                 (target   target-class)
                 (language language-lisp))
  (with-emit-symbols
    (let+ (((&env-r/o name name-name code-name))
           (pairs (map 'list #'recur (contents node :value)))
           (code-type (generate (type1 node) :reference language)))
      `(progn
         (deftype ,name ()
           '(member ,@(mapcar #'first pairs)))

         (declaim (ftype (function (,code-type) (values ,name &optional)) ,name-name))

         (defun ,name-name (code)
           (case code
             ,@(mapcar #'reverse pairs)
             (t (error "~@<Code ~D is invalid for enum ~S.~@:>"
                       code ',name))))

         (declaim (ftype (function (,name) (values ,code-type &optional)) ,code-name))

         (defun ,code-name (name)
           (case name
             ,@pairs
             (t (error "~@<Symbol ~S is invalid for enum ~S.~@:>"
                       name ',name))))

         (values ',name ',name-name ',code-name)))))

(defmethod emit ((node     enum-value)
                 (target   target-class)
                 (language language-lisp))
  (let+ (((&env-r/o name)))
    (list (make-keyword name) (value node))))

(defmethod emit ((node     enum)
                 (target   target-value->code)
                 (language language-lisp))
  (let+ (((&env-r/o source-var code-name)))
    `(,code-name ,source-var)))

(defmethod emit ((node     enum)
                 (target   target-code->value)
                 (language language-lisp))
  (let+ (((&env-r/o source-var name-name)))
    `(,name-name ,source-var)))

(defmethod emit ((node     enum)
                 (target   target-instantiate)
                 (language language-lisp))
  (let+ (((&plist-r/o (value :value)) (target-initargs target))
         (value (if value
                    (lookup node :value (string value))
                    (first (contents node :value)))))
    (generate value target language)))

(defmethod emit ((node     enum-value)
                 (target   target-instantiate)
                 (language language-lisp))
  (let+ (((&env-r/o name)))
    (make-keyword name)))

;;; Structure

(defmethod emit ((node     field-mixin)
                 (target   target-class)
                 (language language-lisp))
  "Emit a slot specification for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o (type type1)) node)
           ((&env-r/o name))
           (initarg     (make-keyword name))
           (type1       (if (typep type '(or fundamental-type-mixin singleton))
                            (recur type)
                            (progn
                              (let+ (((&env (:lisp-toplevel-emitted? nil))))
                                (recur type))
                              t #+later (name type)))) ; TODO(jmoringe, 2012-05-09): dependency architecture
           (reader-name name)
           (writer-name `(setf ,name))
           (initform    (generate type :instantiate language)))
      `(,name :initarg  ,initarg
              :type     ,type1
              :reader   ,reader-name
              :writer   ,writer-name
              :initform ,initform))))

(defmethod emit ((node     structure-mixin)
                 (target   target-class)
                 (language language-lisp))
  "Generate code which defines a CLOS class for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o (metaclass    target-metaclass)
                            (superclasses target-superclasses)) target)
           ((&env-r/o name)))
      ;; Emit the actual class definition.
      `(progn
         (defclass ,name () ())
         (defclass ,name (,@superclasses)
           ,(map 'list #'recur (contents node :field))
           ,@(when metaclass
               `((:metaclass ,metaclass))))))))

(defmethod emit ((node     structure-mixin)
                 (target   target-instantiate)
                 (language language-lisp))
  (let+ (((&env-r/o name))
         ((&accessors-r/o (initargs target-initargs)) target))
    ;; Make sure that all initargs in INITARGS refer to slots of NODE
    ;; and the supplied values are valid for the respective field
    ;; types.
    (iter (for (key value) on initargs :by #'cddr)
          (if-let ((field (lookup node :field (string key)
                                       :if-does-not-exist nil)))
            ;; The initarg names a field => validate value against
            ;; field type.
            (let+ (((&values valid? cause)
                    (validate-value field value :if-invalid nil)))
              (unless valid?
                (value-invalid-for-type node initargs cause)))
            ;; The initarg does not name a field => signal error.
            (value-invalid-for-type
             node initargs
             (make-condition 'simple-error
                             :format-control   "~@<The supplied initarg ~
~S does not name a field of ~A.~@:>"
                             :format-arguments (list key node)))))

    ;; Emit instantiation code.
    `(make-instance ',name ,@initargs)))

;;; Array types

(defmethod emit ((node     array-mixin)
                 (target   target-class)
                 (language language-lisp))
  `(array t *))

(defmethod emit ((node     array-mixin)
                 (target   target-instantiate)
                 (language language-lisp))
  `(make-array 0 :adjustable t))

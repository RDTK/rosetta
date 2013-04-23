;;;; type-fundamental.lisp --- Fundamental data types.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

;;; `fundamental-type-mixin' mixin class

(defclass fundamental-type-mixin ()
  ((category :initarg  :category
             :type     keyword
             :reader   category
             :documentation
             "Stores the category to which the type belongs. Examples
are :integer, :string"))
  (:default-initargs
   :category (missing-required-initarg
              'fundamental-type-mixin :category))
  (:documentation
   "This mixin class adds a category slot to data type classes
representing fundamental data types."))

(defmethod kind ((type fundamental-type-mixin))
  :fundamental)

(defmethod name ((type fundamental-type-mixin))
  (subseq (string (class-name (class-of type))) 5))

(defmethod qname ((type fundamental-type-mixin))
  (list :absolute (name type)))

;;; `fixed-width-mixin' mixin class

(defclass fixed-width-mixin ()
  ((width :initarg  :width
          :type     t ;; positive-integer
          :reader   width
          :documentation
          "Stores the amount of storage in bits required to store
values of the data type."))
  (:default-initargs
   :width (missing-required-initarg 'fixed-width-mixin :width))
  (:documentation
   "This mixin class adds a width slot to data type classes
representing (fundamental) data types which require a fixed amount of
storage."))

;;; `variable-width-mixin' mixin class

(defclass variable-width-mixin ()
  ()
  (:documentation
   "This mixin class is intended to used as a superclass for data type
classes representing data types which require a variable amount of
storage."))

;;; Concrete fundamental types

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-fundamental-type ((name (&rest supertypes) category)
                                     &rest properties &key &allow-other-keys)
    (let* ((class-name    (symbolicate '#:type- name))
           (variable-name (symbolicate '#:+ name '#:+)))
      `(progn
         (defclass ,class-name (,@supertypes fundamental-type-mixin)
           ()
           (:default-initargs
            :category ,category
            ,@properties))

         (defparameter ,variable-name (make-instance ',class-name)
           ,(format nil "Singleton instance of the `~(~A~)' ~
                         fundamental type."
                    name))))))

(define-fundamental-type (bool (fixed-width-mixin) :bool)
  :width 1)

(defmethod validate-value ((type type-bool) (value t)
                           &key &allow-other-keys)
  (typep value 'boolean))

;;; Integer types

(defclass sign-mixin ()
  ((signed? :initarg  :signed?
            :type     boolean
            :reader   signed?
            :documentation
            "Stores either nil or t to indicate whether the numeric
data type represents signed or unsigned numbers."))
  (:default-initargs
   :signed? (missing-required-initarg 'sign-mixin :signed?))
  (:documentation
   "This mixin class adds a signed? slot to data type classes."))

(defclass type-integer* (sign-mixin
                         fundamental-type-mixin)
  ()
  (:default-initargs
   :category :integer))

(defmethod validate-value ((type type-integer*) (value integer)
                           &key &allow-other-keys)
  (typep value (if (signed? type) 'signed-byte 'unsigned-byte)))

(defclass type-fixed-width-integer (type-integer*
                                    fixed-width-mixin)
  ()
  (:documentation
   "This class serves as a superclass for integer classes of a fixed
width."))

(defmethod validate-value ((type type-fixed-width-integer) (value integer)
                           &key &allow-other-keys)
  (typep value `(,(if (signed? type) 'signed-byte 'unsigned-byte)
                 ,(width type))))

(macrolet
    ((define-fundamental-integer-type (signed? &optional width)
       (let ((name       (format-symbol *package* "~:[U~;~]INT~@[~D~]"
                                   signed? width))
             (superclass (if width
                             'type-fixed-width-integer
                             'type-integer*)))
         `(define-fundamental-type (,name (,superclass) :integer)
            :signed? ,signed?
            ,@(when width
                `(:width ,width))))))
  ;; Variable width integers.
  (define-fundamental-integer-type t)
  (define-fundamental-integer-type nil)
  ;; Fixed width integers.
  (define-fundamental-integer-type t    8)
  (define-fundamental-integer-type nil  8)
  (define-fundamental-integer-type t   16)
  (define-fundamental-integer-type nil 16)
  (define-fundamental-integer-type t   32)
  (define-fundamental-integer-type nil 32)
  (define-fundamental-integer-type t   64)
  (define-fundamental-integer-type nil 64))

;;; Float types

(defclass type-float* (fixed-width-mixin
		       fundamental-type-mixin)
  ()
  (:default-initargs
   :category :float))

(define-fundamental-type (float32 (type-float*) :float)
  :width 32)

(defmethod validate-value ((type type-float32) (value float)
                           &key &allow-other-keys)
  (typep value `(real ,most-negative-single-float ,most-positive-single-float)))

(define-fundamental-type (float64 (type-float*) :float)
  :width 64)

(defmethod validate-value ((type type-float64) (value float)
                           &key &allow-other-keys)
  (typep value `(real ,most-negative-double-float ,most-positive-double-float)))

;;; Strings and octet sequences

(defclass string-mixin ()
  ((encoding :initarg  :encoding
             :type     (or (eql t) keyword)
             :reader   encoding
             :documentation
             "Stores the encoding implied by the data type."))
  (:documentation
   "This mixin class adds an encoding slot to string type classes."))

(define-fundamental-type
    (string* (variable-width-mixin string-mixin) :string))

(define-fundamental-type (ascii-string (type-string*) :string)
  :encoding :ascii)

(defmethod validate-value ((type type-ascii-string) (value string)
                           &key &allow-other-keys)
  (every (lambda (code) (<= 0 (char-code code) 127)) value))

(define-fundamental-type (utf-8-string (type-string*) :string)
  :encoding :utf-8)

(defmethod validate-value ((type type-utf-8-string) (value string)
                           &key &allow-other-keys)
  t)

(define-fundamental-type (octet-vector (variable-width-mixin) :bytes))

(defmethod validate-value ((type type-octet-vector) (value simple-array)
                           &key &allow-other-keys)
  (typep value 'nibbles:octet-vector))

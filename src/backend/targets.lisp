;;;; target-serializer.lisp --- Targets for serializers and deserializers.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend)

;;; Target class

(define-target class (code-generating-target-mixin)
  ((metaclass    :initarg  :metaclass
                 :type     (or null symbol)
                 :reader   target-metaclass
                 :initform nil
                 :documentation
                 "Stores the desired metaclass of the to-be-generated
class.")
   (superclasses :initarg  :superclasses
                 :type     list
                 :reader   target-superclasses
                 :initform nil
                 :documentation
                 "Stores the list of desired superclasses of the
to-be-generated class."))
  (:documentation
   "Define Lisp classes based on data type definitions. The generated
classes will not automatically have associated `pack' and `unpack'
methods. These have to be generated separately."))

(define-target reference (code-generating-target-mixin)
  ()
  (:documentation
   "Emit a reference to a data type. In most cases, this corresponds
to the name of the data type."))

(define-target instantiate (code-generating-target-mixin)
  ((initargs :initarg  :initargs
             :type     list
             :reader   target-initargs
             :initform nil
             :documentation
             "Stores initargs describing the instance that should be
emitted."))
  (:documentation
   "Emit code for creating an instance of a data type."))

(define-target value->code (code-generating-target-mixin)
    ()
  (:documentation
   "Emit code for translating an numeric value to its corresponding
symbolic value within an enumeration."))

(define-target code->value (code-generating-target-mixin)
    ()
  (:documentation
   "Emit code for translating a symbol value to its corresponding
numeric value within an enumeration."))

(define-target convert (code-generating-target-mixin)
    ((to :initarg  :to
         :reader   target-to
         :documentation
         "An object representing the target type of the conversion."))
  (:default-initargs
   :to (missing-required-initarg 'convert :to))
  (:documentation
   "Emit code for converting from a source type to a target type.

The source type is the NODE argument of the surrounding `emit' call
and the target type is stored in the `to' slot of the target
object."))

;;; Serialization-related target classes

(macrolet
    ((define-serialization-target (name)
       `(define-target ,name (code-generating-target-mixin
                              mechanism-target-mixin)
            ()
          (:default-initargs
           :mechanism nil) ; TODO(jmoringe, 2012-05-08): ok?
          (:documentation
           ,(format nil "The ~A target, for example, emits methods on ~
                         `rosetta.serialization:~(~:*~A~)' or ~
                         otherwise generates code for given ~
                         serialization mechanisms and classes ~
                         described by model component instances."
                    name)))))

  (define-serialization-target packed-size)
  (define-serialization-target pack)
  (define-serialization-target unpack)
  (define-serialization-target location)
  ;; determine the location of a part of a structure within the serialized representation of that containing structure.
  (define-serialization-target extract)
  ;; to unpack individual parts of structures from serialized representations without unpacking the entire serialized representation.
  )

;;; Method target classes

(define-target/method packed-size ()
    ())
(define-target/method pack ()
    ())
(define-target/method unpack ()
    ())

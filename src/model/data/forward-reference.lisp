;;;; forward-reference.lisp --- Representation of not-yet-defined things.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

(defclass forward-reference (print-items-mixin)
  ((kind :initarg  :kind
         :accessor kind
         :documentation
         "Stores a description of the kind of the object represented
by this forward reference.")
   (args :initarg  :args
         :type     list
         :accessor args
         :documentation
         "Stores the arguments that would have been passed to the
object represented by this forward reference."))
  (:default-initargs
   :kind (missing-required-initarg 'forward-reference :kind)
   :args (missing-required-initarg 'forward-reference :args))
  (:documentation
   "Instances of this class represent unresolved references to model
objects.

They store all information that was available when the resolution of
the reference failed. This information can later be used to resolve
the reference and eventually replace (via `change-class') the forward
reference object with the desired object."))

(defmethod name ((thing forward-reference))
  (format nil "F<~A~@[ ~A~]>" (kind thing) (getf (args thing) :name)))

(defmethod qname ((thing forward-reference))
  (list :absolute (name thing)))

(defmethod upgrade! ((instance forward-reference)
                     (other    t))
  (let ((new-class (class-of other)))
    (apply #'change-class instance new-class nil)
    (iter (for slot in (closer-mop:class-slots new-class))
          (let ((name (closer-mop:slot-definition-name slot)))
            (setf (slot-value instance name)
                  (slot-value other name))))
    instance))

(defmethod print-items append ((object forward-reference))
  `((:kind ,(kind object))
    (:name ,(getf (args object) :name) "~@[ ~A~]" ((:after :kind)))))

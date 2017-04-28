;;;; mixins.lisp --- Mixins class for the model module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model)

;;; `parented-mixin' mixin class

(defclass parented-mixin ()
  ((parent :initarg  :parent
           :accessor parent
           :initform nil
           :documentation
           "Stores the parent of the type."))
  (:documentation
   "Intended to be mixed into classes that have an associated parent
    object."))

(defmethod qname ((thing parented-mixin))
  (if-let ((parent (parent thing)))
    (append (qname parent) (list (name thing)))
    (list :absolute (name thing))))

(defmethod qname/kind ((thing parented-mixin))
  (let ((cell (cons (name thing) (kind thing))))
    (if-let ((parent (parent thing)))
      (append (qname/kind parent) (list cell))
      (list :absolute cell))))

(defmethod (setf parent) :before ((new-value t)
                                  (thing     parented-mixin))
  (when (member thing (ancestors new-value))
    (simple-child-error new-value thing
                        "~@<Cyclic parent relation ~{~A~^ -> ~}.~@:>"
                        (list* thing (ancestors new-value)))))

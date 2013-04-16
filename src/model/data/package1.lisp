;;;; package1.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

(defclass package1 (named-mixin
                    parented-mixin
                    container/relative-mixin
                    documentation-mixin
                    print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent packages, that is named
containers for data types."))

(defmethod kind ((type package1))
  :package)

(defmethod qname ((package package1))
  (if-let ((parent (parent package)))
    (call-next-method)
    (list :absolute)))

(defmethod contents/plist ((package package1))
  (hash-table-plist (%nested package)))

(defmethod add-child ((builder t) ; TODO(jmoringe, 2012-04-24):
                      (parent  package1)
                      (child   named-mixin))
  (assert (not (eq parent child))) ; TODO(jmoringe, 2012-10-24): proper condition

  (setf (lookup parent (kind child) (name child)) child) ; TODO(jmoringe, 2012-04-24):
  parent)

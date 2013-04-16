;;;; type-singleton.lisp --- Singleton data type.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.data)

;; TODO(jmoringe, 2012-05-03): mixin?
(defclass singleton (typed-mixin
                     print-items-mixin)
  ((value :initarg  :value
          :reader   value
          :documentation
          "Stores the singleton value."))
  (:default-initargs
   :value (missing-required-initarg 'singleton :value))
  (:documentation
   "Instances of this type class represent types the extension of
which consist of singleton values."))

(defmethod shared-initialize :after ((instance   singleton)
                                     (slot-names t)
                                     &key
                                     value)
  (validate-value (type1 instance) value))

(defmethod kind ((type singleton))
  :singleton)

(defmethod name ((type singleton))
  (format nil "=~A" (value type)))

(defmethod validate-value ((type singleton) (value t)
                           &key &allow-other-keys)
  (equal value (value type)))

(defmethod print-items append ((type singleton))
  (list (list :value (value type))))

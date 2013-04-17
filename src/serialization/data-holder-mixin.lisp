;;;; data-holder-mixin.lisp --- Mixin class for mechanisms that use data-holders.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.serialization)

(defclass data-holder-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
unpack into data-holder instances \(as opposed to schema-free
mechanisms which could, for example, return nested lists)."))

(defmethod unpack ((mechanism   data-holder-mixin)
                   (source      t)
                   (destination class)
                   &rest args &key)
  "Make an instance of the class DESTINATION and load SOURCE into the
instance."
  (apply #'unpack mechanism source (make-instance destination) args))

(defmethod unpack ((mechanism   data-holder-mixin)
                   (source      t)
                   (destination symbol)
                   &rest args &key)
  "Make an instance of the class designated by DESTINATION and load
SOURCE into the instance."
  (apply #'unpack mechanism source (find-class destination) args))

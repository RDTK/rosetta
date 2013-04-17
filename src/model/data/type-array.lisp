;;;; type-array.lisp --- Representation of basic array types.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

;;; Class `base-array'

(defclass base-array (array-mixin
                      print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent simple array types"))

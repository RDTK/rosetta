;;;; type-structure.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data)

;;; `base-field' class

(defclass base-field (field-mixin
                      documentation-mixin
                      print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent fields, consisting of a name and
a type, of structure types."))

;;; `base-structure' class

(defclass base-structure (named-mixin
                          parented-mixin
                          ordered-mixin
                          structure-mixin
                          toplevel-mixin
                          documentation-mixin
                          print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent simple structures, that is,
named objects containing ordered sets of named, typed fields."))

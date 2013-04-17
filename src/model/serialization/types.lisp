;;;; types.lisp --- Types used in the model.serialization package.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.serialization)

;;; Endianness specifications

(deftype endian ()
  `(member :little-endian
           :big-endian))

(deftype endian-designator ()
  '(or endian
       (member :machine-endian
               :opposite-of-machine-endian)))

(declaim (ftype (function (endian-designator) (values endian &rest nil))
                resolve-endian))

(defun resolve-endian (endian-designator)
  "Return an `endian' which realizes ENDIAN-DESIGNATOR for the current
machine."
  (case endian-designator
    (:machine-endian
     #+big-endian    :big-endian
     #+little-endian :little-endian)
    (:opposite-of-machine-endian
     (opposite-endian (resolve-endian :machine-endian)))
    (t
     endian-designator)))

(declaim (ftype (function (endian) (values endian &rest nil))
                opposite-endian))

(defun opposite-endian (endian)
  "Return the opposite of ENDIAN."
  (ecase endian
    (:little-endian :big-endian)
    (:big-endian    :little-endian)))

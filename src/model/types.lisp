;;;; types.lisp --- Types used in the model module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model)

;;; Naming-related types

(deftype name-component ()
  'string)

(deftype name-components ()
  '(and list (satisfies %every-name-component)))

(deftype name/absolute ()
  '(cons (eql :absolute) (or null name-components)))

(deftype name-expression/absolute ()
  "A `name/absolute' or a disjunction (with `cl:or') of multiple
`name/absolute's."
  '(or name/absolute
       (cons (eql or) (cons name/absolute))))

(deftype name/relative ()
  '(cons (eql :relative) name-components))

(deftype name-expression/relative ()
  "A `name/relative' or a disjunction (with `cl:or') of multiple
`name/relative's."
  '(or name/relative
       (cons (eql or) (cons name/relative))))

(deftype name ()
  '(or name/absolute name/relative))

(deftype name-expression ()
  "A `name' or a disjunction (with `cl:or') of multiple `name's."
  '(or name (cons (eql or) (cons name))))

;;; Utility functions

(defun %every-name-component (list)
  (every (of-type 'name-component) list))

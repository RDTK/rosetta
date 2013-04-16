;;;; types.lisp --- Types used in the frontend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend)


;;; Location-related types
;;

(deftype bounds/cons ()
  "Upper and lower bound of a region within a string. Upper bound can
  be nil if not known or applicable."
  '(and (cons non-negative-integer
              (or non-negative-integer null))
        (satisfies %valid-bounds)))


;;; Helper functions
;;

(defun %valid-bounds (bounds)
  (and (consp bounds)
       (let+ (((start . end) bounds))
	 (or (null end)
	     (and (integerp start) (integerp end)
		  (<= start end))))))

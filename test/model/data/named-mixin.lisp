;;;; named-mixin.lisp --- Unit tests for the named-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data.test)

(deftestsuite named-mixin-root (model-data-root)
  ()
  (:documentation
   "Test suite for `named-mixin' class."))

(addtest (named-mixin-root
          :documentation
	  "Test constructing `named-mixin' instances.")
  construction

  (ensure-condition 'missing-required-initarg
    (make-instance 'named-mixin)))

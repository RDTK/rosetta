;;;; list-builder.lisp --- Unit tests for list-based builder.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend.test)

(deftestsuite list-builder-root (frontend-root)
  ()
  (:documentation
   "Unit tests for builder methods which construct list-based
representations."))

(addtest (list-builder-root
          :documentation
          "Test methods on `lookup' for the list-based
representation.")
  lookup/smoke

  (let* ((foo       `(:structure nil         :name "foo"))
         (bar       `(:enum      nil         :name "bar"))
         (container `(:package   (,foo ,bar) :name "baz")))
    (ensure-cases (kind key expected)
        `((:structure "foo" ,foo)
          (:enum      "foo" nil)
          (t          "foo" ,foo)
          (:structure "bar" nil)
          (:enum      "bar" ,bar)
          (t          "bar" ,bar)
          (:structure "fez" nil)
          (:enum      "fez" nil)
          (t          "fez" nil))
      (ensure-same (lookup container kind key :if-does-not-exist nil)
                   expected :test #'eq))))

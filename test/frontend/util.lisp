;;;; util.lisp --- Unit tests for utility functions of the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.frontend.test)

(deftestsuite frontend-util-root (frontend-root)
  ()
  (:documentation
   "Tests for utilities in the frontend module."))

(deftestsuite maybe-shorten-root (frontend-util-root)
  ()
  (:documentation
   "Unit tests for the `maybe-shorten' function."))

(addtest (maybe-shorten-root
          :documentation
          "Smoke test for the `maybe-shorten' function.")
  smoke

  (ensure-cases (input length expected)
      `((""    1 "")
        ("foo" 1 "…")
        ("foo" 2 "f…")
        ("foo" 3 "foo")
        ("foo" 4 "foo"))

    (ensure-same (maybe-shorten input :max-length length) expected
                 :test #'string=)))

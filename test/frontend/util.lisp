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

(deftestsuite guess-format-root (frontend-util-root)
  ()
  (:documentation
   "Unit tests for the `guess-format' function."))

(addtest (guess-format-root
          :documentation
          "Smoke test for the `guess-format' function.")
  smoke

  (ensure-cases (input format/expected found?/expected)
      `(#+later ("foo"                          :mock nil) ; no substring match
        #+later ("mock"                         :mock nil) ; substring match

        (,#P"foo/"                      nil   nil) ; no type
        (,#P"foo."                      nil   nil) ; empty type
        (,#P"foo.bla"                   :bla  nil) ; unknown pathname type
        (,#P"foo.MOCK"                  :mock t)   ; pathname type match
        (,#P"foo.mock"                  :mock t)   ; pathname type match

        (,(puri:uri "file:///foo/")     nil   nil) ; all of these should also
        (,(puri:uri "file:///foo.")     nil   nil) ; work for file URLs
        (,(puri:uri "file:///foo.bla")  :bla  nil)
        (,(puri:uri "file:///foo.MOCK") :mock t)
        (,(puri:uri "file:///foo.mock") :mock t))

    (ensure-same (guess-format input)
                 (values format/expected found?/expected))))

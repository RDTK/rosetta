;;;; util.lisp --- Unit tests for utilities used in the rosetta system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.test)

(deftestsuite normalize-name-root (root)
  ()
  (:documentation
   "Test suite for the `normalize-name' function."))

(addtest (normalize-name-root
          :documentation
          "Smoke test for the `normalize-name' function.")
  smoke

  (ensure-cases (input args expected)
      `(("Vec2dDouble" ()                            "vec-2d-double")
        ("Vec2DDouble" ()                            "vec-2d-double")
        ("Vec2dDOUBLE" ()                            "vec-2d-double")
        ("DataXOP"     ()                            "data-xop")
        ("XOPData"     ()                            "xop-data")
        ("xop_data"    ()                            "xop-data")

        ;; Some corner cases.
        ("_foo"        ()                            "foo")           ; separator at start
        ("f1_o"        ()                            "f1-o")          ; touching separators
        ("f_o"         ()                            "f-o")           ; short components

        ;; Other transforms and separators.
        ("Vec2dDouble" (:transform ,#'string-upcase) "VEC-2D-DOUBLE")
        ("xop_data"    (:transform ,#'string-capitalize
                        :separator nil)              "XopData"))

    (ensure-same (apply #'normalize-name input args)
                 expected
                 :test #'string=)))

;;;; type-fundamental.lisp --- Unit tests for fundamental types.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-fundamental-root (model-data-root)
  ()
  (:documentation
   "Unit tests for fundamental data types."))

(addtest (model-data-fundamental-root
          :documentation
          "Tests methods on `validate-value' for fundamental data
types.")
  validate-value

  (ensure-cases (type value expected)
      '(;; Boolean type
        (type-bool         :foo   nil) ; not valid

        (type-bool         nil    t)   ; valid
        (type-bool         t      t)

        ;; Integer types
        (type-uint8        "foo"  nil) ; not valid
        (type-uint8        :foo   nil)
        (type-uint8        -1     nil)
        (type-uint8        256    nil)

        (type-uint8        0      t)   ; valid
        (type-uint8        255    t)
        (type-int8         -1     t)

        ;; Float types
        (type-float32      "foo"  nil) ; not valid
        (type-float32      :foo   nil)
        (type-float32      1      nil)
        (type-float32      -1     nil)
        (type-float32      1/2    nil)

        (type-float32      1.0d0  t)   ; valid
        (type-float32      -1.0d0 t)
        (type-float32      0.5d0  t)

        ;; String types
        (type-ascii-string "ä"    nil) ; not valid

        (type-ascii-string "a"    t)   ; valid
        (type-utf-8-string "ä"    t))

    (ensure-same (validate-value (make-instance type) value
                                 :if-invalid nil)
                 expected)))

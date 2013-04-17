;;;; type-singleton.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-singleton-root (model-data-root)
  ()
  (:documentation
   "Unit tests for the `singleton' class."))

(addtest (model-data-singleton-root
          :documentation
          "Test constructing instances of class `singleton'.")
  construction

  (ensure-cases (initargs expected-value)
      `(;; These are invalid.
        (()                                              missing-required-initarg)
        ((:type ,+uint8+ :value -1)  value-invalid-for-type)

        ;; These are valid.
        ((:type ,+uint8+ :value 1)   1)
        ((:type ,+uint8+ :value 255) 255))

    (let+ (((&flet do-it ()
              (value (apply #'make-instance 'singleton initargs)))))
      (case expected-value
        (missing-required-initarg
         (ensure-condition 'missing-required-initarg (do-it)))
        (value-invalid-for-type
         (ensure-condition 'value-invalid-for-type (do-it)))
        (t
         (ensure-same (do-it) expected-value))))))

(addtest (model-data-singleton-root
          :documentation
          "Test method on `validate-value' for class `singleton'.")
  validate-value

  (ensure-cases (initargs value expected)
      `(;; These are invalid.
        ((:type ,+uint8+ :value 1)   -1   nil)
        ((:type ,+uint8+ :value 1)   2    nil)

        ;; These are valid.
        ((:type ,+uint8+ :value 1)   1    t)
        ((:type ,+uint8+ :value 255) 255  t))

    (ensure-same (validate-value
                  (apply #'make-instance 'singleton initargs) value
                  :if-invalid nil)
                 expected)))

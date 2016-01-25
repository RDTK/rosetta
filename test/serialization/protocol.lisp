;;;; protocol.lisp --- Unit tests for the serialization protocol functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.serialization.test)

;;; Mock mechanism

(defclass mechanism-mock-for-protocol ()
  ((arg :initarg  :arg
        :reader   mechanism-arg
        :initform 0)))

(service-provider:register-provider/class
 'mechanism :mock-for-protocol :class 'mechanism-mock-for-protocol)

(defmethod packed-size ((mechanism mechanism-mock-for-protocol)
                        (source    t)
                        &key)
  (mechanism-arg mechanism))

(defmethod pack ((mechanism   mechanism-mock-for-protocol)
                 (source      t)
                 (destination t)
                 &key)
  (values (mechanism-arg mechanism) destination))

(defmethod pack ((mechanism   mechanism-mock-for-protocol)
                 (source      t)
                 (destination (eql nil))
                 &key)
  (values (mechanism-arg mechanism) :created))

(defmethod unpack ((mechanism   mechanism-mock-for-protocol)
                   (source      t)
                   (destination t)
                   &key)
  (values destination (mechanism-arg mechanism)))

;;; Test suite

(deftestsuite protocol-root (serialization-root)
  ((mechanism (make-instance 'mechanism-mock-for-protocol)))
  (:documentation
   "Unit test suite for the `packed-size', `pack', `pack*' and
`unpack' protocol functions."))

(addtest (protocol-root
          :documentation
          "Smoke test for the `packed-size' function.")
  packed-size/smoke

  (ensure-cases (input expected-output)
      `((:no-such-mechanism          :error)
        (:mock-for-protocol          0)
        ((:mock-for-protocol)        0)
        ((:mock-for-protocol :arg 5) 5)
        (,mechanism                  0))

    (if (eq expected-output :error)
        (ensure-condition 'service-provider:missing-provider-error
          (packed-size input :does-not-matter))
        (ensure-same (packed-size input :does-not-matter) expected-output
                     :test #'equalp))))

(addtest (protocol-root
          :documentation
          "Smoke test for the `pack' and `pack*' functions.")
  pack-and-pack*/smoke

  (ensure-cases (mechanism source destination expected-output)
      `((:no-such-mechanism          :bar :foo :error)
        (:mock-for-protocol          :bar :foo (0 :foo))
        ((:mock-for-protocol)        :bar :foo (0 :foo))
        ((:mock-for-protocol :arg 5) :bar :foo (5 :foo))
        (,mechanism                  :bar :foo (0 :foo)))

    ;; Test `pack'
    (if (eq expected-output :error)
        (ensure-condition 'service-provider:missing-provider-error
          (pack mechanism source destination))
        (ensure-same (pack mechanism source destination)
                     (apply #'values expected-output)
                     :test #'equalp))

    ;; Test `pack*'
    (if (eq expected-output :error)
        (ensure-condition 'service-provider:missing-provider-error
          (pack* mechanism source))
        (ensure-same (pack* mechanism source) :created
                     :test #'eq))))

(addtest (protocol-root
          :documentation
          "Smoke test for the `unpack' method.")
  unpack/smoke

  (ensure-cases (mechanism destination expected-output)
      `((:no-such-mechanism          :dest :error)
        (:mock-for-protocol          :dest (:dest 0))
        ((:mock-for-protocol)        :dest (:dest 0))
        ((:mock-for-protocol :arg 5) :dest (:dest 5))
        (,mechanism                  :dest (:dest 0)))

    (if (eq expected-output :error)
        (ensure-condition 'service-provider:missing-provider-error
          (unpack mechanism :does-not-matter destination))
        (ensure-same (unpack mechanism :does-not-matter destination)
                     (apply #'values expected-output)
                     :test #'equalp))))

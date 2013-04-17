;;;; protocol.lisp --- Unit tests for the protocol of the model.serialization module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; This file may be licensed under the terms of the

(cl:in-package :rosetta.model.serialization.test)

(deftestsuite model-serialization-protocol-root (model-serialization-root)
  ()
  (:documentation
   "Unit tests for the protocol of the model.serialization module."))

(deftestsuite validate-type-root (model-serialization-protocol-root)
  ()
  (:documentation
   "Tests for the `validate-type' generic function."))

(addtest (validate-type-root
          :documentation
          "Test default behavior of the `validate/type' generic
function.")
  default-behavior

  ;; Test behavior in case of invalid types.
  (let ((mechanism :does-not-matter)
        (type      :does-not-matter))

    ;; Type is invalid; should signal an error.
    (ensure-condition type-invalid-for-mechanism
      (validate-type mechanism type))

    ;; Return nil instead of signaling an error.
    (ensure-same (validate-type mechanism type :if-invalid nil)
                 nil)

    ;; Use `continue' restart
    (ensure-same (validate-type mechanism type :if-invalid #'continue)
                 t)))

(addtest (validate-type-root
          :documentation
          "Test that `validate-type' returns the causing condition as
a second return value.")
  cause

  ;; Expect result nil, second return value causing condition.
  (let+ ((mechanism (make-instance 'mock-mechanism/validate-type))
         ((&values result cause) (validate-type mechanism :does-not-matter
                                                :if-invalid nil)))
    (ensure-null result)
    (ensure (typep cause 'type-invalid-for-mechanism))))

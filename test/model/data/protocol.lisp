;;;; protocol.lisp --- Tests for the protocol provided by the model.data module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.data.test)

(deftestsuite model-data-protocol-root (model-data-root)
  ()
  (:documentation
   "Unit tests for the protocol of the model.data module."))

(deftestsuite lookup (model-data-protocol-root)
  ()
  (:documentation
   "Tests for the `lookup' generic function."))

(addtest (lookup
          :documentation
	  "Test default behavior of the `lookup' generic function.")
  default-behavior

  ;; A relative name with no components should refer to the object
  ;; itself.
  (let ((container :does-not-matter))
    (ensure-same (lookup container :does-not-matter '(:relative))
		 container))

  ;; The following cases should all lead to unsuccessful lookup.
  (ensure-cases (key)
      '("does-not-matter"
	(:relative "does-not-matter")
	(:relative "does-not-matter" "does-not-matter"))

    ;; Default behavior consists in not finding the requested object.
    (ensure-condition 'no-such-child
      (lookup :does-not-matter :does-not-matter key))

    ;; Test returning nil instead of signaling an error.
    (ensure-null (lookup :does-not-matter :does-not-matter key
			 :if-does-not-exist nil))

    ;; Test returning a particular value instead of signaling an error.
    (ensure-same (lookup :does-not-matter :does-not-matter key
			 :if-does-not-exist (curry #'use-value :replacement))
		 :replacement)))

(deftestsuite validate-value-root (model-data-protocol-root)
  ()
  (:documentation
   "Tests for the `validate-value' generic function."))

(addtest (validate-value-root
          :documentation
	  "Test default behavior of the `validate-value' generic
function.")
  default-behavior

  ;; Default behavior consists in considering every value invalid and
  ;; signaling an error.
  (ensure-condition 'value-invalid-for-type
    (validate-value :does-not-matter :does-not-matter))

  ;; Test returning nil instead of signaling an error.
  (ensure-null (validate-value :does-not-matter :does-not-matter
			       :if-invalid nil))

  ;; Test continuing despite an invalid value.
  (ensure-same (validate-value :does-not-matter :does-not-matter
			       :if-invalid #'continue)
	       t))

(addtest (validate-value-root
          :documentation
	  "Test that `validate-value' returns the causing condition as
a second return value.")
  cause-return-value

  ;; Expect result nil, second return value causing condition.
  (let+ ((type (make-instance 'mock-type/validate-value))
	 ((&values result cause) (validate-value type :does-not-matter
						 :if-invalid nil)))
    (ensure-null result)
    (ensure (typep cause 'value-invalid-for-type))))

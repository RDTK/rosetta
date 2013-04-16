;;;; protocol.lisp --- Unit tests for protocol functions of the backend module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.backend.test)

(deftestsuite backend-protocol-root (backend-root)
  ()
  (:documentation
   "Unit tests for protocol functions of the backend module."))

(addtest (backend-protocol-root
          :documentation
	  "Test default behavior of methods on `generate'.")
  generate/smoke

  (ensure-cases ((node target language) expected-condition expected-result)

    `(;; No emitter => `emit-error'.
      ((,(make-instance 'mock-node/no-methods) :reference :lisp)
       emit-error   nil)
      ;; `emit' warns => `emit-warning'.
      ((,(make-instance 'mock-node/warning)    :reference :lisp)
       emit-warning :result)
      ;; `emit/context' is used
      ((,(make-instance 'mock-node/context)    :reference :lisp)
       nil          :result-from-context))

    (let+ (((&flet do-it ()
	      (generate node target language)))
	   ((&values result result?)
	    (case expected-condition
	      (emit-error   (ensure-condition 'emit-error (do-it)))
	      (emit-warning (progn
			      (ensure-condition 'emit-warning (do-it))
			      (values (do-it) t)))
	      ((nil)        (values (do-it) t)))))
      (when result?
	(ensure-same result expected-result :test #'equal)))))

(addtest (backend-protocol-root
          :documentation
	  "Test context established by `generate'.")
  generate/context

  (macrolet
      ((check-context (&body body)
	 `(generate (make-instance
		     'mock-node/callback
		     :callback (lambda (node target language)
				 (declare (ignorable node target language))
				 ,@body))
		    :reference
		    :lisp)))

    ;; Ensure `*context*' gets bound.
    (check-context (ensure *context*))

    ;; Ensure `retry', `continue' and `use-value' restarts are
    ;; established.
    (ensure-same (check-context
		  (ensure (find-restart 'rs.b::retry))
		  (setf (slot-value node 'callback) nil)
		  (invoke-restart 'rs.b::retry))
		 :result-after-callback)
    (ensure-null (check-context
		  (ensure (find-restart 'continue))
		  (invoke-restart 'continue)))
    (ensure-same (check-context
		  (ensure (find-restart 'use-value))
		  (invoke-restart 'use-value :result-instead-of-callback))
		 :result-instead-of-callback)))

(addtest (backend-protocol-root
          :documentation
	  "Smoke test for methods on `make-target-like'.")
  make-target-like/smoke

  (ensure-cases (class like expected-type)
      '((target-mock/little-endian-pack :packed-size target-mock/little-endian-packed-size)
	(target-mock/little-endian-pack :unpack      target-mock/little-endian-unpack)
	(target-mock/big-endian-pack    :packed-size target-mock/big-endian-packed-size)
	(target-mock/big-endian-pack    :unpack      target-mock/big-endian-unpack))

    (ensure (typep (make-target-like (make-instance class) like)
		   expected-type))))

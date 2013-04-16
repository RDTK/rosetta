;;;; protocol.lisp --- Unit tests for the protocol of the frontend module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.frontend.test)

(deftestsuite rosetta.frontend.protocol-root (frontend-root)
  ()
  (:documentation
   "Unit tests for protocol functions of the frontend module."))

(addtest (rosetta.frontend.protocol-root
          :documentation
	  "Test the format and builder lookup performed by
`process'.")
  process/lookup

  (ensure-cases (args expected)
      `(;; Invalid format.
	((:no-such-format   #P"does-not-matter"  list)               no-such-format-class)
	(((:no-such-format) #P"does-not-matter"  list)               no-such-format-class)
	((:guess            #P"1.no-such-format" list)               no-such-format-class)

	;; Invalid builder.
	((:mock             #P"does-not-matter"  :no-such-builder)   no-such-builder-class)
	((:mock             #P"does-not-matter"  (:no-such-builder)) no-such-builder-class)

	;; These are OK.
	((:mock             #P"does-not-matter"  list)               t)
	((:mock             #P"does-not-matter"  :mock)              t)
	((:mock             (#P"1" #P"2")        list)               (cons t (cons t null)))
	((:mock             ,#(#P"1" #P"2")      list)               (cons t (cons t null)))
	((:mock             "just-a-string"      list)               t)

	;; Format guessing.
	((:guess            #P"file.mock"        list)               t)
	((:guess            (#P"1.mock" #P"2.mock") list)            (cons t (cons t null))))
    (let+ (((&flet do-it () (apply #'process args))))
     (case expected
       (no-such-format-class  (ensure-condition 'no-such-format-class  (do-it)))
       (no-such-builder-class (ensure-condition 'no-such-builder-class (do-it)))
       (t                     (ensure (typep (do-it) expected)))))))

(addtest (rosetta.frontend.protocol-root
          :documentation
	  "Test the restarts established by `process'.")
  process/restarts

  ;; Restarts specs in RESTART-SPECS are processed and consumed
  ;; sequentially.
  (ensure-cases (sources restart-specs expected)
      `((,#P"does-not-matter" ((use-value :foo))                        :foo)
	("does-not-matter"    ((use-value :foo))                        :foo)
	(,#P"does-not-matter" (retry (use-value :foo))                  :foo)
	((,#P"1" ,#P"2")      ((use-value :foo) retry (use-value :bar)) (:foo :bar))
	(("1"    ,#P"2")      ((use-value :foo) retry (use-value :bar)) (:foo :bar))
	((,#P"1" ,#P"2")      (continue (use-value :bar))               (:bar))
	((,#P"1" ,#P"2")      ((use-value :foo) continue)               (:foo)))

    (ensure-same
     (handler-bind ((error #'(lambda (condition)
			       (apply #'invoke-restart
				      (ensure-list (pop restart-specs))))))
       (process :mock sources :error))
     expected
     :test #'equal)))

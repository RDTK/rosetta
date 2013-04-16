;;;; emitter-conversion.lisp --- Unit tests for conversion emitter.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.backend.test)

(deftestsuite emitter-conversion-root (backend-root)
  ()
  (:setup
  ;; Generate enums
   (generate +enum/uint8/simple+  :class :lisp/compiled)
   (generate +enum/uint32/simple+ :class :lisp/compiled)
   (generate +enum/int32/simple+  :class :lisp/compiled))
  (:documentation
   "Unit tests suite for conversion emitter methods."))

(addtest (emitter-conversion-root
          :documentation
	  "Test `emit-conversion' methods for different data types.")
  emit-conversion/smoke

  (ensure-cases (left right
		 left->right-condition left->right-cases
		 right->left-condition right->left-cases)
      `(;; Completely impossible
	(,+uint32+       ,+ascii-string+ error nil       error   nil)
	(,+utf-8-string+ ,+ascii-string+ error nil       error   nil)

	;; uint32 <-> int32 => different signedness
	(,+uint32+       ,+int32+   error nil       error   nil)

	;; uint32 <-> uint64 => can widen, cannot narrow
	(,+uint32+       ,+uint64+  nil   ((1 . 1)) error   ((1 . 1)))

	;; uint32 <-> float64 => ok, different signedness
	(,+uint32+       ,+float64+
	 nil     ((1     . 1.0d0))
	 error   ((1.0d0 . 1)))

	;; int32    <-> float64 => loss of precision
	(,+int32+        ,+float64+
	 nil     ((1     . 1.0d0)
		  (5     . 5.0d0))
	 warning ((1.0d0 . 1)
		  (1.1d0 . 1)
		  (5.0d0 . 5)))

	;; float32    <-> float64 => loss of precision
	(,+float32+      ,+float64+
	 nil     ((1     . 1.0d0)
		  (5     . 5.0d0))
	 warning ((1.0d0 . 1.0f0)
		  (1.1d0 . 1.1f0)
		  (5.0d0 . 5.0f0)))

	;; uint32 <-> uint32 => no problem
	(,+uint32+       ,+uint32+  nil   ((1 . 1)) nil     ((1 . 1)))

	;; singleton/int32 <-> float64
	((singleton :type  ,+int32+ :value 1) ,+float64+
	 nil     ((1     . 1.0d0))
	 warning ((1.0d0 . 1)
		  (1.1d0 . 1)
		  (5.0d0 . error))) ; 1 is the only acceptable value

	;; enum/uint8 <-> uint32 => can widen, cannot narrow
	(,+enum/uint8/simple+ ,+uint32+
	 nil   ((:a . 1)
		(:b . 2))
	 error ())

	;; enum/uint32 <-> uint32 => no problem
	(,+enum/uint32/simple+ ,+uint32+
	 nil     ((:a . 1)
		  (:b . 2))
	 nil     ((1  . :a)
		  (2  . :b)
		  (3  . error))) ; 3 is not a value of the enum

	;; enum/int32 <-> float32 => loss of precision
	(,+enum/int32/simple+ ,+float32+
	 nil     ((:a    . 1.0f0)
		  (:b    . 2.0f0))
	 warning ((1.0f0 . :a)
		  (2.0f0 . :b)
		  (3.0f0 . error))))

    (let+ ((left      (if (listp left)  (apply #'make-instance left)  left))
	   (right     (if (listp right) (apply #'make-instance right) right))
	   ((&with-gensyms source destination))
	   ((&flet generate-conversion (from to)
	      (let+ ((*context* (make-instance 'context))
		     ((&env (:source-var             source)
			    (:destination-var        destination)
			    (:lisp-toplevel-emitted? t))))
		(generate from `(:convert :to ,to) :lisp))))
	   ((&flet do-cases (expr cases)
	      (let+ (((&flet do-it (input)
			(eval `(let ((,source ,input) (,destination))
				 ,expr)))))
		(iter (for (input . expected) in cases)
		      (case expected
			(error (ensure-condition 'error (do-it input)))
			(t     (ensure-same (do-it input) expected
					    :test #'equal)))))))
	   ((&flet do-direction (from to condition cases)
	      (when-let ((expr (case condition
				 (warning (progn
					    (ensure-condition 'emit-warning
					      (generate-conversion from to))
					    (generate-conversion from to)))
				 (error   (ensure-condition 'emit-error
					    (generate-conversion from to)))
				 ((nil)   (generate-conversion from to)))))
		(do-cases expr cases)))))

      (do-direction left  right left->right-condition left->right-cases)
      (do-direction right left  right->left-condition right->left-cases))))

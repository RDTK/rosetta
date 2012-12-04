;;; emitter-conversion.lisp --- Unit tests for conversion emitter.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.backend.test)

(deftestsuite emitter-conversion-root (backend-root)
  ()
  (:setup
  ;; Generate enums
   (generate +enum/uint8/simple+ :class :lisp)
   (generate +enum/uint32/simple+ :class :lisp))
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
	((type-uint32)       (type-ascii-string) error nil       error   nil)
	((type-utf-8-string) (type-ascii-string) error nil       error   nil)

	;; uint32 <-> int32 => different signedness
	((type-uint32)  (type-int32)   error nil       error   nil)

	;; uint32 <-> uint64 => can widen, cannot narrow
	((type-uint32)  (type-uint64)  nil   ((1 . 1)) error   ((1 . 1)))

	;; uint32    <-> float64 => loss of precision
	((type-uint32)  (type-float64)
	 nil     ((1 . 1.0d0)
		  (5 . 5.0d0))
	 warning ((1.0d0 . 1)
		  (1.1d0 . 1)
		  (5.0d0 . 5)))

	;; float32    <-> float64 => loss of precision
	((type-float32) (type-float64)
	 nil     ((1 . 1.0d0)
		  (5 . 5.0d0))
	 warning ((1.0d0 . 1.0f0)
		  (1.1d0 . 1.1f0)
		  (5.0d0 . 5.0f0)))

	;; uint32 <-> uint32 => no problem
	((type-uint32)  (type-uint32)  nil   ((1 . 1)) nil     ((1 . 1)))

	;; singleton/uint32 <-> float64
	((singleton :type  ,(make-instance 'type-uint32)
		    :value 1)
	 (type-float64)
	 nil     ((1 . 1.0d0))
	 warning ((1.0d0 . 1)
		  (1.1d0 . 1)
		  (5.0d0 . error))) ; 1 is the only acceptable value

	;; enum/uint8 <-> uint32 => can widen, cannot narrow
	(,+enum/uint8/simple+ (type-uint32)
	 nil   ((:a . 1)
		(:b . 2))
	 error ())

	;; enum/uint32 <-> uint32 => no problem
	(,+enum/uint32/simple+ (type-uint32)
	 nil     ((:a . 1)
		  (:b . 2))
	 nil     ((1 . :a)
		  (2 . :b)
		  (3 . error))) ; 3 is not a value of the enum

	;; enum/uint32 <-> float32 => loss of precision
	(,+enum/uint32/simple+ (type-float32)
	 nil     ((:a . 1.0f0)
		  (:b . 2.0f0))
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

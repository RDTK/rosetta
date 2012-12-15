;;; emitter-conversion.lisp --- Emitter for conversions between types.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.backend)

(defmethod emit ((node     t)
		 (target   target-convert)
		 (language rs.m.l:language-lisp))
  ;; Call `emit-conversion' for source and destination (stored in
  ;; `target-to' of TARGET) type.
  (let+ (((&accessors-r/o (to target-to)) target)
	 ((&env-r/o destination-var)))
    `(setf ,destination-var ,(emit-conversion node to language))))


;;; Helper macros
;;

(defmacro defemit/conversion ((from to) &body body)
  "Define a conversion from type FROM to type TO implemented by BODY.

BODY can use the macro (with-conversion (FORM) &body BODY) to bind the
variable `source-form' to FORM around BODY. BODY can call
`call-next-method' if FORM produces the result of the conversion."
  (let+ (((&values body nil documentation)
	  (parse-body body :documentation t)))
    `(defmethod emit-conversion ((from     ,from)
				 (to       ,to)
				 (language rs.m.l:language-lisp))
       ,@(when documentation (list documentation))
       (let+ (((&env-r/o source-var))
	      ((&flet convert-using-coerce ()
		 `(coerce ,source-var ',(generate to :reference language)))))
	 (declare (ignorable #'convert-using-coerce))
	 ,@body))))


;;; Conversion methods
;;

(defemit/conversion (t t)
  (error 'conversion-error :from from :to to))


;;; Float and integer types
;;

(defemit/conversion (rs.m.d::integer-mixin rs.m.d::integer-mixin)
  ;; Reject signed/unsigned conversions.
  (when (xor (signed? from) (signed? to))
    (cerror "Force the conversion."
	    'simple-conversion-error
	    :from             from
	    :to               to
	    :format-control   "~@<Cannot convert between ~:[un~;~]signed ~
integer and ~:[un~;~]signed integer~:>"
	    :format-arguments (list (signed? from) (signed? to))))

  (cond
    ;; Reject narrowing conversion.
    ((< (width to) (width from))
     (cerror "Force the conversion."
	     'cannot-narrow :from from :to to)
     (convert-using-coerce))

    ;; Widening conversion.
    ((> (width to) (width from))
     (convert-using-coerce))

    ;; No conversion necessary.
    (t
     source-var)))

(defemit/conversion (rs.m.d::integer-mixin type-float*)
  (convert-using-coerce))

(defemit/conversion (type-float* rs.m.d::integer-mixin)
  (warn 'loss-of-precision :from from :to to)
  `(floor ,source-var))

(defemit/conversion (type-float* type-float*)
  (cond
    ;; Allow narrowing conversion, but warn.
    ((< (width to) (width from))
     (warn 'loss-of-precision :from from :to to)
     (convert-using-coerce))

    ;; Widening conversion.
    ((> (width to) (width from))
     (convert-using-coerce))

    ;; No conversion necessary.
    (t
     source-var)))


;;; `typed-mixin'
;;

(defemit/conversion (typed-mixin t)
  (emit-conversion (type1 from) to language))

(defemit/conversion (t typed-mixin)
  (emit-conversion from (type1 to) language))


;;; `singleton'
;;

(defemit/conversion (singleton t)
  (let+ (((&env (:source-var (value from)))))
    (call-next-method)))

(defemit/conversion (t singleton)
  (let ((result (call-next-method)))
    (cond
      ((constantp result)
       (let ((result (eval result)))
	 (unless (validate-value to result :if-invalid nil))
	 (cerror "Force the conversion."
		 'simple-conversion-error
		 :from             from
		 :to               to
		 :format-control   "~@<Cannot convert ~A to singleton value ~A.~@:>"
		 :format-arguments (list result (value to)))))
      ;; TODO(jmoringe): omit check at runtime when optimizing for speed
      (t
       (with-gensyms (result-var)
	`(let ((,result-var ,result))
	   (assert (equal ,result-var ,(value to)))
	   ,result-var))))))


;;; `enum'
;;

(defemit/conversion (enum t)
  (let+ (((&env (:source-var (generate from :value->code :lisp)))))
    (call-next-method)))

(defemit/conversion (t enum)
  (let+ (((&env (:source-var (call-next-method)))))
    (generate to :code->value :lisp)))

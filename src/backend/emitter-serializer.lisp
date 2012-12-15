;;; emitter-serializer.lisp --- Generic serialization-related emitter functions.
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


;;; Fundamental types
;;

(defmethod emit ((node     fixed-width-mixin)
		 (target   target-packed-size)
		 (language t))
  (max (ceiling (width node) 8) 1))

(macrolet
    ((define-variable-width-method ((target &optional qualifier) &body body)
       `(defmethod emit ,@(when qualifier `(,qualifier))
	  ((node     variable-width-mixin)
	   (target   ,target)
	   (language t))
	  (let+ (((&accessors-r/o length-type) (mechanism target))
		 (length-size (generate length-type :packed-size language)))
	    ,@body))))

  (define-variable-width-method (target-packed-size)
    (let+ (((&env-r/o source-var)))
      `(+ ,length-size ,(if source-var
			    `(length ,source-var)
			    0))))

  (define-variable-width-method (target-pack :around)
    (let+ (((&env-r/o source-var offset-var)))
      `(+ ,(let+ (((&env (:source-var (if source-var `(length ,source-var) 0)))))
	     (generate length-type :pack language))
	  ,(let+ (((&env (:offset-var `(+ ,offset-var ,length-size)))))
	     (call-next-method)))))

  (define-variable-width-method (target-unpack :around)
    (let+ (((&with-gensyms temp-var)))
      `(let ((,temp-var 0)) ;;; TODO(jmoringe, 2012-12-05): 0 is temp
	 (declare (type ,(generate length-type :reference language) ,temp-var))
	 (+ ,(let+ (((&env (:destination-var temp-var))))
	       (generate length-type :unpack language))
	    ,(let+ (((&env-r/o offset-var))
		    ((&env (:offset-var `(+ ,offset-var ,length-size))
			   (:end-var    `(+ ,offset-var ,length-size ,temp-var)))))
	       (call-next-method)))))))


;;; `typed-mixin'
;;

(defmethod emit ((node     typed-mixin)
		 (target   target-packed-size)
		 (language t))
  (generate (type1 node) target language))

(defmethod emit ((node     typed-mixin)
		 (target   target-pack)
		 (language t))
  (generate (type1 node) target language))

(defmethod emit ((node     typed-mixin)
		 (target   target-unpack)
			 (language t))
  (generate (type1 node) target language))


;;; Singleton types
;;

(defmethod emit/context ((node     singleton)
			 (target   target-packed-size)
			 (language t))
  (let+ (((&env (:source-var (value node)))))
    (call-next-method)))

(defmethod emit/context ((node     singleton)
			 (target   target-pack)
			 (language t))
  (let+ (((&env (:source-var (value node)))))
    (call-next-method)))

(defmethod emit/context ((node     singleton)
			 (target   target-unpack)
			 (language t))
  (let+ (((&accessors-r/o (type type1) value) node)
	 ((&env-r/o destination-var)))
    `(progn
       ,@(when destination-var
	   `((setf ,destination-var ,value)))
       ,(let+ (((&env (:destination-var nil))))
	  (generate type target language)))))


;;; Enum types
;;

(defmethod emit/context ((node     enum)
			 (target   target-pack)
			 (language t))
  (let+ ((values (contents node :value))
	 ((&env-r/o source-var))
	 (new-source-var
	  (cond
	    ((not source-var)
	     nil)

	    ;; If we are asked to pack a constant value, we can look
	    ;; it up now and just store the result at runtime.
	    ((constantp source-var)
	     (value (lookup node :value (string (eval source-var)))))

	    ;; If the enum NODE only has one value, we can just store
	    ;; that.
	    ((length= 1 values)
	     (value (first values)))

	    ;; Otherwise we generate code to perform the lookup at
	    ;; runtime.
	    (t
	     (generate node :value->code language)))))

    (let+ (((&env (:source-var new-source-var))))
      (call-next-method))))

(defmethod emit/context ((node     enum)
			 (target   target-unpack)
			 (language t))
  (let+ (((&accessors-r/o offset-type) (mechanism target))
	 ((&accessors-r/o (type type1)) node)
	 (values (contents node :value))
	 ((&env-r/o source-var destination-var)))
    (cond
      ;; If we are not supposed to store the unpacked value, we can
      ;; just delegate to the next method for `typed-mixin'.
      ((not destination-var)
       (call-next-method))

      ;; If we get a constant SOURCE-VAR (which is the numeric value),
      ;; we can lookup the corresponding symbolic now and emit a
      ;; static assignment.
      ((constantp source-var)
       (let ((value (lookup node :value (eval source-var))))
	 `(progn
	    (setf ,destination-var ,(generate value :instantiate language))
	    ,(let+ (((&env (:destination-var nil))))
	       (call-next-method)))))

      ;; If the enum NODE only has on value, we can generate a static
      ;; assignment of the symbolic name of that value.
      ((length= 1 values)
       `(progn
	  (setf ,destination-var ,(generate (first values) :instantiate language))
	  ,(let+ (((&env (:destination-var nil))))
	     (call-next-method))))

      ;; Otherwise we emit code to unpack the numeric value and then
      ;; lookup the symbolic value.
      (t
       (let+ (((&with-gensyms temp1 temp2)))
	 `(let* ((,temp1 0)
		 (,temp2 ,(let+ (((&env (:destination-var temp1))))
			    (call-next-method))))
	    (declare (type ,(generate type        :reference language) ,temp1)
		     (type ,(generate offset-type :reference language) ,temp2))
	    (setf ,destination-var ,(let+ (((&env (:source-var temp1))))
				      (generate node :code->value language)))
	    ,temp2))))))


;;; Fields
;;

(defmethod emit/context :before ((node     field-mixin)
				 (target   target-pack) ;; TODO extend to code-generating-target-mixin
				 (language t))
  (let+ (((&env-r/o (instance-var nil) (source-var nil))))
    (unless (or instance-var source-var)
      (error "~@<Exactly? one of ~S (~:[not supplied~;~:*~A supplied~]) ~
and ~S (~:[not supplied~;~:*~A supplied~]) has to be supplied for ~
~A~@:>"
	     :instance-var instance-var
	     :source-var   source-var
	     node))))

(defmethod emit/context ((node     field-mixin)
			 (target   target-packed-size)
			 (language t))
  (let+ (((&accessors-r/o (type type1)) node)
	 ((&env-r/o name (instance-var nil) (source-var nil)))
	 ((&env (:source-var (if (eq source-var t)
				 `(slot-value ,instance-var ',name)
				 source-var)))))
    (generate type target language)))

(defmethod emit/context ((node     field-mixin)
			 (target   target-pack)
			 (language t))
  (let+ (((&accessors-r/o (type type1)) node)
	 ((&env-r/o name offset-var (instance-var nil) (source-var nil)))
	 ((&env (:source-var (if (eq source-var t)
				 `(slot-value ,instance-var ',name)
				 source-var)))))
    `(incf ,offset-var ,(generate type target language))))

(defmethod emit/context ((node     field-mixin)
			 (target   target-unpack)
			 (language t))
  (let+ (((&accessors-r/o (type type1)) node)
	 ((&env-r/o name offset-var (instance-var nil) (destination-var nil)))
	 ((&env (:destination-var (if (eq destination-var t)
				      `(slot-value ,instance-var ',name)
				      destination-var)))))
    `(incf ,offset-var ,(generate type target language))))


;;; Structure types
;;

;;; TODO(jmoringe, 2012-04-25): all composites?
(macrolet
    ((define-structure-method (target op var)
       `(defmethod emit ((node     structure-mixin)
			 (target   ,target)
			 (language t))
	  (with-emit-symbols
	    (let+ (((&env-r/o ,var (locations nil)))
		   ((&env (instance-var ,var)))
		   ((&labels field-location (field)
		      (if locations (funcall locations field) t)))
		   ((&labels recur/location (field)
		      (let+ (((&values location nested-locations)
			      (field-location field))
			     ((&env (,var       location)
				    (:locations nested-locations))))
			(check-type nested-locations (or null function))
			(recur field)))))
	      (check-type locations (or null function))
	      (if ,(if (eq target 'target-pack) 'source-var t)
		  `(,',op ,@(mapcar #'recur/location (contents node :field)))
		  (generate node (make-target-like target :packed-size) language)))))))

  (define-structure-method target-packed-size +     source-var)
  (define-structure-method target-pack        progn source-var)
  (define-structure-method target-unpack      progn destination-var))


;;; Array types
;;

(defmethod emit/context ((node     array-mixin)
			 (target   mechanism-target-mixin)
			 (language t))
  (let+ (((&accessors-r/o element-type (fixed-dimensions? fixed-size?)) node)
	 ;; Compute element size expression with dummy
	 ;; SOURCE-VAR. This can only be constant if it does not use
	 ;; the dummy variable.
	 (element-size (let+ (((&env source-var)))
			 (generate element-type
				   (make-target-like target :packed-size)
				   language)))
	 ((&env (:fixed-dimensions? fixed-dimensions?)
		(:element-size      (when (constantp element-size)
				      (eval element-size))))))
    (call-next-method)))

(defmethod emit ((node     array-mixin)
		 (target   target-packed-size)
		 (language t))
  (let+ (((&accessors-r/o element-type index-type) node)
	 ((&env-r/o source-var fixed-dimensions? element-size (locations nil)))
	 ((&flet element-location (index)
	    (cond
	      ((or (not locations)
		   (eq (funcall locations index) t))
	       (if (eq index :length)
		   `(length ,source-var)
		   `(aref ,source-var ,index)))
	      (t
	       (funcall locations index))))))
    (cond
      ((and fixed-dimensions? (zerop (value index-type)))
       0)

      ((and fixed-dimensions? element-size)
       `(* ,element-size ,(value index-type)))

      ((and (not source-var) (not locations))
       (generate index-type target language))

      (element-size
       `(+ ,(generate index-type target language)
	   (* ,element-size ,(element-location :length))))

      (t
       (let+ (((&with-gensyms size i)))
	 `(let ((,size 0))
	    (declare (type ,(generate index-type :reference language) ,size))
	    (dotimes (,i ,(element-location :length))
	      (declare (type ,(generate index-type :reference language) ,i))
	      (incf ,size
		    ,(let+ (((&env (:source-var (element-location i)))))
		       (generate element-type target language))))
	    (+ ,(generate index-type target language) ,size)))))))

(defmethod emit ((node     array-mixin)
		 (target   target-pack)
		 (language t))
  (let+ (((&accessors-r/o element-type index-type) node)
	 ((&env-r/o source-var offset-var fixed-dimensions? element-size)))
    (cond
      ((or (not source-var)
	   (and fixed-dimensions?
		(or (zerop (value index-type))
		    element-size))) ;;; TODO(jmoringe, 2012-04-24):
       (let ((length (if source-var (value index-type) 0)))
	 `(progn
	    (incf ,offset-var ,(let+ (((&env (:source-var length))))
				 (generate index-type :pack language)))
	    ,@(iter (for i :from 0 :below length)
		    (collect
			(let+ (((&env (source-var `(aref ,source-var ,i))
				      (offset-var `(+ ,offset-var ,(* element-size i))))))
			  (generate element-type target language))))
	    ,@(when (plusp length)
		`((incf ,offset-var ,(* element-size length)))))))

      (element-size
       (let+ (((&with-gensyms i)))
	 `(progn
	    (incf ,offset-var
		  ,(let+ (((&env (source-var `(length ,source-var)))))
		     (generate index-type :pack language)))
	    (dotimes (,i (length ,source-var))
	      (declare (type ,(generate index-type :reference language) ,i))
	      ,(let+ (((&env (source-var `(aref ,source-var ,i))
			     (offset-var `(+ ,offset-var (* ,element-size ,i))))))
	         (generate element-type target language)))
	    (incf ,offset-var (* ,element-size (length ,source-var))))))

      (t
       (let+ (((&with-gensyms i)))
	 `(progn
	    (incf ,offset-var
		  ,(let+ (((&env (source-var `(length ,source-var)))))
		     (generate index-type :pack language)))
	    (dotimes (,i (length ,source-var))
	      (declare (type ,(generate index-type :reference language) ,i))
	      (incf ,offset-var
		    ,(let+ (((&env (source-var `(aref ,source-var ,i)))))
		       (generate element-type target language))))))))))

(defmethod emit ((node     array-mixin)
		 (target   target-unpack)
		 (language t))
  (let+ (((&accessors-r/o element-type index-type) node)
	 ((&env-r/o destination-var offset-var fixed-dimensions? element-size)))
    (cond
      ((and fixed-dimensions?
	    (or (zerop (value index-type))
		element-size))
       (let+ ((length (value index-type)))
	 `(progn
	    (incf ,offset-var ,(let+ (((&env (:destination-var nil))))
				 (generate index-type :unpack language)))
	    ,@(iter (for i :from 0 :below length)
		    (collect
			(let+ (((&env (:destination-var (when destination-var
							  `(aref ,destination-var ,i)))
				      (:offset-var      `(+ ,offset-var ,(* element-size i))))))
			  (generate element-type target language))))
	    ,@(when (plusp length)
		`((incf ,offset-var ,(* element-size length)))))))

      (element-size
       (let+ (((&with-gensyms i length)))
	 `(let ((,length))
	    (declare (type ,(generate index-type :reference language) ,length))
	    (incf ,offset-var
		  ,(let+ (((&env (:destination-var length))))
		     (generate index-type :unpack language)))
	    (adjust-array ,destination-var (,length))
	    (dotimes (,i (length ,length))
	      (declare (type ,(generate index-type :reference language) ,i))
	      ,(let+ (((&env (:destination-var (when destination-var
						 `(aref ,destination-var ,i)))
			     (:offset-var      `(+ ,offset-var (* ,element-size ,i))))))
	         (generate element-type target language)))
	    (incf ,offset-var (* ,element-size ,length)))))

      (t
       (let+ (((&with-gensyms i length)))
	 `(let ((,length))
	    (declare (type ,(generate index-type :reference language) ,length))
	    (incf ,offset-var
		  ,(let+ (((&env (:destination-var length))))
		     (generate index-type :unpack language)))
	    (adjust-array ,destination-var (,length))
	    (dotimes (,i ,length)
	      (declare (type ,(generate index-type :reference language) ,i))
	      (incf ,offset-var
		    ,(let+ (((&env (:destination-var (when destination-var
						       `(aref ,destination-var ,i))))))
		       (generate element-type target language))))))))))


;;; Offset computation
;;

(defun invoke-with-offset-computation (target language thunk)
  (let+ ((offset-type (offset-type (mechanism target)))
	 ((&env-r/o start-var))
	 ((&env (:offset-computation-emitted? t) offset-var)))
    `(let ((,offset-var ,start-var))
       (declare (type ,(generate offset-type :reference language) ,offset-var))
       ,(let+ (((&env (:start-var offset-var))))
	  (funcall thunk))
       (- ,offset-var ,start-var))))

(defmacro with-offset-computation ((target language) &body body)
  `(invoke-with-offset-computation ,target ,language #'(lambda () ,@body)))

(macrolet
    ((define-offset-computation-method (target)
       `(defmethod emit/context :around ((node     t)
					 (target   ,target)
					 (language t))
	  (if (requires-offset-computation? node (mechanism target))
	      (with-offset-computation (target language)
		(call-next-method))
	      (call-next-method)))))

  (define-offset-computation-method target-pack)
  (define-offset-computation-method target-unpack))

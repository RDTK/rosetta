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


;;; Generic behavior
;;

;;; TODO(jmoringe, 2012-06-06): called too often: LANGUAGE may still be a symbol or list
(defmethod generate :before ((node     t)
			     (target   mechanism-target-mixin)
			     (language t)
			     &key)
  (when (mechanism target) ;;; TODO(jmoringe, 2012-05-08): ok?
    (rs.m.s:validate-type (mechanism target) node)))

#+difficult
(macrolet
    ((define-skip-method (type)
       `(defmethod emit ((node     ,type)
			 (target   target-pack)
			 (language t)
			 &key)
	  (let+ (((&env-r/o source-var)))
	    (if source-var
		(call-next-method)
		(generate node :packed-size language))))))

  (define-skip-method fundamental-type-mixin)
  (define-skip-method enum)
  (define-skip-method structure-mixin))


;;; Fundamental types
;;

(defmethod emit/leaf ((node     fixed-width-mixin)
		      (target   target-packed-size)
		      (language t))
  (max (ash (width node) -3) 1))

(defmethod emit ((node     fixed-width-mixin)
		 (target   target-unpack)
		 (language t)
		 &key)
  (let+ (((&env-r/o destination-var)))
    (if destination-var
	(call-next-method)
	(generate node (list target :packed-size) language))))

(macrolet
    ((define-variable-width-method ((target &optional qualifier) &body body)
       `(defmethod emit/leaf ,@(when qualifier `(,qualifier))
	  ((node     variable-width-mixin)
	   (target   ,target)
	   (language t))
	  (let+ ((mechanism (or (mechanism target)
				(mechanism (second (second (context-stack *context*)))))) ;; TODO hack
		 ((&accessors-r/o length-type) mechanism)
		 (length-size (generate length-type :packed-size language)))
	    ,@body))))

  (define-variable-width-method (target-packed-size)
    (let+ (((&env-r/o source-var)))
      `(+ ,length-size ,(if source-var
			    `(length ,source-var)
			    0))))

  (define-variable-width-method (target-pack :around)
    (let+ (((&env-r/o source-var offset-var)))
      `(+ ,(let+ (((&env (source-var (if source-var `(length ,source-var) 0)))))
	     (generate length-type :pack language))
	  ,(let+ (((&env (offset-var `(+ ,offset-var ,length-size)))))
	     (call-next-method)))))

  (define-variable-width-method (target-unpack :around)
    (let+ (((&env-r/o offset-var))
	   ((&with-gensyms temp-var)))
      `(let ((,temp-var))
	 (declare (type ,(generate length-type :reference language) ,temp-var))
	 (+ ,(let+ (((&env (destination-var temp-var))))
	       (generate length-type :unpack language))
	    ,(let+ (((&env (offset-var `(+ ,offset-var ,length-size))
			   (end-var    `(+ ,offset-var ,temp-var)))))
		   (call-next-method)))))))


;;; `typed-mixin'
;;

;;; TODO(jmoringe, 2012-05-10): simplify
(defmethod emit ((node     typed-mixin)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (emit (type1 node) target language))

(defmethod emit ((node     typed-mixin)
		 (target   target-pack)
		 (language t)
		 &key)
  (emit (type1 node) target language))

(defmethod emit ((node     typed-mixin)
		 (target   target-unpack)
		 (language t)
		 &key)
  (emit (type1 node) target language))


;;; Singleton types
;;

(defmethod emit ((node     singleton)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o (type rs.m.d::type1) (value rs.m.d::value)) node)
	 ((&env (source-var value))))
    (emit type target language)))

(defmethod emit ((node     singleton)
		 (target   target-pack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o (type rs.m.d::type1) (value rs.m.d::value)) node)
	 ((&env-r/o offset-var))
	 ((&env (source-var value))))
    `(incf ,offset-var ,(emit type target language))))

(defmethod emit ((node     singleton)
		 (target   target-unpack)
		 (language t)
		 &key)
  "Generate code to unpack a single field."
  (let+ (((&accessors-r/o (type rs.m.d::type1) (value rs.m.d::value)) node)
	 ((&env-r/o offset-var destination-var)))
    `(progn
       (incf ,offset-var ,(emit type :packed-size language))
       ,@(when destination-var
	   `((setf ,destination-var ,value))))))


;;; Fields
;;

(defmethod emit ((node     field-mixin)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o name (type type1)) node)
	 ((&env-r/o instance-var))
	 ((&env (source-var `(slot-value ,instance-var ,(make-keyword name))))))
    (emit type target language)))

(defmethod emit ((node     field-mixin)
		 (target   target-pack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o name (type type1)) node)
	 ((&env-r/o offset-var ((instance :source-var))))
	 ((&env (source-var `(slot-value ,instance ,(make-keyword name))))))
    `(incf ,offset-var ,(emit type target language))))

(defmethod emit ((node     field-mixin)
		 (target   target-unpack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o name (type type1)) node)
	 ((&env-r/o offset-var (destination-var nil) (instance-var nil)))
	 ((&env (destination-var (progn
				   (cond
				    ((eq destination-var :skip)
				     nil)
				    (destination-var)
				    (instance-var
				     `(slot-value ,instance-var ,(make-keyword name)))
				    (t
				     (error "No destination"))))))))
    `(incf ,offset-var ,(emit type target language))))


;;; Structure types
;;

;;; TODO(jmoringe, 2012-04-27): to this?
(defmethod emit :around ((node     structure-mixin)
			 (target   target-packed-size)
			 (language t)
			 &key)
  (if (contents node :fields)
      (call-next-method)
      0))

;;; TODO(jmoringe, 2012-04-25): all composites?
(macrolet
    ((define-structure-method (target op)
       `(defmethod emit ((node     structure-mixin)
			 (target   ,target)
			 (language t)
			 &key)
	  (with-emit-symbols
	    (let+ (((&env-r/o source-var))
		   ((&env (instance-var source-var)))
		   (destinations (or (context-get *context* :destinations :default nil)
				     #+no (make-list (length (composite-children node))
						:initial-element t)
				     (make-hash-table :test #'eq)))
		   ((&flet do-child (child)
		      (let ((destination (or (gethash child destinations)
					     (let+ (((&values value found?)
						     (gethash :default destinations)))
					       (if found?
						   value
						   t)))))
		       (case destination
			 ((t)
			  (recur child))
			 (t
			  (let+ (((&env (destination-var destination))))
			    (recur child))))))))
	      `(,',op ,@(map 'list #'do-child (contents node :fields))))))))

  (define-structure-method target-packed-size +)
  (define-structure-method target-pack        progn)
  (define-structure-method target-unpack      progn))


;;; Array types
;;

(defmethod emit ((node     array-mixin)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o index-type element-type (fixed-size? rs.m.d::fixed-size?)) node)
	 ((&env-r/o source-var))
	 (element-size (emit element-type target language)))
    (if fixed-size?
	(let+ ((length (rs.m.d::value (index-type node))))
	  `(* ,element-size ,length))
	`(+ ,(emit index-type target language)
	    (* ,(emit element-type target language)
	       (length ,source-var))))))

(defmethod emit ((node     array-mixin)
		 (target   target-pack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o index-type element-type (fixed-size? rs.m.d::fixed-size?)) node)
	 ((&env-r/o source-var offset-var))
	 (element-size (emit element-type :packed-size language))) ;;; TODO(jmoringe, 2012-04-24): same mechanism!
    (if fixed-size? ;;; TODO(jmoringe, 2012-04-24):
	(let+ ((length (rs.m.d::value (index-type node))))
	  `(+ ,@(iter (for i :from 0 :below length)
		      (collect
			  (let+ (((&env (source-var `(aref ,source-var ,i))
					(offset-var `(+ ,offset-var ,(* element-size i))))))
			    (emit element-type target language))))))
	(let+ (((&with-gensyms i)))
	  `(+ ,(let+ (((&env (source-var `(length ,source-var)))))
		     (emit index-type target language))
	      (dotimes (,i (length ,source-var))
		,(let+ (((&env (source-var `(aref ,source-var ,i))
			       (offset-var `(+ ,offset-var (* ,element-size ,i))))))
		   (emit element-type target language))))))))

(defmethod emit ((node     array-mixin)
		 (target   target-unpack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o index-type element-type (fixed-size? rs.m.d::fixed-size?)) node)
	 ((&env-r/o destination-var offset-var))
	 (element-size (emit element-type :packed-size language))) ;;; TODO(jmoringe, 2012-04-24): same mechanism!
    (if fixed-size? ;;; TODO(jmoringe, 2012-04-24):
	(let+ ((length (rs.m.d::value (index-type node))))
	  `(progn ,@(iter (for i :from 0 :below length)
			  (collect
			      (let+ (((&env (destination-var `(aref ,destination-var ,i))
					    (offset-var      `(+ ,offset-var ,(* element-size i))))))
				(emit element-type target language))))))
	(let+ (((&with-gensyms i length)))
	  `(dotimes (,i ,(let+ (((&env (destination-var length))))
			   (emit index-type target language)))
	     (incf ,offset-var ,(let+ (((&env (destination-var `(aref ,destination-var ,i))
					     #+no (offset-var `(+ ,offset-var (* ,element-size ,i))))))
				     (emit element-type target language))))))))


;;; Toplevel
;;

(declaim (special *toplevel-emitted?*))

(defvar *toplevel-emitted?* nil
  "TODO(jmoringe): document")

(macrolet
    ((define-toplevel-method (target)
       `(defmethod emit :around ((node     toplevel-mixin)
				 (target   ,target)
				 (language t)
				 &key)
	  (if *toplevel-emitted?*
	      (call-next-method)
	      (let+ ((*toplevel-emitted?* t)
		     ((&env-r/o start-var))
		     ((&env offset-var)))
		`(let ((,offset-var ,start-var))
		   (declare (type ,(emit (make-instance 'rs.m.d::type-uint64) :class language) ,offset-var)) ;;; TODO(jmoringe, 2012-05-02):
		   ,(let+ (((&env (start-var offset-var))))
		      (call-next-method))
		   (- ,offset-var ,start-var)))))))

  (define-toplevel-method target-pack)
  (define-toplevel-method target-unpack))

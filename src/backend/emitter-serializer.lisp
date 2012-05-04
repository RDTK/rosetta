;;; emitter-serializer.lisp --- Generic serialization-related emitter functions.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rosetta.backend)


;;; Checks
;;

(defmethod emit :before ((node     t)
			 (target   mechanism-target-mixin)
			 (language t)
			 &key
			 (validate? t))
  (when validate?
    (rs.m.s:validate-type (mechanism target) node)))


;;; Fundamental types
;;

(defmethod emit ((node     rs.m.d::fundamental-type-mixin)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (ash (width node) -3))

(defmethod emit ((node     rs.m.d::type-octet-vector)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (let+ (((&env-r/o source-var)))
    `(length ,source-var)))

;;; TODO(jmoringe, 2012-04-24): variable-size-mixin
(defmethod emit ((node     rs.m.d::type-string*)
		 (target   target-packed-size)
		 (language t)
		 &key)
  (let+ (((&env-r/o source-var)))
    `(length ,source-var)))


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
  (let+ (((&accessors-r/o (name field-name)
			  (type field-type)) node)
	 ((&env-r/o instance-var))
	 ((&env (source-var `(slot-value ,instance-var ,(make-keyword name))))))
    (emit type target language)))

(defmethod emit ((node     field-mixin)
		 (target   target-pack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o (name field-name)
			  (type field-type)) node)
	 ((&env-r/o offset-var ((instance :source-var))))
	 ((&env (source-var `(slot-value ,instance ,(make-keyword name))))))
    `(incf ,offset-var ,(emit type target language))))

(defmethod emit ((node     field-mixin)
		 (target   target-unpack)
		 (language t)
		 &key)
  (let+ (((&accessors-r/o (name field-name)
			  (type field-type)) node)
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
  (if (composite-children node)
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
	      `(,',op ,@(mapcar #'do-child (composite-children node))))))))

  (define-structure-method target-packed-size +)
  (define-structure-method target-pack        progn)
  (define-structure-method target-unpack      progn))


;;; Array types
;;

(defmethod emit ((node     rs.m.d::array-mixin)
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

(defmethod emit ((node     rs.m.d::array-mixin)
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

(defmethod emit ((node     rs.m.d::array-mixin)
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
       `(defmethod emit :around ((node     rs.m.d::toplevel-mixin)
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

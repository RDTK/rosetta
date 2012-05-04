;;; target-class.lisp --- Generate Lisp classes data type definitions.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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


;;; Generic behavior
;;

#+maybe
(defmethod emit :around ((node     package1)
			 (target   method-target-mixin)
			 (language language-lisp)
			 &key)
  (let* ((package-name (pb::file-desc-package node))
	 (package      (progn
			 (pbb::maybe-make-package package-name)
			 (pbb::maybe-find-package-or-loose package-name))))
    (setf (context-package *context*) package)
    (unwind-protect
	 (handler-bind (#+sbcl (sb-c::redefinition-warning #'muffle-warning))
	   (call-next-method))
      (setf (context-package *context*) nil))))

#+maybe
(defmethod emit :around ((node     toplevel-mixin)
			 (target   method-target-mixin)
			 (language language-lisp)
			 &key)
  (let* ((package-name (pb::file-desc-package node))
	 (package      (progn
			 (pbb::maybe-make-package package-name)
			 (pbb::maybe-find-package-or-loose package-name))))
    (setf (context-package *context*) package)
    (unwind-protect
	 (handler-bind (#+sbcl (sb-c::redefinition-warning #'muffle-warning))
	   (call-next-method))
      (setf (context-package *context*) nil))))

(defmethod emit :around ((node     rs.m.d::toplevel-mixin)
			 (target   code-generating-target-mixin)
			 (language language-lisp)
			 &key)
  (let+ (((&env-r/o (toplevel? t)))
	 ((&env (toplevel? nil)))
	 ((&accessors-r/o optimization-settings) target)
	 (speed (second (find 'speed optimization-settings
			      :key #'first))))
    (if toplevel?
	(handler-bind (#+sbcl (sb-c::redefinition-warning #'muffle-warning))
	  (eval `(progn
		   (declaim (optimize ,@(when speed `((speed ,speed)))))
		   ,(call-next-method))))
	(call-next-method))))


;;; Generic stuff
;;

(defmethod emit :around ((node     named-mixin)
			 (target   t)
			 (language language-lisp)
			 &key)
  (let+ (((&env (name (intern (data-type-name node)))))) ;;; TODO(jmoringe, 2012-05-04): lispify name
    (call-next-method)))

(defmethod emit :after ((node     rs.m.d::documentation-mixin)
			(target   target-class)
			(language language-lisp)
			&key)
  (when-let ((name          (context-get *context* :name :default nil))
	     (documentation (rs.m.d::documentation1 node)))
    (setf (documentation name 'type) documentation)))

(defmethod emit :after ((node     rs.m.d::documentation-mixin)
			(target   code-generating-target-mixin)
			(language language-lisp)
			&key)
  (when-let ((name          (context-get *context* :name :default nil))
	     (documentation (rs.m.d::documentation1 node)))
    (setf (documentation name 'function) documentation)))

(defmethod emit :after ((node     t)
			(target   code-generating-target-mixin)
			(language language-lisp)
			&key)
  (when-let ((name (context-get *context* :name :default nil)))
    (export name (symbol-package name))))
;;; TODO(jmoringe): config in target; could use a predicate (funcall export node)


;;; Enum types
;;

(defmethod emit :around ((node     rs.m.d::enum)
			 (target   target-class)
			 (language language-lisp)
			 &key)
  "Emit an enum definition for NODE."
  (let+ (((&env-r/o name))
	 ((&env (name-name (format-symbol (symbol-package name)
					  "~A-NAME" name))
		(code-name (format-symbol (symbol-package name)
					  "~A-CODE" name)))))
    (call-next-method)))

(defmethod emit ((node     rs.m.d::enum)
		 (target   target-class)
		 (language language-lisp)
		 &key)
  "Emit an enum definition for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o (values composite-children)) node)
	   ((&env-r/o name name-name code-name))
	   (pairs (mapcar #'recur values))) ;;; TODO(jmoringe, 2012-05-04): call-next-method?
      `(progn
	 (deftype ,name ()
	   '(member ,@(mapcar #'first pairs)))

	 (defun ,name-name (code)
	   (case code
	     ,@(mapcar #'reverse pairs)
	     (t (error "~@<Code ~D is invalid for enum ~S.~@:>"
		       code ',name))))

	 (defun ,code-name (name)
	   (case name
	     ,@pairs
	     (t (error "~@<Symbol ~S is invalid for enum ~S.~@:>"
		       name ',name))))))))

(defmethod emit ((node     rs.m.d::enum-value)
		 (target   target-class)
		 (language language-lisp)
		 &key)
  "Emit an enum definition for NODE."
  (let+ (((&env-r/o name)))
    (list (make-keyword name) (rs.m.d::value node)))) ;;; TODO(jmoringe, 2012-05-04):


;;; Structure
;;

(defmethod emit ((node     field-mixin)
		 (target   target-class)
		 (language language-lisp)
		 &key)
  "Emit a slot specification for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o (type field-type)) node)
	   ((&env-r/o name))
	   (initarg     (make-keyword name))
	   (type        (recur type))
	   (reader-name name)
	   (writer-name name)
	   (initform    nil))
      `(,name :initarg ,initarg
	      :type    ,type
	      :reader  ,reader-name
	      :writer  ,writer-name
	      ,@(when nil
	          `(:initform ,initform))))))

(defmethod emit ((node     structure-mixin)
		 (target   target-class)
		 (language language-lisp)
		 &key)
  "Define a Lisp class for NODE. "
  (with-emit-symbols
    (let+ (((&accessors-r/o (fields composite-children)) node)
	   #+no((&accessors-r/o
		 (metaclass    target-metaclass)
		 (superclasses target-superclasses)) target)
	   ((&env-r/o name)))
      ;; Emit the actual class definition.
      `(progn
	 (defclass ,name () ()) ;; TODO do we want to this here?
	 (defclass ,name (#+no ,@superclasses)
	   ,(mapcar #'recur fields) ;;; TODO(jmoringe, 2012-05-04): next method should do this
	   #+no ,@(when metaclass
			`((:metaclass ,metaclass))))))))


;;; Class Generation
;;

#+no
(defun generate-initform (type repeated? packed?
			  &key
			    (default nil default-supplied?))
  "Generate an initform for a slot of type TYPE."
  (cond
    ;; Scalar types
    ((not repeated?)
     (cond
       ((eq type :bool)
	(if default-supplied?
	    (cond
	      ((string= default "true")  t)
	      ((string= default "false") nil)
	      (t                         (error "invalid default"))) ;; TODO
	    nil))
       ((eq type :double)
	(if default-supplied?
	    (coerce (read-from-string default) 'double-float)
	    0d0))
       ((eq type :float)
	(if default-supplied?
	    (coerce (read-from-string default) 'single-float)
	    0s0))
       ((enum-type-p type)
	(if default-supplied?
	    (make-lisp-enum-value default)
	    nil))
       ((integer-type-p type)
	(if default-supplied?
	    (read-from-string default)
	    0))
       ((eq type :string)
	(or default ""))
       ((eq type :bytes)
	(if default-supplied?
	    (sb-ext:string-to-octets default) ;; TODO this is not correct: from descriptor.proto: For bytes, contains the C escaped value.  All bytes >= 128 are escaped.
	    '(make-array 0 :element-type '(unsigned-byte 8))))
       ((find-class type)
	#+or-maybe?
	`(let ((class (find-class ',type)))
	   (unless (closer-mop:class-finalized-p class)
	     (closer-mop:finalize-inheritance class))
	   (closer-mop:class-prototype class))
	`(make-instance ',type))

       (t
	(error "Cannot generate initform for ~:[~; repeated~] ~:[~; ~
packed~] type ~S"
	       repeated? packed? type)))) ;; TODO can this happen? proper condition?

    ;; Not packed array
    ((not packed?)
     `(make-array 0
		  :element-type ',(proto-type->lisp-type type)
		  :fill-pointer t
		  :adjustable   t))

    ;; Packed array
    (t
     `(make-array 0
		  :element-type ',(proto-type->lisp-type type)
		  :fill-pointer nil
		  :adjustable   nil))))

#+no
(defun generate-slot (name type label packed?
		      &key
			class-name
			(default nil default-supplied?))
  (let ((repeated? (eq label :repeated))
	(optional? (eq label :optional)))
    `(,name
      :initarg  ,(make-keyword name)
      :type     ,(proto-type->lisp-type type repeated? optional?)
      ,@(when class-name
	      `(:accessor ,(%make-lisp-accessor-name class-name name)))
      ;; TODO maybe (unless optional?
      :initform ,(apply #'generate-initform type repeated? packed?
			(when default-supplied?
			  (list :default default))))))

#+no
(defun generate-coercing-writer (name type label packed?
				 &key
				   class-name)
  (let* ((accessor-name (%make-lisp-accessor-name class-name name))
	 (repeated?     (eq label :repeated))
	 (optional?     (eq label :optional))
	 (slot-type     (proto-type->lisp-type type repeated? optional?)))
    `(defmethod (setf ,accessor-name) :around ((new-value sequence)
					       (instance  ,class-name))
		(if (typep new-value ,slot-type)
		    (call-next-method)
		    (call-next-method
		     (make-array (length new-value)
				 :element-type     ',(proto-type->lisp-type type)
				 :fill-pointer     t
				 :adjustable       t
				 :initial-contents new-value)
		     instance)))))

#+test
(emit (make-instance 'rs.m.d::base-structure
		     :name "foo"
		     :documentation "1 2 3")
      :class
      :lisp)

#+test
(emit (make-instance 'rs.m.d::enum
		     :name "foo"
		     :fields `("bla" ,(make-instance 'rs.m.d::enum-value
						     :name "bla"
						     :type (make-instance 'rs.m.d::type-uint32)
						     :value 5))
		     :documentation "1 2 3")
      :class
      :lisp)

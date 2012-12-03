;;; target-class.lisp --- Generate Lisp classes data type definitions.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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


;;; Evaluate generated code
;;

(defmethod emit/context :around ((node     toplevel-mixin)
				 (target   code-generating-target-mixin)
				 (language language-lisp))
  ;; If lisp-toplevel-emitted? is nil, bind it to t to prevent next
  ;; method/recursive calls from evaluating anything. With
  ;; lisp-toplevel-emitted? bound to t, call next method to obtain
  ;; generated code, then evaluate it.
  ;;
  ;; If lisp-toplevel-emitted? is non-nil, just call the next method.
  (let+ (((&env-r/o (lisp-toplevel-emitted? nil)))
	 ((&env ((nil :lisp-toplevel-emitted?) t))))
    (if lisp-toplevel-emitted?
	(call-next-method)
	(let ((code (call-next-method)))
	  (handler-bind
	      (#+sbcl (sb-c::redefinition-warning #'muffle-warning)
	       (error #'(lambda (condition)
			  (error "~@<Failed to compile code~2%~S~2%Caused by:~%~A~@:>"
				 code condition))))
	    (eval code))))))


;;; Generic stuff
;;

(defmethod emit/context ((node     named-mixin)
			 (target   t)
			 (language language-lisp))
  (let+ (((&env (name (intern (name node)))))) ;;; TODO(jmoringe, 2012-05-04): lispify name
    (call-next-method)))

(defmethod emit :after ((node     documentation-mixin)
			(target   target-class)
			(language language-lisp))
  (when-let ((name          (context-get *context* :name :default nil))
	     (documentation (documentation1 node)))
    (setf (documentation name 'type) documentation)))

(defmethod emit :after ((node     documentation-mixin)
			(target   method-target-mixin)
			(language language-lisp))
  (when-let ((name          (context-get *context* :name :default nil))
	     (documentation (documentation1 node)))
    (setf (documentation name 'function) documentation)))

(defmethod emit :after ((node     t)
			(target   code-generating-target-mixin)
			(language language-lisp))
  (when-let ((name (context-get *context* :name :default nil)))
    (export name (symbol-package name))))
;;; TODO(jmoringe): config in target; could use a predicate (funcall export node)

(defmethod emit ((node     named-mixin)
		 (target   target-reference)
		 (language language-lisp))
  (let+ (((&env-r/o name)))
    name))


;;; Fundamental types
;;

(defmethod emit ((node     fundamental-type-mixin)
		 (target   target-class)
		 (language language-lisp))
  (ecase (category node)
    (:integer
     `(,(if (signed? node) 'signed-byte 'unsigned-byte) ,(width node)))
    (:float
     (ecase (width node)
       (32 'single-float)
       (64 'double-float)))
    (:string
     'string)
    (:bytes
     'nibbles:octet-vector)))

(defmethod emit ((node     fundamental-type-mixin)
		 (target   target-reference)
		 (language language-lisp))
  (generate node :class language))

(defmethod emit ((node     type-octet-vector)
		 (target   target-instantiate)
		 (language t))
  `(nibbles:octet-vector))


;;; Singleton type
;;

(defmethod emit ((node     singleton)
		 (target   target-class)
		 (language language-lisp))
  `(eql ,(value node)))

(defmethod emit ((node     singleton)
		 (target   target-reference)
		 (language language-lisp))
  (generate node :class language))


;;; Enum types
;;

(defmethod emit :around ((node     enum)
			 (target   target-class)
			 (language language-lisp))
  "Emit an enum definition for NODE."
  (let+ (((&env-r/o name))
	 ((&env (name-name (symbolicate name '#:-name))
		(code-name (symbolicate name '#:-code)))))
    (call-next-method)))

(defmethod emit ((node     enum)
		 (target   target-class)
		 (language language-lisp))
  (with-emit-symbols
    (let+ (((&env-r/o name name-name code-name))
	   (pairs (map 'list #'recur (contents node :value))))
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
		       name ',name))))

	 (values ',name ',name-name ',code-name)))))

(defmethod emit ((node     enum-value)
		 (target   target-class)
		 (language language-lisp))
  (let+ (((&env-r/o name)))
    (list (make-keyword name) (value node))))

(defmethod emit ((node     enum)
		 (target   target-value->code)
		 (language language-lisp))
  (let+ (((&env-r/o source-var code-name)))
    `(,code-name ,source-var)))


;;; Structure
;;

(defmethod emit ((node     field-mixin)
		 (target   target-class)
		 (language language-lisp))
  "Emit a slot specification for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o (type type1)) node)
	   ((&env-r/o name))
	   (initarg     (make-keyword name))
	   (type        (if (typep type '(or fundamental-type-mixin singleton))
			    (recur type)
			    (progn
			      (let+ (((&env (lisp-toplevel-emitted? nil))))
				(recur type))
			      t #+later (name type)))) ;;; TODO(jmoringe, 2012-05-09): dependency architecture
	   (reader-name name)
	   (writer-name `(setf ,name))
	   (initform    nil))
      `(,name :initarg ,initarg
	      :type    ,type
	      :reader  ,reader-name
	      :writer  ,writer-name))))

(defmethod emit ((node     structure-mixin)
		 (target   target-class)
		 (language language-lisp))
  "Generate code which defines a CLOS class for NODE."
  (with-emit-symbols
    (let+ (((&accessors-r/o
	     (metaclass    target-metaclass)
	     (superclasses target-superclasses)) target)
	   ((&env-r/o name)))
      ;; Emit the actual class definition.
      `(progn
	 (defclass ,name () ())
	 (defclass ,name (,@superclasses)
	   ,(map 'list #'recur (contents node :field))
	   ,@(when metaclass
	       `((:metaclass ,metaclass))))))))

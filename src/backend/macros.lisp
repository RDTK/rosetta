;;; macros.lisp --- Macros for backends.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rosetta.backend)


;;; Housekeeping macros
;;

(defmacro with-emit-restarts ((node target) &body body)
  "Establish restarts."
  (with-unique-names (result-var read-value-var)
    (once-only (node target)
      `(bind ((,result-var)
	      ((:flet ,read-value-var ())
	       (format *query-io* "Replacement value: ")
	       (force-output *query-io*)
	       (list (read *query-io*))))
	 (tagbody
	  :retry
	    (restart-case
		(setf ,result-var (multiple-value-list (progn ,@body)))

	      ;; Retry running the emit method.
	      (retry ()
		:report
		(lambda (stream)
		  (format stream
			  "~@<Retry running the emit method for node ~
~S and target ~S.~@:>"
			  ,node ,target))
		(go :retry))

	      ;; Skip the emit method.
	      (skip ()
		:report
		(lambda (stream)
		  (format stream
			  "~@<Skip the emit method for node ~S and ~
target ~S.~@:>"
			  ,node ,target)))

	      ;; Use a replacement value.
	      (use-value (value)
		:report
		(lambda (stream)
		  (format stream
			  "~@<Specify a value instead of running the ~
emit method for node ~S and target ~S.~@:>"
			  ,node ,target))
		:interactive ,read-value-var
		(setf ,result-var (list value)))))
	 (values-list ,result-var)))))

(defmacro with-updated-context ((node-var target-var &optional (language-var :keep))
				&body body)
  "During the execution of BODY, set the current target type to
TARGET-VAR and push NODE-VAR onto the context stack."
  (with-unique-names (old-target-var old-language-var)
    `(let ((,old-target-var   (context-target *context*))
	   (,old-language-var (context-language *context*)))
       (setf (context-target   *context*) ,target-var
	     ,@(unless (eq language-var :keep)
		       `((context-language *context*) ,language-var)))
       (push ,node-var (context-stack *context*))
       (push (make-hash-table) (%context-environment *context*))
       (unwind-protect
	    (progn ,@body)
	 (pop (%context-environment *context*))
	 (pop (context-stack *context*))
	 (setf (context-target   *context*) ,old-target-var
	       (context-language *context*) ,old-language-var)))))


;;; Convenience macros for clients
;;

(defmacro with-emit-symbols (&body body)
  "Execute BODY with the following symbols added to the lexical scope:
+ package           :: The package that is the current target of the
                       emission process.
+ parent            :: The parent of the current node or nil.
+ grandparent       :: The parent of the parent of the current node
                       or nil.
+ ancestors         :: List of all ancestor nodes.
+ recur             :: A closure that accepts a node and calls `emit'
                       with the current target and language on that node.
+ cget, (setf cget) :: Retrieve and store values in the current
                       environment of context.
+ intern*           :: Similar to `intern' but use the context package."
  `(symbol-macrolet ((package     (context-package *context*))
		     (stack       (context-stack *context*))
		     (parent      (second (context-stack *context*)))
		     (grandparent (third (context-stack *context*)))
		     (ancestors   (rest (context-stack *context*))))
     (flet ((recur (node)
	      (emit node
		    (context-target *context*)
		    (context-language *context*)))
	    (cget (key)
	      (context-get *context* key))
	    ((setf cget) (new-value key)
	      (setf (context-get *context* key) new-value))
	    (intern* (name) ;; TODO rename
	      (intern name package)))
       (declare (ignorable #'recur #'cget #'(setf cget) #'intern*))
       ,@body)))

;;; let-plus.lisp --- Binding forms for things in the emit context.
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

(define-let+-expansion (&env args
			:value-var context
			:body-var  body)
  (multiple-value-bind (bindings setters cleanup)
      (iter (for name in args)
	    ;; Split the variable into the name and optional value
	    ;; parts.
	    (let+ (((name &optional (value :gensym)) (ensure-list name))
		   ((&with-gensyms old)))
	      ;; Collect a binding.
	      (collect `(,name ,(case value
				      (:gensym `(gensym ,(string name)))
				      (t       value)))
		:into bindings)
	      (collect `(,old (context-get ,context ,(make-keyword name)))
		:into bindings)
	      ;; Collect a form to store the value in the `emit'
	      ;; context.
	      (collect
		  `(setf (context-get ,context ,(make-keyword name)) ,name)
		:into setters)
	      (collect `(setf (context-get ,context ,(make-keyword name)) ,old)
		:into cleanup))
	    (finally (return (values bindings setters cleanup))))
    `(let* ,bindings
       ,@setters
       (unwind-protect
	    ,@body
	 ,@cleanup))))

(define-let+-expansion (&env-r/o args
			:value-var context
			:body-var  body)
  `(let* ,(iter (for name in args)
		(let+ (((name &optional (item-name name) (default :error))
			(ensure-list name))
		       (key (make-keyword item-name)))
		  (collect
		      `(,name (context-get ,context ,key
					   :default ,default)))))
     ,@body))

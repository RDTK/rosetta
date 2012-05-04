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
			:uses-value? nil
		        :body-var    body)
  (let+ (((args &optional (context '(*context*)))
	  (split-sequence '&context args))
	 ((&values bindings setters cleanup)
	  (iter (for name in args)
		;; Split the variable into the name and optional value
		;; parts.
		(let+ (((name &optional (value `(gensym ,(string name))))
			(ensure-list name))
		       (place `(context-get ,@context ,(make-keyword name)))
		       ((&with-gensyms new old)))
		  ;; Collect a binding.
		  (collect `(,new ,value) :into bindings)
		  (collect `(,old ,place) :into bindings)
		  ;; Collect a form to store the value in the `emit'
		  ;; context.
		  (collect `(setf ,place ,new) :into setters)
		  (collect `(setf ,place ,old) :into cleanup))
		(finally (return (values bindings setters cleanup))))))
    `(let* ,bindings
       ,@setters
       (unwind-protect
	    ,@body
	 ,@cleanup))))

(define-let+-expansion (&env-r/o args
		        :uses-value? nil
			:body-var    body)
  (let+ (((args &optional (context '(*context*)))
	  (split-sequence '&context args))
	 (bindings
	  (iter (for name in args)
		(let+ (((name &optional (default :error))
			(ensure-list name))
		       ((name &optional (item-name name))
			(ensure-list name))
		       (key (make-keyword item-name)))
		  (collect
		      `(,name (context-get ,@context ,key
					   :default ,default)))))))
    `(let* ,bindings ,@body)))

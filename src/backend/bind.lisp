;;; bind.lisp --- Binding forms for things in the emit context.
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

(bind::defbinding-form (:env
			:docstring
			"Store in context generated unique names and
other things in the environment of the current `emit'
context. Bindings can be of the form VARIABLE in which case a `gensym'
will be stored and assigned, or of the form (VARIABLE VALUE) in which
case VALUE will be stored and assigned."
			:accept-multiple-forms-p nil)
  (multiple-value-bind (bindings body)
      (iter (for name in bind::variables)
	    ;; Split the variable into the name and optional value
	    ;; parts.
	    (bind (((name &optional (value :gensym)) (ensure-list name)))
	      ;; Collect a binding.
	      (collect `(,name ,(case value
				      (:gensym `(gensym ,(string name)))
				      (t       value)))
		:into bindings)
	      ;; Collect a form to store the value in the `emit'
	      ;; context.
	      (collect
		  `(setf (context-get ,values ',name) ,name)
		:into body))
	    (finally (return (values bindings body))))
    `(let* ,bindings
       ,@body)))

(bind::defbinding-form (:env-r/o
			:docstring
			"For all binding variables, retrieve values
from `emit' context and bind to the variables."
			:accept-multiple-forms-p nil)
  `(let ,(iter (for name in bind::variables)
	       (collect `(,name (context-get ,values ',name
					     :default :error))))))

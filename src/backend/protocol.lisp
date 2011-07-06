;;; protocol.lisp --- Protocol of the compiler backend.
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


;;; Targets
;;

(intern "TARGET") ;; for (documentation :TARGET 'rosetta.backend:target)

(dynamic-classes:define-findable-class-family target
    "This family consists of target classes. Each target class is used
to control the emission of one kind of thing based on an abstract
description in form of model component instances.")

(defmethod documentation ((thing symbol) (type (eql 'target)))
  "Obtain documentation of type TARGET from the target class
designated by THING."
  (documentation (find-target-class thing) t))


;;; Languages
;;

(intern "LANGUAGE") ;; for (documentation :LANGUAGE 'rosetta.backend:language)

(dynamic-classes:define-findable-class-family language
    "This family consists of language classes. Each language class is
used to control the output language when emitting things based on an
abstract description in form of model component instances.")

(defmethod documentation ((thing symbol) (type (eql 'language)))
  "Obtain documentation of type LANGUAGE from the language class
designated by THING."
  (documentation (find-language-class thing) t))


;;; Backend Context
;;

(defgeneric context-get (context key)
  (:documentation
   "Retrieve the item designated by KEY from CONTEXT."))

(defgeneric (setf context-get) (new-value context key)
  (:documentation
   "Store NEW-VALUE as the value of the item designated by KEY in
CONTEXT."))

(defclass context ()
  ((target      :initarg  :target
		:accessor context-target
		:initform nil
		:documentation
		"The target of the current emission process.")
   (language    :initarg  :language
		:type     symbol
		:accessor context-language
		:initform nil
		:documentation
		"")
   (stack       :initarg  :stack
		:type     list
		:accessor context-stack
		:initform nil
		:documentation
		"A stack of nodes currently being processed in the current
emission process.")
   (environment :initarg  :environment
		:type     list
		:accessor %context-environment
		:initform (list (make-hash-table))
		:documentation
		"A stack of hash-tables that stores additional
context-dependent information.")
   (package     :initarg  :package
		:type     (or null package)
		:accessor context-package
		:initform nil
		:documentation
		"The target package of the current emission process."))
  (:documentation
   "Instances of this class are used to keep track of the current
state of a particular emission process. This state consists of:
+ the emission target
+ the stack of nodes currently being processed
+ the package to which symbols are currently emitted"))

(defmethod context-get ((context context) (key symbol))
  (iter (for bla in (%context-environment context))
	(let ((value (gethash key bla)))
	  (when value (return value)))))

(defmethod (setf context-get) ((new-value t)
			       (context   context)
			       (key       symbol))
  (setf (gethash key (first (%context-environment context))) new-value))

(declaim (special *context*))

(defvar *context* (make-instance 'context)
  "This variable holds the emission context of the current thread.")


;;; Emit protocol
;;

(defgeneric emit (node target language
		  &key
		  verbose
		  print
		  &allow-other-keys)
  (:argument-precedence-order language target node)
  (:documentation
   "Emit the appropriate object for NODE with respect to TARGET."))


;;; Target class lookup
;;

(defmethod emit ((node t) (target list) (language t)
		 &key)
  (bind (((target-name &rest target-args) target)
	 (target-class    (find-target-class target-name))
	 (target-instance (apply #'make-instance
				 target-class target-args)))
    (emit node target-instance language)))

(defmethod emit ((node t) (target symbol) (language t)
		 &key)
  (emit node (list target) language))


;;; Language class lookup
;;

(defmethod emit ((node t) (target t) (language list)
		 &key)
  (bind (((language-name &rest language-args) language)
	 (language-class    (find-language-class language-name))
	 (language-instance (apply #'make-instance
				 language-class language-args)))
    (emit node target language-instance)))

(defmethod emit ((node t) (target t) (language symbol)
		 &key)
  (emit node target (list language)))


;;; Housekeeping and such
;;

(defmethod emit :before ((node t) (target t) (language t)
			 &key
			 (verbose *emit-verbose*)
			 (print   *emit-print*))
  ;; Printing
  (when verbose
    (format *standard-output* "~@<; ~@;emitting ~S for target ~S~@:>~%"
	    node target))
  (when print
    (format *standard-output* "~@<; ~@;emitting (~A)~@:>~%" (type-of node))))

(defmethod emit :around ((node t) (target t) (language t)
			 &key)
  (with-emit-restarts node target
    (with-updated-context (node target language)
      (call-next-method))))


;;; Default recursion behavior
;;

(defmethod emit ((node t) (target t) (language t)
		 &key)
  (values))

(defmethod emit ((node pb:file-set-desc) (target t) (language t)
		 &key)
  "Multi-file container; default behavior is recursion over files."
  (with-emit-symbols
    (map 'list #'recur (pb::file-set-desc-file node))))

(defmethod emit ((node pb:file-desc) (target t) (language t)
		 &key)
  "File; default behavior is recursion over enums, messages, services,
extensions and options."
  (with-emit-symbols
    (nconc
     (map 'list #'recur (pb::file-desc-enum-type    node))
     (map 'list #'recur (pb::file-desc-service      node))
     (map 'list #'recur (pb::file-desc-extension    node))
     (map 'list #'recur (pb::file-desc-options      node))
     (map 'list #'recur (pb::file-desc-message-type node)))))

(defmethod emit ((node pb:message-desc) (target t) (language t)
		 &key)
  "Message; default behavior is recursion over contained elements."
  (with-emit-symbols
    (map nil #'recur (pb::message-desc-enum-type   node))
    (map nil #'recur (pb::message-desc-nested-type node))
    (map nil #'recur (pb::message-desc-field       node))
    (map nil #'recur (pb::message-desc-extension   node))))

(defmethod emit ((node pb:enum-desc) (target t) (language t)
		 &key)
  "Enum; default behavior is recursion over enum values."
  (with-emit-symbols
    (map 'list #'recur (pb::enum-desc-value node))))

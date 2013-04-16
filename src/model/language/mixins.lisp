;;;; mixins.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; This file may be licensed under the terms of the

(cl:in-package :rosetta.model.language)


;;; `constrained-identifiers-mixin' mixin class
;;

(defclass constrained-identifiers-mixin ()
  ((char-legalizer :initarg  :char-legalizer
		   :type     (or symbol function)
		   :reader   language-char-legalizer
		   :documentation
		   "Stores a function which is called to replace
illegal characters with legal characters."))
  (:default-initargs
   :char-legalizer (missing-required-initarg
		    'constrained-identifiers-mixin :char-legalizer))
  (:documentation
   "This class adds to subclasses the ability to identify and replace
illegal characters in identifiers.

Methods on `legal-name?' and `legalize-name' take into account illegal
characters when processing identifiers."))

(defmethod legal-name? ((language constrained-identifiers-mixin)
			(name     string))
  (and (every (curry #'legal-identifier-char? language)
	      name (iota (length name)))
       (or (not (next-method-p))
	   (call-next-method))))

(defmethod legalize-name ((language constrained-identifiers-mixin)
			  (name     string))
  ;; Replace illegal characters with legal ones.
  (let ((more-legal
	  (map 'string
	       #'(lambda (char position)
		   (if (legal-identifier-char? language char position)
		       char
		       (funcall (language-char-legalizer language) char)))
	       name (iota (length name)))))
    (if (next-method-p)
	(call-next-method language more-legal)
	more-legal)))


;;; `reserved-words-mixin' mixin class
;;

(defclass reserved-words-mixin ()
  ((reserved-words :initarg  :reserved-words
		   :type     list
		   :reader   language-reserved-words
		   :initform nil
		   :documentation
		   "Stores a list of reserved words for a given
programming language.")
   (name-legalizer :initarg  :name-legalizer
		   :type     (or symbol function)
		   :reader   language-name-legalizer
		   :documentation
		   "Stores a function of one parameter which is called
to transform reserved words into legal identifiers."))
  (:default-initargs
   :name-legalizer (missing-required-initarg 'reserved-words-mixin :name-legalizer))
  (:documentation
   "This class is intended to be mixed into language classes
representing programming languages with reserved words that cannot be
used for identifiers.

Methods on `legal-name?' and `legalize-name' take into account these
reserved words when processing identifiers."))

(defmethod reserved-word? ((language reserved-words-mixin)
			   (word     string))
  (find word (language-reserved-words language) :test #'string=))

(defmethod legal-name? ((language reserved-words-mixin)
			(name     string))
  (and (not (reserved-word? language name))
       (or (not (next-method-p))
	   (call-next-method))))

(defmethod legalize-name ((language reserved-words-mixin)
			  (name     string))
  ;; Modify NAME if it is a reserved word, then let the next method
  ;; legalize the result further, if there is one.
  (let ((more-legal (if (reserved-word? language name)
			(funcall (language-name-legalizer language) name)
			name)))
    (if (next-method-p)
	(call-next-method language more-legal)
	more-legal)))


;;; `foreign-mixin'
;;

(defclass foreign-mixin ()
  ()
  (:documentation
   "This mixin class can be used to mark language classes as \"not
Lisp\"."))

(defmethod foreign? ((language foreign-mixin))
  t)

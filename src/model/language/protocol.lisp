;;;; protocol.lisp --- Protocol for the model.language module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rosetta.model.language)


;;; Language Protocol
;;

(defgeneric legal-name? (language name)
  (:documentation
   "Return non-nil if NAME is a legal name in LANGUAGE."))

(defgeneric legalize-name (language name)
  (:documentation
   "Return a string which is similar to NAME but a legal name in
LANGUAGE."))


;;; Identifier character protocol
;;

(defgeneric legal-identifier-char? (language character position)
  (:documentation
   "Return non-nil if CHAR is legal at POSITION in an identifier in
LANGUAGE."))


;;; Reserved word protocol
;;

(defgeneric reserved-word? (language word)
  (:documentation
   "Return non-nil if WORD is a reserved word in LANGUAGE."))


;;; Foreign language protocol
;;

(defgeneric foreign? (language)
  (:documentation
   "Return non-nil if LANGUAGE is not Lisp."))

;; default behavior

(defmethod foreign? ((language t))
  nil)


;;; Languages
;;

(intern "LANGUAGE") ;; for (documentation :LANGUAGE 'rosetta.model:language)

(dynamic-classes:define-findable-class-family language
    "This family consists of language classes. Each language class is
used to control the output language when emitting things based on an
abstract description in form of model component instances.")

(defmethod documentation ((thing symbol) (type (eql 'language)))
  "Obtain documentation of type LANGUAGE from the language class
designated by THING."
  (documentation (find-language-class thing) t))

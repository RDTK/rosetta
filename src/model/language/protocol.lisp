;;;; protocol.lisp --- Protocol for the model.language module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.language)

;;; Language Protocol

(defgeneric legal-name? (language name)
  (:documentation
   "Return non-nil if NAME is a legal name in LANGUAGE."))

(defgeneric legalize-name (language name)
  (:documentation
   "Return a string which is similar to NAME but a legal name in
LANGUAGE."))

;; Default behavior

(defmethod legalize-name ((language t) (name list))
  (typecase name
    (name
     (let+ (((anchor &rest components) name))
       (list* anchor (mapcar (curry #'legalize-name language) components))))
    ((cons (member :relative :absolute) (cons (cons string keyword))) ; qname/kind
     (let+ (((anchor &rest components) name))
       (list* anchor (mapcar (lambda+ ((component . kind))
                               (cons (legalize-name language component) kind))
                             components))))
    (t
     (call-next-method))))

;;; Identifier character protocol

(defgeneric legal-identifier-char? (language character position)
  (:documentation
   "Return non-nil if CHAR is legal at POSITION in an identifier in
LANGUAGE."))

;;; Reserved word protocol

(defgeneric reserved-word? (language word)
  (:documentation
   "Return non-nil if WORD is a reserved word in LANGUAGE."))

;;; Foreign language protocol

(defgeneric foreign? (language)
  (:documentation
   "Return non-nil if LANGUAGE is not Lisp."))

;; default behavior

(defmethod foreign? ((language t))
  nil)

;;; Languages

(service-provider:define-service language
  (:documentation
   "Each provider of this service is used to control the output
    language when emitting things based on an abstract description in
    form of model component instances."))

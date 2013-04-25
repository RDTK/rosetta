;;;; package.lisp --- Package definition for model.language module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rosetta.model.language
  (:nicknames
   #:rs.m.l)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rosetta
   #:rosetta.model)

  ;; Language protocol
  (:export
   #:legal-name?
   #:legalize-name)

  ;; Language class family
  (:export
   #:no-such-language-class
   #:find-language-class
   #:language-classes)

  ;; Name legalizer mixins
  (:export
   #:name-legalizer-mixin
   #:unconditional-name-legalizer-mixin)

  ;; Reserved words protocol and `reserved-words-mixin'
  (:export
   #:reserved-word?

   #:language-reserved-words
   #:language-name-legalizer

   #:reserved-words-mixin)

  ;; Identifier character protocol and `constrained-identifiers-mixin'
  (:export
   #:legal-identifier-char?

   #:language-char-legalizer

   #:constrained-identifiers-mixin)

  ;; Foreign language protocol and `foreign-mixin'
  (:export
   #:foreign?

   #:foreign-mixin)

  ;; Languages
  (:export
   #:language-abstract

   #:language-lisp
   #:language-lisp/compiled

   #:language-c++
   #:language-python
   #:language-java)

  (:documentation
   "This package contains functions and classes which model
programming languages.

There is a class family of languages which can be manipulated using:

* `no-such-language-class'          [condition]
* `find-language-class'             [generic function]
* `language-classes'                [function]

See

  (documentation SYMBOL 'rs.m.l:language)"))

;;;; languages.lisp --- Builtin programming languages.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.language)

;;; Abstract language

(defmethod find-language-class ((spec (eql :abstract)))
  (find-class 'language-abstract))

(defclass language-abstract ()
  ()
  (:documentation
   "This language is intended to be used as an abstract intermediate
representation and for debugging purposes."))

;;; Lisp language

(defmethod find-language-class ((spec (eql :lisp)))
  (find-class 'language-lisp))

(defclass language-lisp (unconditional-name-legalizer-mixin)
  ()
  (:default-initargs
   :name-legalizer (rcurry #'normalize-name :transform #'string-upcase))
  (:documentation
   "Common Lisp programming language. S-EXPRs are generated, but not
compiled.

See `language-lisp/compiled'."))

;;; Compiled Lisp language

(defmethod find-language-class ((spec (eql :lisp/compiled)))
  (find-class 'language-lisp/compiled))

(defclass language-lisp/compiled ()
  ()
  (:documentation
   "Common Lisp programming language. S-EXPRs are generated and
compiled.

See `language-lisp'."))

;;; `common-imperative-language' class

(defclass common-imperative-language (constrained-identifiers-mixin
                                      reserved-words-mixin
                                      foreign-mixin)
  ()
  (:default-initargs
   :char-legalizer (lambda (char)
                     (if (digit-char-p char)
                         (string-downcase (char-name char))
                         #\_))
   :name-legalizer (lambda (name) (concatenate 'string name "_")))
  (:documentation
   "Superclass for common imperative languages."))

(defmethod legal-identifier-char? ((language common-imperative-language)
                                   (char     character)
                                   (position integer))
  (or (char<= #\a char #\z) (char<= #\A char #\A)
      (char= char #\_)
      (and (plusp position) (char<= #\0 char #\9))))

;;; C++ language

(defparameter *c++-reserved-words*
  '(;; Reserved words in the C programming language that have to be
    ;; carried over into C++ [1].
    "auto" "const" "double" "float" "int" "short" "struct" "unsigned"
    "break" "continue" "else" "for" "long" "signed" "switch" "void" "case"
    "default" "enum" "goto" "register" "sizeof" "typedef" "volatile"
    "char" "do" "extern" "if" "return" "static" "union" "while"

    ;; There are another 30 reserved words that were not in C, are
    ;; therefore new to C++ [1].
    "asm" "dynamic_cast" "namespace" "reinterpret_cast" "try" "bool"
    "explicit" "new" "static_cast" "typeid" "catch" "false" "operator"
    "template" "typename" "class" "friend" "private" "this" "using"
    "const_cast" "inline" "public" "throw" "virtual" "delete" "mutable"
    "protected" "true" "wchar_t"

    ;; The following C++ reserved words are not essential when the
    ;; standard ASCII character set is being used, but they have been
    ;; added to provide more readable alternatives for some of the C++
    ;; operators, and also to facilitate programming with character
    ;; sets that lack characters needed by C++ [1].
    "and" "bitand" "compl" "not_eq" "or_eq" "xor_eq" "and_eq" "bitor"
    "not" "or" "xor"

    ;; Some common names defined by the standard library [1].
    "cin" "endl" "INT_MIN" "iomanip" "main" "npos"
    "std" "cout" "include" "INT_MAX" "iostream" "MAX_RAND" "NULL"
    "string")
  "Sources:

[1]  http://cs.smu.ca/~porter/csc/ref/cpp_keywords.html")

(defmethod find-language-class ((spec (eql :c++)))
  (find-class 'language-c++))

(defclass language-c++ (common-imperative-language)
  ()
  (:default-initargs
   :reserved-words *c++-reserved-words*)
  (:documentation
   "C++ programming language."))

;;; Python language

(defparameter *python-reserved-words*
  '("and" "as" "assert" "break" "class" "continue" "def" "del" "elif"
    "else" "except" "exec" "finally" "for" "from" "global" "if" "import"
    "in" "is" "lambda" "not" "or" "pass" "print" "raise" "return" "try"
    "while" "with" "yield")
  "Sources:

[1] http://docs.python.org/release/2.5.4/ref/keywords.html")

(defmethod find-language-class ((spec (eql :python)))
  (find-class 'language-python))

(defclass language-python (common-imperative-language)
  ()
  (:default-initargs
   :reserved-words *python-reserved-words*)
  (:documentation
   "Python programming language."))

;;; Java language

(defparameter *java-reserved-words*
  '("abstract" "assert" "boolean" "break" "byte" "case" "catch" "char"
    "class" "const" "continue" "default" "do" "double" "else" "enum"
    "extends" "false" "final" "finally" "float" "for" "goto" "if"
    "implements" "import" "instanceof" "int" "interface" "long"
    "native" "new" "null" "package" "private" "protected" "public"
    "return" "short" "static" "strictfp" "super" "switch"
    "synchronized" "this" "throw" "throws" "transient" "true" "try"
    "void" "volatile" "while")
  "Sources:

[1] http://www.jwrider.com/riderist/java/javaidrs.htm")

(defmethod find-language-class ((spec (eql :java)))
  (find-class 'language-java))

(defclass language-java (common-imperative-language)
  ()
  (:default-initargs
   :reserved-words *java-reserved-words*)
  (:documentation
   "Java programming language."))

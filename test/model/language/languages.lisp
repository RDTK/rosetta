;;;; package.lisp --- Unit test for builtin languages of the model.language module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.model.language.test)

(eval-when (:compile-toplevel)
  (defmacro define-language-test-case ((name method) &body cases)
    (let ((suite-name (format-symbol *package* "ROSETTA.MODEL.LANGUAGE.~A-ROOT" name))
          (case-name  (format-symbol *package* "~A/SMOKE" method))
          (class-name (format-symbol *package* "LANGUAGE-~A" name)))
      `(addtest (,suite-name
                 :documentation
                 ,(format nil "Smoke test for the `~(~A~)' method for ~
                               class `~(~A~)'."
                          method class-name))
         ,case-name

         (let ((language (make-instance (find-language-class ,(make-keyword name)))))
           ,(if (length= 1 cases)
                `(ensure-same (,method language) ,@cases)
                `(ensure-cases (input expected)
                     (list ,@cases)

                   (ensure-same (,method language input) expected)))))))

  (defmacro define-language-test-suite ((name) &body specs)
    (let+ ((suite-name (format-symbol *package* "ROSETTA.MODEL.LANGUAGE.~A-ROOT" name))
           (class-name (format-symbol *package* "LANGUAGE-~A" name))
           ((&flet+ process-spec ((method &body cases))
              `(define-language-test-case (,name ,method)
                 ,@cases))))
      `(progn
         (deftestsuite ,suite-name (rosetta.model.language-root)
           ()
           (:documentation
            ,(format nil "Unit tests for the `~(~A~)' language
                          class."
                     class-name)))

         ,@(mapcar #'process-spec specs)))))

(define-language-test-suite (lisp)
  (foreign? nil)

  (legalize-name
   '("1foo"     "1FOO")
   '("foo-bar"  "FOO-BAR")
   '("foo?"     "FOO?")
   '("foo bar"  "FOO BAR")
   '("try"      "TRY")
   '("template" "TEMPLATE")

   '("foo"      "FOO")
   '("foo1"     "FOO-1")
   '("foo_bar"  "FOO-BAR")))

(define-language-test-suite (c++)
  (foreign? t)

  (legal-name?
   '("1foo"     nil) ; invalid char at position 0
   '("foo-bar"  nil) ; invalid char
   '("foo?"     nil) ; likewise
   '("foo bar"  nil) ; likewise
   '("try"      nil) ; reserved word
   '("template" nil) ; likewise

   '("foo"      t)
   '("foo1"     t)
   '("foo_bar"  t))

  (legalize-name
   '("1foo"     "digit_onefoo")
   '("foo-bar"  "foo_bar")
   '("foo?"     "foo_")
   '("foo bar"  "foo_bar")
   '("try"      "try_")
   '("template" "template_")

   '("foo"      "foo")
   '("foo1"     "foo1")
   '("foo_bar"  "foo_bar")))

(define-language-test-suite (python)
  (foreign? t)

  (legal-name?
   '("1foo"    nil) ; invalid char at position 0
   '("foo-bar" nil) ; invalid char
   '("foo?"    nil) ; likewise
   '("foo bar" nil) ; likewise
   '("try"     nil) ; reserved word
   '("class"   nil) ; likewise

   '("foo"     t)
   '("foo1"    t)
   '("foo_bar" t))

  (legalize-name
   '("1foo"    "digit_onefoo")
   '("foo-bar" "foo_bar")
   '("foo?"    "foo_")
   '("foo bar" "foo_bar")
   '("try"     "try_")
   '("class"   "class_")

   '("foo"     "foo")
   '("foo1"    "foo1")
   '("foo_bar" "foo_bar")))

(define-language-test-suite (java)
  (foreign? t)

  (legal-name?
   '("1foo"     nil) ; invalid char at position 0
   '("foo-bar"  nil) ; invalid char
   '("foo?"     nil) ; likewise
   '("foo bar"  nil) ; likewise
   '("try"      nil) ; reserved word
   '("final"    nil) ; likewise

   '("foo"      t)
   '("foo1"     t)
   '("foo_bar"  t))

  (legalize-name
   '("1foo"    "digit_onefoo")
   '("foo-bar" "foo_bar")
   '("foo?"    "foo_")
   '("foo bar" "foo_bar")
   '("try"     "try_")
   '("final"   "final_")

   '("foo"     "foo")
   '("foo1"    "foo1")
   '("foo_bar" "foo_bar")))

;;;; macros.lisp --- Unit tests for the backend macros.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rosetta.backend.test)

(deftestsuite backend-macros-root (backend-root)
  ()
  (:documentation
   "Unit tests for macros provided by the backend module."))

(defun ensure-&env-cases (cases)
  "Ensure test cases CASES which should be a list of entries of the
form

  (((&env | &env-r/o) REST-OF-SPEC) VAR EXPECTED/VALUE EXPECTED/ENTRY1 EXPECTED/ENTRY2)

where VAR is the variable which is expected to be bound by
REST-OF-SPEC and EXPECTED/* are the expected values of the variable
and the entries in a context (where :foo = 1) and
*context* (where :foo = 2)."
  (ensure-cases (spec var &rest expected)
      cases

    (let+ (((&flet do-it ()
              (eval `(let ((context   (make-instance 'context))
                           (*context* (make-instance 'context)))
                       (setf (context-get context   :foo) 1
                             (context-get *context* :foo) 2)
                       (let+ ((,spec))
                         (values ,var
                                 (context-get context   :foo)
                                 (context-get *context* :foo))))))))
      (case (first expected)
        (error (ensure-condition 'error (do-it)))
        (t     (let+ (((&values result expected)
                       (iter (for value  in (multiple-value-list (do-it)))
                             (for expect in expected)
                             (when expect
                               (collect value  :into values)
                               (collect expect :into expects))
                             (finally (return (values values expects))))))
                 (ensure-same (values-list result)
                              (values-list expected))))))))

(addtest (backend-macros-root
          :documentation
          "Smoke test for `&env' `let+' expansion.")
  &env/smoke

  (ensure-&env-cases
    '(((&env foo            &context context) foo nil   nil   2)
      ((&env foo                            ) foo nil   1     nil)
      ((&env :foo           &context context) nil nil   nil   2)
      ((&env :foo                           ) nil nil   1     nil)
      ((&env ((nil :foo))   &context context) nil nil   nil   2)
      ((&env ((nil :foo))                   ) nil nil   1     nil)
      ((&env ((var :foo))   &context context) var nil   nil   2)
      ((&env ((var :foo))                   ) var nil   1     nil)
      ((&env ((var :bar))   &context context) var nil   1     2)
      ((&env ((var :bar))                   ) var nil   1     2)
      ((&env ((var :foo) 3) &context context) var 3     3     2)
      ((&env ((var :foo) 3)                 ) var 3     1     3)
      ((&env ((var :bar) 3) &context context) var 3     1     2)
      ((&env ((var :bar) 3)                 ) var 3     1     2)
      ;; New value can reference entry.
      ((&env (foo (1+ foo)) &context context) foo 2     2     2)
      ((&env (foo (1+ foo))                 ) foo 3     1     3)
      ;;  But only if it exists.
      ((&env ((var :bar) (1+ var))          ) var error error error))))

(addtest (backend-macros-root
          :documentation
          "Smoke test for `&env-r/o' `let+' expansion.")
  &env-r/o/smoke

  (ensure-&env-cases
    '(((&env-r/o foo            &context context) foo 1     1     2)
      ((&env-r/o foo                            ) foo 2     1     2)
      ((&env-r/o ((var :foo))   &context context) var 1     1     2)
      ((&env-r/o ((var :foo))                   ) var 2     1     2)
      ((&env-r/o ((var :bar))   &context context) var error error error)
      ((&env-r/o ((var :bar))                   ) var error error error)
      ((&env-r/o ((var :foo) 3) &context context) var 1     1     2)
      ((&env-r/o ((var :foo) 3)                 ) var 2     1     2)
      ((&env-r/o ((var :bar) 3) &context context) var 3     1     2)
      ((&env-r/o ((var :bar) 3)                 ) var 3     1     2))))

(addtest (backend-macros-root
          :documentation
          "Smoke test for `optimization-case' macro.")
  optimization-case/smoke

  (let+ (((&flet+ do-it ((settings clauses))
            (let ((result
                    (eval
                     `(let ((target (make-instance
                                     'code-generating-target-mixin
                                     :optimization-settings ',settings)))
                        (rs.b:optimization-case (target) ,@clauses)))))
              (ensure-same result :selected)))))

    (mapc #'do-it '((((speed 2))            ((speed  :selected) (safety)))
                    (((safety 2))           ((safety :selected) (debug)))
                    (((debug 2))            ((space) (debug :selected)))
                    (((space 2))            ((speed) (space :selected)))
                    (((speed 2) (safety 3)) ((speed) (safety :selected)))))))

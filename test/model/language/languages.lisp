;;; package.lisp --- Unit test for builtin languages of the model.language module.
;;
;; Copyright (C) 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.model.language.test)

(eval-when (:compile-toplevel)
  (defmacro define-language-test-case ((name method) &body cases)
    (let ((suite-name (format-symbol *package* "ROSETTA.MODEL.LANGUAGE.~A-ROOT" name))
	  (case-name  (format-symbol *package* "~A/SMOKE" method))
	  (class-name (format-symbol *package* "LANGUAGE-~A" name)))
      `(addtest (,suite-name
		 :documentation
		 ,(format nil "Smoke test for the `~(~A~)' method."
			  method))
	 ,case-name

	 (let ((language (make-instance (find-language-class ,(make-keyword name)))))
	   (ensure-cases (input expected)
	       (list ,@cases)

	     (ensure-same (,method language input) expected))))))

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

(define-language-test-suite (c++)
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
   '("1foo"     "_foo")
   '("foo-bar"  "foo_bar")
   '("foo?"     "foo_")
   '("foo bar"  "foo_bar")
   '("try"      "try_")
   '("template" "template_")

   '("foo"      "foo")
   '("foo1"     "foo1")
   '("foo_bar"  "foo_bar")))

(define-language-test-suite (python)
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
   '("1foo"    "_foo")
   '("foo-bar" "foo_bar")
   '("foo?"    "foo_")
   '("foo bar" "foo_bar")
   '("try"     "try_")
   '("class"   "class_")

   '("foo"     "foo")
   '("foo1"    "foo1")
   '("foo_bar" "foo_bar")))

(define-language-test-suite (java)
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
   '("1foo"    "_foo")
   '("foo-bar" "foo_bar")
   '("foo?"    "foo_")
   '("foo bar" "foo_bar")
   '("try"     "try_")
   '("final"   "final_")

   '("foo"     "foo")
   '("foo1"    "foo1")
   '("foo_bar" "foo_bar")))

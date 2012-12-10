;;; languages.lisp --- Builtin programming languages.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rosetta.model.language)


;;; Abstract language
;;

(defmethod find-language-class ((spec (eql :abstract)))
  (find-class 'language-abstract))

(defclass language-abstract ()
  ()
  (:documentation
   "This language is intended to be used as an abstract intermediate
representation and for debugging purposes."))


;;; Lisp language
;;

(defmethod find-language-class ((spec (eql :lisp)))
  (find-class 'language-lisp))

(defclass language-lisp ()
  ()
  (:documentation
   "Common Lisp programming language. S-EXPRs are generated, but not
compiled.

See `language-lisp/compiled'."))


;;; Compiled Lisp language
;;

(defmethod find-language-class ((spec (eql :lisp/compiled)))
  (find-class 'language-lisp/compiled))

(defclass language-lisp/compiled ()
  ()
  (:documentation
   "Common Lisp programming language. S-EXPRs are generated and
compiled.

See `language-lisp'."))


;;; C++ language
;;

(defmethod find-language-class ((spec (eql :c++)))
  (find-class 'language-c++))

(defclass language-c++ ()
  ()
  (:documentation
   "C++ programming language."))


;;; Python language
;;

(defmethod find-language-class ((spec (eql :python)))
  (find-class 'language-python))

(defclass language-python ()
  ()
  (:documentation
   "Python programming language."))


;;; Java language
;;

(defmethod find-language-class ((spec (eql :java)))
  (find-class 'language-java))

(defclass language-java ()
  ()
  (:documentation
   "Java programming language."))

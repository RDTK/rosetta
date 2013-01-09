;;; package.lisp --- Package definition for model.language module.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:defpackage :rosetta.model.language
  (:nicknames
   :rs.m.l)

  (:use
   :cl
   :alexandria
   :more-conditions)

  ;; Language protocol
  (:export
   :legal-name?
   :legalize-name)

  ;; Language class family
  (:export
   :no-such-language-class
   :find-language-class
   :language-classes)

  ;; Reserved words protocol and `reserved-words-mixin'
  (:export
   :reserved-word?

   :language-reserved-words
   :language-name-legalizer

   :reserved-words-mixin)

  ;; Identifier character protocol and `constrained-identifiers-mixin'
  (:export
   :legal-identifier-char?

   :language-char-legalizer

   :constrained-identifiers-mixin)

  ;; Languages
  (:export
   :language-abstract

   :language-lisp
   :language-lisp/compiled

   :language-c++
   :language-python
   :language-java)

  (:documentation
   "This package contains functions and classes which model
programming languages.

There is a class family of languages which can be manipulated using:

* `no-such-language-class'          [condition]
* `find-language-class'             [generic function]
* `language-classes'                [function]

See

  (documentation SYMBOL 'rs.m.l:language)"))

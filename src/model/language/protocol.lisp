;;; protocol.lisp --- Protocol for the model.language module.
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

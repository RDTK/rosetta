;;; protocol.lisp --- Protocol for the model.language module.
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

;;; types.lisp --- Types used in the model.serialization package.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rosetta.model.serialization)


;;; Endianness specifications
;;

(deftype endian ()
  `(member :little-endian
	   :big-endian))

(deftype endian-designator ()
  '(or endian
       (member :machine-endian
	       :opposite-of-machine-endian)))

(declaim (ftype (function (endian-designator) (values endian &rest nil))
		resolve-endian))

(defun resolve-endian (endian-designator)
  "Return an `endian' which realizes ENDIAN-DESIGNATOR for the current
machine."
  (case endian-designator
    (:machine-endian
     #+big-endian    :big-endian
     #+little-endian :little-endian)
    (:opposite-of-machine-endian
     (opposite-endian (resolve-endian :machine-endian)))
    (t
     endian-designator)))

(declaim (ftype (function (endian) (values endian &rest nil))
		opposite-endian))

(defun opposite-endian (endian)
  "Return the opposite of ENDIAN."
  (ecase endian
    (:little-endian :big-endian)
    (:big-endian    :little-endian)))

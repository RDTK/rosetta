;;; package1.lisp ---
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(cl:in-package :rosetta.model.data)

(defclass package1 (named-mixin
		    parented-mixin
		    container/relative-mixin
		    documentation-mixin
		    print-items-mixin)
  ()
  (:documentation
   "Instances of this class represent packages, that is named
containers for data types."))

(defmethod kind ((type package1))
  :package)

(defmethod qname ((package package1))
  (if-let ((parent (parent package)))
    (call-next-method)
    (list :absolute)))

(defmethod contents/plist ((package package1))
  (hash-table-plist (%nested package)))

(defmethod add-child ((builder t) ;;; TODO(jmoringe, 2012-04-24):
		      (parent  package1)
		      (child   named-mixin))
  (assert (not (eq parent child))) ; TODO(jmoringe, 2012-10-24): proper condition

  (setf (lookup parent (kind child) (name child)) child) ; TODO(jmoringe, 2012-04-24):
  parent)

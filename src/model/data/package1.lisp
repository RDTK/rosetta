;;; package1.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
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
		    print-items-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod kind ((type package1))
  :package)

(defmethod qname ((package package1))
  (if-let ((parent (parent package)))
    (append (qname parent) (list (name package)))
    (list :absolute)))

(defmethod contents/plist ((package package1))
  (hash-table-plist (%nested package)))

(defmethod lookup ((package package1)
		   (kind    symbol)
		   (name    list)
		   &key &allow-other-keys)
  (check-type name (or name/absolute name/relative))

  (typecase name
    ((and (cons (eql :relative) (cons string null)))
     (lookup package kind (second name)))

    ((and (cons (eql :relative) (cons string cons)))
     (lookup (lookup package :package (second name))
	     kind (cons :relative (nthcdr 2 name))))

    (name/absolute
     (lookup (root package) kind (cons :relative (rest name))))))

;;; protocol.lisp --- Protocol provided by the model module.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen  <jmoringe@techfak.uni-bielefeld.de>
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

(cl:in-package :rosetta.model)


;;; Name protocol
;;

(defgeneric kind (thing)
  (:documentation
   "Return a keyword describing the kind of THING."))

(defgeneric name (thing)
  (:documentation
   "Return the name of THING."))

(defgeneric qname (thing)
  (:documentation
   "Return the fully qualified name of THING."))


;;; Printing qnames
;;

(declaim (ftype (function (stream name &optional t t character)
			  (values name &rest nil))
		print-qname))

(defun print-qname (stream qname &optional colon? at? (separator #\.))
  "Print the relative or absolute qualified name QNAME onto STREAM.

For relative names, a single SEPARATOR is printed in front of the
remaining printed representation.

COLON? controls the behavior in case QNAME is (:absolute). If COLON?
is non-nil, \"<root>\" is printed. Otherwise, the empty string is
printed.

SEPARATOR is printed to separate name components."
  (declare (ignore at?))

  (let+ ((format (format nil "~~:[~~;~C~~]~~{~~A~~^~:*~C~~}" separator))
	 ((anchor &rest components) qname)
	 (relative? (eq anchor :relative)))
    (cond
      ((or relative? (not (length= 1 qname)))
       (format stream format relative? components))
      (colon?
       (format stream "<root>")))
    qname))

;;; print-items.lisp --- Composable printing mechanism
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

(cl:in-package :rosetta)


;;; Print Items Protocol
;;

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
representation of OBJECT."))

(defmethod print-items append ((object t))
  "Default behavior is to not return any print items for OBJECT."
  nil)


;;; Print Items Mixin
;;

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format-print-items (print-items object) stream)))


;;; Utility Functions
;;

(defun format-print-items (items stream)
  "Print ITEMS onto STREAM.
ITEMS is a list of items of the form ITEM where
ITEM   ::= (KEY VALUE [FORMAT])
KEY    ::= any Lisp object
VALUE  ::= any Lisp object
FORMAT ::= a format string (Default is \"~A\")"
  (mapc (lambda+ ((&ign value &optional format))
	  (format stream (or format "~A") value))
	(remove-duplicates items
			   :key      #'first
			   :from-end t)))

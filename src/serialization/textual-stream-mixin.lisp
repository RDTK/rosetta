;;; textual-stream-mixin.lisp --- Mixin for textual mechanisms based on streams.
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

(cl:in-package :rosetta.serialization)

(defclass textual-stream-mixin (textual-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
implement textual mechanisms based on streams."))

(defmethod pack ((mechanism   textual-stream-mixin)
		 (source      t)
		 (destination (eql 'string))
		 &rest args
		 &key)
  "The value of DESTINATION indicates that a string should be created
as destination."
  (let ((result (with-output-to-string (stream)
		  (apply #'pack mechanism source stream args))))
    (values (length result) result)))

(defmethod unpack ((mechanism   textual-stream-mixin)
		   (source      pathname)
		   (destination t)
		   &rest args
		   &key)
  "Open a stream for SOURCE and unpack the contents into DESTINATION."
  (with-input-from-file (stream source)
    (apply #'unpack mechanism stream destination args)))

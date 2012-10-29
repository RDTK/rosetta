;;; textual-stream-mixin.lisp --- Unit tests for the textual-stream-mixin class.
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

(cl:in-package :rosetta.serialization.test)


;;; Mock mechanism class
;;

(defmethod find-mechanism-class ((spec (eql :mock-for-textual-stream-mixin)))
  (find-class 'mechanism-mock-for-textual-stream-mixin))

(defclass mechanism-mock-for-textual-stream-mixin (textual-stream-mixin)
  ())

(defmethod pack ((mechanism   mechanism-mock-for-textual-stream-mixin)
		 (source      t)
		 (destination stream)
		 &key)
  (format destination "~S" source)
  (values nil destination))

(defmethod unpack ((mechanism   mechanism-mock-for-textual-stream-mixin)
		   (source      stream)
		   (destination t)
		   &key)
  (values (read source) nil))


;;; Test suite
;;

(deftestsuite textual-stream-mixin-root (serialization-root)
  ((simple-mechanism (make-instance 'mechanism-mock-for-textual-stream-mixin)))
  (:documentation
   "Root test suite for the `textual-stream-mixin' class."))

(define-basic-textual-mechanism-tests textual-stream-mixin)

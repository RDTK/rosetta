;;; forward-reference.lisp --- Representation of not-yet-defined things.
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

(defclass forward-reference (print-items-mixin)
  ((kind :initarg  :kind
	 :accessor kind
	 :documentation
	 "Stores a description of the kind of the object represented
by this forward reference.")
   (args :initarg  :args
	 :type     list
	 :accessor args
	 :documentation
	 "Stores the arguments that would have been passed to the
object represented by this forward reference."))
  (:default-initargs
   :kind (missing-required-initarg 'forward-reference :kind)
   :args (missing-required-initarg 'forward-reference :args))
  (:documentation
   "Instances of this class represent unresolved references to model
objects.

They store all information that was available when the resolution of
the reference failed. This information can later be used to resolve
the reference and eventually replace (via `change-class') the forward
reference object with the desired object."))

(defmethod name ((thing forward-reference))
  (format nil "F<~A~[ ~A~]>" (kind thing) (getf (args thing) :name)))

(defmethod qname ((thing forward-reference))
  (list :absolute (name thing)))

(defmethod upgrade! ((instance forward-reference)
		     (other    t))
  (let ((new-class (class-of other)))
    (apply #'change-class instance new-class nil)
    (iter (for slot in (closer-mop:class-slots new-class))
	  (let ((name (closer-mop:slot-definition-name slot)))
	    (setf (slot-value instance name)
		  (slot-value other name))))
    instance))

(defmethod print-items append ((object forward-reference))
  (list (list :kind (kind object))
	(list :name (getf (args object) :name) "~@[ ~A~]")))

;;; mixins.lisp --- Mixin classes used in the model.serialization module.
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

(cl:in-package :rosetta.model.serialization)


;;; `wire-type-mixin' mixin class
;;

(defclass wire-type-mixin ()
  ((wire-type :initarg  :wire-type
	      :reader   wire-type
	      :documentation
	      "Stores the wire-type to/from the which the
corresponding mechanism serializes/deserializes."))
  (:default-initargs
   :wire-type (missing-required-initarg 'wire-type-mixin :wire-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
wire-type slot describing the wire-type to/from which the
corresponding mechanism serializes/deserializes."))


;;; `offset-type-mixin' mixin class
;;

(defclass offset-type-mixin ()
  ((offset-type :initarg  :offset-type
		:reader   offset-type
		:documentation
		"Stores the type of offsets in binary buffers used by
the corresponding serialization mechanism."))
  (:default-initargs
   :offset-type (missing-required-initarg 'offset-type-mixin :offset-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
offset-type slot describing the type of offset in binary buffers used
by the corresponding serialization mechanism."))


;;; `length-type-mixin' mixin class
;;

(defclass length-type-mixin ()
  ((length-type :initarg  :length-type
		:reader   length-type
		:documentation
		"Stores the array length type used by the
corresponding serialization mechanism."))
  (:default-initargs
   :length-type (missing-required-initarg 'length-type-mixin :length-type))
  (:documentation
   "This mixin class adds to serialization mechanism classes a
length-type slot describing the array length type used by the
corresponding serialization mechanism."))

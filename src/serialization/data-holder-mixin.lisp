;;; data-holder-mixin.lisp --- Mixin class for mechanisms that use data-holders.
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

(defclass data-holder-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into mechanism classes that
unpack into data-holder instances \(as opposed to schema-free
mechanisms which could, for example, return nested lists)."))

(defmethod unpack ((mechanism   data-holder-mixin)
		   (source      t)
		   (destination class)
		   &rest args &key)
  "Make an instance of the class DESTINATION and load SOURCE into the
instance."
  (apply #'unpack mechanism source (make-instance destination) args))

(defmethod unpack ((mechanism   data-holder-mixin)
		   (source      t)
		   (destination symbol)
		   &rest args &key)
  "Make an instance of the class designated by DESTINATION and load
SOURCE into the instance."
  (apply #'unpack mechanism source (find-class destination) args))

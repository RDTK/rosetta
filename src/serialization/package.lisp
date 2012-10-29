;;; package.lisp --- Package definition for the serialization module.
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

(cl:defpackage :rosetta.serialization
  (:nicknames
   :rs.s)

  (:use
   :cl
   :alexandria
   :let-plus
   :iterate

   :nibbles

   :rosetta.model.serialization)

  ;; Conditions
  (:export
   :serialization-error
   :serialization-error-mechanism
   :serialization-error-source
   :serialization-error-destination

   :pack-error

   :unpack-error)

  ;; Serialization and deserialization protocol
  (:export
   :packed-size

   :pack :unpack
   :pack*)

  ;; Partial deserialization protocol
  (:export
   :location
   :extract)

  ;; `textual-mixin' mixin class
  (:export
   :textual-mixin)

  ;; `textual-stream-mixin' mixin class
  (:export
   :textual-stream-mixin)

  ;; `binary-mixin' mixin class
  (:export
   :binary-mixin)

  ;; `data-holder' mixin class
  (:export
   :data-holder-mixin)

  (:documentation
   "This package contains protocols and functionality related to
serialization and deserialization using different serialization
mechanisms."))

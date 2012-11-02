;;; macros.lisp --- Package definition for the backend module.
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

(cl:defpackage :rosetta.backend
  (:nicknames
   :rs.b)

  (:use
   :cl
   :alexandria
   :split-sequence
   :let-plus
   :iterate
   :more-conditions

   :rosetta.model.data
   :rosetta.model.language
   :rosetta.model.serialization)

  (:shadowing-import-from :rosetta.model.data
   :name
   :type1)

  ;; Conditions
  (:export)

  ;; Variables
  (:export
   :*context*)

  ;; Generic emitter
  (:export
   :emit)

  ;; Context protocol
  (:export
   :context-stack

   :context-node
   :context-taget
   :context-language

   :context-environment/alist
   :context-get)

  ;; Targets
  (:export
   :no-such-target
   :find-target-class
   :target-classes)

  ;; Languages
  (:export
   :no-such-language
   :find-language-class
   :language-classes)

  (:export
   :code-generating-target-mixin)

  ;; Serialization-related target classes
  (:export
   :serialization-mixin
   :target-mechanism

   :target-packed-size :target-packed-size/method
   :target-pack        :target-pack/method
   :target-unpack      :target-unpack/method

   :target-location    :target-location/method
   :target-extract     :target-extract/method)

  ;; Macros
  (:export
   :define-target
   :define-target/method
   :define-mechanism-target
   :define-mechanism-target/method
   :define-mechanism-targets

   :with-emit-symbols
   :parent
   :grandparent
   :ancestors
   :recur

   ;; let-plus
   :&env :&env-r/o)

  (:documentation
   "This package contains backend-related protocols and infrastructure
of the rosetta compiler."))

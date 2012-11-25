;;; variables.lisp --- Variables used in the backend module.
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

(cl:in-package :rosetta.backend)


;;; Special variables
;;

(declaim (special *emit-verbose*))

(defvar *emit-verbose* nil
  "When non-nil, print strings to `*standard-output*' during `emit'
calls which describe what is being emitted. Analogue to
`cl:*compile-verbose*'.")

(declaim (special *emit-print*))

(defvar *emit-print* nil
  "When non-nil, print concise messages to `*standard-output*' during
`emit' calls. Analogue to `*load-print*'.")


;;; Emission context
;;

(declaim (special *context*))

(defvar *context* nil
  "This variable holds the emission context of the current thread.")

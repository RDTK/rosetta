;;; emitter-lisp.lisp --- Unit tests for the Lisp emitter.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(cl:in-package :rosetta.backend.test)

(deftestsuite backend-emitter-lisp-root (backend-root)
  ()
  (:documentation
   "Unit test for emitter for Lisp."))

(addtest (backend-emitter-lisp-root
          :documentation
	  "Smoke test for emitting Lisp code for \"instantiate\"
target.")
  emit-instantiate/smoke

  (ensure-cases (node args expected)
      `((,(make-instance 'type-bool)         ()          nil)
	(,(make-instance 'type-uint8)        ()          0)
	(,(make-instance 'type-int16)        ()          0)
	(,(make-instance 'type-float32)      ()          0.0f0)
	(,(make-instance 'type-float64)      ()          0.0d0)
	(,(make-instance 'type-utf-8-string) ()          "")
	(,(make-instance 'type-octet-vector) ()          (octet-vector))
	(,+enum/uint32/simple+               ()          :a)
	(,+enum/uint32/simple+               (:value :b) :b)
	(,+enum/uint32/simple+               (:value :c) error))
    (let+ ((initargs (if args
			`(:instantiate :initargs ,args)
			:instantiate))
	   ((&flet do-it () (generate node initargs :lisp))))
      (case expected
	(error (ensure-condition 'error (do-it)))
	(t     (ensure-same (do-it) expected))))))

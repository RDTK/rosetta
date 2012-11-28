;;; model-builder.lisp --- Unit tests for the model-builder class.
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

(cl:in-package :rosetta.frontend.test)

(deftestsuite model-builder-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the `model-builder' class."))

(addtest (model-builder-root
          :documentation
	  "Smoke test for method on `ensure-package'.")
  ensure-package/smoke

  (let ((builder (make-instance (find-builder-class :model)
				:resolver   (make-instance 'search-path-resolver)
				:locations  (make-instance 'location-repository)
				:repository (make-instance 'rs.m.d::base-repository))))
    (ensure-package builder :qname '(:absolute))
    (ensure-package builder :qname '(:absolute))
    (ensure-package builder :qname '(:absolute "bar" "baz"))
    (ensure-package builder :qname '(:absolute "bar" "baz"))))

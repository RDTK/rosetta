;;; list-builder.lisp --- Unit tests for list-based builder.
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

(cl:in-package :rosetta.frontend.test)

(deftestsuite list-builder-root (frontend-root)
  ()
  (:documentation
   "Unit tests for builder methods which construct list-based
representations."))

(addtest (list-builder-root
          :documentation
	  "Test methods on `lookup' for the list-based
representation.")
  lookup/smoke

  (let* ((foo       `(:structure nil         :name "foo"))
	 (bar       `(:enum      nil         :name "bar"))
	 (container `(:package   (,foo ,bar) :name "baz")))
    (ensure-cases (kind key expected)
	`((:structure "foo" ,foo)
	  (:enum      "foo" nil)
	  (t          "foo" ,foo)
	  (:structure "bar" nil)
	  (:enum      "bar" ,bar)
	  (t          "bar" ,bar)
	  (:structure "fez" nil)
	  (:enum      "fez" nil)
	  (t          "fez" nil))
      (ensure-same (lookup container kind key :if-does-not-exist nil)
		   expected :test #'eq))))

;;; util.lisp --- Unit tests for utilities used in the rosetta system.
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

(cl:in-package :rosetta.test)

(deftestsuite normalize-name-root (root)
  ()
  (:documentation
   "Test suite for the `normalize-name' function."))

(addtest (normalize-name-root
          :documentation
	  "Smoke test for the `normalize-name' function.")
  smoke

  (ensure-cases (input args expected)
      `(("Vec2dDouble" ()                            "vec-2d-double")
	("Vec2DDouble" ()                            "vec-2d-double")
	("Vec2dDOUBLE" ()                            "vec-2d-double")
	("DataXOP"     ()                            "data-xop")
	("XOPData"     ()                            "xop-data")
	("xop_data"    ()                            "xop-data")

	;; Some corner cases.
	("_foo"        ()                            "foo")           ; separator at start
	("f1_o"        ()                            "f1-o")          ; touching separators
	("f_o"         ()                            "f-o")           ; short components

	;; Other transforms and separators.
	("Vec2dDouble" (:transform ,#'string-upcase) "VEC-2D-DOUBLE")
	("xop_data"    (:transform ,#'string-capitalize
			:separator nil)              "XopData"))

    (ensure-same (apply #'normalize-name input args)
		 expected
		 :test #'string=)))

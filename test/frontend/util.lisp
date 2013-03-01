;;; util.lisp --- Unit tests for utility functions of the frontend module.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(deftestsuite frontend-util-root (frontend-root)
  ()
  (:documentation
   "Tests for utilities in the frontend module."))

(deftestsuite maybe-shorten-root (frontend-util-root)
  ()
  (:documentation
   "Unit tests for the `maybe-shorten' function."))

(addtest (maybe-shorten-root
          :documentation
	  "Smoke test for the `maybe-shorten' function.")
  smoke

  (ensure-cases (input length expected)
      `((""    1 "")
	("foo" 1 "…")
	("foo" 2 "f…")
	("foo" 3 "foo")
	("foo" 4 "foo"))

    (ensure-same (maybe-shorten input :max-length length) expected
		 :test #'string=)))

(deftestsuite guess-format-root (frontend-util-root)
  ()
  (:documentation
   "Unit tests for the `guess-format' function."))

(addtest (guess-format-root
          :documentation
	  "Smoke test for the `guess-format' function.")
  smoke

  (ensure-cases (input format/expected found?/expected)
      `((,#P"foo/"     nil   nil) ; no type
	(,#P"foo."     nil   nil) ; empty type
	(,#P"foo.bla"  :bla  nil) ; unknown format class
	(,#P"foo.mock" :mock t))  ; use `format-mock' class

    (ensure-same (guess-format input)
		 (values format/expected found?/expected))))

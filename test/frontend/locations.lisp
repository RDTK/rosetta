;;; location.lisp --- Unit tests for the location machinery.
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

(deftestsuite frontend-location-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the location machinery, namely `location-info',
`format-content' and `format-location'."))

(deftestsuite location-info-root (frontend-location-root)
  ()
  (:documentation
   "Unit tests for the `location-info' class."))

(addtest (location-info-root
          :documentation
	  "Test constructing `location-info' instance.")
  construct

  (ensure-cases (initargs expected)
      '(;; Invalid constructions.
	((:bounds   1)             type-error)
	((:bounds   (3 . 1))       type-error)
	((:position (3 . 1))       type-error)
	((:bounds   (1 . 3)
	  :position 1)             incompatible-initargs)

	((:source-content "foo"    ; start out of bounds
	  :bounds         (3 . 3)) incompatible-initargs) 
	((:source-content "foo"    ; end out of bounds
	  :bounds         (1 . 5)) incompatible-initargs)
	((:source-content "foo"    ; start out of bounds
	  :position       5)       incompatible-initargs)

	;; These are OK.            source content bounds  line column
	((:source         "foo")   ("foo"  nil     nil     nil  nil))
	((:source         "foo"
	  :source-content "foo")   ("foo"  "foo"   nil     nil  nil))
	((:source         "foo"
	  :position       1)       ("foo"  nil     (1)     nil  nil))
	((:source         "foo"
	  :bounds         (1 . 2)) ("foo"  nil     (1 . 2) nil  nil))
	((:source         "foo"
	  :source-content "foo"
	  :bounds         (1 . 2)) ("foo"  "foo"   (1 . 2) 0    1))
	((:source         "foo"    ; in case of a newline, the newline
	  :source-content "f
oo"                                ; itself is on the "previous" line
	  :bounds         (2 . 3)) ("foo"  "f
oo"                                                (2 . 3) 1    0)))

    (case expected
      (type-error
       (ensure-condition 'type-error
	 (apply #'make-instance 'location-info initargs)))
      (incompatible-initargs
       (ensure-condition 'incompatible-initargs
	 (apply #'make-instance 'location-info initargs)))
      (t
       (let+ (((expected-source expected-content expected-bounds
		expected-line expected-column) expected)
	      (location (apply #'make-instance 'location-info initargs)))
	 (ensure-same (source         location) expected-source)
	 (ensure-same (source-content location) expected-content)
	 (ensure-same (bounds         location) expected-bounds)
	 (ensure-same (line           location) expected-line
		      :report "Wrong line")
	 (ensure-same (column         location) expected-column
		      :report "Wrong column"))))))

(addtest (location-info-root
          :documentation
	  "Test the method on `print-object' for `location-info'.")
  print-object

  (ensure-cases (initargs expected)
      '(((:source         "foo")   "<string>")
	((:source         "foo"
	  :source-content "foo")   "<string>")
	((:source         "foo"
	  :position       1)       "<string>")
	((:source         "foo"
	  :bounds         (1 . 2)) "<string>")
	((:source         "foo"
	  :source-content "foo"
	  :bounds         (1 . 2)) "<string>:1:2")
	((:source         "foo"
	  :source-content "f
oo"                            
	  :bounds         (2 . 3)) "<string>:2:1"))

    (let ((result (princ-to-string
		   (apply #'make-instance 'location-info initargs))))
      (ensure (search expected result :test #'string=)))))

;;; locations.lisp --- Unit tests for the location machinery.
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

(deftestsuite frontend-locations-root (frontend-root)
  ()
  (:documentation
   "Unit tests for the location machinery, namely `location-info',
`format-content' and `format-location'."))

(deftestsuite location-info-root (frontend-locations-root)
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

	;; These are OK.            source content bounds  l/s c/s l/e c/e
	((:source         "foo")   ("foo"  nil     nil     nil nil nil nil))
	((:source         "foo"
	  :source-content "foo")   ("foo"  "foo"   nil     nil nil nil nil))
	((:source         "foo"
	  :position       1)       ("foo"  nil     (1)     nil nil nil nil))
	((:source         "foo"
	  :bounds         (1 . 2)) ("foo"  nil     (1 . 2) nil nil nil nil))
	((:source         "foo"
	  :source-content "foo"
	  :bounds         (1 . 2)) ("foo"  "foo"   (1 . 2) 0   1   0   2))
	((:source-content "foo"
	  :bounds         (0 . 3)) (nil    "foo"   (0 . 3) 0   0   0   3))
	((:source         "foo"    ; in case of a newline, the newline
	  :source-content "f
oo"                                ; itself is on the "previous" line
	  :bounds         (2 . 3)) ("foo"  "f
oo"                                                (2 . 3) 1   0   1   1)))

    (case expected
      (type-error
       (ensure-condition 'type-error
	 (apply #'make-instance 'location-info initargs)))
      (incompatible-initargs
       (ensure-condition 'incompatible-initargs
	 (apply #'make-instance 'location-info initargs)))
      (t
       (let+ (((expected-source expected-content expected-bounds
		expected-line/start expected-column/start
		expected-line/end expected-column/end) expected)
	      (location (apply #'make-instance 'location-info initargs)))
	 (ensure-same (source         location) expected-source)
	 (ensure-same (source-content location) expected-content)
	 (ensure-same (bounds         location) expected-bounds)
	 (ensure-same (line           location :of :start)
		      expected-line/start
		      :report "Wrong start line")
	 (ensure-same (line           location :of :end)
		      expected-line/end
		      :report "Wrong end line")
	 (ensure-same (column         location :of :start)
		      expected-column/start
		      :report "Wrong start column")
	 (ensure-same (column         location :of :end)
		      expected-column/end
		      :report "Wrong end column"))))))

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
      (ensure-same expected result
		   :test (rcurry #'search :test #'string=)))))

(addtest (frontend-locations-root
          :documentation
	  "Test `format-content' on different `location-info'
instances.")
  format-location/smoke

  (ensure-cases ((source content bounds) expected expected/colon expected/at)
      ;;  source     content bounds   expected           expected/colon                         expected/at
      `(((nil        nil     nil)     "<unknown source>" "<unknown source>"                     "<unknown source>")
	((,#P"foo.c" nil     nil)     "foo.c"            "foo.c"                                "foo.c")
	(("foo"      nil     nil)     "<string>"         "<string>"                             "\"foo\"")
	(("foo"      "foo"   nil)     "<string>"         "<string>"                             "\"foo\"")
	(("foo"      nil     (1))     "<string>"         "<string>"                             "\"foo\"")
	(("foo"      nil     (1 . 2)) "<string>"         "<string>"                             "\"foo\"")
	(("foo"      ""      (0 . 0)) "<string>:1:1"     "columns 1 to 1 of line 1 of <string>" "\"foo\":1:1")
	(("foo"      "foo"   (1))     "<string>:1:2"     "column 2 of line 1 of <string>"       "\"foo\":1:2")
	(("foo"      "foo"   (1 . 2)) "<string>:1:2"     "columns 2 to 3 of line 1 of <string>" "\"foo\":1:2")
	(("foo"      "f
oo"                          (2))     "<string>:2:1"     "column 1 of line 2 of <string>"       "\"foo\":2:1"))

    (let+ ((info   (apply #'make-instance 'location-info
			  (append
			   (when source  (list :source         source))
			   (when content (list :source-content content))
			   (when bounds  (list :bounds         bounds)))))
	   ((&flet do-it (colon? at?)
	      (with-output-to-string (stream)
		(format-location stream info colon? at?)))))
      (ensure-same (do-it nil nil) expected       :test #'string=
		   :report "Without colon or at")
      (ensure-same (do-it t   nil) expected/colon :test #'string=
		   :report "With colon")
      (ensure-same (do-it nil t)   expected/at    :test #'string=
		   :report "With at"))))

(addtest (frontend-locations-root
          :documentation
	  "Test `format-content' on different `location-info'
instances.")
  format-content/smoke

  (ensure-cases ((content bounds colon? *print-length*) expected)
      ;;  content   bounds   colon *print-length* expected
      `(((nil       nil      nil   nil)           "<No content and/or")
	((nil       nil      t     nil)           "")
	(("foo bar" nil      nil   nil)           "foo bar")
	(("foo bar" nil      nil   4)             "foo…")
	(("foo bar" (0 . 0)  nil   nil)           "foo bar")
	(("foo bar" (1 . 2)  nil   nil)           "foo bar")
	(("foo bar
baz fez"            (2 . 3)  nil   nil)           "foo bar")
	(("foo bar
baz fez"            (8 . 11) nil   nil)           "baz fez")
	(("foo bar
baz fez"            (8 . 11) nil   4)             "baz…"))

    (let* ((info   (apply #'make-instance 'location-info
			  (append
			   (when content (list :source-content content))
			   (when bounds  (list :bounds         bounds)))))
	   (result (with-output-to-string (stream)
		     (format-content stream info colon?))))
      (ensure-same result  expected
		   :test   (if (null content)
			       #'(lambda (result expected)
				   (search expected result :test #'string=))
			       #'string=)))))

(addtest (frontend-locations-root
          :documentation
	  "Test `format-content' on different `location-info'
instances.")
  format-content-with-delimiters/smoke

  (ensure-cases ((content bounds colon? *print-length*) expected)
      ;;  content   bounds   colon *print-length* expected
      `(((nil       nil      nil   nil)           "<No content and/or")
	((nil       nil      t     nil)           "")
	(("foo bar" nil      nil   nil)           "| foo bar")
	(("foo bar" nil      nil   4)             "| foo…")
	(("foo bar" (0 . 0)  nil   nil)           "  v
| foo bar
  ^")
	(("foo bar" (1 . 2)  nil   nil)           "   v
| foo bar
    ^")
	(("foo bar
baz fez"            (2 . 3)  nil   nil)           "    v
| foo bar
     ^")
	(("foo bar
baz fez"            (8 . 11) nil   nil)           "  v
| baz fez
     ^")
	(("foo bar
baz fez"            (8 . 11) nil   4)             "  v
| baz…
     ^"))

    (let* ((info   (apply #'make-instance 'location-info
			  (append
			   (when content (list :source-content content))
			   (when bounds  (list :bounds         bounds)))))
	   (result (with-output-to-string (stream)
		     (format-content-with-delimiters stream info colon?))))
      (ensure-same result  expected
		   :test   (if (null content)
			       #'(lambda (result expected)
				   (search expected result :test #'string=))
			       #'string=)))))

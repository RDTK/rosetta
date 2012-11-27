;;; protocol.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(cl:in-package :rosetta.model.test)

(deftestsuite protocol-root (model-root)
  ()
  (:documentation
   "Tests for protocol functions of the model module."))

(addtest (protocol-root
          :documentation
	  "Smoke test for the `print-qname' function.")
  print-qname/smoke

  (ensure-cases (input separator colon? at? expected)
      ;; input                   sep col at  expected
      '(((:absolute)             nil nil nil "")
	((:absolute)             nil t   nil "<root>")
	((:relative)             nil nil nil ".")
	((:relative)             nil t   nil ".")
	((:absolute)             #\/ nil nil "")
	((:absolute)             #\/ t   nil "<root>")
	((:relative)             #\/ nil nil "/")
	((:relative)             #\/ t   nil "/")

	((:absolute "foo")       nil nil nil "foo")
	((:relative "foo")       nil nil nil ".foo")
	((:absolute "foo")       #\/ nil nil "foo")
	((:relative "foo")       #\/ nil nil "/foo")

	((:absolute "foo" "bar") nil nil nil "foo.bar")
	((:relative "foo" "bar") nil nil nil ".foo.bar")
	((:absolute "foo" "bar") #\/ nil nil "foo/bar")
	((:relative "foo" "bar") #\/ nil nil "/foo/bar"))

    (ensure-same (with-output-to-string (stream)
		   (apply #'print-qname stream input colon? at?
			  (when separator (list separator))))
		 expected :test #'string=)))

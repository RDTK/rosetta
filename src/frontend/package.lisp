;;; package.lisp --- Package definition for frontend module.
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

(cl:defpackage :rosetta.frontend
  (:nicknames
   :rs.f)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
   :more-conditions

   :rosetta
   :rosetta.model.data)

  ;; Conditions
  (:export
   :location-condition
   :location

   :builder-condition
   :builder

   :parse-error1

   :parse-warning

   :processing-error

   :processing-warning

   :dependency-error
   :dependency-error-dependency

   :cannot-resolve-dependency
   :dependency-error-locations

   :ambiguous-dependency
   :dependency-error-candidates)


;;; Location-related stuff
;;

  ;; Location protocol
  (:export
   :source
   :source-content
   :bounds
   :line
   :column

   :location=)

  ;; Location repository protocol and class
  (:export
   :location-of

   :location-repository)

  ;; Location Utilities
  (:export
   :location-info

   :format-location
   :format-content
   :format-content-with-delimiters)


;;; Parsing- and format-related stuff
;;

  ;; Parse protocol
  (:export
   :parse)

  ;; Format class family
  (:export
   :no-such-format-class
   :find-format-class
   :format-classes)

  ;; `binary-format-mixin' mixin class
  (:export
   :binary-format-mixin)

  ;; `text-format-mixin' mixin class
  (:export
   :text-format-mixin)


;;; Builder-related stuff
;;

  ;; Comment attaching protocol
  (:export
   :most-recent-comment
   :comment
   :comment?)

  ;; Dependency resolution protocol
  (:export
   :resolve)

  ;; Search path-based resolution protocol and class
  (:export
   :search-path
   :if-ambiguous

   :search-path-resolver)

  ;; `location-attach-mixin' mixin class
  (:export
   :location-attaching-mixin)

  ;; `comment-attaching-mixin' mixin class
  (:export
   :comment-attaching-mixin)

  ;; `root-package-creating-mixin' mixin class
  (:export
   :root-package-creating-mixin)

  ;; `lazy-resolver-mixin' mixin class
  (:export
   :lazy-resolver-mixin)

  ;; `dependency-delegating-mixin' mixin class
  (:export
   :dependency-delegating-mixin)

  ;; `source-level-caching-mixin' mixin class
  (:export
   :source-level-caching-mixin)

  (:documentation
   "This package contains frontend-related protocols and
infrastructure of the rosetta compiler.

* `parse'                           [generic function]

There is a class family of format classes which can be manipulated
using:

* `no-such-format-class'            [condition]
* `find-format-class'               [generic function]
* `format-classes'                  [function]

See

  (documentation SYMBOL 'rs.f:format)"))

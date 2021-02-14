;;; ztree-protocol.el --- generic protocol for ztree-view -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.
;;
;; Author: Alexey Veretennikov <alexey.veretennikov@gmail.com>
;;
;; Created: 2021-02-12
;;
;; Keywords: files tools
;; URL: https://github.com/fourier/ztree
;; Compatibility: GNU Emacs 24.x
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; Generic protocol for ztree-view

;;; Code:

(eval-when-compile (require 'cl-lib))

;;; Node protocol

;;; Obligatory to implement

(cl-defgeneric ztree-node-visible-p (node)
  "Return T if the NODE shall be visible.")

(cl-defgeneric ztree-node-short-name (node)
  "Return the short name for a node.")

(cl-defgeneric ztree-node-expandable-p (node)
  "Return T if the node is expandable.")

(cl-defgeneric ztree-node-equal (node1 node2)
  "Equality function for NODE1 and NODE2.
Return T if nodes are equal")

(cl-defgeneric ztree-node-children (node)
  "Return a list of NODE children")

;;; Optional to implement
(cl-defgeneric ztree-node-side (node)
  "Determine the side of the NODE.")

(cl-defgeneric ztree-node-face (node)
  "Return a face to write a NODE in")

(cl-defgeneric ztree-node-action (node)
  "Perform an action when the Return is pressed on a NODE.")

(cl-defgeneric ztree-node-left-short-name (node)
  "Return the left short name for a node in 2-sided tree.")

(cl-defgeneric ztree-node-right-short-name (node)
  "Return the right short name for a node in 2-sided tree.")


;;; Default implentations of optional methods

(cl-defmethod ztree-node-side ((node t))
  (ignore node)
  :left)

(cl-defmethod ztree-node-face ((node t))
  "Return a face to write a NODE in"
  (ignore node))

(cl-defmethod ztree-node-action ((node t) hard)
  "Perform an action when the Return is pressed on a NODE.
Argument HARD specifies if the Return was pressed (t) or
Space (nil)"
  (ignore node)
  (ignore hard))

(cl-defmethod ztree-node-left-short-name ((node t))
  "Return the left short name for a node in 2-sided tree."
  (ztree-node-short-name node))

(cl-defmethod ztree-node-right-short-name ((node t))
  "Return the right short name for a node in 2-sided tree."
  (ztree-node-short-name node))

(provide 'ztree-protocol)
;;; ztree-protocol.el ends here

;;; ztree-util.el --- Auxiliary utilities for the ztree package -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Free Software Foundation, Inc.
;;
;; Author: Alexey Veretennikov <alexey.veretennikov@gmail.com>
;; 
;; Created: 2013-11-11
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

;;; Code:
(defun ztree-find (where which)
  "Find element of the list WHERE matching predicate WHICH."
  (catch 'found
    (dolist (elt where)
      (when (funcall which elt)
        (throw 'found elt)))
    nil))

(defun ztree-filter (condp lst)
  "Filter out elements not satisfying predicate CONDP in the list LST.
Taken from http://www.emacswiki.org/emacs/ElispCookbook#toc39"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun ztree-printable-string (string)
  "Strip newline character from file names, like 'Icon\n.
Argument STRING string to process.'."
  (replace-regexp-in-string "\n" "" string))

(defun ztree-file-short-name (file)
  "By given FILE name return base file/directory name.
Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (ztree-printable-string (file-name-nondirectory (directory-file-name file))))

(defun ztree-car-atom (value)
  "Return VALUE if value is an atom, otherwise (car value) or nil.
Used since `car-safe' returns nil for atoms"
  (if (atom value) value (car value)))


(defun ztree-insert-with-face (text face)
  "Insert TEXT with the FACE provided."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face face)))

(provide 'ztree-util)

;;; ztree-util.el ends here

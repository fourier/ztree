;;; ztree-util.el --- Auxulary utilities for the ztree package -*- lexical-binding: t; -*-

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


(defmacro ztree-defrecord (record-name record-fields)
  "Create a record (structure) and getters/setters.

Record is the following set of functions:
 - Record constructor with name \"RECORD-NAME\"-create and list of
arguments which will be assigned to RECORD-FIELDS
 - Record getters with names \"record-name\"-\"field\" accepting one
argument - the record; \"field\" is from \"record-fields\" symbols
 - Record setters with names \"record-name\"-set-\"field\" accepting two
arguments - the record and the field value

Example:
\(ztree-defrecord person (name age))

will be expanded to the following functions:

\(defun person-create (name age) (...)
\(defun person-name (record) (...)
\(defun person-age (record) (...)
\(defun person-set-name (record value) (...)
\(defun person-set-age (record value) (...)

To test expansion one can use GNU Emacs's pp library:
\(require 'pp)
\(pp-macroexpand-expression
 '(ztree-defrecord person (name age)))"
  (let ((ctor-name (intern (concat (symbol-name record-name) "-create")))
        (rec-var (make-symbol "record")))
    `(progn
       ;; constructor with the name "record-name-create"
       ;; with arguments list "record-fields" expanded
       (defun ,ctor-name (,@record-fields)
         (let ((,rec-var))
           ,@(mapcar #'(lambda (x)
                         (list 'setq rec-var (list 'plist-put rec-var (list 'quote x) x)))
                     record-fields)))
       ;; getters with names "record-name-field" where the "field"
       ;; is from record-fields
       ,@(mapcar #'(lambda (x)
                     (let ((getter-name (intern (concat (symbol-name record-name)
                                                        "-"
                                                        (symbol-name x)))))
                       `(progn
                          (defun ,getter-name (,rec-var)
                            (plist-get ,rec-var ',x)
                            ))))
                 record-fields)
       ;; setters wit names "record-name-set-field where the "field"
       ;; is from record-fields
       ;; arguments for setters: (record value)
       ,@(mapcar #'(lambda (x)
                     (let ((setter-name (intern (concat (symbol-name record-name)
                                                        "-set-"
                                                        (symbol-name x)))))
                       `(progn
                          (defun ,setter-name (,rec-var value)
                            (setq ,rec-var (plist-put ,rec-var ',x value))
                            ))))
                 record-fields))))


(provide 'ztree-util)

;;; ztree-util.el ends here

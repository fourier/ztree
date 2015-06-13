;;; ztree-dir.el --- Text mode directory tree -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Free Software Foundation, Inc.
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; 
;; Created: 2013-11-1l
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
;;
;; Add the following to your .emacs file:
;; 
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
;; (require 'ztree-dir)
;;
;; Call the ztree interactive function:
;; M-x ztree-dir
;; Open/close directories with double-click, Enter or Space keys
;;
;;; Issues:
;;
;;; TODO:
;; 1) Add some file-handling and marking abilities
;;
;;; Code:

(require 'ztree-util)
(require 'ztree-view)

;;
;; Constants
;;

(defconst ztree-hidden-files-regexp "^\\."
  "Hidden files regexp.
By default all filest starting with dot '.', including . and ..")

;;
;; Configurable variables
;; 

(defvar ztree-dir-move-focus nil
  "If set to true moves the focus to opened window when the
user press RETURN on file ")t


;;
;; Faces
;;

(defface ztreep-header-face
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)
    (((background dark)) (:height 1.2 :foreground "lightblue" :weight bold))
    (t :height 1.2 :foreground "darkblue" :weight bold))
  "*Face used for the header in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-header-face 'ztreep-header-face)


;;
;; File bindings to the directory tree control
;;

(defun ztree-insert-buffer-header ()
  "Insert the header to the ztree buffer."
  (let ((start (point)))
    (insert "Directory tree")
    (insert "\n")
    (insert "==============")
    (set-text-properties start (point) '(face ztreep-header-face)))
  (insert "\n"))

(defun ztree-file-not-hidden (filename)
  "Determines if the file with FILENAME should be visible."
  (not (string-match ztree-hidden-files-regexp
                     (ztree-file-short-name filename))))

(defun ztree-find-file (node hard)
  "Find the file at NODE.

If HARD is non-nil, the file is opened in another window.
Otherwise, the ztree window is used to find the file."
  (when (and (stringp node) (file-readable-p node))
    (cond ((and hard ztree-dir-move-focus)
           (find-file-other-window node))
          (hard
           (save-selected-window (find-file-other-window node)))
          (t 
           (find-file node)))))

;;;###autoload
(defun ztree-dir (path)
  "Create an interactive buffer with the directory tree of the PATH given."
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf-name (concat "*Directory " path " tree*")))
      (ztree-view buf-name
                  (expand-file-name (substitute-in-file-name path))
                  'ztree-file-not-hidden
                  'ztree-insert-buffer-header
                  'ztree-file-short-name
                  'file-directory-p
                  'string-equal
                  '(lambda (x) (directory-files x 'full))
                  nil                   ; face
                  'ztree-find-file)))) ; action


(provide 'ztree-dir)
;;; ztree-dir.el ends here

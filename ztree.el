;;; ztree.el --- Text mode directory tree

;; Copyright (C) 2013 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;; Created: 2013-11-1l
;; Version: 1.0.0
;; Keywords: files
;; URL: https://github.com/fourier/ztree
;; Compatibility: GNU Emacs GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; 
;; (push (substitute-in-file-name "path-to-ztree-directory") load-path)
;; (require 'ztree)
;;
;; Call the ztree interactive function:
;; M-x ztree
;; Open/close directories with double-click, Enter or Space keys
;;
;;; Issues:
;;
;;; TODO:
;; 1) Add some file-handling and marking abilities
;; 2) Extract tree code as as separate package
;;
;;
;;; Change Log:
;; 
;; 2013-11-10 (1.0.0)
;;    Initial Release.
;;
;;; Code:

(require 'ztree-util)
(require 'ztree-view)

;;
;; Constants
;;

(defconst ztree-hidden-files-regexp "^\\."
  "Hidden files regexp. By default all filest starting with dot '.',
including . and ..")


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
  (let ((start (point)))
    (insert "Directory tree")
    (newline)
    (insert "==============")
    (set-text-properties start (point) '(face ztreep-header-face)))
  (newline))

(defun ztree (path)
  "Creates an interactive buffer with the directory tree of the path given"
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf-name (concat "*Directory " path " tree*")))
      (ztree-view buf-name
                  (expand-file-name (substitute-in-file-name path))
                  (list ztree-hidden-files-regexp)
                  'ztree-insert-buffer-header
                  'file-short-name
                  'file-directory-p
                  'string-equal
                  '(lambda (x) (directory-files x 'full))
                  nil))))


(provide 'ztree)
;;; ztree.el ends here

;;; ztree-diff.el --- Text mode diff for directory trees

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

(require 'ztree-view)
(require 'ztree-diff-model)

(defconst ztree-diff-hidden-files-regexp "^\\."
  "Hidden files regexp. By default all filest starting with dot '.',
including . and ..")

(defun ztree-diff-insert-buffer-header ()
  (insert "Differences tree")
  (newline)
  (insert "==============")
  (newline))


(defun ztree-diff (dir1 dir2)
  "Creates an interactive buffer with the directory tree of the path given"
  (interactive "DLeft directory \nDRight directory")
  (let* ((difference (ztree-diff-model-create dir1 dir2))
         (buf-name (concat "*" (ztree-diff-model-short-name difference) "*")))
    (ztree-view buf-name
                difference
                (list ztree-diff-hidden-files-regexp)
                'ztree-diff-insert-buffer-header
                'ztree-diff-model-short-name
                'ztree-diff-model-is-directory
                'equal
                'ztree-diff-model-children
                'ztree-diff-model-side)))


(provide 'ztree-diff)
;;; ztree-diff.el ends here

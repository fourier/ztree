;;; ztree-dir.el --- Text mode directory tree -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018  Free Software Foundation, Inc.
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
(eval-when-compile (require 'cl-lib))

;;
;; Constants
;;

(defconst ztree-hidden-files-regexp "^\\."
  "Hidden files regexp.
By default all filest starting with dot `.', including . and ..")

;;
;; Configurable variables
;;

(defvar ztree-dir-move-focus nil
  "Defines if move focus to opened window on hard-action command (RETURN) on a file.")

(defvar ztree-dir-info-timer 0.5
  "Specifies the time after which to present the information about a current file")

(defvar-local ztree-dir-filter-list (list ztree-hidden-files-regexp)
  "List of regexp file names to filter out.
By default paths starting with dot (like .git) are ignored.
One could add own filters in the following way:

(setq-default ztree-dir-filter-list (cons \"^.*\\.pyc\" ztree-dir-filter-list))
")

(defvar-local ztree-dir-show-filtered-files nil
  "Show or not files from the filtered list.")

;;
;; Operational variables
;; 

(defvar-local ztree-dir-timer nil
  "Idle timer used to show information about files in minibuffer.")

(defvar-local ztree-dir-last-message nil
  "Saves the last message in minibuffer in ztree-dir mode")


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


(define-minor-mode ztreedir-mode
  "A minor mode for displaying the directory trees in text mode."
  ;; initial value
  nil
  ;; modeline name
  " Dir"
  ;; The minor mode keymap
  `(
    (,(kbd "H") . ztree-dir-toggle-show-filtered-files)
    (,(kbd ">") . ztree-dir-narrow-to-dir)
    (,(kbd "<") . ztree-dir-widen-to-parent)
    (,(kbd "d") . ztree-dir-open-dired-at-point))
  (cond
   (ztreedir-mode
    (add-hook 'post-command-hook 'ztree-dir-schedule-timer nil t)
    (add-hook 'pre-command-hook 'ztree-dir-pre-command-refresh-echo-area nil t))
   (t
    (remove-hook 'post-command-hook 'ztree-dir-schedule-timer t)
    (remove-hook 'pre-command-hook 'ztree-dir-pre-command-refresh-echo-area t))))

;; the implementation is based on message implementation in eldoc
;; hence we use some of its functions.
(defun ztree-dir-schedule-timer ()
  "Schedule a timer which will present a text about a current file
after `ztree-dir-info-timer' timeout.
This is a hook which called after the interactive command."
  (setq ztree-dir-timer
        (run-with-idle-timer
         ztree-dir-info-timer nil
         (lambda ()
           (when (and ztreedir-mode
                      (eldoc-display-message-no-interference-p)
                      (not this-command)
                      (eldoc--message-command-p last-command))
             (ztree-dir-message-current-file-info)))))
  t)


(defun ztree-dir-pre-command-refresh-echo-area ()
  "Print previous message before executing the command."
  (when (and ztree-dir-last-message     ;; last command printed
             (not (minibufferp))            ;; not in minibuffer
             ;; not inside a keyboard macro or debugger
         (eldoc-display-message-no-interference-p) 
         ;; a current command is a one of motion commands
         (eldoc--message-command-p this-command))
    ;; print and clear the last message
    (ztree-dir-message ztree-dir-last-message)
    (setq ztre-dir-last-message nil)))


(defun ztree-dir-message (message)
  "Present a message in minibuffer but don't add it to *Messages*"
  (let ((message-log-max nil))
    (setf ztree-dir-last-message message)
    (minibuffer-message ztree-dir-last-message)))

(defun ztree-dir-message-current-file-info ()
  (let ((line (line-number-at-pos)))
    (when-let (node (ztree-find-node-in-line line))
      (and (not (file-directory-p node))
           (ztree-dir-message node)))));;(nth 8 (file-attributes node)))))))

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
  (let ((name (ztree-file-short-name filename)))
    (and (not (or (string= name ".") (string= name "..")))
         (or
          ztree-dir-show-filtered-files
          (not (cl-find-if (lambda (rx) (string-match rx name)) ztree-dir-filter-list))))))


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


(defun ztree-dir-toggle-show-filtered-files ()
  "Toggle visibility of the filtered files."
  (interactive)
  (setq ztree-dir-show-filtered-files (not ztree-dir-show-filtered-files))
  (message (concat (if ztree-dir-show-filtered-files "Show" "Hide") " filtered files"))
  (ztree-refresh-buffer))


(defun ztree-dir-directory-files (path)
  "Return the list of files/directories for the given PATH."
  ;; remove . and .. from the list of files to avoid infinite
  ;; recursion
  (cl-remove-if (lambda (x) (string-match-p "/\\.\\.?$" x))
                (directory-files path 'full)))


(defun ztree-dir-narrow-to-dir ()
  "Interactive command to narrow the current directory buffer.
The buffer is narrowed to the directory under the cursor.
If the cursor is on a file, the buffer is narrowed to the parent directory."
  (interactive)
  (let* ((line (line-number-at-pos))
         (node (ztree-find-node-in-line line))
         (parent (ztree-get-parent-for-line line)))
    (if (file-directory-p node)
        (ztree-change-start-node node)
      (when parent
        (ztree-change-start-node (ztree-find-node-in-line parent))))))


(defun ztree-dir-widen-to-parent ()
  "Interactive command to widen the current directory buffer to parent.
The buffer is widened to the parent of the directory of the current buffer.
This allows to jump to the parent directory if this directory is one level
up of the opened."
  (interactive)
  (let* ((node ztree-start-node)
         (parent (file-name-directory (directory-file-name node))))
    (when parent
      (ztree-change-start-node parent))))


(defun ztree-dir-open-dired-at-point ()
  "If the point is on a directory, open DIRED with this directory.
Otherwise open DIRED with the parent directory"
  (interactive)
  (let* ((line (line-number-at-pos))
         (node (ztree-find-node-in-line line))
         (parent (ztree-get-parent-for-line line)))
    (cond ((and node (file-directory-p node))
           (dired node))
          (parent 
           (dired (ztree-find-node-in-line parent))))))




;;;###autoload
(defun ztree-dir (path)
  "Create an interactive buffer with the directory tree of the PATH given."
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf-name (concat "*Directory " path " tree*")))
      (ztree-view buf-name
                  (expand-file-name (substitute-in-file-name path))
                  #'ztree-file-not-hidden
                  #'ztree-insert-buffer-header
                  #'ztree-file-short-name
                  #'file-directory-p
                  #'string-equal
                  #'ztree-dir-directory-files
                  nil                   ; face
                  #'ztree-find-file)    ; action
      (ztreedir-mode))))



(provide 'ztree-dir)
;;; ztree-dir.el ends here

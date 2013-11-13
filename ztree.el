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

;;
;; Constants
;;

(defconst ztree-hidden-files-regexp "^\\."
  "Hidden files regexp. By default all filest starting with dot '.',
including . and ..")

;;
;; Globals
;;

(defvar ztree-expanded-nodes-list nil
  "A list of Expanded nodes (i.e. directories) entries.")
(make-variable-buffer-local 'ztree-expanded-nodes-list)

(defvar ztree-start-node nil
  "Start node(i.e. directory) for the window.")
(make-variable-buffer-local 'ztree-start-node)

(defvar ztree-node-to-line-list nil
  "List of tuples with full node(i.e. file/directory name
 and the line.")
(make-variable-buffer-local 'ztree-node-to-line-list)

(defvar ztree-filter-list nil
  "List of regexp for node names to filter out")
(make-variable-buffer-local 'ztree-filter-list)

(defvar ztree-start-line nil
  "Index of the start line - the root")
(make-variable-buffer-local 'ztree-start-line)

(defvar ztree-parent-lines-array nil
  "Array of parent lines, there the ith value of the array
is the parent line for line i. If ith value is i - it is the root
line")
(make-variable-buffer-local 'ztree-parent-lines-array)

(defvar ztree-count-subsequent-bs nil
  "Counter for the subsequest BS keys (to identify double BS). Used
in order to not to use cl package and lexical-let")
(make-variable-buffer-local 'ztree-count-subsequent-bs)


;;
;; Major mode definitions
;;

(defvar ztree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\r") 'ztree-perform-action)
    (define-key map (kbd "SPC") 'ztree-perform-action)
    (define-key map [double-mouse-1] 'ztree-perform-action)
    (define-key map (kbd "g") 'ztree-refresh-buffer)
    (if window-system
        (define-key map (kbd "<backspace>") 'ztree-move-up-in-tree)
      (define-key map "\177" 'ztree-move-up-in-tree))
    map)
  "Keymap for `ztree-mode'.")

(defface ztreep-header-face
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)
    (((background dark)) (:height 1.2 :foreground "lightblue" :weight bold))
    (t :height 1.2 :foreground "darkblue" :weight bold))
  "*Face used for the header in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-header-face 'ztreep-header-face)


(defface ztreep-node-face
  '((((background dark)) (:foreground "#ffffff"))
    (((type nil))        (:inherit 'font-lock-function-name-face))
    (t                   (:foreground "Blue")))
  "*Face used for expandable entries(directories etc) in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-node-face 'ztreep-node-face)

(defface ztreep-leaf-face
  '((((background dark)) (:foreground "cyan1"))
    (((type nil))        (:inherit 'font-lock-variable-name-face))
    (t                   (:foreground "darkblue")))
  "*Face used for not expandable nodes(leafs, i.e. files) in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-leaf-face 'ztreep-leaf-face)

(defface ztreep-arrow-face
  '((((background dark)) (:foreground "#7f7f7f"))
    (t                   (:inherit 'font-lock-comment-face)))
  "*Face used for arrows in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-arrow-face 'ztreep-arrow-face)

(defface ztreep-expand-sign-face
  '((((background dark)) (:foreground "#7f7fff"))
    (t                   (:inherit 'font-lock-comment-face)))
  "*Face used for expand sign [+] in Ztree buffer."
  :group 'Ztree :group 'font-lock-highlighting-faces)
(defvar ztreep-expand-sign-face 'ztreep-expand-sign-face)


;;;###autoload
(define-derived-mode ztree-mode special-mode "Ztree"
  "A major mode for displaying the directory tree in text mode.")

(defun ztree-find (where which)
  "find element of the list `where` matching predicate `which`"
  (catch 'found
    (dolist (elt where)
      (when (funcall which elt)
        (throw 'found elt)))
    nil))

(defun ztree-filter (condp lst)
  "Filter out elements of the list `lst` not satisfying predicate `condp`.
Taken from http://www.emacswiki.org/emacs/ElispCookbook#toc39"
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun ztree-find-node-in-line (line)
  "Search through the array of node-line pairs and return the
node name for the line specified"
  (let ((found (ztree-find ztree-node-to-line-list
                           #'(lambda (entry) (eq line (cdr entry))))))
    (when found
      (car found))))

(defun ztree-is-expanded-node (node)
  "Find if the node is in the list of expanded nodes"
  (ztree-find ztree-expanded-nodes-list #'(lambda (x) (string-equal x node))))


(defun ztree-set-parent-for-line (line parent)
  (aset ztree-parent-lines-array (- line ztree-start-line) parent))

(defun ztree-get-parent-for-line (line)
  (when (and (>= line ztree-start-line)
             (< line (+ (length ztree-parent-lines-array) ztree-start-line)))
    (aref ztree-parent-lines-array (- line ztree-start-line))))

(defun scroll-to-line (line)
  "Recommended way to set the cursor to specified line"
  (goto-char (point-min))
  (forward-line (1- line)))


(defun ztree-perform-action ()
  "Toggle expand/collapsed state for nodes"
  (interactive)
  (let* ((line (line-number-at-pos))
         (file (ztree-find-node-in-line line)))
    (when file
      (if (file-directory-p file)  ; only for directories
          (ztree-toggle-expand-state file)
        nil)                            ; do nothiang for files for now
      (let ((current-pos (window-start))) ; save the current window start position
        (ztree-refresh-buffer line)    ; refresh buffer and scroll back to the saved line
        (set-window-start (selected-window) current-pos))))) ; restore window start position


(defun ztree-toggle-expand-state (node)
  "Toggle expanded/collapsed state for nodes"
  (if (ztree-is-expanded-node node)
      (setq ztree-expanded-nodes-list (ztree-filter #'(lambda (x) (not (string-equal node x)))
                                                  ztree-expanded-nodes-list))
    (push node ztree-expanded-nodes-list)))


(defun ztree-move-up-in-tree ()
  "Action on Backspace key: to jump to the line of a parent node or
if previous key was Backspace - close the node"
  (interactive)
  (when ztree-parent-lines-array
    (let* ((line (line-number-at-pos (point)))
           (parent (ztree-get-parent-for-line line)))
      (when parent
        (if (and (equal last-command 'ztree-move-up-in-tree)
                 (not ztree-count-subsequent-bs))
            (progn 
              (ztree-toggle-expand-state
               (ztree-find-node-in-line line))
              (setq ztree-count-subsequent-bs t)
              (ztree-refresh-buffer line))
          (progn (setq ztree-count-subsequent-bs nil)
                 (scroll-to-line parent)))))))


(defun file-basename (file)
  "Base file/directory name. Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (file-name-nondirectory (directory-file-name file)))

(defun printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (replace-regexp-in-string "\n" "" string))  


(defun ztree-get-directory-contens (path)
  "Returns pair of 2 elements: list of subdirectories and
list of files"
  (let ((files (directory-files path 'full)))
    (cons (ztree-filter #'(lambda (f) (file-directory-p f)) files)
          (ztree-filter #'(lambda (f) (not (file-directory-p f))) files))))

(defun ztree-node-is-in-filter-list (node)
  "Determine if the node is in filter list (and therefore
apparently shall not be visible"
  (ztree-find ztree-filter-list #'(lambda (rx) (string-match rx node))))

(defun ztree-draw-char (c x y)
  "Draw char c at the position (1-based) (x y)"
  (save-excursion
    (scroll-to-line y)
    (beginning-of-line)
    (goto-char (+ x (-(point) 1)))
    (delete-char 1)
    (insert-char c 1)
    (set-text-properties (1- (point)) (point) '(face ztreep-arrow-face))))

(defun ztree-draw-vertical-line (y1 y2 x)
  (if (> y1 y2)
      (dotimes (y (1+ (- y1 y2)))
        (ztree-draw-char ?\| x (+ y2 y)))
    (dotimes (y (1+ (- y2 y1)))
      (ztree-draw-char ?\| x (+ y1 y)))))

(defun ztree-draw-horizontal-line (x1 x2 y)
  (if (> x1 x2)
      (dotimes (x (1+ (- x1 x2)))
        (ztree-draw-char ?\- (+ x2 x) y))
    (dotimes (x (1+ (- x2 x1)))
      (ztree-draw-char ?\- (+ x1 x) y))))


(defun ztree-draw-tree (tree offset)
  "Draw the tree of lines with parents"
  (if (atom tree)
      nil
    (let ((root (car tree))
          (children (cdr tree)))
      (when children
        ;; draw the line to the last child
        ;; since we push'd children to the list, the last line
        ;; is the first
        (let ((last-child (car children))
              (x-offset (+ 2 (* offset 4))))
          (if (atom last-child)
              (ztree-draw-vertical-line (1+ root) last-child x-offset)
            (ztree-draw-vertical-line (1+ root) (car last-child) x-offset)))
        ;; draw recursively
        (dolist (child children)
          (ztree-draw-tree child (1+ offset))
          (if (listp child)
              (ztree-draw-horizontal-line (+ 3 (* offset 4))
                                          (+ 4 (* offset 4))
                                          (car child))
            (ztree-draw-horizontal-line (+ 3 (* offset 4))
                                        (+ 7 (* offset 4))
                                        child)))))))

(defun ztree-fill-parent-array (tree)
  ;; set the root line
  (let ((root (car tree))
        (children (cdr tree)))
    (dolist (child children)
      (if (atom child)
          (ztree-set-parent-for-line child root)
        (progn 
          (ztree-set-parent-for-line (car child) root)
          (ztree-fill-parent-array child))))))


(defun ztree-insert-node-contents (path)
  ;; insert node contents with initial offset 0
  (let ((tree (ztree-insert-node-contents-1 path 0))
        (num-of-items (- (line-number-at-pos (point)) ztree-start-line)))
    (setq ztree-parent-lines-array (make-vector num-of-items 0))
    (ztree-set-parent-for-line ztree-start-line ztree-start-line)
    (ztree-fill-parent-array tree)
    (ztree-draw-tree tree 0)))



(defun ztree-insert-node-contents-1 (path offset)
  (let* ((expanded (ztree-is-expanded-node path))
         (root-line (ztree-insert-entry path offset expanded))
         (children nil))
    (when expanded 
      (let* ((contents (ztree-get-directory-contens path))
             (nodes (car contents))
             (leafs (cdr contents)))
        (dolist (node nodes)
          (let ((short-dir-name (file-basename node)))
            (unless (ztree-node-is-in-filter-list short-dir-name)
              (push (ztree-insert-node-contents-1 node (1+ offset)) children))))
        (dolist (leaf leafs)
          (let ((short-file-name (file-basename leaf)))
            (when (not (ztree-node-is-in-filter-list short-file-name))
              (push (ztree-insert-entry leaf (1+ offset) nil)
                    children))))))
    (cons root-line children)))

(defun ztree-insert-entry (path offset expanded)
  (let ((short-name (printable-string (file-basename path)))
        (dir-sign #'(lambda (exp)
                      (insert "[" (if exp "-" "+") "]")
                      (set-text-properties (- (point) 3)
                                           (point)
                                           '(face ztreep-expand-sign-face))))
        (is-dir (file-directory-p path))
        (line (line-number-at-pos)))
    (when (> offset 0)
      (dotimes (i offset)
        (insert " ")
        (insert-char ?\s 3)))           ; insert 3 spaces
    (if is-dir
        (progn                          
          (funcall dir-sign expanded)   ; for directory insert "[+/-]"
          (insert " ")
          (put-text-property 0 (length short-name) 'face 'ztreep-node-face short-name)
          (insert short-name))
      (progn
        (insert "    ")
        (put-text-property 0 (length short-name) 'face 'ztreep-leaf-face short-name)
        (insert short-name)))
    (push (cons path (line-number-at-pos)) ztree-node-to-line-list)
    (newline)
    line))

(defun ztree-insert-buffer-header ()
  (let ((start (point)))
    (insert "Directory tree")
    (newline)
    (insert "==============")
    (set-text-properties start (point) '(face ztreep-header-face))
    (newline))
  (setq ztree-start-line (line-number-at-pos (point))))


(defun ztree-refresh-buffer (&optional line)
  (interactive)
  (when (and (equal major-mode 'ztree-mode)
             (boundp 'ztree-start-node))
    (setq ztree-node-to-line-list nil)
    (toggle-read-only)
    (erase-buffer)
    (ztree-insert-buffer-header)
    (ztree-insert-node-contents ztree-start-node)
    (scroll-to-line (if line line ztree-start-line))
    (toggle-read-only)))


(defun ztree (path)
  "Creates an interactive buffer with the directory tree of the path given"
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf (get-buffer-create (concat "*Directory " path " tree*"))))
      (switch-to-buffer buf)
      (ztree-mode)
      (setq ztree-start-node (expand-file-name (substitute-in-file-name path)))
      (setq ztree-expanded-nodes-list (list ztree-start-node))
      (setq ztree-filter-list (list ztree-hidden-files-regexp))
      (ztree-refresh-buffer))))


(provide 'ztree)
;;; ztree.el ends here

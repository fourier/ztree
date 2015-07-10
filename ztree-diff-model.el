;;; ztree-diff-model.el --- diff model for directory trees -*- lexical-binding: t; -*-

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

;; Diff model

;;; Code:
(require 'ztree-util)

(defvar ztree-diff-model-wait-message nil
  "Message showing while constructing the diff tree.")
(make-variable-buffer-local 'ztree-diff-model-wait-message)

(defvar ztree-diff-model-ignore-fun nil
  "Function which determines if the node should be excluded from comparison.")
(make-variable-buffer-local 'ztree-diff-model-ignore-fun)

(defun ztree-diff-model-update-wait-message ()
  "Update the wait mesage with one more '.' progress indication."
  (when ztree-diff-model-wait-message
    (setq ztree-diff-model-wait-message (concat ztree-diff-model-wait-message "."))
    (message ztree-diff-model-wait-message)))

;; Create a record ztree-diff-node with defined fields and getters/setters
;; here:
;; parent - parent node
;; left-path is the full path on the left side of the diff window,
;; right-path is the full path of the right side,
;; short-name - is the file or directory name
;; children - list of nodes - files or directories if the node is a directory
;; different = {nil, 'same, 'new, 'diff, 'ignore} - means comparison status
(ztree-defrecord ztree-diff-node (parent left-path right-path short-name right-short-name children different))

(defun ztree-diff-model-ignore-p (node)
  "Determine if the NODE should be excluded from comparison results."
  (when ztree-diff-model-ignore-fun
    (funcall ztree-diff-model-ignore-fun node)))

(defun ztree-diff-node-to-string (node)
  "Construct the string with contents of the NODE given."
  (let ((string-or-nil #'(lambda (x) (if x
                                         (cond ((stringp x) x)
                                               ((eq x 'new) "new")
                                               ((eq x 'diff) "different")
                                               ((eq x 'ignore) "ignored")
                                               ((eq x 'same) "same")
                                               (t (ztree-diff-node-short-name x)))
                                       "(empty)")))
        (children (ztree-diff-node-children node))
        (ch-str ""))
    (dolist (x children)
      (setq ch-str (concat ch-str "\n   * " (ztree-diff-node-short-name x)
                           ": "
                           (funcall string-or-nil (ztree-diff-node-different x)))))
    (concat "Node: " (ztree-diff-node-short-name node)
            "\n"
            " * Parent: " (funcall string-or-nil (ztree-diff-node-parent node))
            "\n"
            " * Status: " (funcall string-or-nil (ztree-diff-node-different node))
            "\n"
            " * Left path: " (funcall string-or-nil (ztree-diff-node-left-path node))
            "\n"
            " * Right path: " (funcall string-or-nil (ztree-diff-node-right-path node))
            "\n"
            " * Children: " ch-str
            "\n")))


(defun ztree-diff-node-short-name-wrapper (node &optional right-side)
  "Return the short name of the NODE given.
If the RIGHT-SIDE is true, take the right leaf"
  (if (not right-side)
      (ztree-diff-node-short-name node)
    (ztree-diff-node-right-short-name node)))


(defun ztree-diff-node-is-directory (node)
  "Determines if the NODE is a directory."
  (let ((left (ztree-diff-node-left-path node))
        (right (ztree-diff-node-right-path node)))
    (if left
        (file-directory-p left)
      (file-directory-p right))))

(defun ztree-diff-node-side (node)
  "Determine the side there the file is present for NODE.
Return BOTH if the file present on both sides;
LEFT if only on the left side and
RIGHT if only on the right side."
  (let ((left (ztree-diff-node-left-path node))
        (right (ztree-diff-node-right-path node)))
    (if (and left right) 'both
      (if left 'left 'right))))

(defun ztree-diff-node-equal (node1 node2)
  "Determines if NODE1 and NODE2 are equal."
  (and (string-equal (ztree-diff-node-short-name node1)
                     (ztree-diff-node-short-name node2))
       (string-equal (ztree-diff-node-left-path node1)
                     (ztree-diff-node-left-path node2))
       (string-equal (ztree-diff-node-right-path node1)
                     (ztree-diff-node-right-path node1))))

(defun ztree-diff-untrampify-filename (file)
  "Return FILE as the local file name."
  (require 'tramp)
  (if (not (tramp-tramp-file-p file))
      file
    (tramp-file-name-localname (tramp-dissect-file-name file))))

(defun ztree-diff-modef-quotify-string (x)
  "Surround string X with quotes."
  (concat "\"" x "\""))

(defun ztree-diff-model-files-equal (file1 file2)
  "Compare files FILE1 and FILE2 using external diff.
Returns t if equal."
  (let* ((file1-untrampified (ztree-diff-untrampify-filename (ztree-diff-modef-quotify-string file1)))
         (file2-untrampified (ztree-diff-untrampify-filename (ztree-diff-modef-quotify-string file2)))
         (diff-command (concat diff-command " -q" " " file1-untrampified " " file2-untrampified))
         (diff-output (shell-command-to-string diff-command)))
    (if (<= (length diff-output) 2) 'same 'diff)))

(defun ztree-directory-files (dir)
  "Return the list of full paths of files in a directory DIR.
Filters out . and .."
  (ztree-filter #'(lambda (file) (let ((simple-name (ztree-file-short-name file)))
                                   (not (or (string-equal simple-name ".")
                                            (string-equal simple-name "..")))))
                (directory-files dir 'full)))

(defun ztree-diff-model-partial-rescan (node)
  "Rescan the NODE."
  ;; assuming what parent is always exists
  ;; otherwise the UI shall force the full rescan
  (let ((parent (ztree-diff-node-parent node))
        (isdir (ztree-diff-node-is-directory node))
        (left (ztree-diff-node-left-path node))
        (right (ztree-diff-node-right-path node)))
    (if (not parent)      ;; if no parent - traverse
        (ztree-diff-node-traverse node)
      ;; verify if the node is not ignored by any chance
      (when (or (ztree-diff-node-ignore-p node)
                (eql (ztree-diff-node-different node) 'ignore))
        (ztree-diff-node-set-different node 'ignore))
      ;; if node is a directory - traverse
      (when (and left right
                 (file-exists-p left)
                 (file-exists-p right))
        (if isdir ;; traverse directory
            (ztree-diff-node-traverse node)
          ;; node is a file
          (ztree-diff-node-set-different
           node
           (if (ztree-diff-node-ignore-p node) 'ignore
             (ztree-diff-model-files-equal left right)))))
      ;; update all parents statuses
      (ztree-diff-node-update-all-parents-diff node))))

(defun ztree-diff-model-subtree (parent path side diff)
  "Create a subtree with given PARENT for the given PATH.
Argument SIDE either 'left or 'right side.
Argument DIFF different status to be assigned to all created nodes."
  (let ((files (ztree-directory-files path))
        (result nil))
    (dolist (file files)
      (if (file-directory-p file)
          (let* ((node (ztree-diff-node-create
                        parent
                        (when (eq side 'left) file)
                        (when (eq side 'right) file)
                        (ztree-file-short-name file)
                        (ztree-file-short-name file)
                        nil
                        diff))
                 (children (ztree-diff-model-subtree node file side diff)))
            (ztree-diff-node-set-children node children)
            (push node result))
        (push (ztree-diff-node-create
               parent
               (when (eq side 'left) file)
               (when (eq side 'right) file)
               (ztree-file-short-name file)
               (ztree-file-short-name file)
               nil
               diff)
              result)))
    result))

(defun ztree-diff-node-update-diff-from-children (node)
  "Set the diff status for the NODE based on its children."
                                        ;(unless (eq (ztree-diff-node-different node 'ignore))
  (let ((diff (cl-reduce 'ztree-diff-model-update-diff
                         (ztree-diff-node-children node)
                         :initial-value 'same
                         :key 'ztree-diff-node-different)))
    (ztree-diff-node-set-different node diff)))

(defun ztree-diff-node-update-all-parents-diff (node)
  "Recursively update all parents diff status for the NODE."
  (let ((parent node))
    (while (setq parent (ztree-diff-node-parent parent))
      (ztree-diff-node-update-diff-from-children parent))))


(defun ztree-diff-model-update-diff (old new)
  "Get the diff status depending if OLD or NEW is not nil.
If the OLD is 'ignore, do not change anything"
  ;; if the old whole directory is ignored, ignore children's status
  (cond ((eql old 'ignore) 'ignore)
        ;; if the new status is ignored, use old
        ((eql new 'ignore) old)
        ;; if the old or new status is different, return different
        ((or (eql old 'diff)
             (eql new 'diff)) 'diff)
        ;; if new is 'new, return new
        ((eql new 'new) 'new)
        ;; all other cases return old
        (t old)))


(defun ztree-diff-model-find-in-files (list shortname is-dir)
  "Find in LIST of files the file with name SHORTNAME.
If IS-DIR searching for directories; assume files otherwise"
  (ztree-find list
              (lambda (x) (and (string-equal (ztree-file-short-name x)
                                             shortname)
                               (eq is-dir (file-directory-p x))))))



(defun ztree-diff-node-traverse (node)
  "Traverse 2 paths defined in the NODE updating its children and status."
  (let* ((list1 (ztree-directory-files (ztree-diff-node-left-path node))) ;; left list of liles
         (list2 (ztree-directory-files (ztree-diff-node-right-path node))) ;; right list of files
         (diff-status 'same) ;; status of this node
         (parent (ztree-diff-node-parent node))
         ;; when parent defined and its status is 'ignore
         (parent-ignored
          (and parent (eql (ztree-diff-node-different parent) 'ignore)))
         ;; status automatically assigned to children of the node when
         ;; they shouldn't be compared
         (children-status (if parent-ignored 'ignore 'new))
         (children nil))    ;; list of children
    ;; update waiting status
    (ztree-diff-model-update-wait-message)
    ;; update node status ignore status either inhereted from the
    ;; parent or the own
    (when (or parent-ignored
              (and parent (ztree-diff-model-ignore-p node)))
      (ztree-diff-node-set-different node 'ignore)
      (setf diff-status 'ignore))
    ;; first - adding all entries from left directory
    (dolist (file1 list1)
      ;; for every entry in the first directory
      ;; we are creating the node
      (let* ((simple-name (ztree-file-short-name file1))
             (isdir (file-directory-p file1))
             ;; create a child. The current node is a parent
             ;; new by default - will be overriden below if necessary
             (child (ztree-diff-node-create node file1 nil simple-name simple-name nil children-status))
             ;; find if the file is in the second directory and the type
             ;; is the same - i.e. both are directories or both are files
             (file2 (ztree-diff-model-find-in-files list2 simple-name isdir)))
        ;; entry set right path if found or nil otherwise
        (ztree-diff-node-set-right-path child file2)
        ;; update child own ignore status
        (when (ztree-diff-model-ignore-p child)
          (ztree-diff-node-set-different child 'ignore))
        (cond
         ;; when exist just on a left side and is a directory, add all
         ((and isdir (not file2))
          (ztree-diff-node-set-children child
                                        (ztree-diff-model-subtree child
                                                                  file1
                                                                  'left
                                                                  (ztree-diff-node-different child))))
         ;; if 1) exists on both sides and 2) it is a file
         ;; and 3) not ignored file
         ((and file2 (not isdir) (not (eql (ztree-diff-node-different child) 'ignore)))
          (ztree-diff-node-set-different child
                                         (ztree-diff-model-files-equal file1 file2)))
         ;; if exists on both sides and it is a directory, traverse further
         ((and file2 isdir)
          (ztree-diff-node-traverse child)))
        ;; push the created node to the children list
        (push child children)))
    ;; second - adding entries from the right directory which are not present
    ;; in the left directory
    (dolist (file2 list2)
      ;; for every entry in the second directory
      ;; we are creating the node
      (let* ((simple-name (ztree-file-short-name file2))
             (isdir (file-directory-p file2))
             ;; create the child to be added to the results list
             (child (ztree-diff-node-create node nil file2 simple-name simple-name nil children-status))
             ;; find if the file is in the first directory and the type
             ;; is the same - i.e. both are directories or both are files
             (file1 (ztree-diff-model-find-in-files list1 simple-name isdir)))
        ;; update ignore status of the child
        (when (ztree-diff-model-ignore-p child)
          (ztree-diff-node-set-different child 'ignore))
        ;; if it is not in the first directory, add it as a child
        (unless file1
          ;; if it is a directory, set the whole subtree to children
          (when isdir
            (ztree-diff-node-set-children child
                                          (ztree-diff-model-subtree child
                                                                    file2
                                                                    'right
                                                                    (ztree-diff-node-different child))))
          ;; push the created node to the result list
          (push child children))))
    ;; finally set different status based on all children
    ;; depending if the node should participate in overall result
    (unless parent-ignored
      (setq diff-status (cl-reduce 'ztree-diff-model-update-diff
                                   children
                                   :initial-value diff-status
                                   :key 'ztree-diff-node-different)))
    (ztree-diff-node-set-different node diff-status)
    ;; and set children
    (ztree-diff-node-set-children node children)))


(defun ztree-diff-model-create (dir1 dir2 &optional ignore-p)
  "Create a node based on DIR1 and DIR2.
IGNORE-P is the optional filtering function, taking node as
an argument, which determines if the node should be excluded
from comparison."
  (unless (file-directory-p dir1)
    (error "Path %s is not a directory" dir1))
  (unless (file-directory-p dir2)
    (error "Path %s is not a directory" dir2))
  (setf ztree-diff-model-ignore-fun ignore-p)
  (setq ztree-diff-model-wait-message (concat "Comparing " dir1 " and " dir2 " ..."))
  (let* ((model
          (ztree-diff-node-create nil dir1 dir2
                                  (ztree-file-short-name dir1)
                                  (ztree-file-short-name dir2)
                                  nil
                                  nil)))
    (ztree-diff-node-traverse model)
    (message "Done.")
    model))

(defun ztree-diff-model-update-node (node)
  "Refresh the NODE."
  (setq ztree-diff-model-wait-message
        (concat "Updating " (ztree-diff-node-short-name node) " ..."))
  (ztree-diff-node-traverse node)
  (message "Done."))



(provide 'ztree-diff-model)

;;; ztree-diff-model.el ends here

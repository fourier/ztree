;; Diff model

(require 'ztree-util)

(defvar ztree-diff-model-wait-message nil
  "Message showing while constructing the diff tree")
(make-variable-buffer-local 'ztree-diff-model-wait-message)


(defun ztree-diff-model-update-wait-message ()
  (when ztree-diff-model-wait-message
    (setq ztree-diff-model-wait-message (concat ztree-diff-model-wait-message "."))
    (message ztree-diff-model-wait-message)))



;; Create a record ztree-diff-node with defined fielsd and getters/setters
;; here left-path is the full path on the left side of the diff window,
;; right-path is the full path of the right side,
;; short-name - is the file or directory name
;; children - list of nodes - files or directories if the node is a directory
;; different = {nil, 'new, 'diff} - means comparison status
(defrecord ztree-diff-node (left-path right-path short-name children different))


(defun ztree-diff-node-is-directory (node)
  (let ((left (ztree-diff-node-left-path node))
        (right (ztree-diff-node-right-path node)))
    (if left
        (file-directory-p left)
      (file-directory-p right))))

(defun ztree-diff-node-side (node)
 (let ((left (ztree-diff-node-left-path node))
        (right (ztree-diff-node-right-path node)))
   (if (and left right) 'both
    (if left 'left 'right))))

(defun ztree-diff-model-files-equal (file1 file2)
  "Compare files using external diff. Returns t if equal"
  (let ((diff-output (shell-command-to-string (concat "diff -q" " " file1 " " file2))))
    (not (> (length diff-output) 2))))


(defun ztree-directory-files (dir)
  "Returns the list of full paths of files in a directory, filtering out . and .."
  (ztree-filter #'(lambda (file) (let ((simple-name (file-short-name file)))
                                   (not (or (string-equal simple-name ".")
                                            (string-equal simple-name "..")))))
                (directory-files dir 'full)))

(defun ztree-diff-model-subtree (path side)
  "Creates a subtree for the given path for either 'left or 'right sides"
  (let ((files (ztree-directory-files path))
        (result nil))
    (dolist (file files)
      (if (file-directory-p file)
          (push (ztree-diff-node-create
                 (when (eq side 'left) file)
                 (when (eq side 'right) file)
                 (file-short-name file)
                 (ztree-diff-model-subtree file side)
                 'new)
                result)
        (push (ztree-diff-node-create
               (when (eq side 'left) file)
               (when (eq side 'right) file)
               (file-short-name file)
               nil
               'new)
              result)))
    result))


(defun ztree-diff-model-update-diff (old new)
  (if new
      (if (or (not old)
              (eq old 'new))
          new
        old)
    old))

(defun ztree-diff-node-traverse (parent path1 path2)
  "Function traversing 2 paths returning the list where the
first element is the difference status (nil, 'diff, 'new') and
the rest is the combined list of nodes"
  (let ((list1 (ztree-directory-files path1))
        (list2 (ztree-directory-files path2))
        (different-dir nil)
        (result nil))
    (ztree-diff-model-update-wait-message)
    ;; first - adding all entries from left directory
    (dolist (file1 list1)
      ;; for every entry in the first directory 
      ;; we are creating the node
      (let* ((simple-name (file-short-name file1))
             (isdir (file-directory-p file1))
             (children nil)
             (different nil)
             ;; 1. find if the file is in the second directory and the type
             ;;    is the same - i.e. both are directories or both are files
             (file2 (ztree-find list2
                                #'(lambda (x) (and (string-equal (file-short-name x)
                                                                 simple-name)
                                                   (eq isdir (file-directory-p x)))))))
        ;; 2. if it is not in the second directory, add it as a node
        (if (not file2)
            (progn
              ;; 2.1 if it is a directory, add the whole subtree
              (when (file-directory-p file1)
                (setq children (ztree-diff-model-subtree file1 'left)))
              ;; 2.2 update the difference status for this entry
              (setq different 'new))
          ;; 3. if it is found in second directory and of the same type
          ;; 3.1 if it is a file
          (if (not (file-directory-p file1))
              ;; 3.1.1 set difference status to this entry
              (setq different (if (ztree-diff-model-files-equal file1 file2) nil 'diff))
            ;; 3.2 if it is the directory
            ;; 3.2.1 get the result of the directories comparison together with status
            (let ((traverse (ztree-diff-node-traverse parent file1 file2)))
              ;; 3.2.2 update the difference status for whole comparison from
              ;;       difference result from the 2 subdirectories comparison
              (setq different (car traverse))
              ;; 3.2.3 set the children list from the 2 subdirectories comparison
              (setq children (cdr traverse)))))
        ;; 2.3 update difference status for the whole comparison
        (setq different-dir (ztree-diff-model-update-diff different-dir different))
        (let ((node (ztree-diff-node-create file1 file2 simple-name children different)))
          ;; push the created node to the result list
          (push node result))))
    ;; second - adding entries from the right directory which are not present
    ;; in the left directory
    (dolist (file2 list2)
      ;; for every entry in the second directory 
      ;; we are creating the node
      (let* ((simple-name (file-short-name file2))
             (isdir (file-directory-p file2))
             (children nil)
             ;; 1. find if the file is in the first directory and the type
             ;;    is the same - i.e. both are directories or both are files
             (file1 (ztree-find list1
                                #'(lambda (x) (and (string-equal (file-short-name x)
                                                                 simple-name)
                                                   (eq isdir (file-directory-p x)))))))
        ;; if it is not in the first directory, add it as a node
        (when (not file1)
          ;; if it is a directory, set the whole subtree to children
          (when (file-directory-p file2)
            (setq children (ztree-diff-model-subtree file2 'right)))
          ;; update the different status for the whole comparison
          (setq different-dir (ztree-diff-model-update-diff different-dir 'new))
          ;; push the created node to the result list
          (push (ztree-diff-node-create file1 file2 simple-name children 'new)
                result))))
    (cons different-dir result)))

(defun ztree-diff-model-create (dir1 dir2)
  (when (not (file-directory-p dir1))
    (error "Path %s is not a directory" dir1))
  (when (not (file-directory-p dir2))
    (error "Path %s is not a directory" dir2))
  (setq ztree-diff-model-wait-message (concat "Comparing " dir1 " and " dir2 " ..."))
  (let* ((model 
          (ztree-diff-node-create dir1 dir2
                                   (concat (file-short-name dir1)
                                           " <--> "
                                           (file-short-name dir2))
                                   nil
                                   nil))
         (traverse (ztree-diff-node-traverse model dir1 dir2)))
    (ztree-diff-node-set-children model (cdr traverse))
    (print model)
    (ztree-diff-node-set-different model (car traverse))
    (message "Done.")
    model))

  
(provide 'ztree-diff-model)

;; Diff model

(require 'ztree-util)

;; different = {nil, 'new, 'diff}
(defun ztree-diff-model-create-node (left-full-path right-full-path short-name children different)
  (let (node)
    (setq node (plist-put node 'left left-full-path))
    (setq node (plist-put node 'right right-full-path))
    (setq node (plist-put node 'short short-name))
    (setq node (plist-put node 'children children))
    (setq node (plist-put node 'different different))))

;; Getters

(defun ztree-diff-model-get-left-path (node)
  (plist-get node 'left))

(defun ztree-diff-model-get-right-path (node)
  (plist-get node 'right))

(defun ztree-diff-model-short-name (node)
    (plist-get node 'short))

(defun ztree-diff-model-children (node)
    (plist-get node 'children))

(defun ztree-diff-model-differet (node)
    (plist-get node 'different))

;; Setters

(defun ztree-diff-model-set-parent (node)
  (plist-put node 'parent parent))

(defun ztree-diff-model-set-children (node children)
  (plist-put node 'children children))

(defun ztree-diff-model-set-different (node different)
  (plist-put node 'different different))


(defun ztree-diff-model-is-directory (node)
  (let ((left  (plist-get node 'left))
        (right (plist-get node 'right)))
    (if left
        (file-directory-p left)
      (file-directory-p right))))

(defun ztree-diff-model-side (node)
  (let ((left  (plist-get node 'left))
        (right (plist-get node 'right)))
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
          (push (ztree-diff-model-create-node
                 (when (eq side 'left) file)
                 (when (eq side 'right) file)
                 (file-short-name file)
                 (ztree-diff-model-subtree file side)
                 'new)
                result)
        (push (ztree-diff-model-create-node
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

(defun ztree-diff-model-traverse (parent path1 path2)
  "Function traversing 2 paths returning the list where the
first element is the difference status (nil, 'diff, 'new') and
the rest is the combined list of nodes"
  (let ((list1 (ztree-directory-files path1))
        (list2 (ztree-directory-files path2))
        (different-dir nil)
        (result nil))
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
            (let ((traverse (ztree-diff-model-traverse parent file1 file2)))
              ;; 3.2.2 update the difference status for whole comparison from
              ;;       difference result from the 2 subdirectories comparison
              (setq different (car traverse))
              ;; 3.2.3 set the children list from the 2 subdirectories comparison
              (setq children (cdr traverse)))))
        ;; 2.3 update difference status for the whole comparison
        (setq different-dir (ztree-diff-model-update-diff different-dir different))
        (let ((node (ztree-diff-model-create-node file1 file2 simple-name children different)))
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
          (push (ztree-diff-model-create-node file1 file2 simple-name children 'new)
                result))))
    (cons different-dir result)))

(defun ztree-diff-model-create (dir1 dir2)
  (when (not (file-directory-p dir1))
    (error "Path %s is not a directory" dir1))
  (when (not (file-directory-p dir2))
    (error "Path %s is not a directory" dir2))
  (message (concat "Comparing " dir1 " and " dir2 " ..."))
  (let* ((model 
          (ztree-diff-model-create-node dir1 dir2
                                        (concat (file-short-name dir1)
                                                " <--> "
                                                (file-short-name dir2))
                                        nil
                                        nil))
         (traverse (ztree-diff-model-traverse model dir1 dir2)))
    (ztree-diff-model-set-children model (cdr traverse))
    (print model)
    (ztree-diff-model-set-different model (car traverse))
    (message "Done.")
    model))

  
(provide 'ztree-diff-model)

                              
        


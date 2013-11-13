;; Diff model

(defun ztree-diff-model-get-left-path (node)
  (plist-get node 'path))

(defun ztree-diff-model-create-node (left-full-path right-full-path short-name children different)
  (let (node)
    (setq node (plist-put node 'left left-full-path))
    (setq node (plist-put node 'right right-full-path))
    (setq node (plist-put node 'short short-name))
    (setq node (plist-put node 'children children))
    (setq node (plist-put node 'different different))))


(defun file-short-name (file)
  "Base file/directory name. Taken from
 http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (printable-string (file-name-nondirectory (directory-file-name file))))


(defun ztree-diff-model-files-are-different (file1 file2)
  (let ((diff-output (shell-command-to-string (concat "diff -q" " " file1 " " file2))))
    (> (length diff-output) 2)))

(defun ztree-diff-model-traverse (path1 path2)
  (let ((list1 (directory-files path1 'full))
        (list2 (directory-files path2 'full))
        (result nil))
    ;; first - adding all files from left directory
    (dolist (file1 list1)
      (let ((simple-name (file-short-name file1)))
        ;; if a file is not a special
        (if (not (or (string-equal simple-name ".")
                     (string-equal simple-name "..")))
            ;; find if the file is in the second directory
            (let ((file2 (ztree-find list2
                                     #'(lambda (x) (string-equal (file-short-name x)
                                                                 simple-name))))
                  (children nil)
                  (different nil))
              (when (and file2
                         (not (file-directory-p file1))
                         (setq different
                               (ztree-diff-model-files-are-different file1 file2))))
              (push (ztree-diff-model-create-node file1 file2 simple-name nil different)
                    result)))))
    ;; second - adding files from right directory which are missing in first directory
    (dolist (file2 list2)
      (let ((simple-name (file-short-name file2)))
        ;; if a file is not a special
        (if (not (or (string-equal simple-name ".")
                     (string-equal simple-name "..")))
            ;; find if the file is in the second directory
            (let ((file1 (ztree-find list1
                                     #'(lambda (x) (string-equal (file-short-name x)
                                                                 simple-name))))
                  (children nil))
              (when (not file1)
                (push (ztree-diff-model-create-node nil file2 simple-name children nil)
                    result))))))
    result))

(dolist (diff 
         (ztree-diff-model-traverse "/Users/alexeyv/Sources/Science/libsexp"
                                    "/Users/alexeyv/Sources/Science/libsexpv1"))
  (print diff))
                              
        

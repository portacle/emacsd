(provide 'portacle-update)

(defun portacle-pull-preserving-changes (place)
  (let ((default-directory place))
    (call-process magit-git-executable nil (current-buffer) t "stash")
    (call-process magit-git-executable nil (current-buffer) t "pull")
    (call-process magit-git-executable nil (current-buffer) t "stash" "pop")))

(defun portacle-recompile (&optional force)
  (byte-recompile-directory (portacle-path "all/emacsd/portacle/") 0 force))

(defun portacle-update-packages ()
  (interactive)
  (package-refresh-contents)
  (cl-flet ((get-version (name where)
              (let ((pkg (cl-second (assq name where))))
                (when pkg
                  (package-desc-version pkg)))))
    (save-window-excursion
     (let ((to-delete ()))
       (cl-loop for (name package-desc) in package-alist
                for in-archive = (get-version name package-archive-contents)
                do (when (and in-archive
                              (version-list-< (get-version name package-alist) in-archive))
                     (package-install name)
                     (push package-desc to-delete)))
       (mapcar #'package-delete to-delete)))))

(defun portacle-update ()
  (interactive)
  (with-help-window "*portacle-update*"
    (with-current-buffer "*portacle-update*"
      (switch-to-buffer (current-buffer))
      (insert "===> Starting Portacle update\n")
      (insert "  --> Updating config via GIT\n")
      (portacle-pull-preserving-changes (portacle-path "config/"))
      (insert "  --> Updating emacsd via GIT\n")
      (portacle-pull-preserving-changes (portacle-path "all/emacsd/portacle/"))
      (insert "  --> Recompiling ELISP sources\n")
      (portacle-recompile)
      (insert "  --> Updating dists via QL\n")
      (slime-eval '(ql:update-all-dists :prompt cl:nil))
      (insert "  --> Updating client via QL\n")
      (slime-eval '(ql:update-client :prompt cl:nil))
      (insert "  --> Updating packages via ELPA\n")
      (portacle-update-packages)
      (insert "===> All done\n")
      (insert "\n Please restart Portacle for the changes to take full effect.\n")
      (insert "\n Press q to close this buffer."))))

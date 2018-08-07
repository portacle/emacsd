(provide 'portacle-user)

(setq custom-file (portacle-path "config/user.el")
      user-init-file custom-file)

(defun portacle-load-user-init ()
  "Load the user.el customisation file."
  (interactive)
  (when (file-exists-p user-init-file)
    (load user-init-file)))

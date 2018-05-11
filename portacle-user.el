(provide 'portacle-user)

(setq custom-file (portacle-path "config/user.el")
      user-init-file custom-file)
(when (file-exists-p user-init-file)
  (load user-init-file))

(provide 'portacle-user)

(setq custom-file (portacle-path "config/user.el"))
(when (file-exists-p (portacle-path "config/user.el"))
  (load (portacle-path "config/user.el")))

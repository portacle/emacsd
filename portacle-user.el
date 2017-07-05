(provide 'portacle-user)

;; Make sure to prompt to save changes in customize
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

(setq custom-file (portacle-path "config/user.el"))
(when (file-exists-p (portacle-path "config/user.el"))
  (load (portacle-path "config/user.el")))

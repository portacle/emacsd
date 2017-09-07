(provide 'portacle-magit)
(ensure-installed 'magit)

;; Set the Magit executable explicitly
(setq magit-git-executable (portacle-bin-path "git"))

(setq magit-delete-by-moving-to-trash nil)
(setq magit-no-confirm '(stage-all-changes unstage-all-changes))

;; Stop with these fucking annoying "'"style"'" conventions
(setq git-commit-fill-column 9999)
(setq git-commit-summary-max-length 9999)
(setq git-commit-finish-query-functions nil)

;; Ensure it uses the proper emacsclient
(setq with-editor-emacsclient-executable (portacle-bin-path "emacsclient"))

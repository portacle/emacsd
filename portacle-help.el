(provide 'portacle-help)

(with-current-buffer (get-buffer-create "*portacle-help*")
  (insert-file-contents (portacle-path "config/help.txt"))
  (read-only-mode)
  (emacs-lock-mode 'kill))

(defun portacle-help ()
  (interactive)
  (switch-to-buffer (get-buffer "*portacle-help*")))

(define-portacle-key "C-h h" 'portacle-help)

;; Customise the scratch buffer
(setq initial-scratch-message (portacle-fread (portacle-path "config/scratch.txt")))
(setq initial-major-mode 'common-lisp-mode)

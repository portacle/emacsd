(provide 'portacle-cursors)

(ensure-installed 'multiple-cursors 'expand-region)

(require 'multiple-cursors)
(require 'expand-region)

;; Workaround for https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

(define-portacle-key "C-q" 'er/expand-region)
(define-portacle-key "C-S-c C-S-c" 'mc/edit-lines)
(define-portacle-key "C-M-<next>" 'mc/mark-next-like-this)
(define-portacle-key "C-M-<prior>" 'mc/mark-previous-like-this)
(define-portacle-key "C-M-m <down>" 'mc/mark-next-like-this)
(define-portacle-key "C-M-m <up>" 'mc/mark-previous-like-this)
(define-portacle-key "C-M-m <right>" 'mc/unmark-next-like-this)
(define-portacle-key "C-M-m <left>" 'mc/unmark-previous-like-this)
(define-portacle-key "C-M-m a" 'mc/mark-all-like-this)

;; Populate default MC lists
(unless (file-exists-p mc/list-file)
  (setq mc/cmds-to-run-for-all
        '(backward-sexp
          downcase-region
          electric-newline-and-maybe-indent
          end-of-buffer
          forward-sexp
          indent-for-tab-command
          kill-region
          paredit-backslash
          paredit-backward
          paredit-close-round
          paredit-close-square
          paredit-comment-dwim
          paredit-convolute-sexp
          paredit-doublequote
          paredit-forward
          paredit-forward-barf-sexp
          paredit-forward-delete
          paredit-forward-down
          paredit-forward-slurp-sexp
          paredit-kill
          paredit-newline
          paredit-open-round
          paredit-open-square
          paredit-reindent-cl-defun
          paredit-semicolon
          paredit-splice-sexp-killing-backward
          paredit-backslash
          reindent-then-newline-and-indent
          scroll-other-window
          slime-autodoc-space
          slime-space
          switch-to-buffer
          upcase-region
          yank-rectangle))
  (setq mc/cmds-to-run-once
        '(down-list
          ido-list-directory
          mouse-drag-mode-line)))

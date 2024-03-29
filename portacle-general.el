(provide 'portacle-general)
(load-library "iso-transl")

(ensure-installed 'smex 'doom-themes 'doom-modeline 'helpful 'centaur-tabs)

(require 'smex)
(require 'helpful)
(require 'doom-modeline)
(require 'centaur-tabs)

(doom-modeline-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(setq show-paren-delay 0) ; must be set before show-paren-mode
(show-paren-mode 1)
(electric-indent-mode 1)
(semantic-mode 1)
(delete-selection-mode 1)
(global-display-line-numbers-mode 1)

(smex-initialize)

(setq-default indent-tabs-mode nil)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq ido-enable-flex-matching t)
(setq enable-local-variables :all)
(setq backup-by-copying t)
(setq backup-directory-alist `((,tramp-file-name-regexp . nil)
                               (".*" . ,(portacle-path "config/saves/"))))
(setq auto-save-file-name-transforms `((".*" ,(portacle-path "config/saves/") t)))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq vc-follow-symlinks t)
(setq centaur-tabs-height 32)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(centaur-tabs-headline-match)
(centaur-tabs-mode t)

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))

(defun sudo ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file (concat "/sudo::" (buffer-file-name (current-buffer))))
    (goto-char position)))

(define-portacle-key "C-c k" 'delete-this-buffer-and-file)
(define-portacle-key "C-h f" 'helpful-callable)
(define-portacle-key "C-h v" 'helpful-variable)
(define-portacle-key "C-h k" 'helpful-key)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'portacle-general)
(load-library "iso-transl")

(ensure-installed 'smex 'powerline)

(require 'powerline)
(require 'smex)

(ido-mode 1)
(show-paren-mode 1)
(electric-indent-mode 1)
(semantic-mode 1)
(delete-selection-mode 1)
(global-display-line-numbers-mode 1)

(smex-initialize)
(powerline-default-theme)

(setq-default indent-tabs-mode nil)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq show-paren-delay 0)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
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

(setq browse-url-browser-function
      (lambda (url &optional new-tab)
        (os-case (gnu/linux
                  (call-process "xdg-open" nil 0 nil url))
                 (darwin
                  (call-process "open" nil 0 nil url))
                 (windows-nt
                  (call-process "cmd" nil 0 nil "/c" (concat "start " url))))))

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
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

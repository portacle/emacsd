(provide 'portacle-window)

(ensure-installed 'sublime-themes)

(setq inhibit-startup-screen t)
(setq pop-up-frame-function (lambda () (split-window-right)))
(setq split-height-threshold 1400)
(setq split-width-treshold 1500)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program (or (getenv "BROWSER") "xdg-open"))
(setq ring-bell-function 'ignore)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'spolsky t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (set-frame-font (os-case (gnu/linux "Monospace-10")
                           (darwin "Monaco-10")
                           (windows-nt "Consolas-10")) nil t))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq ns-function-modifier 'super)
  (setq ns-right-alternate-modifier nil))

;; http://directed-procrastination.blogspot.co.uk/2014/04/some-emacs-hacks-for-gdb-and-other-stuff.html
(defun undedicate-window (&optional window)
  (interactive)
  (set-window-dedicated-p (or window (get-buffer-window)) nil))

;; Removing annoying dedicated buffer nonsense
(defun switch-to-buffer! (buffer-or-name &optional norecord force-same-window)
  "Like switch-to-buffer but works for dedicated buffers \(though
it will ask first)."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer: ") nil 'force-same-window))
  (when (and (window-dedicated-p (get-buffer-window))
             (yes-or-no-p "This window is dedicated, undedicate it? "))
    (undedicate-window))
  (switch-to-buffer buffer-or-name norecord force-same-window))

(defun toggle-window-dedication (&optional window)
  (interactive)
  (let ((window (or window (get-buffer-window))))
    (set-window-dedicated-p window (not (window-dedicated-p window)))))

(define-portacle-key "C-c d" 'toggle-window-dedication)

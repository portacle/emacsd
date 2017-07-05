(provide 'portacle-keys)

(defvar portacle-keys-minor-mode-map (make-keymap) "portacle-keys-minor-mode keymap.")
(defmacro define-portacle-key (kbd func)
  `(define-key portacle-keys-minor-mode-map (kbd ,kbd) ,func))

;; Activate portacle-keys possibly everywhere
(define-minor-mode portacle-keys-minor-mode
  "A minor mode so that portacle key settings override annoying major modes."
  t " Portacle" 'portacle-keys-minor-mode-map)

(portacle-keys-minor-mode 1)
(defun portacle-minibuffer-setup-hook () (portacle-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'portacle-minibuffer-setup-hook)

(defadvice load (after give-portacle-keybindings-priority)
  "Try to ensure that portacle keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'portacle-keys-minor-mode))
      (let ((portaclekeys (assq 'portacle-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'portacle-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist portaclekeys))))
(ad-activate 'load)

(when (and (eq system-type 'darwin)
           window-system)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta))

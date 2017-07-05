(provide 'portacle-neotree)

(ensure-installed 'neotree)

(setq neo-smart-open nil)
(setq neo-autorefresh nil)
(setq neo-theme 'arrow)
(setq neo-window-width 40)

(define-portacle-key "C-x d" 'neotree-show)
(define-portacle-key "C-x e" 'neotree-hide)

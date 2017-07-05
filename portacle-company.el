(provide 'portacle-company)

(ensure-installed 'company 'company-quickhelp 'slime-company)

(require 'company)

(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.7)

(add-hook 'after-init-hook 'global-company-mode)
(push 'slime-company portacle-slime-contribs)

(define-key company-active-map (kbd "<up>") 'company-select-previous)
(define-key company-active-map (kbd "<down>") 'company-select-next)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

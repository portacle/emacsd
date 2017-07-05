(provide 'portacle-elisp)

(ensure-installed 'elisp-slime-nav 'paredit)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)

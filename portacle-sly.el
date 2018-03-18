(provide 'portacle-sly)

(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(ensure-installed 'sly)
(require 'sly)

(setq sly-auto-select-connection 'always)
(setq sly-kill-without-query-p t)
(setq sly-description-autofocus t) 
(setq sly-inhibit-pipelining nil)
(setq sly-load-failed-fasl 'always)

;; XXX: Work around a bug in SLY whereby installing/compiling it as
;; above fails to correctly set version (not very problematic for
;; Portacle, which always ensures matching versions anyway)
(setq sly-ignore-protocol-mismatches t)

;; Make sure SLY knows about our SBCL
(setq sly-lisp-implementations
      `((sbcl (,(portacle-bin-path "sbcl")))))

;; Make sure we don't clash with SLIME
(advice-add 'sly :before
            (lambda (&rest ignored)
              (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
              (add-hook 'lisp-mode-hook 'sly-editing-mode t))
            '((name . portacle-advice-before-sly)))

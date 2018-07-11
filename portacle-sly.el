(provide 'portacle-sly)

;; SLY checks lisp-mode-hook for incompatibilities at load-time and
;; queries the user immediately. This being a no-no in Portacle, make
;; sure we get rid of such problems even before installing/loading
;; SLY.
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

;; Portacle's use of powerline.el hides SLY's non-intrusive mode-line,
;; so just bite that bullet and make it as intrusive as SLIME's.
(add-to-list 'minor-mode-alist '(sly-mode
                                 (" " sly--mode-line-format " ")))

;; Don't turn on paredit in REPL, but at least use electric-pair-mode
(add-hook 'sly-mrepl-mode-hook 'electric-pair-local-mode)

;; Make sure we don't clash with SLIME when starting
(require 'portacle-slime) ; for portacle--resolve-ide-conflict
(advice-add 'sly :before
            (lambda (&rest ignored)
              (portacle--resolve-ide-conflict 'sly-editing-mode
                                              'slime-lisp-mode-hook))
            '((name . portacle-advice-before-sly)))

(provide 'portacle-slime)

(ensure-installed 'slime)

(require 'slime)

(setq slime-contribs '(slime-fancy slime-asdf slime-sprof slime-mdot-fu
                       slime-compiler-notes-tree slime-hyperdoc
                       slime-indentation slime-repl))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)
(setq slime-auto-select-connection 'always)
(setq slime-kill-without-query-p t)
(setq slime-description-autofocus t) 
(setq slime-fuzzy-explanation "")
(setq slime-asdf-collect-notes t)
(setq slime-inhibit-pipelining nil)
(setq slime-load-failed-fasl 'always)
(setq slime-when-complete-filename-expand t)
(setq slime-repl-history-remove-duplicates t)
(setq slime-repl-history-trim-whitespaces t)
(setq slime-export-symbol-representation-auto t)
(setq lisp-indent-function 'common-lisp-indent-function)
(setq lisp-loop-indent-subclauses nil)
(setq lisp-loop-indent-forms-like-keywords t)
(setq lisp-lambda-list-keyword-parameter-alignment t)

(add-hook 'slime-repl-mode-hook 'set-slime-repl-return)

(defun set-slime-repl-return ()
  (define-key slime-repl-mode-map (kbd "RET") 'slime-repl-return-at-end)
  (define-key slime-repl-mode-map (kbd "<return>") 'slime-repl-return-at-end))

(defun slime-repl-return-at-end ()
  (interactive)
  (if (<= (point-max) (point))
      (slime-repl-return)
      (slime-repl-newline-and-indent)))

;; Make sure Slime knows about our SBCL
(setq slime-lisp-implementations
      `((sbcl (,(portacle-bin-path "sbcl")))))

;; Make sure Slime stores the FASLs within Portacle
;; @Override
(defun slime-init-command (port-filename _coding-system)
  "Return a string to initialize Lisp."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                    (concat slime-path slime-backend))))
    ;; Return a single form to avoid problems with buffered input.
    (format "%S\n\n"
            `(progn
               (load ,(slime-to-lisp-filename (expand-file-name loader))
                     :verbose t)
               (setf (symbol-value (read-from-string "swank-loader:*fasl-directory*"))
                     ,(slime-to-lisp-filename (portacle-app-path "asdf" "cache/swank/")))
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,(slime-to-lisp-filename port-filename))))))

;; Make sure we don't clash with SLY
(defun portacle--resolve-ide-conflict (new-hook
                                       old-hook)
  "Replace OLD-HOOK with NEW-HOOK in `lisp-mode-hook'.
Also re-issue `lisp-mode' in every Lisp source buffer so that SLIME
or SLY are suitably setup there."
  (remove-hook 'lisp-mode-hook old-hook)
  (add-hook 'lisp-mode-hook new-hook t)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'lisp-mode)
              ;; XXX: actually our own *scratch* is special because
              ;; re-issuing lisp-mode there would drown out the pretty
              ;; buttons.
              (unless (equal "*scratch*" (buffer-name))
                (lisp-mode)))))
        (buffer-list)))

(advice-add 'slime :before
            (lambda (&rest ignored)
              (portacle--resolve-ide-conflict 'slime-lisp-mode-hook
                                              'sly-editing-mode))
            '((name . portacle-advice-before-slime)))


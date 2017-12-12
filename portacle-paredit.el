;; -*- mode: elisp; lexical-binding: t; -*-
(provide 'portacle-paredit)

(ensure-installed 'paredit)

(require 'paredit)
(eval-when-compile (require 'cl))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

(define-portacle-key "M-g" 'raise-sexp)

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Fix the spacing for macro characters such as #p, etc.
(defvar known-macro-characters (make-hash-table))

(defun determine-cl-macro-character (macro-char)
  (when (slime-connected-p)
    (lexical-let ((macro-char macro-char))
      (slime-eval-async
       `(cl:ignore-errors
         (cl:not (cl:null (cl:get-macro-character
                           (cl:code-char ,macro-char)))))
       (lambda (result)
         (puthash macro-char result known-macro-characters))))))

(defun cl-macro-character-p (macro-char)
  (pcase (gethash macro-char known-macro-characters :not-found)
         (`t t)
         (`nil nil)
         (:not-found
          (determine-cl-macro-character macro-char)
          (or ;; Don't know the result (yet), determine statically.
              (cl-find macro-char '(?# ?,))))))

(defun paredit-detect-cl-macro-character (endp delimiter)
  (when (cl-find major-mode '(slime-repl-mode lisp-mode))
    (if (not endp)
        (save-excursion
         (let ((1-back (char-before (point)))
               (2-back (char-before (- (point) 1))))
           (null (or (cl-macro-character-p (char-before (point)))
                     (cl-macro-character-p (char-before (1- (point))))))))
        t)))

(with-eval-after-load 'paredit
  (add-to-list 'paredit-space-for-delimiter-predicates
               'paredit-detect-cl-macro-character))

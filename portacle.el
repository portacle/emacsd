(provide 'portacle)
(require 'cl-lib)

;;; Configure OS and Path handling
(cl-defmacro os-case (&body cases)
             `(cond ,@(cl-loop for case in cases collect
                               (if (eql (car case) t)
                                   `(t ,@(cdr case))
                                   `((eql system-type ',(car case)) ,@(cdr case))))))

(setq portacle-root (replace-regexp-in-string "\\\\" "/" (or (getenv "ROOT") (expand-file-name "~/"))))
(setq portacle-os (os-case (gnu/linux "lin") (darwin "mac") (windows-nt "win")))

(defun portacle-path (path)
  (concat portacle-root path))

(defun portacle-os-path (path)
  (portacle-path (concat portacle-os "/" path)))

(defun portacle-bin-path (bin)
  (portacle-os-path (os-case (windows-nt (concat "bin/" bin ".exe"))
                             (t (concat "bin/" bin)))))

(defun portacle-app-path (app path)
  (portacle-os-path (concat app "/" path)))

(defun add-to-path (&rest things)
  (cond ((eql system-type 'windows-nt)
         (setenv "PATH" (concat (mapconcat (lambda (a) (replace-regexp-in-string "/" "\\\\" a)) things ";")
                                ";" (getenv "PATH"))))
        (t
         (setenv "PATH" (concat (mapconcat 'identity things ":")
                                ":" (getenv "PATH")))))
  (setq exec-path (append exec-path things)))

;;; Toolkit functions
(defun portacle-fwrite (contents file &optional append)
  (write-region contents nil file append))

(defun portacle-fread (file)
  (with-temp-buffer
      (insert-file-contents file)
    (buffer-string)))

;;; Load contribs
(require 'portacle-package)
(require 'portacle-keys)
(require 'portacle-general)
(require 'portacle-project)
(require 'portacle-update)
(require 'portacle-neotree)
(require 'portacle-slime)
(require 'portacle-sly)
(require 'portacle-elisp)
(require 'portacle-paredit)
(require 'portacle-company)
(require 'portacle-magit)
(require 'portacle-spell)
(require 'portacle-paste)
(require 'portacle-server)
(require 'portacle-window)
(require 'portacle-cursors)
(require 'portacle-ag)
(require 'portacle-help)
(require 'portacle-user)

;;; Setup SLIME or SLY to start at
;;;
(defun portacle-let-user-choose-ide (implementation)
  (let* ((ide-question
          (concat
           "Portacle comes with SLY and SLIME, two great common Lisp IDE's."
           "\n"
           "Which one would you like to start now? "))
         (choice
          (intern
           (ido-completing-read ide-question'("slime" "sly") nil t nil nil)))
         (save-question
          (concat
           (format
            "Great! Use `%s' from now on? " choice)
           "You can customize `portacle-ide' later if you change your mind."))
         (savep (y-or-n-p save-question)))
    (when savep
      (customize-save-variable 'portacle-ide choice))
    (funcall choice implementation)))

(defgroup portacle nil "Customization group for Portacle.")

(defcustom portacle-ide #'portacle-let-user-choose-ide
  "If non-nil, Common Lisp IDE to run when Portacle launches.

Value is a function that should accept at least one argument,
COMMAND, which is either a pathname string pointing to a Common
Lisp executable, or a symbol designating one, like `sbcl' or
`ecl' that the function should interpret accordingly.

Currently, Portable uses `sbcl' exclusively.

The symbols `slime' or `sly' are suitable candidates for this
variable."
  :type 'function
  :group 'portacle)

(defun portacle--start-ide-maybe ()
  "Start Portacle's IDE iff there is a window system."
  ;; XXX: Why? jt@2018/03/18
  (when (window-system)
    (funcall portacle-ide 'sbcl)))

;; Activate Slime after init
(add-hook 'emacs-startup-hook #'portacle--start-ide-maybe)

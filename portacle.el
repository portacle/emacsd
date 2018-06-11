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
(require 'portacle-config)
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

(portacle-create-help-buffer)
(when window-system (portacle--setup-frame))

(defun portacle--startup ()
  "Start Portacle's IDE iff there is a window system."
  (when (window-system)
    (funcall portacle-ide 'sbcl)
    (portacle-scratch-help)))

;; Activate Slime after init
(add-hook 'emacs-startup-hook #'portacle--startup)

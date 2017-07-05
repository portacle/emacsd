(provide 'portacle-server)
(require 'server)

(when (eql system-type 'windows-nt)
  ;; We hack this to never error because otherwise emacs refuses to work
  ;; as a server on Windows due to requiring the dir being fixed to a
  ;; "safe" directory, which we cannot ensure in our portable environment.
  (cl-defun server-ensure-safe-dir (dir)
            (unless (file-exists-p dir)
              (make-directory dir t))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and window-system (not (server-running-p))) 
              (server-start))))

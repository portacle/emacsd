;; -*- lexical-binding: t -*-
(require 'browse-url)
(require 'cl-lib)
(require 'portacle-keys) ; define-portacle-key
(require 'portacle-config)

;; can't require `portacle', because that requires us, perhaps make a
;; `portacle-common' or `portacle-utils' in the future.
(declare-function portacle-path "portacle" (path))

(defun portacle-help (&optional _event)
  (interactive)
  (switch-to-buffer (get-buffer "*portacle-help*")))

(define-portacle-key "C-h h" 'portacle-help)

(defvar portacle--help-region
  "Bounds of the help text in the *scratch* buffer.")

(defun portacle--help-button (label action)
  (make-text-button label nil
                    'follow-link t
                    'front-sticky '(read-only)
                    'rear-nonsticky nil
                    'action action))

(defun portacle--url-button (url &optional label)
  (list
   (portacle--help-button
    (or label url)
    (lambda (&optional _)
      (browse-url url)))))

(defun portacle--key-button (key &optional label)
  (list
   (portacle--help-button
    (or label key)
    (lambda (&optional _)
      (helpful-key (kbd key))))))

(defun portacle--function-button (fun &optional label)
  (list
   (portacle--help-button
    (or label (symbol-name fun))
    (lambda (&optional _)
      (helpful-callable fun)))))

(defun portacle--buffer-button (buffer &optional label)
  (list
   (portacle--help-button
    (or label buffer)
    (lambda (&optional _)
      (switch-to-buffer (get-buffer buffer))))))

(defun portacle--file-button (file &optional label)
  (list
   (portacle--help-button
    (or label file)
    (lambda (&optional _)
      (find-file (portacle-path file))))))

(defun portacle--first-time-setup ()
  (unless portacle-setup-done-p
    (list "Portacle is currently running" (upcase (format " %s " portacle-ide)) ", but you can"
          "\n;;   " (portacle--help-button
           (format "Switch to %s"
                   (if (eq portacle-ide 'slime) "SLY" "SLIME"))
           (lambda (&optional _event)
             (interactive)
             (let ((target (if (eq portacle-ide 'slime) 'sly 'slime)))
               (funcall target 'sbcl)
               (customize-save-variable 'portacle-ide target)
               (portacle-scratch-help 'preserve))))
          "\n;; You should also configure Portacle with the"
          "\n;;   " (portacle--help-button
           "First-time setup"
           (lambda (&optional _event)
             (interactive)
             (call-interactively 'portacle-configure)))
          "\n;; ")))

(defun portacle--read-inner-list (string)
  (let ((start 0))
    (cl-loop for (val . pos) = (ignore-errors
                                (read-from-string string start))
             while pos
             do (setq start pos)
             collect val)))

(defvar portacle-markup-commands
  '((url . portacle--url-button)
    (key . portacle--key-button)
    (buffer . portacle--buffer-button)
    (function . portacle--function-button)
    (file . portacle--file-button)
    (first-time-setup . portacle--first-time-setup)))

(defun portacle--interpret-scratch-expr (expr)
  (let ((fun (alist-get (cl-first expr) portacle-markup-commands
                        (lambda (&rest _) (list "{?}")))))
    (apply fun (cl-rest expr))))

(defun portacle-markup-file (file)
  (with-temp-buffer
      (insert-file-contents file)
    (goto-char (point-min))
    (let ((parts ()))
      (cl-loop with start = 1
               for char = (char-after (point))
               while char
               do (when (= char ?{)
                    (push (buffer-substring start (point)) parts)
                    (let ((start (point)))
                      ;; FIXME: This is primitive
                      (cl-loop for char = (char-after (point))
                               while char
                               do (cl-case char
                                    (?\\ (forward-char)
                                         (forward-char))
                                    (?} (cl-return))
                                    (t (forward-char))))
                      (dolist (part (portacle--interpret-scratch-expr
                                     (portacle--read-inner-list
                                      (buffer-substring (1+ start) (point)))))
                        (push part parts)))
                    (setf start (1+ (point))))
               (forward-char)
               finally (push (buffer-substring start (point)) parts))
      (nreverse parts))))

;; Workaround for font-lock.el's inability to use easily override
;; faces in lisp comments.
(defun portacle--find-buttons (limit)
  (let* ((prop-change (next-single-property-change (point)
                                                   'button
                                                   nil
                                                   limit))
         (prop-value (and prop-change
                          (get-text-property prop-change 'button)))
         (prop-end (and prop-value
                        (next-single-property-change prop-change
                                                     'button
                                                     nil
                                                     limit)))
         (match (match-data)))
    (when prop-end
      (goto-char prop-end)
      (setcar match prop-change)
      (setcar (cdr match) prop-end)
      (set-match-data match)
      (match-data))))

(defun portacle-scratch-help (&optional preserve-rest-of-buffer)
  "Pop a Portacle-specific *scratch* buffer with basic help."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((inhibit-read-only t))
      (if (not preserve-rest-of-buffer)
          (erase-buffer)
        (delete-region (car portacle--help-region)
                       (cdr portacle--help-region))
        (goto-char (car portacle--help-region)))
      (lisp-mode)
      (font-lock-add-keywords
       nil
       '((portacle--find-buttons . (0 'button prepend))))
      (save-excursion
        (let ((start (point-marker)))
          (apply #'insert (portacle-markup-file (portacle-path "config/scratch.txt")))
          (setq portacle--help-region
                (cons start
                      (point-marker))))))))

(defun portacle--create-help-buffer ()
  "Ensure a *portacle-help* buffer is created and has good tips."
  (with-current-buffer (get-buffer-create "*portacle-help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (apply #'insert (portacle-markup-file (portacle-path "config/help.txt")))
      (read-only-mode)
      (visual-line-mode)
      (emacs-lock-mode 'kill))))

(provide 'portacle-help)


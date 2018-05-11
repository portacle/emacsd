(provide 'portacle-help)

(with-current-buffer (get-buffer-create "*portacle-help*")
  (insert-file-contents (portacle-path "config/help.txt"))
  (read-only-mode)
  (visual-line-mode)
  (emacs-lock-mode 'kill))

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

(defun portacle--help-strings ()
  "Strings to be inserted in the portacle *scratch* buffer.
Some of these strings are interactive buttons"
  (let* ((website "https://github.com/Shinmera/portacle")
         (website-button
          (portacle--help-button
           website
           (lambda (&optional _event)
             (browse-url "https://github.com/Shinmera/portacle"))))
         (help-button
          (portacle--help-button "*portacle-help* buffer" #'portacle-help))
         (ide-text )
         (switch-ide-button
          (portacle--help-button
           (format "switch to %s"
                   (if (eq portacle-ide 'slime) "SLY" "SLIME"))
           (lambda (&optional _event)
             (interactive)
             (let ((target (if (eq portacle-ide 'slime) 'sly 'slime)))
               (funcall target 'sbcl)
               (customize-save-variable 'portacle-ide target)
               (portacle-scratch-help 'preserve)))))
         (configure-button
          (portacle--help-button "configuring a few"
                                 (lambda (&optional _event)
                                   (interactive)
                                   (call-interactively 'portacle-configure))))
         (dismiss-notes-button
          (portacle--help-button
           "click here"
           (lambda (&optional _event)
             (interactive)
             (let ((inhibit-read-only t))
               (delete-region (car portacle--help-region)
                              (cdr portacle--help-region)))))))
    (list
     "Welcome to Portacle, the Portable Common Lisp Environment.

For information on Portacle and how to use it, please read the
website at "
     website-button
     " or see the "
     help-button 
     " (to switch to that buffer, click the link or, later on,
press 'C-h h' , which is pressing 'Control-h', letting go, then
pressing 'h').\n\nWe've already started a Lisp process for you:
below this window there should be another with a ready-to-use
REPL (we used "
     (upcase (format "%s" portacle-ide))
     " to start Lisp, but if you want, you can "
     switch-ide-button
     " instead).\n\nIf you haven't done so, spend some seconds "
     configure-button
     " very basic Portacle variables.\n\nYou can use this buffer
for notes and tinkering with small pieces of code. Finally "
     dismiss-notes-button
     " to dismiss this introductory text.")))

(defgroup portacle nil "Customization group for Portacle.")

(defcustom portacle-ide #'slime
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

;; Workaround for font-lock.el's inability to use easily override
;; faces in lisp comments.
(defun portacle--help-find-scratch-buttons (limit)
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
       '((portacle--help-find-scratch-buttons . (0 'button prepend))))
      (save-excursion
        (let ((start (point-marker)))
          (apply #'insert (portacle--help-strings))
          (set-fill-column 60)
          (fill-region start (point))
          (comment-region start (point))
          (setq portacle--help-region
                (cons start
                      (point-marker)))
          (add-text-properties (car portacle--help-region)
                               (cdr portacle--help-region)
                               '(read-only t))
          (unless preserve-rest-of-buffer
            (insert "\n\n")
            (pp '(defun hello (name)
                   (princ "Howdy, ")
                   (princ (string-capitalize name)))
                (current-buffer))
            (pp '(hello "stranger") (current-buffer))))))))



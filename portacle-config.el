(provide 'portacle-config)

(defgroup portacle nil "Customization group for Portacle.")

(defcustom portacle-ide 'slime
  "If non-nil, Common Lisp IDE to run when Portacle launches.

Value is a symbol naming a function that should accept at least
one argument, COMMAND, which is either a pathname string pointing
to a Common Lisp executable, or a symbol designating one, like
`sbcl' or `ecl' that the function should interpret accordingly.

Currently, Portacle uses `sbcl' exclusively.

The symbols `slime' or `sly' are suitable candidates for this
variable."
  :type 'symbol
  :group 'portacle)

(defcustom portacle-setup-done-p nil
  "If NIL, causes the setup prompts to show in the scratch buffer."
  :type 'boolean
  :group 'portacle)

(defcustom portacle-project-licence "BSD-3"
  "The default licence used for new projects created with `create-project'."
  :type 'string
  :group 'portacle)

(cl-defun portacle-configure (&key name email licence)
  (interactive)
  (let ((name (or name (read-string "Your name: " user-full-name)))
        (email (or email (read-string "Your e-mail address: " user-mail-address)))
        (licence (or licence (read-string "Default project licence: " portacle-project-licence))))
    (call-process magit-git-executable nil nil t "config" "--file" (portacle-path "config/git/config") "user.name" name)
    (call-process magit-git-executable nil nil t "config" "--file" (portacle-path "config/git/config") "user.email" email)
    (customize-save-variable 'user-full-name name)
    (customize-save-variable 'user-mail-address email)
    (customize-save-variable 'portacle-project-licence licence)
    (customize-save-variable 'portacle-setup-done-p t)
    (message "User information set.")))

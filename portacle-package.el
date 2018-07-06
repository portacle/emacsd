(provide 'portacle-package)
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar *package-lists-fetched* nil)

(defun soft-fetch-package-lists ()
  (unless *package-lists-fetched*
    (package-refresh-contents)
    (setf *package-lists-fetched* t)))

;; package-installed-p will always report NIL if a newer
;; version is available. We do not want that.
(defun package-locally-installed-p (package)
  (assq package package-alist))

(defun ensure-installed (&rest packages)
  (unless (cl-loop for package in packages
                   always (package-locally-installed-p package))
    (soft-fetch-package-lists)
    (dolist (package packages)
      (unless (package-locally-installed-p package)
        (package-install package)))))

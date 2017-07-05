(provide 'portacle-spell)

;; Fix Spellchecker
(setq ispell-program-name (portacle-bin-path "hunspell"))

(setq used-spelling-dictionaries '("en_GB" "en_US"))
(setq ispell-dictionary (car used-spelling-dictionaries))

(add-hook 'slime-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(defun switch-dictionary ()
  (interactive)
  (let* ((old ispell-current-dictionary)
    	 (new (nth (mod (1+ (cl-position old used-spelling-dictionaries))
                        (length used-spelling-dictionaries))
                   used-spelling-dictionaries)))
    (ispell-change-dictionary new)
    (message "Dictionary switched from %s to %s" old new)))

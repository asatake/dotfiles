;;; early-init.el --- My early-init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; My early-init.el.

;;; Code:
(custom-set-variables '(warning-suppress-types '((comp))))
(setq debug-on-error t)

(let ((my-init-org (concat user-emacs-directory "init.org"))
      (my-init-el (concat user-emacs-directory "init.el"))
      (my-early-init-el (concat user-emacs-directory "early-init.el")))
  (when (or (file-newer-than-file-p my-init-org my-init-el)
            (file-newer-than-file-p my-init-org my-early-init-el))
    (message "WARN: init.el is old.\n")))

(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

(provide 'early-init)
;;; early-init.el ends here

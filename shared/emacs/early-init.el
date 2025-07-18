;;; early-init.el --- My early-init.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; My early-init.el.

;;; Code:
;; disable some expensive features before GUI starts up
(setq byte-compile-warnings '(not cl-functions obsolete))
(custom-set-variables '(warning-suppress-types '((comp))))
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(eval-when-compile (require 'cl-lib nil t))
(setenv "LIBRARY_PATH" "/opt/homebrew/opt/libgccjit/lib/gcc/13")


;; disable package
(setq package-enable-at-startup nil)

(setq use-file-dialog nil)
(setq inhibit-x-resources t)
(setq inhibit-startup-buffer-menu t)

(setq default-frame-alist
      '(
        (fullscreen . maximized)
        (font . "Cica 16")))

;; ignore x session resources for slight speed-up
(advice-add 'x-apply-session-resources :override 'ignore)

(custom-set-variables '(warning-suppress-types '((comp))))
(setq debug-on-error nil)
;;(setq comp-speed 2)
;; (byte-recompile-directory "~/.emacs.d" 0)

(let ((my-init-org (concat user-emacs-directory "init.org"))
      (my-init-el (concat user-emacs-directory "init.el"))
      (my-early-init-el (concat user-emacs-directory "early-init.el")))
  (when (or (file-newer-than-file-p my-init-org my-init-el)
            (file-newer-than-file-p my-init-org my-early-init-el))
    (message "WARN: init.el is old.\n")))

(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))
(provide 'early-init)

;;; early-init.el ends here

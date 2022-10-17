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

;; disable package
(setq package-enable-at-startup nil)

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

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    (leaf-keywords-init))
  )
;; </leaf-install-code>


;;; early-init.el ends here

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tetsuhiro Manome"
      user-mail-address "manome@curama.jp")

(setq gc-cons-threshold (* gc-cons-threshold 10))
(server-start)
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
(bind-key "C-h" #'delete-backward-char)
(define-key global-map [?\¥] [?\\])
(define-key global-map [?\C-¥] [?\C-\\])
(define-key global-map [?\M-¥] [?\M-\\])
(define-key global-map [?\C-\M-¥] [?\C-\M-\\])

(global-set-key (kbd "C-M-%") 'vr/query-replace)
;; (global-set-key (kbd "C-x p f") 'find-file-in-project)
;; (global-set-key (kbd "C-x f g") 'ripgrep-regexp)


;; emoji
(use-package emoji-cheat-sheet-plus
    :defer t
    :init
    (progn
      ;; enabled emoji in buffer
      (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
      ;; insert emoji with helm
      (global-set-key (kbd "C-c C-e") 'emoji-cheat-sheet-plus-insert)))


;; close-all-buffer command
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x a k") 'close-all-buffers)
(setq ispell-program-name "aspell")


;; window-maximized
  (defvar is-window-maximized nil)
  (defun window-temp-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized))))
  (global-set-key (kbd "<C-M-return>") 'window-temp-maximize)

;; swiper
(global-set-key (kbd "C-s") 'swiper)
(setq swiper-include-line-number-in-search t) ;; line-numberでも検索可能
;; migemo + swiper（日本語をローマ字検索できるようになる）
;; (require 'avy-migemo)
;; (avy-migemo-mode 1)
;; (require 'avy-migemo-e.g.swiper)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Cica" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'atom-one-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; disable evil
(after! evil-snipe (evil-snipe-mode -1))
(setq! evil-want-Y-yank-to-eol nil)
(setq evil-move-cursor-back nil)


;; lsp setting
(load! "~/.doom.d/lsp.el")

;; markdown setting
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))


;; python setting
(with-eval-after-load 'python
    ;; (define-key python-mode-map (kbd "C-c F") 'py-autopep8)
    ;; (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)
    (define-key python-mode-map (kbd "C-c Y") 'yapfify-buffer)
    (define-key python-mode-map (kbd "C-c y") 'yapfify-region)
    )


;;typescript setting
(use-package typescript-mode
  :hook #'lsp)
;; (add-hook typescript-mode-hook #'lsp)

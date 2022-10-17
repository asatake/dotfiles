;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Taketo Asai

;; Author: Taketo Asai <asataken@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el

;; USER SETTING
(leaf emacs
  :pre-setq `(
              (gc-cons-threshold . 104857600)
              (read-process-output-max . ,(* 1024 1024 4))
              )
  )
(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))
(defconst my:d:share (expand-file-name "share/" user-emacs-directory))
(defconst my:d:tmp (expand-file-name "tmp/" user-emacs-directory))

(setq default-frame-alist
      '(
        (fullscreen . maximized)
        (font . "Cica 16")))
(keyboard-translate ?\C-h ?\C-?)
(setq mac-command-modifier 'meta)
;; (setq ns-command-modifier (quote meta))

(leaf leaf-convert :ensure t)
(leaf leaf-tree
  :ensure t
  :custom ((imenu-list-size . 30)
           (imenu-list-position . 'left)))
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))
(leaf leaf
  :init
  (server-start)
  (global-git-gutter-mode +1)
  )

;; (leaf init-loader
;;   :doc "Loader for configuration files"
;;   :req "cl-lib-0.5"
;;   :url "https://github.com/emacs-jp/init-loader/"
;;   :added "2021-09-12"
;;   :ensure t
;;   :custom
;;   (init-loader-show-log-after-init . 'error-only)
;;   :config
;;   (setq custom-file "~/.emacs.d/tmp/custom.el")
;;   (init-loader-load "~/.emacs.d/inits")
;;   )

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf-keys (("C-h"       . backward-delete-char)
            ("C-c M-a"   . 'align-regexp)
            ("C-c ;"     . comment-region)
            ("C-c M-;"   . uncomment-region)
            ("C-/"       . undo)
            ("C-C M-R"   . replace-regexp)
            ("C-c r"     . replace-string)
            ("<home>"    . beginning-of-buffer)
            ("C-c M-l"   . toggle-truncate-lines)
            ("C-c v r"   . vr/query-replace)
            ("C-c v g"   . magit-status)
            ("C-x C-k k" . kill-matching-buffers)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :hook
  `((kill-emacs-hook . (lambda ()
                         (if (file-exists-p custom-file)
                             (delete-file custom-file)))))
  )

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((byte-compile-warnings . '(not cl-functions obsolute))
            (user-full-name . "Taketo Asai")
            (user-mail-address . "asataken@gmail.com")
            (user-login-name . "asatake")
            (create-lockfiles . nil)
            (debug-on-error . nil)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            (use-dialog-box . nil)
            (use-file-dialog . nil)
            (menu-bar-mode . t)
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-27"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :custom
  (exec-path-from-shell-arguments . "")
  (exec-path-from-shell-variables . '("PATH" "GOPATH"))
  :init
  (exec-path-from-shell-initialize))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.3)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf cl-lib
  :doc "Common Lisp extensions for Emacs"
  :tag "builtin"
  :added "2021-04-09")

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :added "2020-08-28"
  :url "http://www.dr-qubit.org/emacs.php"
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :custom
  (undo-tree-auto-save-history . nil)
  )

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf electric
  :doc "window maker and Command loop for `electric' modes"
  :tag "builtin"
  :added "2020-08-27"
  :init (electric-pair-mode 1))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :added "2020-09-03"
  :config
  (global-display-line-numbers-mode t))

(leaf popup
  :doc "Visual Popup User Interface"
  :req "cl-lib-0.5"
  :tag "lisp"
  :added "2020-08-27"
  :ensure t)

(leaf gcmh
  :doc "the Garbage Collector Magic Hack"
  :req "emacs-24"
  :tag "internal" "emacs>=24"
  :url "https://gitlab.com/koral/gcmh"
  :added "2022-01-08"
  :emacs>= 24
  :ensure t
  :custom
  (gcmh-verbose . nil)
  :config
  (gcmh-mode 1))

(leaf window-maximized
  :doc "window maximized"
  :config
  (defvar is-window-maximized nil)
  (defun window-temp-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized))))
  (global-set-key (kbd "<C-M-return>") 'window-temp-maximize))

;; ivy
;; (leaf ivy
;;   :doc "Incremental Vertical completYon"
;;   :req "emacs-24.5"
;;   :tag "matching" "emacs>=24.5"
;;   :url "https://github.com/abo-abo/swiper"
;;   :emacs>= 24.5
;;   :ensure t
;;   :blackout t
;;   :leaf-defer nil
;;   :custom ((ivy-initial-inputs-alist . nil)
;;            (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
;;                                       (swiper . ivy--regex-plus)))
;;            (ivy-use-selectable-prompt . t))
;;   :global-minor-mode t
;;   :config
;;   (leaf swiper
;;     :doc "Isearch with an overview. Oh, man!"
;;     :req "emacs-24.5" "ivy-0.13.0"
;;     :tag "matching" "emacs>=24.5"
;;     :url "https://github.com/abo-abo/swiper"
;;     :emacs>= 24.5
;;     :ensure t
;;     :bind (("C-s" . swiper)))

;;   (leaf counsel
;;     :doc "Various completion functions using Ivy"
;;     :req "emacs-24.5" "swiper-0.13.0"
;;     :tag "tools" "matching" "convenience" "emacs>=24.5"
;;     :url "https://github.com/abo-abo/swiper"
;;     :emacs>= 24.5
;;     :ensure t
;;     :blackout t
;;     :bind (("C-S-s" . counsel-imenu)
;;            ("C-x C-r" . counsel-recentf))
;;     :custom `((counsel-yank-pop-separator . "\n----------\n")
;;               (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
;;     :global-minor-mode t))

(leaf recentf
  :doc "setup a menu of recently opened files"
  :tag "builtin"
  :added "2021-06-13"
  )

(leaf swiper
  :doc "Isearch with an overview. Oh, man!"
  :req "emacs-24.5" "ivy-0.13.4"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2022-09-24"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper-backward)
  )

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :added "2021-06-13"
  :url "https://github.com/minad/vertico"
  :emacs>= 27.1
  :ensure t
  :custom
  ;; 補完スタイルにorderlessを利用する
  (completion-styles . '(orderless))
  (vertico-count . 20)
  :config
  ;; vertico-modeとmarginalia-modeを有効化する
  (defun after-init-hook ()
    (recentf-mode)
    (vertico-mode)
    (marginalia-mode)
    ;; savehist-modeを使ってVerticoの順番を永続化する
    (savehist-mode))
  (add-hook 'after-init-hook 'after-init-hook)
  (with-eval-after-load 'consult
    (with-eval-after-load 'embark
      (require 'embark-consult)))
  )
(leaf counsel
  :doc "Various completion functions using Ivy"
  :req "emacs-24.5" "swiper-0.13.0"
  :tag "tools" "matching" "convenience" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :bind (
         ("C-x C-r" . counsel-recentf)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x)
         )
  :setq-default
  (counsel-yank-pop-separator . "\n----------\n")
  (counsel-find-file-ignore-regexp . (regexp-opt '("./" "../")))
  :global-minor-mode t)
(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/minad/consult"
  :emacs>= 26.1
  :ensure t
  :after vertico
  :bind (("M-g M-g" . 'consult-goto-line))
  )
(leaf consult-ghq
  :doc "Ghq interface using consult"
  :req "emacs-26.1" "consult-0.8" "affe-0.1"
  :tag "ghq" "consult" "usability" "convenience" "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/tomoya/consult-ghq"
  :emacs>= 26.1
  :ensure t
  :after consult affe)
(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/minad/marginalia"
  :emacs>= 26.1
  :ensure t
  :after vertico)

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-26.1"
  :tag "convenience" "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/oantolin/embark"
  :emacs>= 26.1
  :ensure t
  :after embark
  :init
  (leaf embark-consult
    :doc "Consult integration for Embark"
    :req "emacs-25.1" "embark-0.9" "consult-0.1"
    :tag "convenience" "emacs>=25.1"
    :url "https://github.com/oantolin/embark"
    :added "2021-09-16"
    :emacs>= 25.1
    :ensure t
    :after embark consult)
  )

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3" "memoize-1.0.1"
  :tag "lisp" "convenient" "emacs>=24.3"
  :added "2020-08-27"
  :url "https://github.com/domtronn/all-the-icons.el"
  :emacs>= 24.3
  :ensure t
  :after memoize)

(leaf subword
  :doc "Handling capitalized subwords in a nomenclature"
  :tag "builtin"
  :added "2022-03-22"
  :init
  (global-subword-mode 1)
  )

(leaf indent-guide
  :ensure t
  :custom
  (indent-guide-delay . 0.1)
  :init
  (indent-guide-global-mode))

(leaf highlight-symbol
  :ensure t
  :url "https://github.com/nschum/highlight-symbol.el"
  :init
  (global-set-key [(control f4)] 'highlight-symbol)
  (global-set-key [f4] 'highlight-symbol-next)
  (global-set-key [(shift f4)] 'highlight-symbol-prev)
  )

(leaf smartparens
  :ensure t
  :url "https://github.com/Fuco1/smartparens"
  :init
  (require 'smartparens-config))

(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
  :added "2020-08-27"
  :url "https://github.com/hlissner/emacs-doom-theme"
  :emacs>= 25.1
  :ensure t
  :init
  (load-theme 'doom-palenight t)
  :custom
  (doom-themes-enable-italic . t)
  (doom-themes-enable-bold   . t)
  :config
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (set-face-foreground 'default "grey82")
  )

;; (leaf moe-theme
;;   :doc "A colorful eye-candy theme. Moe, moe, kyun!"
;;   :tag "themes"
;;   :url "https://github.com/kuanyui/moe-theme.el"
;;   :added "2022-09-19"
;;   :ensure t)

;; (leaf color-theme-sanityinc-tomorrow
;;   :doc "A version of Chris Kempson's \"tomorrow\" themes"
;;   :tag "themes" "faces"
;;   :url "https://github.com/purcell/color-theme-sanityinc-tomorrow"
;;   :added "2022-09-19"
;;   :ensure t
;;   )

;; (leaf modus-themes
;;   :doc "Elegant, highly legible and customizable themes"
;;   :req "emacs-27.1"
;;   :tag "accessibility" "theme" "faces" "emacs>=27.1"
;;   :url "https://git.sr.ht/~protesilaos/modus-themes"
;;   :added "2022-09-19"
;;   :emacs>= 27.1
;;   :ensure t
;;   :custom
;;   (modus-themes-italic-constructs   . t)
;;   (modus-themes-bold-constructs     . nil)
;;   (modus-themes-region              . '(bg-only no-extend))
;;   (modus-themes-no-mixed-fonts      . t)
;;   (modus-themes-subtle-line-numbers . t)
;;   :init
;;   (modus-themes-load-themes)
;;   (modus-themes-load-vivendi)
;;   )

(leaf shrink-path)

(leaf ido
  :doc "interactively do things with buffers and files"
  :tag "builtin"
  :added "2022-09-16"
  :config (ido-mode t))

(leaf doom-modeline
  :doc "A minimal and modern mode-line"
  :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
  :tag "mode-line" "faces" "emacs>=25.1"
  :added "2020-08-27"
  :url "https://github.com/seagle0128/doom-modeline"
  :emacs>= 25.1
  :ensure t
  ;; :after all-the-icons shrink-path
  :hook
  (window-setup-hook . doom-modeline-mode)
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height                 . 25)
  (doom-modeline-window-width-limit     . fill-column)
  (doom-modeline-project-detection      . 'project)
  (doom-modeline-buffer-file-name-style . 'auto)
  (doom-modeline-buffer-modified        . 'unspecified)
  (doom-modeline-icon                   . (display-graphic-p))
  (doom-modeline-major-mode-icon        . t)
  (doom-modeline-major-mode-color-icon  . t)
  (doom-modeline-buffer-state-icon      . t)
  (doom-modeline-buffer-state-icon      . t)
  (doom-modeline-enable-word-count      . t)
  (doom-modeline-lsp                    . t)
  (doom-modeline-env-version            . t)
  (doom-modeline-vcs-max-length         . 12)
  (doom-modeline-workspace-name         . t))

(leaf hl-todo
  :doc "highlight TODO and similar keywords"
  :req "emacs-25"
  :tag "convenience" "emacs>=25"
  :added "2020-11-24"
  :url "https://github.com/tarsius/hl-todo"
  :emacs>= 25
  :ensure t
  :global-minor-mode t
  :custom
  (hl-todo-keyword-faces . '(("TODO"    . "#FF4500")
                             ("FIXME"   . "#DDAE13")
                             ("DEBUG"   . "#1E90FF")
                             ("NOTE"    . "#1EAE45")
                             ("CAUTION" . "#DDAE13")))
  :init
  (global-hl-todo-mode t)
  )

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :tag "faces"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :added "2022-05-20"
  :ensure t
  :hook
  (css-mode-hook . rainbow-mode)
  (web-mode-hook . rainbow-mode)
  (typescript-mode-hook . rainbow-mode)
  )

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :commands (prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
            (prescient-save-file . ,(locate-user-emacs-file "prescient")))
  :global-minor-mode prescient-persist-mode)

(leaf pangu-spacing
  :doc "Minor-mode to add space between Chinese and English characters."
  :added "2020-09-23"
  :url "http://github.com/coldnew/pangu-spacing"
  :ensure t
  :custom
  (pangu-spacing-real-insert-separtor . t)
  :hook
  ;; (text-mode-hook . pangu-spacing-mode)
  (org-mode-hook . pangu-spacing-mode)
  )

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2020-10-26"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode t)

(leaf neotree
  :doc "A tree plugin like NerdTree for Vim"
  :req "cl-lib-0.5"
  :url "https://github.com/jaypei/emacs-neotree"
  :added "2022-09-20"
  :ensure t
  :custom
  (neo-show-hidden-files . t)
  (neo-theme 'icons)
  )

(leaf treemacs-projectile
  :doc "Projectile integration for treemacs"
  :req "emacs-26.1" "projectile-0.14.0" "treemacs-0.0"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2022-09-20"
  :emacs>= 26.1
  :ensure t
  :after projectile treemacs)

(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :added "2020-08-28"
  :custom
  (add-to-list 'tramp-remote-path . 'tramp-own-remote-path)
  (tramp-default-method . "sshx")
  )

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :hook
  (prog-mode-hook . flycheck-mode)
  (go-mode-hook . flycheck-mode)
  :custom
  (flycheck-display-errors-delay . 0.3)
  :config
  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :req "emacs-24.1" "flycheck-0.22" "package-lint-0.2"
    :tag "lisp" "emacs>=24.1"
    :added "2020-09-03"
    :url "https://github.com/purcell/flycheck-package"
    :emacs>= 24.1
    :ensure t
    :after flycheck package-lint
    :config
    (eval-after-load 'flycheck
      '(flycheck-package-setup))
    )
  (leaf flycheck-color-mode-line
    :doc "Change mode line color with Flycheck status"
    :req "flycheck-0.15" "dash-1.2" "emacs-24.3"
    :tag "tools" "language" "convenience" "emacs>=24.3"
    :url "https://github.com/flycheck/flycheck-color-mode-line"
    :added "2022-02-16"
    :emacs>= 24.3
    :ensure t
    :after flycheck
    :hook
    (flycheck-mode-hook . flycheck-color-mode-line-mode))
  (leaf flycheck-pos-tip
    :doc "Display Flycheck errors in GUI tooltips"
    :req "emacs-24.1" "flycheck-0.22" "pos-tip-0.4.6"
    :tag "convenience" "tools" "emacs>=24.1"
    :url "https://github.com/flycheck/flycheck-pos-tip"
    :added "2022-02-16"
    :emacs>= 24.1
    :ensure t
    :after flycheck pos-tip
    :config
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))
    )
  )

(leaf flyspell
  :doc "On-the-fly spell checker"
  :tag "builtin"
  :added "2021-01-04"
  :hook
  (text-mode-hook . flyspell-mode)
  (org-mode-hook  . flyspell-mode)
  (go-mode-hook   . flyspell-prog-mode)
  :init
  (leaf flyspell-popup
    :doc "Correcting words with Flyspell in popup menus"
    :req "popup-0.5.0"
    :tag "convenience"
    :added "2021-01-04"
    :url "https://github.com/xuchunyang/flyspell-popup"
    :ensure t
    :after flyspell
    :hook (flyspell-mode-hook . flyspell-popup-auto-correct-mode))
  )

(leaf editorconfig
  :doc "EditorConfig Emacs Plugin"
  :req "cl-lib-0.5" "emacs-24"
  :tag "emacs>=24"
  :added "2020-09-09"
  :url "https://github.com/editorconfig/editorconfig-emacs#readme"
  :emacs>= 24
  :ensure t
  :config
  (editorconfig-mode 1)
  ;; :hook
  ;; ('editorconfig-hack-properties-functions . '(lambda (props)
  ;;                                               (when (derived-mode-p 'makefile-mode)
  ;;                                                 (puthash 'indent_style "tab" props))))
  )

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :leaf-defer nil
  ;; :bind ((company-active-map
  ;;         ("M-n" . nil)
  ;;         ("M-p" . nil)
  ;;         ("C-s" . company-filter-candidates)
  ;;         ("C-n" . company-select-next)
  ;;         ("C-p" . company-select-previous)
  ;;         ("<tab>" . company-complete-selection))
  ;;        (company-search-map
  ;;         ("C-n" . company-select-next)
  ;;         ("C-p" . company-select-previous)))
  ;; :custom ((company-idle-delay . 0.2)
  ;;          (company-minimum-prefix-length . 1)
  ;;          ;; (company-transformers . '(company-sort-by-occurrence))
  ;;          )
  ;; :global-minor-mode global-company-mode
  )

;; Corfu + Cape
(leaf corfu
  :doc "Completion Overlay Region FUnction"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/minad/corfu"
  :added "2022-03-27"
  :emacs>= 27.1
  :ensure t
  :custom
  (tab-always-indent . 'complete)
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-auto-prefix . 1)
  (corfu-preselect-first . nil)
  :global-minor-mode global-corfu-mode
  :init
  (global-corfu-mode)
  (defun my-corfu-for-text-mode ()
    (setq-local corfu-auto t
                corfu-auto-prefix 1))
  
  (defun my/corfu-remap-tab-command ()
    (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command))

  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  :custom
  (corfu-quit-no-match . 'separator)
  :hook
  (text-mode-hook . my-corfu-for-text-mode)
  )

(leaf cape
  :doc "Completion At Point Extensions"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :url "https://github.com/minad/cape"
  :added "2022-03-27"
  :emacs>= 27.1
  :ensure t
  :after corfu
  :defun (my-convert-capf)
  :hook
  (prog-mode-hook . my/set-basic-capf)
  (text-mode-hook . my/set-basic-capf)
  (lsp-completion-mode-hook . my/set-lsp-capf)

  :init
  (defun my-convert-capf (arg-capf)
                   (list (cape-capf-case-fold arg-capf)
                         (cape-company-to-capf #'company-yasnippet)
                         #'cape-file
                         #'cape-tex
                         (cape-capf-case-fold #'cape-dabbrev)
                         #'cape-keyword))
  (defun my/set-lsp-capf ()
                   (setq-local completion-at-point-functions (my-convert-capf #'lsp-completion-at-point)))
  (defun my/set-basic-capf ()
                     (setq-local completion-at-point-functions (my-convert-capf (car completion-at-point-functions))))

  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  )

(leaf corfu-doc
  :doc "Documentation popup for Corfu"
  :req "emacs-26.0" "corfu-0.16.0"
  :tag "convenience" "documentation" "popup" "corfu" "emacs>=26.0"
  :url "https://github.com/galeo/corfu-doc"
  :added "2022-03-27"
  :emacs>= 26.0
  :ensure t
  :after corfu
  :hook
  (corfu-mode-hook . corfu-doc-mode))

(leaf orderless
    :doc "Completion style for matching regexps in any order"
    :req "emacs-26.1"
    :tag "extensions" "emacs>=26.1"
    :added "2021-06-13"
    :url "https://github.com/oantolin/orderless"
    :emacs>= 26.1
    :ensure t
    :pre-setq ((completion-styles . '(orderless))
               (completion-category-defaults . nil)
               (completion-category-overrides . nil))
    :hook ((corfu-mode-hook . my/orderless-for-corfu)
           (lsp-completion-mode-hook . my/orderless-for-lsp-mode))
    :init
    (defun my/orderless-dispatch-flex-first (_pattern index _total)
      (and (eq index 0) 'orderless-flex))

    (defun my/orderless-for-corfu ()
      (setq-local orderless-style-dispatchers '(my/orderless-dispatch-flex-first)))

    (defun my/orderless-for-lsp-mode ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	    '(orderless)))
    )

(leaf svg-lib
  :doc "SVG tags, progress bars & icons"
  :req "emacs-27.1"
  :tag "convenience" "tags" "icons" "svg" "emacs>=27.1"
  :url "https://github.com/rougier/svg-lib"
  :added "2022-03-27"
  :emacs>= 27.1
  :ensure t)

(leaf kind-icon
  :doc "Completion kind icons"
  :req "emacs-27.1" "svg-lib-0"
  :tag "completion" "emacs>=27.1"
  :url "https://github.com/jdtsmith/kind-icon"
  :added "2022-03-27"
  :emacs>= 27.1
  :ensure t
  :after svg-lib corfu
  :ensure t
  :defvar (corfu-margin-formatters kind-icon-default-face)
  :defun (kind-icon-margin-formatter)
  :pre-setq
  (kind-icon-default-face . 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (advice-add 'corfu--post-command :around
	      (lambda (func)
	        (condition-case err
		    (funcall func)
		  ((debug error) (signal (car err) (cdr err))))))
  )


(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :req "emacs-26.3"
  :tag "extensions" "emacs>=26.3"
  :added "2020-09-09"
  :url "http://elpa.gnu.org/packages/eldoc.html"
  :emacs>= 26.3
  :ensure t)

(leaf ripgrep
  :doc "Front-end for ripgrep, a command line search tool"
  :tag "search" "grep" "sift" "ag" "pt" "ack" "ripgrep"
  :added "2020-09-09"
  :url "https://github.com/nlamirault/ripgrep.el"
  :ensure t)

(leaf dumb-jump
  :doc "Jump to definition for 40+ languages without configuration"
  :req "emacs-24.3" "s-1.11.0" "dash-2.9.0" "popup-0.5.3"
  :tag "programming" "emacs>=24.3"
  :added "2020-09-09"
  :url "https://github.com/jacktasia/dumb-jump"
  :emacs>= 24.3
  :ensure t
  :custom ((dumb-jump-selector . 'ivy)))

(leaf smart-jump
  :doc "Smart go to definition."
  :req "emacs-25.1" "dumb-jump-0.5.1"
  :tag "tools" "emacs>=25.1"
  :added "2020-09-09"
  :url "https://github.com/jojojames/smart-jump"
  :emacs>= 25.1
  :ensure t
  :after dumb-jump
  :config (smart-jump-setup-default-registers))

(leaf dap-mode
  :doc "Debug Adapter Protocol mode"
  :req "emacs-26.1" "dash-2.14.1" "lsp-mode-6.0" "dash-functional-1.2.0" "bui-1.1.0" "f-0.20.0" "s-1.12.0" "lsp-treemacs-0.1" "posframe-0.7.0"
  :tag "debug" "languages" "emacs>=26.1"
  :added "2020-10-30"
  :url "https://github.com/yyoncho/dap-mode"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode bui lsp-treemacs posframe
  :config
  (require 'dap-go)
  (require 'dap-hydra)
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  ;; (dap-auto-configure-mode 1)
  :custom
  ;; (dap-auto-configure-features . '(sessions locals breakpoints expressions repl controls tooltip))
  ;; (dap-go-debug-path . "/Users/tetsuhiromanome/ghq/github.com/golang/vscode-go/")
  ;; (dap-go-debug-program . `("node", (f-join dap-go-debug-path "dist/debugAdapter.js")))
  )

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2020-10-30"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t
  ;; :custom
  ;; (yas-snippet-dirs . "~/.emacs.d/yasnippets")
  :global-minor-mode yas-global-mode
  :hook
  (after-init-hook . yas-global-mode)
  :init
  (leaf yasnippet-snippets :ensure t)
  (leaf yatemplate :ensure t))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2020-08-27"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :custom
  (lsp-log-io                         . nil)
  (lsp-use-plists                     . t)
  (lsp-keymap-prefix                  . "C-c l")
  (lsp-inhibit-message                . t)
  (lsp-message-project-root-warning   . t)
  (create-lockfiles                   . nil)
  (lsp-signature-auto-activate        . t)
  (lsp-signature-doc-lines            . 1)
  (lsp-print-performance              . t)
  (lsp-eldoc-render-all               . t)
  (lsp-enable-completion-at-point     . t)
  (lsp-enable-xref                    . t)
  (lsp-keep-workspace-alive           . nil)
  (lsp-enable-snippet                 . t)
  (lsp-server-trace                   . nil)
  (lsp-auto-guess-root                . nil)
  ;; (lsp-document-sync-method           . 'lsp--sync-incremental)
  (lsp-document-sync-method           . 2)
  (lsp-diagnostics-provider           . :flycheck)
  (lsp-response-timeout               . 5)
  (lsp-idle-delay                     . 0.500)
  (lsp-enable-file-watchers           . nil)
  (lsp-completion-provider            . :none)
  (lsp-headerline-breadcrumb-segments . nil)
  (lsp-lens-enable                    . nil)

  ;; for vue-mode
  (lsp-vetur-format-default-formatter-css . "none")
  (lsp-vetur-format-default-formatter-html . "none")
  (lsp-vetur-format-default-formatter-js . "none")
  (lsp-vetur-validation-template . nil)
  :commands
  (lsp lsp-deferred)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  (lsp-mode-hook . lsp-ui-mode)
  (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2020-08-27"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode markdown-mode
    :custom
    (lsp-ui-sideline-enable       . t)
    (lsp-ui-doc-enable            . t)
    (lsp-ui-doc-alignment         . 'frame)
    (lsp-ui-doc-position          . 'top)
    (lsp-ui-doc-deley             . 0.5)
    (lsp-ui-doc-header            . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-max-width         . 150)
    (lsp-ui-doc-max-height        . 30)
    (lsp-ui-doc-use-childframe    . t)
    (lsp-ui-doc-use-webkit        . nil)
    (lsp-ui-doc-show-with-cursor  . t)
    (lsp-ui-doc-show-with-mouse  . t)
    (lsp-ui-flycheck-enable       . t)
    (lsp-ui-peek-enable           . t)
    (lsp-ui-peek-peek-height      . 20)
    (lsp-ui-peek-list-width       . 50)
    (lsp-ui-peek-fontify          . 'on-demand) ;; never, on-demand, or always
    :hook ((lsp-mode-hook . lsp-ui-mode)))
  (leaf lsp-treemacs
    :doc "LSP treemacs"
    :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
    :tag "languages" "emacs>=26.1"
    :added "2020-10-22"
    :url "https://github.com/emacs-lsp/lsp-treemacs"
    :emacs>= 26.1
    :ensure t
    :after treemacs lsp-mode)
  )

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-27"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor
  ;; :init
  ;; (leaf transient
  ;;   :custom
  ;;   `((transient-history-file . ,(expand-file-name "transient-history.el" "tmp/"))
  ;;     (transient-levels-file . ,(expand-file-name "transient-levels.el" "tmp/"))
  ;;     (transient-values-file . ,(expand-file-name "transient-values.el" "tmp/")))
  ;;   (transient-force-fixed-pitch . t))
  :custom
  (magit-completing-read-function . 'ido-completing-read)
  (magit-refs-show-commit-count   . 'branch)
  (magit-log-buffer-file-locked   . t)
  (magit-revision-show-gravatars  . nil)
  )

(leaf fringe-helper
  :doc "helper functions for fringe bitmaps"
  :tag "lisp"
  :added "2020-09-03"
  :url "http://nschum.de/src/emacs/fringe-helper/"
  :ensure t)

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :added "2020-09-03"
  :url "https://github.com/emacsorphanage/git-gutter"
  :emacs>= 24.3
  :after display-line-numbers-mode
  :ensure t
  :custom ((left-fringe-width . 20)
           (git-gutter:window-width . 2))
  :config
  (leaf git-gutter-fringe
    :doc "Fringe version of git-gutter.el"
    :req "git-gutter-0.88" "fringe-helper-0.1.1" "cl-lib-0.5" "emacs-24"
    :tag "emacs>=24"
    :added "2020-09-03"
    :url "https://github.com/emacsorphanage/git-gutter-fringe"
    :emacs>= 24
    :ensure t
    :after git-gutter fringe-helper
    )
  )

(leaf visual-regexp
  :doc "A regexp/replace command for Emacs with interactive visual feedback"
  :req "cl-lib-0.2"
  :tag "feedback" "visual" "replace" "regexp"
  :added "2020-08-27"
  :url "https://github.com/benma/visual-regexp.el/"
  :ensure t)

(leaf ispell
  :doc "interface to spell checkers"
  :tag "builtin"
  :added "2020-08-27"
  :custom
  (ispell-program-name . "aspell")
  (ispell-extra-args   . '("--sug-mode=ultra"))
  :config
  ;; ;; sub-word mode との併用
  ;; (eval-when-compile
  ;;   ;; Like looking-at but with an offset
  ;;   (defun my-looking-at (regexp &optional offset)
  ;;     (let ((pos (+ (or offset 0) (point))))
  ;;       (when (and (>= pos (point-min)) (< pos (point-max)))
  ;;         (string-match regexp (string (char-after pos))))))

  ;;   ;; Call the right forward function and move past otherchars
  ;;   (defun my-forward (&optional backward)
  ;;     (let ((ispell-casechars (ispell-get-casechars))
  ;;           (ispell-otherchars (ispell-get-otherchars))
  ;;           (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
  ;;           (offset (if backward -1 0))
  ;;           (dir (if backward -1 1))
  ;;           (continue t))
  ;;       (if subword-mode (subword-forward dir) (forward-word dir))
  ;;       (while (and continue
  ;;                   (not (string= "" ispell-otherchars))
  ;;                   (my-looking-at ispell-otherchars offset)
  ;;                   (my-looking-at ispell-casechars (+ dir offset)))
  ;;         (if subword-mode (subword-forward dir) (forward-word dir))
  ;;         (setq continue ispell-many-otherchars-p))))
  ;;   (defun my-backward () (my-forward t))

  ;;   ;; Properly find boundaries of words in CamelCase
  ;;   (defun my-ispell-get-word (orig-fun &optional following extra-otherchars)
  ;;     (if (not subword-mode)
  ;;         (funcall orig-fun following extra-otherchars)
  ;;       (if following (my-forward) (if (not (eobp)) (forward-char)))
  ;;       (let* ((beg (progn (my-backward) (point-marker)))
  ;;              (end (progn (my-forward) (point-marker)))
  ;;              (word (buffer-substring-no-properties beg end)))
  ;;         (list word beg end))))
  ;;   (advice-add #'ispell-get-word :around #'my-ispell-get-word)
  ;;   (advice-add #'flyspell-get-word :around #'my-ispell-get-word)

  ;;   ;; Simplify and use my-forward to handle CamelCase words
  ;;   (defun my-flyspell-small-region (beg end)
  ;;     ;; (if (not subword-mode)
  ;;     ;;     (funcall orig-fun beg end)
  ;;     (save-excursion
  ;;       (if (> beg end) (setq beg (prog1 end (setq end beg))))
  ;;       (if (< beg (point-min)) (setq beg (point-min)))
  ;;       (if (> end (point-max)) (setq end (point-max)))
  ;;       (goto-char beg)
  ;;       (while (< (point) end)
  ;;         (flyspell-word t)
  ;;         ;; (sit-for 0) ;; uncomment to enable animation
  ;;         (my-forward))))
  ;;   (advice-add #'flyspell-small-region :around #'my-flyspell-small-region)
    
  ;;   )

  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (setq-default ispell-extra-args '("-C"
                                    "--sug-mode=ultra"
                                    "--lang=en_US"
                                    "--camel-case"))
  )

;; (leaf emojify
;;   :doc "Display emojis in Emacs"
;;   :req "seq-1.11" "ht-2.0" "emacs-24.3"
;;   :tag "convenience" "multimedia" "emacs>=24.3"
;;   :url "https://github.com/iqbalansari/emacs-emojify"
;;   :added "2022-09-24"
;;   :emacs>= 24.3
;;   :ensure t
;;   :hook
;;   (after-init-hook . global-emojify-mode)
;;   (after-init-hook . global-emojify-mode-line-mode)
;;   :bind ("C-c m e" . emojify-insert-emoji)
;;   )

(leaf emoji-cheat-sheet-plus
  :doc "emoji-cheat-sheet for emacs"
  :req "emacs-24" "helm-1.6.4"
  :tag "emoji" "emacs" "emacs>=24"
  :added "2020-08-27"
  :url "https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus"
  :emacs>= 24
  :ensure t
  :bind ("C-c m e" . 'emoji-cheat-sheet-plus-insert)
  )

(leaf hideshow
  :doc "minor mode cmds to selectively display code/comment blocks"
  :tag "builtin"
  :added "2020-11-27"
  :hook
  ((typescript-mode-hook . (lambda ()
                             (hs-minor-mode 1))))
  )

(leaf org
  :doc "Export Framework for Org Mode"
  :tag "builtin"
  :added "2020-08-28"
  :custom
  ((org-startup-indented . t)
   (org-structure-template-alist . '(("a" . "export ascii\n")
                                     ("c" . "center\n")
                                     ("C" . "comment\n")
                                     ("e" . "example\n")
                                     ("E" . "export")
                                     ("h" . "export html\n")
                                     ("l" . "export latex\n")
                                     ("q" . "quote\n")
                                     ("s" . "src\n")
                                     ("v" . "verse\n")))
   )
  :custom
  '((org-modules . (org-modules org-tempo)))
  :hook
  (org-mode-hook . hl-todo-mode)
  )

;; Languages
(leaf go-mode
  :doc "Major mode for the Go programming language"
  :tag "go" "languages"
  :added "2020-08-27"
  :url "https://github.com/dominikh/go-mode.el"
  :ensure t
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . smartparens-mode)
  )
(leaf go-dlv
  :doc "Go Delve - Debug Go programs interactively with the GUD."
  :req "go-mode-1.3.1"
  :tag "gud" "interactive" "delve" "debugger" "debug" "go"
  :url "https://github.com/benma/go-dlv.el/"
  :added "2021-12-08"
  :ensure t
  :after go-mode
  )
(leaf flycheck-golangci-lint
  :doc "Flycheck checker for golangci-lint"
  :req "emacs-24" "flycheck-0.22"
  :tag "go" "tools" "convenience" "emacs>=24"
  :url "https://github.com/weijiangan/flycheck-golangci-lint"
  :added "2022-02-17"
  :emacs>= 24
  :ensure t
  :after flycheck
  :hook
  (go-mode . 'flycheck-golangci-lint-setup)
  )

(leaf python-mode
  :doc "Python major mode"
  :added "2020-08-27"
  :ensure t
  :bind
  ("C-c Y" . yapfify-buffer)
  ("C-c y" . yapfify-region)
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred)))
  :init
  ;; (leaf elpy
  ;;   :doc "Emacs Python Development Environment"
  ;;   :req "company-0.9.2" "emacs-24.4" "highlight-indentation-0.5.0" "pyvenv-1.3" "yasnippet-0.8.0" "s-1.11.0"
  ;;   :tag "emacs>=24.4"
  ;;   :added "2020-09-11"
  ;;   :emacs>= 24.4
  ;;   :ensure t
  ;;   :after company highlight-indentation pyvenv yasnippet)
  (leaf lsp-pyright
    :doc "Python LSP client using Pyright"
    :req "emacs-26.1" "lsp-mode-7.0" "dash-2.14.1" "ht-2.0"
    :tag "lsp" "tools" "languages" "emacs>=26.1"
    :added "2020-09-09"
    :url "https://github.com/emacs-lsp/lsp-pyright"
    :emacs>= 26.1
    :ensure t
    :after python-mode lsp-mode
    :setq-default
    (flycheck-disabled-checkers . '(python-mypy))
    )
  )

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :added "2021-09-17"
  :emacs>= 25.1
  :ensure t
  :hook
  (rust-mode-hook . lsp-deferred)
  )

(leaf rjsx-mode
  :doc "Real support for JSX"
  :req "emacs-24.4" "js2-mode-20170504"
  :tag "languages" "emacs>=24.4"
  :url "https://github.com/felipeochoa/rjsx-mode/"
  :added "2022-01-04"
  :emacs>= 24.4
  :ensure t
  :after js2-mode)

(leaf prettier-js
  :doc "Minor mode to format JS code on file save"
  :tag "js" "edit" "wp" "convenience"
  :url "https://github.com/prettier/prettier-emacs"
  :added "2022-01-22"
  :ensure t)

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2020-08-27"
  :url "http://web-mode.org"
  :emacs>= 23.1
  :ensure t
  :defvar (web-mode-indentation-params)
  :config
  (pangu-spacing-mode nil)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :custom
  (web-mode-css-indent-offset . 2)
  (web-mode-markup-indent-offset . 2)
  (web-mode-code-indent-offset . 2)
  (web-mode-enable-current-element-highlight . t)
  (web-mode-attr-indent-offset . 2)
  (lsp-eslint-enable . t)
  (lsp-eslint-auto-fix-on-save . t)
  :hook
  (web-mode-hook . lsp-deferred)
  (web-mode-hook . smartparens-mode)
  (web-mode-hook . prettier-js-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  )

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :added "2020-08-27"
  :url "http://github.com/ananthakumaran/typescript.el"
  :emacs>= 24.3
  :ensure t
  :hook
  (typescript-mode-hook . tide-mode)
  (typescript-mode-hook . smartparens-mode)
  )

;; (leaf haskell-mode
;;   :doc "A Haskell editing mode"
;;   :req "emacs-25.1"
;;   :tag "haskell" "files" "faces" "emacs>=25.1"
;;   :added "2021-04-08"
;;   :url "https://github.com/haskell/haskell-mode"
;;   :emacs>= 25.1
;;   :ensure t
;;   :init
;;   ;; (leaf company-ghci
;;   ;;   :doc "company backend which uses the current ghci process."
;;   ;;   :req "company-0.8.11" "haskell-mode-13"
;;   ;;   :added "2021-04-08"
;;   ;;   :ensure t
;;   ;;   :after company haskell-mode)
;;   (leaf lsp-haskell
;;     :doc "Haskell support for lsp-mode"
;;     :req "emacs-24.3" "lsp-mode-3.0" "haskell-mode-1.0"
;;     :tag "haskell" "emacs>=24.3"
;;     :added "2021-04-08"
;;     :url "https://github.com/emacs-lsp/lsp-haskell"
;;     :emacs>= 24.3
;;     :ensure t
;;     :after lsp-mode haskell-mode
;;     :custom
;;     (lsp-haskell-process-path-hie . "haskell-language-server-wrapper")
;;     )
;;   )

(leaf vue-mode
  :doc "Major mode for vue component based on mmm-mode"
  :req "mmm-mode-0.5.5" "vue-html-mode-0.2" "ssass-mode-0.2" "edit-indirect-0.1.4"
  :tag "languages"
  :added "2021-05-26"
  :ensure t
  :after mmm-mode vue-html-mode ssass-mode edit-indirect
  :custom
  (lsp-eslint-enable . t)
  (lsp-eslint-auto-fix-on-save . t)
  :hook
  (vue-mode-hook . smartparens-mode)
  (vue-mode-hook . lsp-deferred)
  :config
  (add-hook 'vue-mode-hook (lambda() (setq tab-width 2)))
  )

(leaf json-mode
  :doc "Major mode for editing JSON files."
  :req "json-reformat-0.0.5" "json-snatcher-1.0.0"
  :added "2020-08-27"
  :url "https://github.com/joshwnj/json-mode"
  :ensure t
  :after json-reformat json-snatcher)

(leaf csv
  :doc "Functions for reading and parsing CSV files."
  :tag "csv" "data" "extensions"
  :added "2020-08-31"
  :ensure t)

(leaf nginx-mode
  :doc "major mode for editing nginx config files"
  :tag "nginx" "languages"
  :added "2020-08-27"
  :ensure t)

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2020-08-27"
  :emacs>= 24.1
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq indent-tabs-mode nil)
              )))

;; (leaf markdown-preview-mode
;;   :doc "markdown realtime preview minor mode."
;;   :req "emacs-24.4" "websocket-1.6" "markdown-mode-2.0" "cl-lib-0.5" "web-server-0.1.1"
;;   :tag "convenience" "gfm" "markdown" "emacs>=24.4"
;;   :url "https://github.com/ancane/markdown-preview-mode"
;;   :added "2022-02-14"
;;   :emacs>= 24.4
;;   :ensure t
;;   :after websocket markdown-mode web-server
;;   :config
;;   ;; (defvar markdown-preview-stylesheets)
;;   ;; (add-to-list 'markdown-preview-stylesheets "https://gist.githubusercontent.com/andyferra/2554919/raw/10ce87fe71b23216e3075d5648b8b9e56f7758e1/github.css")
;;   ;; (setq markdown-preview-stylesheets '(list "https://gist.githubusercontent.com/andyferra/2554919/raw/10ce87fe71b23216e3075d5648b8b9e56f7758e1/github.css"))
;;   )

(leaf cue-mode
  :doc "Major mode for CUE language files"
  :req "emacs-25.1"
  :tag "languages" "data" "emacs>=25.1"
  :url "https://github.com/russell/cue-mode"
  :added "2022-07-19"
  :emacs>= 25.1
  :ensure t)

(leaf docker
  :doc "Emacs interface to Docker"
  :req "dash-2.14.1" "docker-tramp-0.1" "emacs-24.5" "json-mode-1.7.0" "s-1.12.0" "tablist-0.70" "transient-0.2.0"
  :tag "convenience" "filename" "emacs>=24.5"
  :added "2020-08-27"
  :url "https://github.com/Silex/docker.el"
  :emacs>= 24.5
  :ensure t
  :after docker-tramp json-mode tablist)

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24" "s-1.12"
  :tag "emacs>=24"
  :added "2020-11-25"
  :url "https://github.com/spotify/dockerfile-mode"
  :emacs>= 24
  :ensure t)

(leaf lsp-sqls
  :doc "SQL Client settings"
  :tag "out-of-MELPA" "lsp" "sql"
  :added "2022-02-09"
  :require t
  :custom
  ;; (lsp-sqls-workspace-config-path . nil)
  (lsp-sqls-connections
   '(((driver . "mysql") (dataSrouceName . "root:@tcp(127.0.0.1:3306)/pokerroom?parseTime=true"))))
  :hook
  (sql-mode-hook . lsp-deferred)
  )

(leaf sqlup-mode
  :doc "Upcase SQL words for you"
  :tag "upcase" "redis" "tools" "sql"
  :added "2020-08-27"
  :url "https://github.com/trevoke/sqlup-mode.el"
  :ensure t)

(leaf sqlformat
  :doc "Reformat SQL using sqlformat or pgformatter"
  :req "emacs-24.3" "reformatter-0.3"
  :tag "languages" "emacs>=24.3"
  :url "https://github.com/purcell/sqlformat"
  :added "2022-07-14"
  :emacs>= 24.3
  :ensure t
  :after reformatter
  :setq-default
  (sqlformat-command . 'sqlformat)
  )

(leaf csv-mode
  :doc "Major mode for editing comma/char separated values"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "convenience" "emacs>=24.1"
  :added "2020-11-04"
  :url "http://elpa.gnu.org/packages/csv-mode.html"
  :emacs>= 24.1
  :ensure t)

(leaf hcl-mode
  :doc "Major mode for Hashicorp"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :url "https://github.com/purcell/emacs-hcl-mode"
  :added "2021-12-28"
  :emacs>= 24.3
  :ensure t)

(leaf groovy-mode
  :doc "Major mode for Groovy source files"
  :req "s-1.12.0" "emacs-24.3" "dash-2.13.0"
  :tag "languages" "emacs>=24.3"
  :added "2022-08-22"
  :emacs>= 24.3
  :ensure t)

(leaf gradle-mode
  :doc "Gradle integration with Emacs' compile"
  :req "s-1.8.0"
  :tag "gradle"
  :url "http://github.com/jacobono/emacs-gradle-mode"
  :added "2022-08-19"
  :ensure t)

(leaf terraform-mode
  :doc "Major mode for terraform configuration file"
  :req "emacs-24.3" "hcl-mode-0.3" "dash-2.17.0"
  :tag "emacs>=24.3"
  :added "2021-05-08"
  :url "https://github.com/syohex/emacs-terraform-mode"
  :emacs>= 24.3
  :ensure t
  :after hcl-mode
  :custom
  (lsp-disabled-clients . '(tfls))
  (lsp-terraform-ls-enable-show-reference . t)
  (lsp-semantic-tokens-enable . t)
  (lsp-semantic-tokens-honor-refresh-requests . t)
  :hook
  (terraform-mode-hook . terraform-format-on-save-mode)
  (terraform-mode-hook . lsp-deferred)
  )

;; (leaf company-terraform
;;   :doc "A company backend for terraform"
;;   :req "emacs-24.4" "company-0.8.12" "terraform-mode-0.6"
;;   :tag "company" "terraform" "convenience" "abbrev" "emacs>=24.4"
;;   :url "https://github.com/rafalcieslak/emacs-company-terraform"
;;   :added "2021-11-27"
;;   :emacs>= 24.4
;;   :ensure t
;;   :after company terraform-mode
;;   :init (company-terraform-init)
;;   :custom
;;   (terraform-indent-level . 2)
;;   )

(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :req "dash-2.0.0" "emacs-25.0"
  :tag "ascii" "plantuml" "uml" "emacs>=25.0"
  :added "2022-02-23"
  :emacs>= 25.0
  :ensure t)

(leaf mermaid-mode
  :doc "major mode for working with mermaid graphs"
  :req "f-0.20.0" "emacs-25.3"
  :tag "processes" "tools" "graphs" "mermaid" "emacs>=25.3"
  :url "https://github.com/abrochard/mermaid-mode"
  :added "2022-05-04"
  :emacs>= 25.3
  :ensure t)

(leaf ob-mermaid
  :doc "org-babel support for mermaid evaluation"
  :tag "lisp"
  :url "https://github.com/arnm/ob-mermaid"
  :added "2022-05-04"
  :ensure t)

(leaf ob-typescript
  :doc "org-babel functions for typescript evaluation"
  :req "emacs-24" "org-8.0"
  :tag "typescript" "reproducible research" "literate programming" "emacs>=24"
  :added "2020-08-27"
  :url "https://github.com/lurdan/ob-typescript"
  :emacs>= 24
  :ensure t
  :after org
  :custom
  ((org-plantuml-jar-path . "~/.emacs.d/plantuml/plantuml.jar"))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (ditaa . t)
     (dot . t)))
  )

(leaf tide
  :doc "Typescript Interactive Development Environment"
  :req "emacs-25.1" "dash-2.10.0" "s-1.11.0" "flycheck-27" "typescript-mode-0.1" "cl-lib-0.5"
  :tag "typescript" "emacs>=25.1"
  :added "2020-08-27"
  :url "http://github.com/ananthakumaran/tide"
  :emacs>= 25.1
  :ensure t
  :after flycheck typescript-mode
  :hook
  (tide-mode-hook . lsp-deferred))

(leaf yapfify
  :doc "(automatically) format python buffers using YAPF."
  :added "2020-08-27"
  :url "https://github.com/JorisE/yapfify"
  :ensure t)

(leaf ox-gfm
  :doc "Github Flavored Markdown Back-End for Org Export Engine"
  :tag "github" "markdown" "wp" "org"
  :added "2020-08-27"
  :after org
  :ensure t
  :init
  '(require 'ox-gfm nil t))

;; (leaf calendar
;;   :doc "calendar functions"
;;   :tag "builtin"
;;   :added "2020-09-18"
;;   :init
;;   (leaf japanese-holidays
;;     :doc "calendar functions for the Japanese calendar"
;;     :req "cl-lib-0.3"
;;     :tag "calendar"
;;     :added "2020-09-18"
;;     :url "https://github.com/emacs-jp/japanese-holidays"
;;     :after calendar
;;     :ensure t
;;     :custom
;;     ((calendar-mark-holidays-flag . t)
;;      (calendar-holidays . (append japanese-holidays holiday-local-holidays holiday-other-holidays)))
;;     :hook
;;     ((calendar-today-visible-hook . calendar-mark-today)
;;      (calendar-today-visible-hook . japanese-holiday-mark-weekend)
;;      (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
;;      )
;;     )
;;   )

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(transient-dwim leaf-convert leaf-tree leaf-keywords el-get hydra blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here

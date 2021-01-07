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

(setq byte-compile-warnings '(cl-functions))

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

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
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; USER SETTING

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))
  :init
  (server-start)
  (global-git-gutter-mode +1)
  )

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf-keys (("C-h"     . backward-delete-char)
            ("C-c M-a" . align-regexp)
            ("C-c ;"   . comment-region)
            ("C-c M-;" . uncomment-region)
            ("C-/"     . undo)
            ("C-C M-R" . replace-regexp)
            ("C-c r"   . replace-string)
            ("<home>"  . beginning-of-buffer)
            ("C-c M-l" . toggle-truncate-lines)
            ("C-c v r" . vr/query-replace)
            ("C-c v g" . magit-status)
            ("C-c m e" . emoji-cheat-sheet-plus-insert)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Taketo Asai")
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
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(setq gc-cons-threshold 25600000)
(setq read-process-output-max (* 1024 1024 4))

(setq default-frame-alist
      '(
        (font . "Cica 12")))

(defconst my:d:tmp (expand-file-name "tmp/" user-emacs-directory))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-27"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :custom
  ((exec-path-from-shell-arguments . ""))
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

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :added "2020-08-28"
  :url "http://www.dr-qubit.org/emacs.php"
  :ensure t
  :global-minor-mode global-undo-tree-mode)

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

;; ivy
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                                      (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

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

(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
  :added "2020-08-27"
  :url "https://github.com/hlissner/emacs-doom-theme"
  :emacs>= 25.1
  :ensure t
  :init (load-theme 'doom-one t))

(leaf doom-modeline
  :doc "A minimal and modern mode-line"
  :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
  :tag "mode-line" "faces" "emacs>=25.1"
  :added "2020-08-27"
  :url "https://github.com/seagle0128/doom-modeline"
  :emacs>= 25.1
  :ensure t
  :after all-the-icons shrink-path
  :custom
  ((doom-modeline-env-version . t)
   (doom-modeline-vcs-max-length . 12)
   (doom-modeline-workspace-name . t))
  :init (doom-modeline-mode 1))

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
  (hl-todo-keyword-faces . '(("TODO"   . "#FF4500")
                             ("FIXME"  . "#DDAE13")
                             ("DEBUG"  . "#1E90FF")))
  :init
  (global-hl-todo-mode t)
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
  :custom ((pangu-spacing-real-insert-separtor . t))
  :hook ((text-mode-hook . pangu-spacing-mode)
         (org-mode-hook . pangu-spacing-mode))
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
  :global-minor-mode global-flycheck-mode
  :init
  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :req "emacs-24.1" "flycheck-0.22" "package-lint-0.2"
    :tag "lisp" "emacs>=24.1"
    :added "2020-09-03"
    :url "https://github.com/purcell/flycheck-package"
    :emacs>= 24.1
    :ensure t
    :after flycheck package-lint)
  )

(leaf flyspell
  :doc "On-the-fly spell checker"
  :tag "builtin"
  :added "2021-01-04"
  :hook
  (text-mode-hook . flyspell-mode)
  (org-mode-hook . flyspell-mode)
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
  :custom ((editorconfig-get-properties-function . 'editorconfig-core-get-properties-hash)))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0.2)
           (company-minimum-prefix-length . 1)
           ;; (company-transformers . '(company-sort-by-occurrence))
           )
  :global-minor-mode global-company-mode
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
  :custom
  '(dap-auto-configure-features . '(sessions locals controls tooltip))
  )

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2020-10-30"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t)

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
  `((lsp-keymap-prefix                  . "C-c l")
    (lsp-inhibit-message                . t)
    (lsp-message-project-root-warning   . t)
    (create-lockfiles                   . nil)
    (lsp-signature-auto-activate        . t)
    (lsp-signature-doc-lines            . 1)
    (lsp-print-performance              . t)
    (lsp-log-io                         . t)
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
    (lsp-completion-provider            . :capf)
    (lsp-headerline-breadcrumb-segments . '(symbols)))
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
    ((lsp-ui-doc-enable            . t)
     (lsp-ui-doc-deley             . 0.5)
     (lsp-ui-doc-header            . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position          . 'at-point)
     (lsp-ui-doc-max-width         . 150)
     (lsp-ui-doc-max-height        . 30)
     (lsp-ui-doc-use-childframe    . nil)
     (lsp-ui-doc-use-webkit        . nil)
     (lsp-ui-flycheck-enable       . t)
     (lsp-ui-peek-enable           . t)
     (lsp-ui-peek-peek-height      . 20)
     (lsp-ui-peek-list-width       . 50)
     (lsp-ui-peek-fontify          . 'on-demand) ;; never, on-demand, or always
     )
    :hook ((lsp-mode-hook . lsp-ui-mode))
    )
  (leaf lsp-treemacs
    :doc "LSP treemacs"
    :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
    :tag "languages" "emacs>=26.1"
    :added "2020-10-22"
    :url "https://github.com/emacs-lsp/lsp-treemacs"
    :emacs>= 26.1
    :ensure t
    :after treemacs lsp-mode)
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

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-27"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor)

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
  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
)
  

(leaf emoji-cheat-sheet-plus
  :doc "emoji-cheat-sheet for emacs"
  :req "emacs-24" "helm-1.6.4"
  :tag "emoji" "emacs" "emacs>=24"
  :added "2020-08-27"
  :url "https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus"
  :emacs>= 24
  :ensure t
  :after helm)

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
  (leaf elpy
    :doc "Emacs Python Development Environment"
    :req "company-0.9.2" "emacs-24.4" "highlight-indentation-0.5.0" "pyvenv-1.3" "yasnippet-0.8.0" "s-1.11.0"
    :tag "emacs>=24.4"
    :added "2020-09-11"
    :emacs>= 24.4
    :ensure t
    :after company highlight-indentation pyvenv yasnippet)
  )

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2020-08-27"
  :url "http://web-mode.org"
  :emacs>= 23.1
  :ensure t)

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

(leaf sqlup-mode
  :doc "Upcase SQL words for you"
  :tag "upcase" "redis" "tools" "sql"
  :added "2020-08-27"
  :url "https://github.com/trevoke/sqlup-mode.el"
  :ensure t)

(leaf csv-mode
  :doc "Major mode for editing comma/char separated values"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "convenience" "emacs>=24.1"
  :added "2020-11-04"
  :url "http://elpa.gnu.org/packages/csv-mode.html"
  :emacs>= 24.1
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

(leaf prettier-js
  :doc "Minor mode to format JS code on file save"
  :tag "js" "edit" "wp" "convenience"
  :added "2020-08-27"
  :url "https://github.com/prettier/prettier-emacs"
  :ensure t
  :hook ((typescript-mode-hook . prettier-js-mode)))

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

(leaf calendar
  :doc "calendar functions"
  :tag "builtin"
  :added "2020-09-18"
  :init
  (leaf japanese-holidays
    :doc "calendar functions for the Japanese calendar"
    :req "cl-lib-0.3"
    :tag "calendar"
    :added "2020-09-18"
    :url "https://github.com/emacs-jp/japanese-holidays"
    :after calendar
    :ensure t
    :custom
    ((calendar-mark-holidays-flag . t)
     (calendar-holidays . (append japanese-holidays holiday-local-holidays holiday-other-holidays)))
    :hook
    ((calendar-today-visible-hook . calendar-mark-today)
     (calendar-today-visible-hook . japanese-holiday-mark-weekend)
     (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
     )
    )
  )

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here


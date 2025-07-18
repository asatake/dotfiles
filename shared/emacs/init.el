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
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))
(defconst my:d:share (expand-file-name "share/" user-emacs-directory))
(defconst my:d:tmp (expand-file-name "tmp/" user-emacs-directory))
(defconst tree-sitter-major-mode-language-alist ())
(setq confirm-kill-emacs 'y-or-n-p)

(keyboard-translate ?\C-h ?\C-?)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(add-to-list 'load-path "~/.local/bin/")
(add-to-list 'load-path "~/.emacs.d/mylisp/")
(add-to-list 'load-path "~/repos/mcp.el/")
;; (setq ns-command-modifier (quote meta))

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"      . "https://elpa.gnu.org/packages/")
                       ("melpa"    . "https://melpa.org/packages/")
                       ("org"      . "https://orgmode.org/elpa/")
                       ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    :config
    (leaf-keywords-init))
  )
;; </leaf-install-code>

;; 何も考えず公式のREADMEからコピペすればいいコード
;; straight.el自身のインストールと初期設定を行ってくれる
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq package-enable-at-startup nil)

(leaf leaf
  :defvar (native-comp-async-report-warnings-errors native-compile-prune-cache)
  :init
  (server-start)
  (global-hl-line-mode +1)
  (column-number-mode t)
  (line-number-mode t)
  (save-place-mode +1)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t)
  (setq-default show-trailing-whitespace t)
  (ido-mode t)
  (recentf-mode)
  (savehist-mode)
  )

(leaf second-buffer-hook
  (defvar my-buffer-count-threshold 2
    "バッファの数がこの値を超えたときにフックを実行します。")

  (defvar my-buffer-two-count-hook nil
    "バッファの数が閾値を超えたときに実行されるフック。")

  (defun my-check-buffer-count ()
    "バッファの数をチェックし、必要に応じてフックを実行します。"
    (let ((buffer-count (length (buffer-list))))
      (when (and (>= buffer-count my-buffer-count-threshold)
                 (< (1- buffer-count) my-buffer-count-threshold))
        (run-hooks 'my-buffer-two-count-hook))))
  (add-hook 'buffer-list-update-hook 'my-check-buffer-count)
  )

(leaf backup-setting
  :init
  (setq make-backup-files nil)
  (setq backup-inhibited nil)
  (setq create-lockfiles nil))

(leaf beep-setting
  :init
  (setq ring-bell-function 'ignore))

(leaf files
  :ensure nil
  :custom `((version-control . t)
            (delete-old-versions . t))
  :config
  (setq auto-save-visited-interval 30)
  (auto-save-visited-mode +1))

(leaf performance-setting
  :config
  (setq process-adaptive-read-buffering t)
  (setq blink-matching-paren nil)
  (setq auto-mode-case-fold nil)
  (setq-default bidi-display-reordering 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq fast-but-imprecise-scrolling t)
  (setq ffap-machine-p-known 'reject)
  (setq idle-update-delay 1.0)
  (setq redisplay-skip-fontification-on-input t)
  (setq linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))

  (when IS-WINDOWS
    (setq w32-use-native-image-API t))

  (unless IS-MAC
    (setq command-line-ns-option-alist nil))

  (unless IS-LINUX
    (setq command-line-x-option-alist nil))
  )

(leaf init-loader
  :doc "Loader for configuration files"
  :req "cl-lib-0.5"
  :url "https://github.com/emacs-jp/init-loader/"
  :added "2021-09-12"
  :ensure t
  :custom
  (init-loader-show-log-after-init . 'error-only)
  :config
  (setq custom-file "~/.emacs.d/tmp/custom.el")
  )

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs"
  :req "cl-lib-0.5"
  :tag "cursors" "editing"
  :url "https://github.com/magnars/multiple-cursors.el"
  :added "2025-01-09"
  :ensure t
  :config
  (global-set-key (kbd "C-c C-<") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-M-c") 'mc/mark-all-like-this))

(leaf leaf-convert
  :doc "Convert many format to leaf format"
  :req "emacs-26.1" "leaf-3.6.0" "leaf-keywords-1.1.0" "ppp-2.1"
  :tag "tools" "emacs>=26.1"
  :url "https://github.com/conao3/leaf-convert.el"
  :added "2024-01-17"
  :emacs>= 26.1
  :ensure t
  :after leaf leaf-keywords ppp)

(leaf leaf-tree
  :doc "Interactive side-bar feature for init.el using leaf"
  :req "emacs-25.1" "imenu-list-0.8"
  :tag "leaf" "convenience" "emacs>=25.1"
  :url "https://github.com/conao3/leaf-tree.el"
  :added "2024-01-17"
  :emacs>= 25.1
  :ensure t
  :after imenu-list
  :custom ((imenu-list-size . 30)
           (imenu-list-position . 'left)))

(leaf transient
  :doc "Transient commands."
  :req "emacs-26.1" "compat-30.1.0.0" "seq-2.24"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/magit/transient"
  :straight (transient :type git :host github :repo "magit/transient" :branch "main")
  :added "2025-05-20"
  :emacs>= 26.1
  :after compat)

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . 'transient-dwim-dispatch)))

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
            ("C-x C-k k" . kill-matching-buffers)
            ("M-["       . switch-to-prev-buffer)
            ("M-]"       . switch-to-next-buffer)))

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
            (tab-width . 4)
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

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :added "2025-03-03"
  :global-minor-mode delete-selection-mode)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-27"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :custom
  ((exec-path-from-shell-check-startup-files)
   (exec-path-from-shell-arguments . "")
   (exec-path-from-shell-variables . '("PATH" "GOPATH" "JAVA_HOME")))
  :config
  (exec-path-from-shell-initialize))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :config
  (global-auto-revert-mode +1))

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf so-long
  :doc "Say farewell to performance problems with minified code."
  :tag "builtin"
  :added "2024-06-28"
  :init
  (global-so-long-mode +1))

(leaf undo-fu
  :doc "Undo helper with redo"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu"
  :added "2024-06-28"
  :emacs>= 25.1
  :ensure t
  :config
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "M-_") 'undo-fu-only-redo)
  :init
  (leaf undo-fu-session
    :doc "Persistent undo, available between sessions"
    :req "emacs-28.1"
    :tag "convenience" "emacs>=28.1"
    :url "https://codeberg.org/ideasman42/emacs-undo-fu-session"
    :added "2024-06-28"
    :emacs>= 28.1
    :ensure t
    :config
    (undo-fu-session-global-mode +1))
  (leaf undo-tree
      :doc "Treat undo history as a tree"
      :req "queue-0.2"
      :tag "tree" "history" "redo" "undo" "files" "convenience"
      :url "https://www.dr-qubit.org/undo-tree.html"
      :added "2024-08-08"
      :ensure t
      :after queue)
  :config
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.
  )

(leaf vundo
  :doc "Visual undo tree"
  :req "emacs-28.1"
  :tag "editing" "text" "undo" "emacs>=28.1"
  :url "https://github.com/casouri/vundo"
  :added "2024-06-28"
  :emacs>= 28.1
  :ensure t)

(leaf word-wrap-mode
  :doc "minor mode for `word-wrap' tweaks"
  :tag "builtin"
  :added "2025-03-04"
  :hook (visual-line-mode . word-wrap-whitespace-mode)
  :config
  (add-to-list 'word-wrap-whitespace-characters ?\]))

(leaf visual-fill-column
  :doc "Fill-column for visual-line-mode"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://codeberg.org/joostkremers/visual-fill-column"
  :added "2025-03-04"
  :emacs>= 25.1
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-line-fringe-indicators '(left-curly-arrow nil))
  :config
  (setq visual-fill-column-width 78))

(leaf adaptive-wrap
  :doc "Smart line-wrapping with wrap-prefix"
  :url "https://elpa.gnu.org/packages/adaptive-wrap.html"
  :added "2025-03-04"
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf smartparens
  :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs"
  :req "dash-2.13.0"
  :tag "editing" "convenience" "abbrev"
  :url "https://github.com/Fuco1/smartparens"
  :added "2025-02-25"
  :ensure t)

(leaf electric
  :doc "window maker and Command loop for `electric' modes"
  :tag "builtin"
  :added "2020-08-27"
  :init (electric-pair-mode 1))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf savehist
  :doc "Save minibuffer history"
  :tag "builtin"
  :added "2025-03-03"
  :custom `((savehist-file . ,(locate-user-emacs-file "savehist"))))

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

(leaf breadcrumb
  :doc "project and imenu-based breadcrumb paths"
  :req "emacs-28.1" "project-0.9.8"
  :tag "emacs>=28.1"
  :added "2024-07-03"
  :emacs>= 28.1
  :ensure t
  :after project
  :config
  (breadcrumb-mode +1))

(leaf vlf
  :doc "View Large Files"
  :tag "utilities" "large files"
  :url "https://github.com/m00natic/vlfi"
  :added "2024-06-28"
  :ensure t)

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
;;   :leaf-defer nil
;;   :custom
;;   (ivy-initial-inputs-alist . nil)
;;   (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
;;                              (swiper . ivy--regex-plus)))
;;   (ivy-use-selectable-prompt . t)
;;   :global-minor-mode t
;;   )

(leaf migemo
  :doc "Japanese incremental search through dynamic pattern expansion"
  :req "cl-lib-0.5"
  :url "https://github.com/emacs-jp/migemo"
  :added "2022-12-27"
  :ensure t
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-dictionary . "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (migemo-coding-system . 'utf-8-unix)
  :init
  (when (and (executable-find "cmigemo"))
    (load-library "migemo")
    (migemo-init))
  )

;; (leaf swiper
;;   :doc "Isearch with an overview.  Oh, man!."
;;   :req "emacs-24.5" "ivy-0.15.0"
;;   :tag "matching" "emacs>=24.5"
;;   :url "https://github.com/abo-abo/swiper"
;;   :added "2025-03-05"
;;   :emacs>= 24.5
;;   :ensure t
;;   :after ivy
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-r" . swiper-backward))

;; (leaf ivy-migemo
;;   :doc "Use migemo on ivy"
;;   :req "emacs-24.3" "ivy-0.13.0" "migemo-1.9.2" "nadvice-0.3"
;;   :tag "matching" "emacs>=24.3"
;;   :url "https://github.com/ROCKTAKEY/ivy-migemo"
;;   :added "2022-12-27"
;;   :emacs>= 24.3
;;   :ensure t
;;   :after ivy migemo nadvice
;;   :custom
;;   (ivy-migemo-enable-command . '(swiper swiper-isearch counsel-recentf counsel-rg))
;;   (ivy-re-builders-alist . '((t                 . ivy--regex-plus)
;;                              (swiper            . ivy-migemo-regex-plus)
;;                              (counsel-find-file . ivy-migemo-regex-plus)))
;;   )

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay . 0.3)
  (flycheck-indication-mode . 'left-margin)
  :config
  (global-flycheck-mode t)
  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
  (flycheck-define-checker textlint
    "A linter for text."
    :command ("/Users/tetsuhiromanome/.local/bin/textlint.sh" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))
  ;; Set up a mode for JSON based templates

  (define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

  (add-to-list 'magic-mode-alist
               '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

  ;; Set up a mode for YAML based templates if yaml-mode is installed
  ;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
  (when (featurep 'yaml-mode)

    (define-derived-mode cfn-yaml-mode yaml-mode
      "CFN-YAML"
      "Simple mode to edit CloudFormation template in YAML format.")
    (add-to-list 'magic-mode-alist
                 '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

  ;; Set up cfn-lint integration if flycheck is installed
  ;; Get flycheck here https://www.flycheck.org/
  (when (featurep 'flycheck)
    (flycheck-define-checker cfn-lint
      "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

      :command ("cfn-lint" "-f" "parseable" source)
      :error-patterns ((warning line-start (file-name) ":" line ":" column
                                ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                (id "W" (one-or-more digit)) ":" (message) line-end)
                       (error line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "E" (one-or-more digit)) ":" (message) line-end))
      :modes (cfn-json-mode cfn-yaml-mode))

    (add-to-list 'flycheck-checkers 'cfn-lint)
    (add-hook 'cfn-json-mode-hook #'flycheck-mode)
    (add-hook 'cfn-yaml-mode-hook #'flycheck-mode)
    (add-hook 'prog-mode-hook #'flycheck-mode))
  )

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
  (with-eval-after-load 'flycheck
    (flycheck-package-setup))
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
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)))

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

(leaf flycheck-inline
  :doc "Display Flycheck errors inline."
  :req "emacs-25.1" "flycheck-32"
  :tag "convenience" "tools" "emacs>=25.1"
  :url "https://github.com/flycheck/flycheck-inline"
  :added "2025-02-25"
  :emacs>= 25.1
  :ensure t
  :after flycheck
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
    )
  )

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-28.1" "compat-30"
  :tag "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/minad/consult"
  :emacs>= 26.1
  :ensure t
  :after compat
  :preface
  (defun c/consult-line (&optional at-point)
    "Consult-line uses things-at-point if set C-u prefix."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line)))
  :bind (("M-g M-g" . 'consult-goto-line)
         ("C-x b"   . 'consult-buffer)
         ("C-x C-r" . 'consult-recent-file)
         ("C-s"     . 'consult-line)
         ("C-r"     . 'consult-line))
  :custom ((xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref)
           (consult-line-start-from-top . t))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (defun consult-line-migemo ()
    (interactive)
    (let ((input (read-string "Input: " nil)))
      (consult-line (migemo-get-pattern input))))
  )

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1"
  :tag "emacs>=27.1"
  :added "2021-06-13"
  :url "https://github.com/minad/vertico"
  :emacs>= 27.1
  :ensure t
  :global-minor-mode t
  ;; :custom ((vertico-cycle . t)
  ;;          (vertico-preselect . 'prompt))
  ;; 補完スタイルにorderlessを利用する
  ;; (completion-styles . '(orderless))
  ;; (vertico-count . 20)
  :config
  (setq vertico-cycle t)
  (setq enable-recursive-minibuffers t)
  (with-eval-after-load 'consult
    (with-eval-after-load 'embark
      (require 'embark-consult)))
  )

(leaf vertico-prescient
  :doc "Prescient.el + Vertico"
  :req "emacs-27.1" "prescient-6.1.0" "vertico-0.28" "compat-29.1"
  :tag "extensions" "emacs>=27.1"
  :url "https://github.com/radian-software/prescient.el"
  :added "2025-03-05"
  :emacs>= 27.1
  :ensure t
  :after prescient vertico compat
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode +1))

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-26.1"
  :tag "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/minad/marginalia"
  :emacs>= 26.1
  :ensure t
  :after vertico
  :global-minor-mode t
  )

(leaf counsel
  :doc "Various completion functions using Ivy"
  :req "emacs-24.5" "swiper-0.13.0"
  :tag "tools" "matching" "convenience" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :bind (
         ;; ("C-x C-r" . counsel-recentf)
         ;; ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x)
         )
  :setq-default
  ;; (counsel-yank-pop-separator . "\n----------\n")
  (counsel-find-file-ignore-regexp . (regexp-opt '("./" "../")))
  ;; :global-minor-mode t
  )

(leaf consult-flycheck
  :doc "Provides the command `consult-flycheck'"
  :req "emacs-28.1" "consult-1.8" "flycheck-34"
  :tag "completion" "tools" "languages" "emacs>=28.1"
  :url "https://github.com/minad/consult-flycheck"
  :added "2025-01-16"
  :emacs>= 28.1
  :ensure t
  :after consult flycheck)

(leaf consult-ghq
  :doc "Ghq interface using consult"
  :req "emacs-26.1" "consult-0.8" "affe-0.1"
  :tag "ghq" "consult" "usability" "convenience" "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/tomoya/consult-ghq"
  :emacs>= 26.1
  :ensure t
  :after consult affe
  )

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

;; (leaf ivy-rich
;;   :doc "More friendly display transformer for ivy."
;;   :req "emacs-24.5" "ivy-0.8.0"
;;   :tag "ivy" "emacs>=24.5"
;;   :emacs>= 24.5
;;   :ensure t
;;   :after ivy
;;   :global-minor-mode t)

;; ;; (leaf all-the-icons
;; ;;   :doc "A library for inserting Developer icons"
;; ;;   :req "emacs-24.3" "memoize-1.0.1"
;; ;;   :tag "lisp" "convenient" "emacs>=24.3"
;; ;;   :added "2020-08-27"
;; ;;   :url "https://github.com/domtronn/all-the-icons.el"
;; ;;   :emacs>= 24.3
;; ;;   :ensure t
;; ;;   :after memoize)

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library"
  :req "emacs-24.3"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :added "2024-06-28"
  :emacs>= 24.3
  :ensure t
  )

(leaf nerd-icons-completion
  :doc "Add icons to completion candidates"
  :req "emacs-25.1" "nerd-icons-0.0.1"
  :tag "lisp" "emacs>=25.1"
  :url "https://github.com/rainstormstudio/nerd-icons-completion"
  :added "2024-06-28"
  :emacs>= 25.1
  :ensure t
  :after nerd-icons
  :hook (after-init . nerd-icons-completion-mode))

(leaf nerd-icons-dired
  :doc "Shows icons for each file in dired mode"
  :req "emacs-24.4" "nerd-icons-0.0.1"
  :tag "lisp" "emacs>=24.4"
  :url "https://github.com/rainstormstudio/nerd-icons-dired"
  :added "2024-06-28"
  :emacs>= 24.4
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(leaf subword
  :doc "Handling capitalized subwords in a nomenclature"
  :tag "builtin"
  :added "2022-03-22"
  :init
  (global-subword-mode 1)
  )

(leaf indent-bars
    :doc "Highlight indentation with bars"
    :req "emacs-27.1" "compat-29.1.4.5"
    :tag "convenience" "emacs>=27.1"
    :url "https://github.com/jdtsmith/indent-bars"
    :added "2024-07-04"
    :emacs>= 27.1
    :ensure t
    :after compat
    :hook
    (prog-mode-hook . indent-bars-mode)
    (yaml-mode-hook . indent-bars-mode)
    (json-mode-hook . indent-bars-mode)
    :config
    (setq indent-bars-treesit-support t)
    (setq indent-bars-treesit-ignore-blank-lines-types '("module"))
    (setq
     indent-bars-color '(highlight :face-bg t :blend 0.15)
     indent-bars-pattern "."
     indent-bars-width-frac 0.1
     indent-bars-pad-frac 0.1
     indent-bars-zigzag nil
     indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
     indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
     indent-bars-display-on-blank-lines t))

(leaf highlight-symbol
  :ensure t
  :url "https://github.com/nschum/highlight-symbol.el"
  :init
  (global-set-key [(control f4)] 'highlight-symbol)
  (global-set-key [f4] 'highlight-symbol-next)
  (global-set-key [(shift f4)] 'highlight-symbol-prev)
  )

(leaf puni
  :doc "Parentheses Universalistic"
  :req "emacs-26.1"
  :tag "tools" "lisp" "convenience" "emacs>=26.1"
  :url "https://github.com/AmaiKinono/puni"
  :added "2024-07-04"
  :emacs>= 26.1
  :ensure t
  :global-minor-mode puni-global-mode
  :bind (puni-mode-map
         ;; default mapping
         ;; ("C-M-f" . puni-forward-sexp)
         ;; ("C-M-b" . puni-backward-sexp)
         ;; ("C-M-a" . puni-beginning-of-sexp)
         ;; ("C-M-e" . puni-end-of-sexp)
         ;; ("M-)" . puni-syntactic-forward-punct)
         ;; ("C-M-u" . backward-up-list)
         ;; ("C-M-d" . backward-down-list)
         ("C-)" . puni-slurp-forward)
         ("C-}" . puni-barf-forward)
         ("M-(" . puni-wrap-round)
         ("M-s" . puni-splice)
         ("M-r" . puni-raise)
         ("M-U" . puni-splice-killing-backward)
         ("M-z" . puni-squeeze))
  )

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :added "2024-07-04"
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf kaolin-themes
  :doc "A set of eye pleasing themes"
  :req "emacs-25.1" "autothemer-0.2.2" "cl-lib-0.6"
  :tag "faces" "theme" "brown" "purple" "violet" "blue" "teal" "light" "dark" "emacs>=25.1"
  :url "https://github.com/ogdenwebb/emacs-kaolin-themes"
  :added "2025-01-23"
  :emacs>= 25.1
  :ensure t
  :config
  (load-theme 'kaolin-galaxy t)
  )

;; (leaf doom-themes
;;   :doc "an opinionated pack of modern color-themes"
;;   :req "emacs-25.1" "cl-lib-0.5"
;;   :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
;;   :added "2020-08-27"
;;   :url "https://github.com/hlissner/emacs-doom-theme"
;;   :emacs>= 25.1
;;   :ensure t
;;   ;; :init
;;   ;; (load-theme 'doom-palenight t)
;;   :custom
;;   (doom-themes-enable-italic . t)
;;   (doom-themes-enable-bold   . t)
;;   :config
;;   (doom-themes-neotree-config)
;;   (doom-themes-treemacs-config)
;;   (doom-themes-org-config)
;;   (set-face-foreground 'default "grey82")
;;   )

;; (leaf copilot
;;   :req "emacs-27.2" "s-1.12.0" "dash-2.19.1" "editorconfig-0.8.2" "jsonrpc-1.0.23"
;;   :tag "emacs>=27.2"
;;   :added "2024-01-17"
;;   :url "copilot-emacs/copilot.el"
;;   :ensure t
;;   :after editorconfig jsonrpc
;;   :config
;;   (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
;;   (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(special-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(magit-wip-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(closure-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(go-mode 4))
;;   (add-to-list 'copilot-indentation-alist '(web-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(typescript-ts-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(json-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(markdown-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(rust-mode 4))
;;   (add-to-list 'copilot-indentation-alist '(sql-mode 4))
;;   :hook
;;   ((text-mode-hook prog-mode-hook go-mode-hook) . copilot-mode)
;;   )

(leaf codeium
  :doc "Codeium client for emacs"
  :url "https://github.com/Exafunction/codeium.el"
  :added "2025-01-13"
  :ensure t
  :init
  (add-hook 'emacs-startup-hook
            (lambda () (run-with-timer 0.1 nil #'codeium-init)))
  :config
  (setq use-dialog-box nil)
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  )

(leaf gptel
  :doc "Interact with ChatGPT or other LLMs"
  :req "emacs-27.1" "transient-0.7.4" "compat-29.1.4.1"
  :tag "tools" "convenience" "emacs>=27.1"
  :straight (gptel :type git :host github :repo "karthink/gptel" :branch "feature-tool-use")
  :added "2025-04-07"
  :emacs>= 27.1
  :after compat
  :custom
  (gptel-model . 'claude-3-7-sonnet-20250219)
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key 'gptel-api-key-from-auth-source))
  )

(leaf claude-code
  :doc "Claude Code client"
  :req "emacs-30.0" "transient-0.7.5" "eat-0.9.2"
  :straight (claude-code :type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                         :files ("*.el" (:exclude "demo.gif")))
  :added "2025-05-05"
  :emacs>= 30.0
  :hook ((claude-code--start . sm-setup-claude-faces))
  :global-minor-mode claude-code-mode
  :config
  (global-set-key (kbd "C-c c") claude-code-command-map)
  (setq claude-code-program "/Users/tetsuhiromanome/.claude/local/claude")
  )

(leaf amazon-q
  :load-path "~/.emacs.d/mylisp"
  :require t
  :config
  (setq amazon-q-buffer-name "*My Amazon Q*")
  ;; (setq amazon-q-strip-ansi-escapes nil)
  (global-amazon-q-mode 1))

(leaf nyan-mode
  :doc "Nyan Cat shows position in current buffer in mode-line"
  :req "emacs-24.1"
  :tag "multimedia" "mouse" "games" "convenience" "emacs>=24.1"
  :url "https://github.com/TeMPOraL/nyan-mode/"
  :added "2022-12-16"
  :emacs>= 24.1
  :ensure t
  :custom
  (nyan-animate-nyancat . t)
  :config
  (setq nyan-bar-length 24)
  (nyan-mode t)
  )

(leaf dashboard
  :doc "A startup screen extracted from Spacemacs."
  :req "emacs-27.1"
  :tag "dashboard" "tools" "screen" "startup" "emacs>=27.1"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :added "2025-02-25"
  :emacs>= 27.1
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(leaf wc-mode
  :doc "Running word count with goals (minor mode)"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :url "https://github.com/bnbeckwith/wc-mode"
  :added "2025-01-10"
  :emacs>= 24.1
  :ensure t
  :hook
  ((text-mode-hook
    prog-mode-hook
    emacs-list-mode-hook
    go-mode-hook) . wc-mode)
  :config
  (setq wc-modeline-format "[LC:(%tl,%tc)]"))

(leaf moody
  :doc "Tabs and ribbons for the mode line"
  :req "emacs-26.1" "compat-30.0.1.0"
  :tag "faces" "emacs>=26.1"
  :url "https://github.com/tarsius/moody"
  :added "2025-01-10"
  :emacs>= 26.1
  :ensure t
  :after compat
  ;; :init
  ;; (defvar moody-mode-line-buffer-identification
  ;;   '(:eval (moody-tab (format-mode-line (propertized-buffer-identification "%12b"))
  ;;                      20 'down)))
  ;; (put 'moody-mode-line-buffer-identification 'risky-local-variable t)
  ;; (make-variable-buffer-local 'moody-mode-line-buffer-identification)

  ;; ;;;###autoload
  ;; (defun moody-replace-mode-line-buffer-identification (&optional reverse)
  ;;   (interactive "P")
  ;;   (moody-replace-element 'mode-line-buffer-identification
  ;;                          'moody-mode-line-buffer-identification
  ;;                          reverse))
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (when (eq system-type 'darwin)
    (setq moody-slant-function 'moody-slant-apple-rgb))
  )

(leaf minions
  :doc "A minor-mode menu for the mode line"
  :req "emacs-26.1" "compat-30.0.1.0"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/tarsius/minions"
  :added "2025-01-10"
  :emacs>= 26.1
  :ensure t
  :after compat moody
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "[+]"))

;; ;; (leaf doom-modeline
;; ;;   :doc "A minimal and modern mode-line"
;; ;;   :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
;; ;;   :tag "mode-line" "faces" "emacs>=25.1"
;; ;;   :added "2020-08-27"
;; ;;   :url "https://github.com/seagle0128/doom-modeline"
;; ;;   :emacs>= 25.1
;; ;;   :ensure t
;; ;;   ;; :after all-the-icons shrink-path
;; ;;   :hook
;; ;;   (after-init . doom-modeline-mode)
;; ;;   :init
;; ;;   (doom-modeline-mode 1)
;; ;;   :custom
;; ;;   (doom-modeline-height                 . 25)
;; ;;   (doom-modeline-window-width-limit     . fill-column)
;; ;;   (doom-modeline-project-detection      . 'project)
;; ;;   (doom-modeline-buffer-file-name-style . 'auto)
;; ;;   (doom-modeline-buffer-modified        . 'unspecified)
;; ;;   (doom-modeline-icon                   . (display-graphic-p))
;; ;;   (doom-modeline-major-mode-icon        . t)
;; ;;   (doom-modeline-major-mode-color-icon  . t)
;; ;;   (doom-modeline-buffer-state-icon      . t)
;; ;;   (doom-modeline-buffer-state-icon      . t)
;; ;;   (doom-modeline-enable-word-count      . t)
;; ;;   (doom-modeline-lsp                    . t)
;; ;;   (doom-modeline-env-version            . t)
;; ;;   (doom-modeline-vcs-max-length         . 12)
;; ;;   (doom-modeline-workspace-name         . t)
;; ;;   (doom-modeline-minor-modes            . nil)
;; ;;   :config
;; ;;   ;; (line-number-mode 0)
;; ;;   ;; (column-number-mode 0)
;; ;;   ;; (doom-modeline-def-modeline 'main
;; ;;   ;;   '(bar workspace-number window-number god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
;; ;;   ;;   '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
;; ;;   )

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :url "https://github.com/abo-abo/avy"
  :added "2024-07-04"
  :emacs>= 24.1
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  )

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :added "2024-07-04"
  :ensure t
  :bind ((kbd "M-o") . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (custom-set-faces
   '(aw-leading-char-face ((t (:foreground "red" :height 4.0)))))
  (add-hook 'window-setup-hook
            (lambda ()
              (message "window-setup-hook ace-window")
              (ace-window)))
  (add-hook 'my-buffer-two-count-hook
            (lambda ()
              (message "my-buffer-two-count-hook HOOKED!!!")
              (ace-window)))
  )

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

(leaf which-key
  :doc "Display available keybindings in popup"
  :tag "builtin"
  :added "2025-02-25"
  :global-minor-mode t)

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
  :doc "Manage and navigate projects in Emacs easily."
  :req "emacs-26.1"
  :tag "convenience" "project" "emacs>=26.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2025-02-25"
  :emacs>= 26.1
  :ensure t
  :global-minor-mode projectile-mode
  :bind (("C-x g f" . 'consult-projectile))
  :config
  (when (executable-find "ghq")
    (setq projectile-known-projects
          (mapcar
           (lambda (x)
             (concat (abbreviate-file-name x) "/"))
           (split-string (shell-command-to-string "ghq list --full-path"))))
    )
  )

(leaf consult-projectile
  :doc "Consult integration for projectile"
  :req "emacs-25.1" "consult-0.12" "projectile-2.5.0"
  :tag "convenience" "emacs>=25.1"
  :url "https://gitlab.com/OlMon/consult-projectile"
  :added "2024-07-04"
  :emacs>= 25.1
  :ensure t
  :after consult projectile
  )

;; ;; (leaf neotree
;; ;;   :doc "A tree plugin like NerdTree for Vim"
;; ;;   :req "cl-lib-0.5"
;; ;;   :url "https://github.com/jaypei/emacs-neotree"
;; ;;   :added "2022-09-20"
;; ;;   :ensure t
;; ;;   :custom
;; ;;   (neo-show-hidden-files . t)
;; ;;   (neo-theme 'icons)
;; ;;   )

(leaf hide-mode-line
  :doc "Minor mode that hides/masks your modeline"
  :req "emacs-24.4"
  :tag "mode-line" "frames" "emacs>=24.4"
  :url "https://github.com/hlissner/emacs-hide-mode-line"
  :added "2025-01-10"
  :emacs>= 24.4
  :ensure t
  :hook
  (treemacs-mode . hide-mode-line-mode)
  )

(leaf treemacs
  :doc "A tree style file explorer package"
  :req "emacs-26.1" "cl-lib-0.5" "dash-2.11.0" "s-1.12.0" "ace-window-0.9.0" "pfuture-1.7" "hydra-0.13.2" "ht-2.2" "cfrs-1.3.2"
  :tag "emacs>=26.1"
  :url "https://github.com/Alexander-Miller/treemacs"
  :added "2023-02-27"
  :emacs>= 26.1
  :ensure t
  :after ace-window pfuture hydra cfrs
  :bind ((kbd "C-c t") . treemacs)
  :hook
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  (treemacs-mode . (lambda () (treemacs-project-follow-mode)))
  :config
  (setq treemacs-follow-mode            nil)
  (setq treemacs-tag-follow-mode        nil)
  (setq treemacs-tag-follow-cleanup     nil)
  (setq treemacs-expand-after-init      t)
  (setq treemacs-indentation            2)
  (setq treemacs-missing-project-action 'remove)
  ;; (setq treemacs-project-follow-cleanup t)
  (setq treemacs-load-theme             "nerd-icons")
  (setq treemacs-text-scale             -1)
  (leaf treemacs-projectile
    :doc "Projectile integration for treemacs"
    :req "emacs-26.1" "projectile-0.14.0" "treemacs-0.0"
    :tag "emacs>=26.1"
    :url "https://github.com/Alexander-Miller/treemacs"
    :added "2022-09-20"
    :emacs>= 26.1
    :ensure t
    :after projectile treemacs
    :custom
    (treemacs-text-scale . -1)
    :config
    (treemacs-resize-icons 44)
    (treemacs-project-follow-mode))
  (leaf treemacs-magit
    :doc "Magit integration for treemacs"
    :req "emacs-26.1" "treemacs-0.0" "pfuture-1.3" "magit-2.90.0"
    :tag "emacs>=26.1"
    :url "https://github.com/Alexander-Miller/treemacs"
    :added "2025-01-10"
    :emacs>= 26.1
    :ensure t
    :after treemacs pfuture magit))

(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :added "2020-08-28"
  :custom
  (add-to-list 'tramp-remote-path . 'tramp-own-remote-path)
  (tramp-default-method . "sshx")
  )

(leaf auth-source
  :doc "authentication sources for Gnus and Emacs"
  :tag "builtin"
  :added "2023-09-07"
  )

(leaf grammarly
  :doc "Grammarly API interface"
  :req "emacs-26.1" "s-1.12.0" "request-0.3.0" "websocket-1.6"
  :tag "english" "interface" "api" "grammar" "convenience" "emacs>=26.1"
  :url "https://github.com/emacs-grammarly/grammarly"
  :added "2023-09-07"
  :emacs>= 26.1
  :ensure t
  :after websocket
  :config
  (setq grammarly-username "mush.mnm.tthr@hotmail.co.jp")
  (setq grammarly-password "rvf@evf_RTP@vrb6vep")
  ;; (with-eval-after-load 'grammarly
  ;;   (grammarly-load-from-authinfo))
  ;; (leaf flycheck-grammarly
  ;;   :doc "Grammarly support for Flycheck"
  ;;   :req "emacs-25.1" "flycheck-0.14" "grammarly-0.3.0" "s-1.12.0"
  ;;   :tag "check" "grammar" "convenience" "emacs>=25.1"
  ;;   :url "https://github.com/emacs-grammarly/flycheck-grammarly"
  ;;   :added "2023-12-15"
  ;;   :emacs>= 25.1
  ;;   :ensure t
  ;;   :after flycheck grammarly
  ;;   :init
  ;;   (flycheck-grammarly-setup)
  ;;   :config
  ;;   (setq flycheck-grammarly-check-time 0.8)
  ;;   )
  )

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :added "2025-01-17"
  :emacs>= 25.1
  :ensure t
  :config
  (setq prescient-aggressive-file-save t)
  )

;; (leaf ivy-prescient
;;   :doc "prescient.el + Ivy"
;;   :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
;;   :tag "extensions" "emacs>=25.1"
;;   :url "https://github.com/raxod502/prescient.el"
;;   :emacs>= 25.1
;;   :ensure t
;;   :after prescient ivy
;;   :custom ((ivy-prescient-retain-classic-highlighting . t))
;;   :global-minor-mode t)

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
  :doc "EditorConfig Plugin"
  :tag "builtin"
  :added "2025-02-25"
  :global-minor-mode editorconfig-mode
  :config
  (setq editorconfig-get-properties-function
        'editorconfig-core-get-properties-hash))

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
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode corfu-history-mode
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-auto-delay . 0.0)
  (corfu-auto-prefix . 2)
  (corfu-auto-prefix . 2)
  (corfu-on-exact-match . 'show)
  (corfu-quit-at-boundary . 'separator)
  (corfu-quit-no-match . t)
  (corfu-preselect . 'valid)
  (corfu-preselect-first . nil)
  (text-mode-ispell-word-completion . nil)
  (tab-always-indent 'complete)
  :config
  (with-eval-after-load 'meow
    (define-key corfu-map (kbd "<escape>")
                (lambda ()
                  (interactive)
                  (corfu-quit)
                  (meow-normal-mode))))

  (with-eval-after-load 'lsp-mode
    (setopt lsp-completion-provider :none))

  (with-eval-after-load 'orderless
    (defun my/orderless-for-corfu ()
      (setq-local orderless-matching-styles '(orderless-flex)))

    (add-hook 'corfu-mode-hook #'my/orderless-for-corfu))

  :hook
  (prog-mode . (lambda ()
                 (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command)))
  )

(leaf corfu-candidate-overlay
  :doc "Show first candidate in an overlay while typing"
  :req "emacs-28.1" "corfu-0.36"
  :tag "emacs>=28.1"
  :url "https://code.bsdgeek.org/adam/corfu-candidate-overlay/"
  :added "2025-01-15"
  :emacs>= 28.1
  :ensure t
  :after corfu)

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
  ((prog-mode-hook
    text-mode-hook
    emacs-lisp-mode-hook) . my/set-super-capf)
  (lsp-completion-mode-hook . (lambda ()
                                (add-to-list 'completion-at-point-functions 'lsp-completion-at-point)))
  ;; (lsp-completion-mode-hook . my/set-super-capf-lsp)
  :init
  (defun my-convert-capf (arg-capf)
    (list (cape-capf-case-fold arg-capf)
          (cape-company-to-capf #'company-yasnippet)
          #'cape-file
          #'cape-tex
          (cape-capf-case-fold #'cape-dabbrev)
          #'cape-keyword))
  ;; (defun my/set-lsp-capf ()
  ;;   (setq-local completion-at-point-functions (my-convert-capf #'lsp-completion-at-point)))
  ;; (defun my/set-basic-capf ()
  ;;   (setq-local completion-at-point-functions (my-convert-capf (car completion-at-point-functions))))

  (defun my/set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-noninterruptible
                       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          (if arg
                              arg
                            (car completion-at-point-functions))
                          #'cape-dabbrev
                          #'cape-file)
                         :sort t
                         :exclusive 'no))))))

  (defun my/set-super-capf-lsp (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-noninterruptible
                       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          (if arg
                              arg
                            (car completion-at-point-functions))
                          #'lsp-completion-at-point
                          #'cape-dabbrev
                          #'cape-file)
                         :sort t
                         :exclusive 'no))))))

  :config
  (setq cape-dabbrev-check-other-buffers nil)
  (advice-add 'codeium-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'codeium-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'codeium-completion-at-point :around #'cape-wrap-noninterruptible)

  (add-to-list 'completion-at-point-functions #'cape-tex t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-ispell t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block t)
  )

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs."
  :req "emacs-28.1" "consult-2.0"
  :tag "completion" "files" "matching" "emacs>=28.1"
  :url "https://github.com/minad/affe"
  :added "2025-03-05"
  :emacs>= 28.1
  :ensure t
  :after consult
  :custom ((affe-highlight-function . 'orderless-highlight-matches)
           (affe-regexp-function    . 'orderless-pattern-compiler)
           (affe-find-command       . "fd --color=never --full-path --hidden")
           )
  :config
  ;; (defun affe-orderless-regexp-compiler (input _type _ignorecase)
  ;;   (setq input (orderless-pattern-compiler input))
  ;;   (cons input (lambda (str) (orderless-highlight-matches input str))))
  ;; (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (with-eval-after-load 'orderless
    (setq affe-regexp-function #'orderless-pattern-compile)
    (setq affe-highlight-function #'orderless-highlight-matches))
  )

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :added "2021-06-13"
  :url "https://github.com/oantolin/orderless"
  :emacs>= 26.1
  :ensure t
  ;; :hook ((corfu-mode-hook . my/orderless-for-corfu)
  ;;        (lsp-completion-mode-hook . my/orderless-for-lsp-mode))
  ;; :init
  ;; (defun my/orderless-dispatch-flex-first (_pattern index _total)
  ;;   (and (eq index 0) 'orderless-flex))

  ;; (defun my/orderless-for-corfu ()
  ;;   (setq-local orderless-style-dispatchers '(my/orderless-dispatch-flex-first)))

  ;; (defun my/orderless-for-lsp-mode ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless)))
  :config
  ;; (with-eval-after-load 'migemo
  ;;   ;; orderlessをmigemo対応
  ;;   (defun orderless-migemo (component)
  ;;     (let ((pattern (downcase (migemo-get-pattern component))))
  ;;       (condition-case nil
  ;;           (progn (string-match-p pattern "") pattern)
  ;;         (invalid-regexp nil))))
  ;;   (add-to-list 'orderless-matching-styles 'orderless-migemo))
  :custom (
           (completion-styles . '(orderless basic))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion)))))
  )

(leaf corfu-prescient
  :doc "Prescient.el + Corfu"
  :req "emacs-27.1" "prescient-6.1.0" "corfu-1.1"
  :tag "extensions" "emacs>=27.1"
  :url "https://github.com/radian-software/prescient.el"
  :added "2025-01-17"
  :emacs>= 27.1
  :ensure t
  :after prescient corfu
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode +1)
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
  :defvar (kind-icon-default-face)
  :defun (kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face . 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (advice-add 'corfu--post-command :around
	      (lambda (func)
	        (condition-case err
		    (funcall func)
		  ((debug error) (signal (car err) (cdr err))))))
  )

(leaf nerd-icons-corfu
  :doc "Icons for Corfu via nerd-icons"
  :req "emacs-27.1" "nerd-icons-0.1.0"
  :tag "icons" "files" "convenience" "emacs>=27.1"
  :url "https://github.com/LuigiPiucco/nerd-icons-corfu"
  :added "2024-06-28"
  :emacs>= 27.1
  :ensure t
  :after nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :req "emacs-26.3"
  :tag "extensions" "emacs>=26.3"
  :added "2020-09-09"
  :url "http://elpa.gnu.org/packages/eldoc.html"
  :emacs>= 26.3
  :ensure t)

(leaf rg
  :doc "A search tool based on ripgrep"
  :req "emacs-26.1" "transient-0.3.0" "wgrep-2.1.10"
  :tag "tools" "matching" "emacs>=26.1"
  :url "https://github.com/dajva/rg.el"
  :added "2023-08-30"
  :emacs>= 26.1
  :ensure t
  :after wgrep
  :bind ((kbd "C-c s") . rg-menu)
  )

(leaf dumb-jump
  :doc "Jump to definition for 40+ languages without configuration"
  :req "emacs-24.3" "s-1.11.0" "dash-2.9.0" "popup-0.5.3"
  :tag "programming" "emacs>=24.3"
  :added "2020-09-09"
  :url "https://github.com/jacktasia/dumb-jump"
  :emacs>= 24.3
  :ensure t
  :custom ((dumb-jump-selector . 'vertico)))

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
  (require 'dap-dlv-go)
  (require 'dap-hydra)
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  ;; (dap-auto-configure-mode 1)
  ;; :custom
  ;; (dap-auto-configure-features . '(sessions locals breakpoints expressions repl controls tooltip))
  ;; (dap-go-debug-path . "/Users/tetsuhiromanome/ghq/github.com/golang/vscode-go/")
  ;; (dap-go-debug-program . `("node", (f-join dap-go-debug-path "dist/debugAdapter.js")))
  )

(leaf dap-chrome
  :doc "Debug Adapter Protocol mode for Chrome"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-4.0"
  :tag "out-of-MELPA" "languages" "emacs>=25.1"
  :url "https://github.com/yyoncho/dap-mode"
  :added "2023-02-16"
  :emacs>= 25.1
  ;; :el-get {{user}}/dap-chrome
  :after lsp-mode
  :require t)

(leaf dap-firefox
  :doc "Debug Adapter Protocol mode for Firefox"
  :req "emacs-25.1" "dash-2.14.1" "lsp-mode-4.0"
  :tag "out-of-MELPA" "languages" "emacs>=25.1"
  :url "https://github.com/yyoncho/dap-mode"
  :added "2023-02-16"
  :emacs>= 25.1
  ;; :el-get {{user}}/dap-firefox
  :after lsp-mode
  :require t)

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
  (lsp-prefer-flymake                 . nil)
  (lsp-print-performance              . t)
  (lsp-eldoc-render-all               . t)
  (lsp-enable-completion-at-point     . t)
  (lsp-enable-xref                    . t)
  (lsp-keep-workspace-alive           . nil)
  (lsp-enable-snippet                 . t)
  (lsp-enable-indentation             . t)
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
  (lsp-rust-server                    . 'rust-analyzer)
  (lsp-rust-analyzer-cargo-watch-command . "clippy")

  ;; for vue-mode
  (lsp-vetur-format-default-formatter-css . "none")
  (lsp-vetur-format-default-formatter-html . "none")
  (lsp-vetur-format-default-formatter-js . "none")
  (lsp-vetur-validation-template . nil)
  :config
  (setq lsp-eslint-download-url "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/dbaeumer/vsextensions/vscode-eslint/3.0.13/vspackage")
  (lsp-register-custom-settings
   '(("gopls.hints" ((assignVariableTypes . t)
                     (compositeLiteralFields . t)
                     (compositeLiteralTypes . t)
                     (constantValues . t)
                     (functionTypeParameters . t)
                     (parameterNames . t)
                     (rangeVariableTypes . t)))))
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
    (lsp-ui-sideline-mode       . t)
    (lsp-ui-sideline-show-code-actions . nil)
    (lsp-ui-sideline-show-diagnostics  . t)
    (lsp-ui-doc-enable            . t)
    ;; (lsp-ui-doc-alignment         . 'frame)
    (lsp-ui-doc-position          . 'top)
    ;; (lsp-ui-doc-use-childframe    . t)
    (lsp-ui-doc-deley             . 0.5)
    ;; (lsp-ui-doc-header            . t)
    ;; (lsp-ui-doc-include-signature . t)
    ;; (lsp-ui-doc-max-width         . 150)
    ;; (lsp-ui-doc-max-height        . 30)
    (lsp-ui-doc-show-with-cursor  . t)
    (lsp-ui-doc-show-with-mouse   . t)
    (lsp-ui-peek-enable           . t)
    (lsp-ui-peek-peek-height      . 20)
    (lsp-ui-peek-list-width       . 50)
    (lsp-ui-peek-fontify          . 'on-demand) ;; never, on-demand, or always
    :config
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

(leaf lsp-eslint
  :doc "lsp-mode eslint integration"
  :tag "out-of-MELPA" "languages"
  :added "2024-07-22"
  :after lsp-mode
  :config
  (setq lsp-eslint-server-command `("node"
                                    "/Users/tetsuhiromanome/repos/vscode-eslint/server/out/eslintServer.js"
                                    "--stdio"))
  :require t)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "transient-20200601" "with-editor-20200522"
  :straight (magit :type git :host github :repo "magit/magit" :tags v4.3.4)
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-27"
  :emacs>= 25.1
  ;; :ensure t
  :after git-commit with-editor
  :custom
  (magit-completing-read-function . 'ido-completing-read)
  (magit-refs-show-commit-count   . 'branch)
  (magit-log-buffer-file-locked   . t)
  (magit-revision-show-gravatars  . nil)
  )

(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :req "cl-lib-0.2" "emacs-25.1"
  :tag "diff" "vc" "emacs>=25.1"
  :url "https://github.com/dgutov/diff-hl"
  :added "2024-06-28"
  :emacs>= 25.1
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  )

;; (leaf difftastic
;;   :doc "Wrapper for difftastic"
;;   :req "emacs-28.1" "compat-29.1.4.2" "magit-20220326"
;;   :tag "diff" "tools" "emacs>=28.1"
;;   :url "https://github.com/pkryger/difftastic.el"
;;   :added "2024-06-28"
;;   :emacs>= 28.1
;;   :ensure t
;;   :after compat magit
;;   :bind ((:map magit-blame-read-only-mode-map
;;               ("D" . difftastic-magit-show)
;;               ("S" . difftastic-magit-show)))
;;   :config
;;   (eval-after-load 'magit-diff
;;     '(transient-append-suffix 'magit-diff '(-1 -1)
;;        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;         ("S" "Difftastic show" difftastic-magit-show)])))

(leaf forge
  :doc "Access Git forges from Magit."
  :req "emacs-25.1" "compat-29.1.3.4" "closql-1.2.0" "dash-2.19.1" "emacsql-sqlite-3.0.0" "ghub-20220621" "let-alist-1.0.6" "magit-20220621" "markdown-mode-2.4" "yaml-0.3.5"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :straight (forge :type git :host github :repo "magit/forge" :tags v0.5.0)
  :added "2023-02-14"
  :emacs>= 25.1
  :after compat closql emacsql-sqlite ghub magit markdown-mode yaml)

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
  (global-git-gutter-mode +1)
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
  :hook
  (magit-log-mode-hook . emoji-chat-sheet-plus-display-mode)
  )

(leaf hideshow
  :doc "minor mode cmds to selectively display code/comment blocks"
  :tag "builtin"
  :added "2020-11-27"
  :hook
  ((typescript-mode-hook . (lambda ()
                             (hs-minor-mode 1))))
  )

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text."
  :req "emacs-28.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=28.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2025-03-04"
  :emacs>= 28.1
  :ensure t
  :hook
  (markdown-mode-hook . turn-off-auto-fill)
  (markdown-mode-hook . turn-on-visual-line-mode))

(leaf org
  :doc "Export Framework for Org Mode"
  :tag "builtin"
  :added "2020-08-28"
  :custom
  (org-startup-indented . t)
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
  (org-src-tab-acts-natively . t)
  (org-image-actual-width . nil)
  '((org-modules . (org-modules org-tempo)))
  :hook
  (org-mode-hook . hl-todo-mode)
  (org-mode-hook . turn-off-auto-fill)
  (org-mode-hook . turn-on-visual-line-mode)
  :config
  (setq org-export-latex-coding-system 'euc-jp-unix)
  (setq org-export-latex-date-format "%Y-%m-%d")
  (setq org-export-latex-classes nil)
  (add-to-list 'org-export-latex-classes
               '("jarticle"
                 "\\documentclass[a4j]{jarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 ))
  )

(leaf org-roam
  :doc "A database abstraction layer for Org-mode"
  :req "emacs-26.1" "dash-2.13" "org-9.4" "emacsql-20230228" "magit-section-3.0.0"
  :tag "convenience" "roam" "org-mode" "emacs>=26.1"
  :url "https://github.com/org-roam/org-roam"
  :added "2024-01-22"
  :emacs>= 26.1
  :ensure t
  :after org emacsql magit-section
  :custom
  (org-roam-directory . "~/Documents/org-files")
  (org-roam-db-location . "~/Documents/org-files/database.db")
  (org-roam-index-file . "~/Documents/org-files/index.org")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (org-roam-db-autosync-mode)
  :hook
  (after-init-hook . org-roam-mode))

(leaf consult-org-roam
  :doc "Consult integration for org-roam"
  :req "emacs-27.1" "org-roam-2.2.0" "consult-0.16"
  :tag "emacs>=27.1"
  :url "https://github.com/jgru/consult-org-roam"
  :added "2024-01-22"
  :emacs>= 27.1
  :ensure t
  :after org-roam consult)

(leaf org-re-reveal
  :doc "Org export to reveal.js presentations"
  :req "emacs-24.4" "org-8.3" "htmlize-1.34"
  :tag "oer" "presentation" "slideshow" "hypermedia" "outlines" "tools" "emacs>=24.4"
  :url "https://gitlab.com/oer/org-re-reveal"
  :added "2023-02-02"
  :emacs>= 24.4
  :ensure t
  :after org
  :custom
  (org-re-reveal-root . "file:///Users/tetsuhiromanome/ghq/github.com/hakimel/reveal.js"))

;; Languages
(leaf go-mode
  :doc "Major mode for the Go programming language."
  :req "emacs-26.1"
  :tag "go" "languages" "emacs>=26.1"
  :url "https://github.com/dominikh/go-mode.el"
  :added "2025-02-25"
  :emacs>= 26.1
  :ensure t
  :custom (gofmt-command . "goimports")
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-ts-mode . lsp-deferred)
  ;; (go-mode-hook . lsp-inlay-hints-mode)
  (go-mode-hook . smartparens-mode))

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
  :config
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-config "~/.golangci.yml")
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
  )

(leaf kotlin-mode
  :doc "Major mode for kotlin"
  :req "emacs-24.3"
  :tag "languages" "emacs>=24.3"
  :url "https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode"
  :added "2024-01-22"
  :emacs>= 24.3
  :ensure t)

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
    :req "emacs-26.1" "lsp-mode" "dash-2.14.1" "ht-2.0"
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
  :custom
  (rust-format-on-save . t)
  )

(leaf cargo
  :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
  :req "emacs-24.3" "markdown-mode-2.4"
  :tag "tools" "emacs>=24.3"
  :added "2024-07-23"
  :emacs>= 24.3
  :ensure t
  :after markdown-mode
  :hook
  (rust-mode . cargo-minor-mode))

;; (leaf jtsx
;;   :doc "Extends JSX/TSX built-in support"
;;   :req "emacs-29.1"
;;   :tag "languages" "emacs>=29.1"
;;   :url "https://github.com/llemaitre19/jtsx"
;;   :added "2024-06-28"
;;   :emacs>= 29.1
;;   :ensure t
;;   :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
;;          ("\\.tsx\\'" . jtsx-tsx-mode)
;;          ("\\.ts\\'" . jtsx-typescript-mode))
;;   :commands jtsx-install-treesit-language
;;   :hook
;;   (jtsx-jsx-mode . hs-minor-mode)
;;   (jtsx-tsx-mode . hs-minor-mode)
;;   (jtsx-typescript-mode . hs-minor-mode)
;;   :custom
;;   ;; Optional customizations
;;   ;; (js-indent-level 2)
;;   ;; (typescript-ts-mode-indent-offset 2)
;;   ;; (jtsx-switch-indent-offset 0)
;;   (jtsx-indent-statement-block-regarding-standalone-parent    . nil)
;;   (jtsx-jsx-element-move-allow-step-out                       . t)
;;   (jtsx-enable-jsx-electric-closing-element                   . t)
;;   (jtsx-enable-electric-open-newline-between-jsx-element-tags . t)
;;   (jtsx-enable-jsx-element-tags-auto-sync                     . t)
;;   (jtsx-enable-all-syntax-highlighting-features               . t)
;;   :config
;;   (defun jtsx-bind-keys-to-mode-map (mode-map)
;;     "Bind keys to MODE-MAP."
;;     (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
;;     (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
;;     (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
;;     (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
;;     (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
;;     (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
;;     (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
;;     (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
;;     (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
;;     (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
;;     (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
;;     (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
;;     (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node))

;;   (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
;;     (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

;;   (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
;;     (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

;;   (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
;;   (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(leaf add-node-modules-path
  :doc "Add node_modules to your exec-path"
  :req "s-1.12.0"
  :tag "eslint" "node_modules" "node" "javascript"
  :url "https://github.com/codesuki/add-node-modules-path"
  :added "2025-02-12"
  :ensure t)

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
  ;; :defvar (web-mode-indentation-params)
  :config
  (pangu-spacing-mode nil)
  ;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  ;; (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))
  :custom
  (web-mode-css-indent-offset                . 2)
  (web-mode-markup-indent-offset             . 2)
  (web-mode-css-indent-offset                . 2)
  (web-mode-code-indent-offset               . 2)
  (web-mode-enable-current-element-highlight . t)
  (web-mode-attr-indent-offset               . 2)
  (lsp-eslint-enable                         . t)
  (lsp-eslint-auto-fix-on-save               . t)
  :hook
  (web-mode-hook . lsp-deferred)
  (web-mode-hook . smartparens-mode)
  )

(leaf emmet-mode
  :doc "Unofficial Emmet's support for emacs"
  :tag "convenience"
  :url "https://github.com/smihica/emmet-mode"
  :added "2023-09-11"
  :ensure t
  :hook
  (html-mode-hook       . emmet-mode)
  (web-mode-hook        . emmet-mode)
  (css-mode-hook        . emmet-mode)
  (js-mode-hook         . emmet-mode)
  (typescript-mode-hook . emmet-mode))

(leaf typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  :hook
  (typescript-mode-hook . lsp-deferred)
  :config
  (eval-after-load 'typescript-mode
    '(progn
       (add-hook 'typescript-mode-hook #'add-node-modules-path)
       (add-hook 'typescript-mode-hook #'prettier-js-mode)))
  )

(leaf tsc
  :doc "Core Tree-sitter APIs."
  :req "emacs-25.1"
  :tag "tree-sitter" "dynamic-modules" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
  :added "2025-02-25"
  :emacs>= 25.1
  :ensure t)

(leaf tree-sitter
  :doc "Incremental parsing system."
  :req "emacs-25.1" "tsc-0.18.0"
  :tag "tree-sitter" "parsers" "tools" "languages" "emacs>=25.1"
  :url "https://github.com/emacs-tree-sitter/elisp-tree-sitter"
  :added "2025-02-25"
  :emacs>= 25.1
  :ensure t
  :after tsc
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; TSXの対応
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  ;; ハイライトの追加
  (tree-sitter-hl-add-patterns 'tsx
                               [
                                ;; styled.div``
                                (call_expression
                                 function: (member_expression
                                            object: (identifier) @function.call
                                            (.eq? @function.call "styled"))
                                 arguments: ((template_string) @property.definition
                                             (.offset! @property.definition 0 1 0 -1)))
                                ;; styled(Component)``
                                (call_expression
                                 function: (call_expression
                                            function: (identifier) @function.call
                                            (.eq? @function.call "styled"))
                                 arguments: ((template_string) @property.definition
                                             (.offset! @property.definition 0 1 0 -1)))
                                ]))

;; typescript
;; (leaf treesit
;;   :doc "tree-sitter utilities"
;;   :tag "out-of-MELPA" "languages" "tree-sitter" "treesit"
;;   :added "2025-01-17"
;;   :require t
;;   :mode (("\\.tsx\\'" . tsx-ts-mode)
;;          ("\\.js\\'"  . typescript-ts-mode)
;;          ("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.ts\\'"  . typescript-ts-mode)
;;          ("\\.jsx\\'" . tsx-ts-mode))
;;   :config
;;   (setq treesit-font-lock-level 4)
;;   :hook
;;   ((tsx-ts-mode
;;     typescript-ts-mode
;;     js-ts-mode) . lsp-deferred)
;;   )
;; (leaf treesit-auto
;;   :doc "Automatically use tree-sitter enhanced major modes"
;;   :req "emacs-29.0"
;;   :tag "convenience" "fallback" "mode" "major" "automatic" "auto" "treesitter" "emacs>=29.0"
;;   :url "https://github.com/renzmann/treesit-auto.git"
;;   :added "2025-01-17"
;;   :emacs>= 29.0
;;   :ensure t
;;   :config
;;   (setq treesit-auto-install t)
;;   (global-treesit-auto-mode)
;;   )
;; (leaf tide
;;   :doc "Typescript Interactive Development Environment"
;;   :req "emacs-25.1" "dash-2.10.0" "s-1.11.0" "flycheck-27" "typescript-mode-0.1" "cl-lib-0.5"
;;   :tag "typescript" "emacs>=25.1"
;;   :added "2020-08-27"
;;   :url "http://github.com/ananthakumaran/tide"
;;   :emacs>= 25.1
;;   :ensure t
;;   :after flycheck typescript-mode
;;   :hook
;;   (tsx-ts-mode    . setup-tide-mode)
;;   (tide-mode-hook . lsp-deferred)
;;   (tide-mode-hook . prettier-js-mode)
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     ))

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
  :hook
  (yaml-mode-hook . lsp-deferred)
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

(leaf cfn-mode
  :doc "AWS cloudformation mode"
  :req "emacs-27.0" "f-0.20.0" "s-1.12.0" "yaml-mode-0.0.13"
  :tag "tools" "languages" "convenience" "emacs>=27.0"
  :url "https://gitlab.com/worr/cfn-mode"
  :added "2024-06-26"
  :emacs>= 27.0
  :ensure t
  :after yaml-mode)

(leaf lsp-cfn
  :doc "LSP integration for cfn-lsp-extra"
  :req "emacs-27.0" "lsp-mode-8.0.0" "yaml-mode-0.0.15"
  :tag "emacs>=27.0"
  :url "https://github.com/LaurenceWarne/lsp-cfn.el"
  :added "2024-06-26"
  :emacs>= 27.0
  :ensure t
  :after lsp-mode yaml-mode)

(leaf flycheck-cfn
  :doc "Flycheck backend for AWS cloudformation"
  :req "emacs-27.0" "flycheck-31"
  :tag "convenience" "emacs>=27.0"
  :url "https://gitlab.com/worr/cfn-mode"
  :added "2024-06-26"
  :emacs>= 27.0
  :ensure t
  :after flycheck
  :config
  )

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
  :req "dash-2.14.1" "tramp-container" "emacs-24.5" "json-mode-1.7.0" "s-1.12.0" "tablist-0.70" "transient-0.2.0"
  :tag "convenience" "filename" "emacs>=24.5"
  :added "2020-08-27"
  :url "https://github.com/Silex/docker.el"
  :emacs>= 24.5
  :ensure t
  :after tramp-container json-mode tablist)

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24" "s-1.12"
  :tag "emacs>=24"
  :added "2020-11-25"
  :url "https://github.com/spotify/dockerfile-mode"
  :emacs>= 24
  :ensure t)

;; (leaf ht
;;   :doc "The missing hash table library for Emacs."
;;   :req "dash-2.12.0"
;;   :tag "hash" "hash map" "hash table"
;;   :url "https://github.com/Wilfred/ht.el"
;;   :added "2025-02-25"
;;   :ensure t)

;; (leaf lsp-sqls
;;   :doc "SQL Client settings"
;;   :tag "out-of-MELPA" "lsp" "sql"
;;   :added "2022-02-09"
;;   :require t ht
;;   :config
;;   (setq lsp-sqls-workspace-config-path nil)
;;   (setq lsp-sqls-connections
;;         '(
;;           ((alias . "wlt_core") (driver . "mysql") (dataSourceName . "root:rootpass@tcp(127.0.0.1:3308)/core"))
;;           ((alias . "wlt_auth") (driver . "mysql") (dataSourceName . "root:rootpass@tcp(127.0.0.1:3308)/auth"))
;;           ((alias . "pokerroom") (driver . "mysql") (dataSourceName . "root:@tcp(127.0.0.1:3306)/pokerroom"))
;;           ))
;;   :hook
;;   (sql-mode-hook . lsp-deferred)
;;   )

(leaf sqlup-mode
  :doc "Upcase SQL words for you"
  :tag "upcase" "redis" "tools" "sql"
  :added "2020-08-27"
  :url "https://github.com/trevoke/sqlup-mode.el"
  :ensure t)

(leaf sql-indent
  :doc "Support for indenting code in SQL files."
  :req "cl-lib-0.5"
  :tag "sql" "languages"
  :url "https://github.com/alex-hhh/emacs-sql-indent"
  :added "2024-07-03"
  :ensure t
  :hook
  (sql-mode . sqlind-minor-mode))

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
  (sqlformat-command . 'sqlfluff)
  (sqlformat-args . '("--dialect" "mysql"))
  :hook
  (sql-mode-hook . 'sql-format-on-save-mode)
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
  :defvar lsp-disabled-clients
  :config
  (setq lsp-disabled-clients '(tfls))
  :custom
  (lsp-terraform-ls-enable-show-reference     . t)
  (lsp-semantic-tokens-enable                 . nil)
  (lsp-semantic-tokens-honor-refresh-requests . nil)
  (lsp-enable-links                           . t)
  :hook
  (terraform-mode-hook . tree-sitter-hl-mode)
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

;; (leaf plantuml-mode
;;   :doc "Major mode for PlantUML"
;;   :req "dash-2.0.0" "emacs-25.0"
;;   :tag "ascii" "plantuml" "uml" "emacs>=25.0"
;;   :added "2022-02-23"
;;   :emacs>= 25.0
;;   :ensure t)

(leaf mermaid-mode
  :doc "major mode for working with mermaid graphs"
  :req "f-0.20.0" "emacs-25.3"
  :tag "processes" "tools" "graphs" "mermaid" "emacs>=25.3"
  :url "https://github.com/abrochard/mermaid-mode"
  :added "2022-05-04"
  :emacs>= 25.3
  :ensure t)

(leaf d2-mode
  :doc "Major mode for working with d2 graphs"
  :req "emacs-26.1"
  :tag "processes" "tools" "graphs" "d2" "emacs>=26.1"
  :url "https://github.com/andorsk/d2-mode"
  :added "2022-12-26"
  :emacs>= 26.1
  :ensure t
  :custom
  (d2-flags . '("--layout=elk" "--theme=104"))
  :config
  (setq d2-output-format ".png")
  )

(leaf ob-d2
  :tag "out-of-MELPA"
  :req "emacs-26.1"
  :added "2023-01-04"
  :url "https://github.com/dmacvicar/ob-d2"
  :vc (ob-d2 :url "https://github.com/dmacvicar/ob-d2.git")
  :require t
  )

(leaf ob-mermaid
  :doc "Org-babel support for mermaid evaluation"
  :tag "lisp"
  :url "https://github.com/arnm/ob-mermaid"
  :added "2025-02-25"
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
  )

(leaf org
  :doc "Outline-based notes management and organizer"
  :tag "builtin"
  :added "2023-01-04"
  :after ob-d2
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (mermaid . t)
     (d2 . t)))
  )

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

(leaf garbage-collection-config
  :setq
  (gc-cons-percentage          . 0.2)
  (gc-cons-threshold           . 134217728)
  (read-process-output-max     . 4194304)
  (garbage-collection-messages . t)
  :config
  (add-function :after after-focus-change-function #'garbage-collect)
  )

(leaf emacs
  )

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(transient-dwim leaf-convert leaf-tree hydra blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here

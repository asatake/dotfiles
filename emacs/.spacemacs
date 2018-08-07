;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are laz%Sy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; lang
     go
     (elm :variables
          elm-format-command "elm-format")
     python

     ruby
     html
     java
     javascript
     typescript
     php
     erlang
     latex
     elixir
     yaml
     c-c++
     ;; framework
     ;; react
     ;; middleware
     nginx
     docker
     helm
     ivy
     emacs-lisp
     csv
     git
     github
     markdown
     pandoc
     emoji
     colors
     ;; pdf-tools
     japanese
     twitter
     search-engine
     (sql :variables
          sql-capitalize-keywords t
          sql-completion-column t
          sql-completion-sqlbuf t
          sql-capitalize-keywords-black-list '("name" "date" "month"))
     (org :variables
          org-enable-reveal-js-support t
          org-enable-github-support t
          org-latex-listings t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables enable-flyspell-auto-completion t)
     better-defaults
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     version-control
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '
   (
    editorconfig
    quickrun
    ;; php
    flymake-php
    flymake-phpcs
    php-eldoc
    php-auto-yasnippets
    phpunit
    ac-php
    string-inflection
    all-the-icons
    codic
    org-preview-html
    vue-mode
    visual-regexp-steroids
    )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; lastest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(
                               "M+ 1m"
                               :size 14
                               :weight medium
                               :width medium
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "M-m"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "M-x"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; (if window-system (progn
  ;;                     (when (equal system-type 'darwin)
  ;;                       (setq mac-option-modifire 'meta)))
  ;;   )
  (add-to-list 'exec-path (expand-file-name "~/homebrew/bin"))
  (add-to-list 'exec-path (expand-file-name "~/dev/go-workspace/bin"))
  (server-start)
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; (global-set-key (kbd "\C-h") 'delete-backward-char)
  (bind-key "C-h" #'delete-backward-char)
  (global-set-key (kbd "\C- ") 'mark)
  (global-set-key (kbd "<M-down>") (kbd "C-u 5 C-n"))
  (global-set-key (kbd "<M-up>") (kbd "C-u 5 C-p"))
  (global-set-key (kbd "M-%") 'vr/query-replace)
  (define-key global-map [?\¥] [?\\])
  (define-key global-map [?\C-¥] [?\C-\\])
  (define-key global-map [?\M-¥] [?\M-\\])
  (define-key global-map [?\C-\M-¥] [?\C-\M-\\])
  (set-face-font 'default "M+ 1m-14")
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (defalias 'ps-mule-header-string-charsets 'ignore)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; window-maximized
  ;; 画面が最大化されている or NOTの状態を保持
  (defvar is-window-maximized nil)

  ;; 1. 最大化されている場合
  ;;  -> `balance-windows` で画面のバランスを調整
  ;; 2. 最大化されていない場合
  ;;  -> `maximize-window` で画面を最大化

  (defun window-temp-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized))))
  (global-set-key (kbd "<C-M-return>") 'window-temp-maximize)

  (setq display-time-24hr-format t)
  (setq display-time-format "%m/%d %H:%M")
  (setq display-time-default-load-average nil)
  (setq display-time-mail-string "")
  (display-time-mode 1)

  (global-company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

  ;; magit
  (setq git-commit-summary-max-length 100)

  ;; (add-hook 'php-mode 'flycheck-mode)
  (add-hook 'php-mode 'flymake-php-load)
  (add-hook 'php-mode 'flymake-phpcs-load)
  (editorconfig-mode 1)
  (add-hook 'php-mode-hook (lambda () (subword-mode 1)))

  (setq php-mode-coding-style (quote psr2))
  ;; spell-checking
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))

  ;; dictionary
  (push '(eow
          :name "eow"
          :url "http://eow.alc.co.jp/%s/UTF-8/")
        search-engine-alist)
  (setq codic-api-token "ST1BeRleZtOjWgEPHQNMckoWvBbiweNXT")

  ;; flycheck
  (setq flycheck-elixir-credo-strict t)
  (setq flycheck-phpcs-standard "~/src/ruleset.xml")
  ;; (setq flycheck-phpmd-rulesets "~/src/mdruleset.xml")

  ;; shell
  (setq exec-path-from-shell-check-startup-files nil)

  ;; rainbow
  (add-hook 'css-mode-hook 'rainbow-mode)

  ;; twitter
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode nil)
  (setq twittering-status-format "%s\(%S\) %p: \n %T \n [%@]%r %R %f\n ------------------------------")

  ;; org-capture
  ;; (setq org-capture-templates
  ;;       '(
  ;;         ("n" "note" entry (file (expand-file-name (concat org-directory "/notes.org")))
  ;;          "#+OPTIONS: toc:nil ^:{}\n%?")
  ;;         ("t" "task" entry (file (expand-file-name (concat org-directory "~/org/tasks.org")))
  ;;          "* TODO %?\n    %T")
  ;;         )
  ;;       )

  ;; org-qiita
  ;; (setq org-qiita-token "03f7d5a7f6633d9108382899566ab937378b5130")

  ;;org-babel
  (setq org-plantuml-jar-path "~/my_setting/plantuml.jar")
  (require 'ob-python)
  (require 'ob-ruby)
  (require 'ob-clojure)
  ;; (require 'ob-perl)
  ;; (require 'ob-dot)
  ;; (require 'ob-R)
  (require 'ob-gnuplot)
  (require 'ob-lisp)
  (require 'ob-org)
  (require 'ob-screen)
  ;; (require 'ob-calc)
  (require 'ob-js)
  (require 'ob-latex)
  (require 'ob-plantuml)
  ;; (require 'ob-ditaa)
  ;; (require 'ob-awk)
  ;; (require 'ob-octave)
  (require 'ob-sed)
  ;; (require 'ob-sh)
  (require 'ob-sql)
  ;; (require 'ob-sqlite)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '( ;; (perl . t)
      ;; (dot . t)
      ;; (R . t)
      (gnuplot . t)
      (clojure . t)
      ;; (graphviz . t)
      (lisp . t)
      ;;    (stan . t)
      (org . t)
      (screen . t)
      ;; (calc . t)
      (js . t)
      (latex . t)
      (plantuml . t)
      (ruby . t)
      (shell . t)
      (python . t)
      (emacs-lisp . t)
      ;; (ditaa . t)
      ;; (awk . t)
      ;; (octave . t)
      (sed . t)
      (sql . t)
      ;; (sqlite . t)
      ))
  ;; all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; auto-insert
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-directory "~/.template/")
  (define-auto-insert "\\.org$" "template.org")

  ;; latex-settings
  (setq org-latex-packages-alist
        '(("" "graphicx" t)
          ("" "hyperref" nil)))
  (setq org-latex-pdf-process
        '("ptex2pdf -u -l %b"))
  (setq org-latex-classes '(("jsarticle"
                             "\\documentclass{jsarticle}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage[dvipdfmx]{hyperref}
\\setlength{\\textheight}{\\paperheight}
\\setlength{\\topmargin}{-3.0truemm}
\\addtolength{\\topmargin}{-\\headheight}
\\addtolength{\\topmargin}{-\\headsep}
\\addtolength{\\textheight}{-55truemm}
\\usepackage{ascmac}
\\usepackage{listings}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")
)
("twocolumn"
 "\\documentclass[twocolumn, 10pt]{jsarticle}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage[dvipdfmx]{hyperref}
\\setlength{\\textheight}{\\paperheight}
\\setlength{\\topmargin}{-3.0truemm}
\\addtolength{\\topmargin}{-\\headheight}
\\addtolength{\\topmargin}{-\\headsep}
\\addtolength{\\textheight}{-55truemm}
\\set length{\\textwidth}{\\paperwidth}
\\setlength{\\oddsidemargin}{-2.5truemm}
\\setlength{\\evensidemargin}{-2.5truemm}
\\addtolength{\\textwidth}{-45truemm}
\\setlength{\\columnsep}{3zw}
\\usepackage{ascmac}
\\usepackage{listings}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")
)
("beamer"
 "\\documentclass\[dvipdfmx, 12pt, presentation\]\{beamer\}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
 ("\\section\{%s\}" . "\\section*\{%s\}")
 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
 ("\\paragraph\{%s\}" . "\\paragraph*\{%s\}")
 ("\\subparagraph\{%s\}" . "\\subparagraph*\{%s\}")
 )
))
(setq org-latex-listings t)
(setq org-latex-default-class "jsarticle")
(setq org-latex-with-hyperref nil)
(setq org-src-fontify-natively t)
;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets wgrep web-mode toc-org tide typescript-mode sql-indent rspec-mode restart-emacs pyvenv pippel pipenv pip-requirements persp-mode pandoc-mode org-projectile org-category-capture org-preview-html org-mime org-download org-brain neotree meghanada magithub ghub+ apiwrap magit-gh-pulls ivy-xref ivy-hydra hl-todo highlight-parentheses helm-make groovy-mode godoctor go-rename go-guru git-timemachine git-link flycheck-rtags flycheck-pos-tip expand-region evil-nerd-commenter evil-mc evil-magit evil-iedit-state evil-escape erlang ensime emojify emmet-mode editorconfig dumb-jump dockerfile-mode docker json-mode define-word ddskk counsel-projectile projectile company-anaconda browse-at-remote avy-migemo anaconda-mode aggressive-indent ace-window ac-php-core company counsel swiper helm helm-core highlight smartparens elixir-mode flycheck window-purpose imenu-list rtags ivy multiple-cursors avy skewer-mode js2-mode markdown-mode magit magit-popup git-commit ghub with-editor yasnippet php-mode pythonic spaceline memoize mmm-mode which-key use-package org-plus-contrib hydra yapfify yaml-mode xterm-color xcscope ws-butler winum web-beautify vue-mode volatile-highlights visual-regexp-steroids vi-tilde-fringe uuidgen unfill twittering-mode tagedit tablist symon string-inflection sqlup-mode spaceline-all-the-icons smex smeargle slim-mode shell-pop scss-mode scala-mode sbt-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop robe request rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters quickrun pytest pyenv-mode py-isort pug-mode powerline popwin phpunit phpcbf php-extras php-eldoc php-auto-yasnippets password-generator paradox pangu-spacing ox-reveal ox-pandoc ox-gfm overseer orgit org-present org-pomodoro org-bullets open-junk-file ob-elixir nginx-mode nameless mwim mvn multi-term move-text minitest migemo maven-test-mode markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode link-hint json-snatcher json-reformat json-navigator js2-refactor js-doc japanese-holidays ivy-yasnippet ivy-rtags ivy-purpose indent-guide importmagic impatient-mode iedit hungry-delete highlight-numbers highlight-indentation groovy-imports gradle-mode google-translate google-c-style golden-ratio go-tag go-impl go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-messenger git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy font-lock+ flyspell-popup flyspell-correct-ivy flymake-phpcs flymake-php flycheck-mix flycheck-elm flycheck-credo flx-ido fill-column-indicator fancy-battery eyebrowse evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor-ja evil-surround evil-org evil-numbers evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-goggles evil-exchange evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help engine-mode emoji-cheat-sheet-plus elm-mode elisp-slime-nav drupal-mode dotenv-mode docker-tramp disaster diminish diff-hl cython-mode csv-mode counsel-css company-web company-tern company-statistics company-rtags company-quickhelp company-php company-go company-emoji company-emacs-eclim company-c-headers company-auctex column-enforce-mode color-identifiers-mode codic clean-aindent-mode clang-format chruby centered-cursor-mode cdb ccc bundler bind-key auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk async alchemist ace-link ac-php ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)

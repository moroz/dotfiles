;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.


(defun km/get-font ()
  (setq default-font "FiraMono NF")
  (setq base-size 18)
  (setq screencast-size 28)
  (setq is-screencast (not (eq (getenv "SCREENCAST") nil)))
  (setq is-retina (and (string-equal system-type "gnu/linux") (string-equal (shell-command-to-string "gsettings get org.cinnamon.desktop.interface scaling-factor") "uint32 2\n")))
  (setq size (if is-screencast screencast-size base-size))
  (setq scaling-factor (if is-retina 2 1))
  (setq font-size (* scaling-factor size))
  (setq powerline-scale (if is-retina 0.1 0.8))
  (list default-font :size font-size :powerline-scale 0.75)
  )

(km/get-font)

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; a layer lazemacs lint configily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(rust
     docker
     erlang
     javascript
     ansible
     lsp
     neotree
     nginx
     ruby
     sql
     systemd
     typescript
     terraform
     yaml
     (elixir :variables
             elixir-backend 'lsp)
     html osx latex
     markdown ivy git
     (chinese :variables
              chinese-enable-fcitx (string-equal system-type "gnu/linux")
              )
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking auto-completion
     version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(helm-ag basic-theme base16-theme exec-path-from-shell color-theme-modern prettier-js graphql-mode mmm-mode rjsx-mode moe-theme afternoon-theme doom-themes simpleclip lsp-sourcekit format-sql)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes (if (display-graphic-p) '(doom-tomorrow-night spacemacs-dark moe-dark doom-laserwave base16-gruvbox-dark-medium base16-material-palenight base16-solarized-dark base16-monokai) '(base16-default-dark moe-dark spacemacs-dark))
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font (km/get-font)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
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
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
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
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
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
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
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
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ;; (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun my-tsx-setup-hook ()
  (company-mode)
  (lsp)
  (mmm-mode)
  (tide-setup)
  (prettier-js-mode)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (global-hl-line-mode -1)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Make evil mode recognize snake_case words
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
        ad-do-it)))
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)
  (setq mac-control-modifier 'super)
  (use-package lsp-sourcekit)
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
  (add-hook 'swift-mode-hook (lambda () (lsp)))
  (add-hook 'rjsx-mode-hook (lambda () (lsp)))
  (add-hook 'js2-mode-hook (lambda () (lsp)))
  (add-hook 'web-mode-hook (lambda () (lsp)))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; Key mappings
  (global-set-key (kbd "<f2>") 'lsp-rename)
  (global-set-key (kbd "<f8>") 'multi-term)
  (global-set-key (kbd "<f9>") 'neotree-project-dir-toggle)
  ;; (global-set-key (kbd "<f10>") 'save-buffers-kill-terminal)

  (spacemacs/set-leader-keys "s q p" 'sql-postgres)
  (spacemacs/set-leader-keys "s n" 'smerge-next)
  (spacemacs/set-leader-keys "s l" 'smerge-keep-other)
  (spacemacs/set-leader-keys "s m" 'smerge-keep-mine)
  (spacemacs/set-leader-keys "s -" 'smerge-keep-all)
  (spacemacs/set-leader-keys "m t" 'alchemist-mix-test-this-buffer)
  (spacemacs/set-leader-keys "m a" 'alchemist-mix-test)
  (global-set-key (kbd "<M-up>") 'move-text-line-up)
  (global-set-key (kbd "<M-down>") 'move-text-line-down)
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-n") 'make-frame-command)
  (define-key evil-normal-state-map (kbd "C-S-p") 'helm-M-x)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-`") 'other-frame)

  (display-time-mode 1)
  (simpleclip-mode 1)
  (define-key evil-normal-state-map (kbd "C-v") 'simpleclip-paste)
  (defun set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and (projectile-project-p)
                                     (projectile-locate-dominating-file buffer-file-name
                                                                        ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))

  (advice-add 'elixir-format :around #'set-default-directory-to-mix-project-root)
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (eval-after-load "elixir-mode"
    '(defun elixir-format--mix-executable ()
       (string-trim-right (shell-command-to-string "asdf which mix"))))
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq global-visual-mode t)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'spacemacs/toggle-auto-completion-off)
  (remove-hook 'LaTeX-mode-hook #'latex/auto-fill-mode)


  (defun convert-buffer-to-docx ()
    "Converts current buffer to DOCX format using Pandoc."
    (interactive)
    (if (buffer-file-name)
        (progn ()
               (set 'output-file-name (concat (file-name-sans-extension buffer-file-name) ".docx"))
               (set 'args (list "-o" output-file-name "-t" "docx" "--" (buffer-file-name)))
               (with-temp-buffer
                 (apply 'call-process-region (point-min) (point-max) "pandoc" t t nil args)
                 (buffer-substring-no-properties (point-min) (point-max)))
               (message "Converted to DOCX, output written to %s" (car (last (split-string output-file-name "/"))))
               )
      (message "Can't convert: Buffer has no file name.")
      ))

  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "d" 'convert-buffer-to-docx)
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "d" 'convert-buffer-to-docx)

  (mmm-add-classes
   '((elixir-graphql
      :submode graphql-mode
      :face mmm-declaration-submode-face
      :front "@[A-Za-z_]+ \"\"\"" ;; regex to find the opening tag
      :back "\"\"\""))) ;; regex to find the closing tag
  (mmm-add-classes
   '((js-graphql
      :submode graphql-mode
      :face mmm-declaration-submode-face
      :front "[^a-zA-Z]gql`" ;; regex to find the opening tag
      :back "`"))) ;; regex to find the closing tag
  (mmm-add-mode-ext-class 'typescript-mode "\\.ts" 'js-graphql)
  (mmm-add-mode-ext-class 'rjsx-mode "\\.js" 'js-graphql)
  (mmm-add-mode-ext-class 'rjsx-mode "\\.jsx" 'js-graphql)
  (mmm-add-mode-ext-class 'typescript-mode "\\.tsx" 'js-graphql)
  (mmm-add-mode-ext-class 'elixir-mode "\\.exs" 'elixir-graphql)
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0)

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'slim-mode 'display-line-numbers-mode)
  (add-hook 'typescript-mode-hook 'my-tsx-setup-hook)
  (add-hook 'web-mode-hook
            '(lambda ()
               (if (buffer-file-name)
                   (if (not (string-match "\\.eex\\'" buffer-file-name)) (prettier-js-mode)))))

  (setq require-final-newline t)
  (setq vc-follow-symlinks t)
  (setq tags-add-tables nil)
  (setq
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   typescript-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (defun reset-powerline ()
    (setq powerline-default-separator 'arrow)
    (setq powerline-height 25)
    (spaceline-compile)
    (spaceline-toggle-minor-modes-off)
    )
  (setq select-enable-clipboard nil)
  (setq font-latex-fontify-sectioning 1.0)

  (setq-default TeX-engine 'xetex)
  (setq-default TeX-master "main.tex")
  (spacemacs/set-leader-keys "s a p" 'counsel-ag)
  (setq base16-theme-256-color-source 'colors)

  (setq additional-ignored-lsp-patterns '("[/\\\\]deps" "[/\\\\]build" "[/\\\\]\\.elixir_ls" "[/\\\\]ansible" "[/\\\\]priv[/\\\\]static"))
  (setq lsp-file-watch-ignored (append lsp-file-watch-ignored additional-ignored-lsp-patterns))
  (setq terraform-format-on-save t)

  (use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "/home/karol/elixir-ls/release"))


  ;; OS-specific configuration
  (cond
   ((string-equal system-type "darwin")
    (progn
      (global-set-key (kbd "<s-tab>") 'other-frame)
      (setq TeX-view-program-list '(("Xreader" "xreader --page-index=%(outpage) %o")))
      (setq TeX-view-program-selection '((output-pdf "Xreader")))
      ))
   ((string-equal system-type "gnu/linux")
    (progn
      (setq TeX-view-program-list '(("Xreader" "xreader --page-index=%(outpage) %o")))
      (setq TeX-view-program-selection '((output-pdf "Xreader")))
      ;; Make sure the following comes before `(fcitx-aggressive-setup)'
      ;; (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
      ;; (fcitx-aggressive-setup)
      ;; (fcitx-prefix-keys-add "SPC") ; M-m is common in Spacemacs
      ;; (setq fcitx-use-dbus t) ; uncomment if you're using Linux
      ;; (global-set-key (kbd "<C-escape>") nil)
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family "Noto Sans CJK TC")))
      ))
   )

    (reset-powerline)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (terraform-mode hcl-mode jinja2-mode company-ansible ansible-doc ansible base16-gruvbox-dark-theme doom-monokai-pro-theme toml-mode racer flycheck-rust cargo rust-mode pcre2el lsp-mode ht parent-mode haml-mode fringe-helper git-gutter+ format-sql json-snatcher json-reformat web-completion-data yapfify yaml-mode xterm-color winum which-key wgrep web-mode uuidgen use-package toc-org tide typescript-mode sql-indent spaceline powerline smeargle shell-pop request rainbow-delimiters pytest pyim xr popwin persp-mode paradox pangu-spacing osx-dictionary orgit org-bullets neotree multi-term minitest markdown-toc lorem-ipsum live-py-mode link-hint ivy-hydra indent-guide hy-mode hungry-delete hl-todo helm-make helm-ag helm helm-core golden-ratio git-messenger git-link git-gutter-fringe git-gutter flycheck-mix fill-column-indicator eyebrowse expand-region exec-path-from-shell evil-unimpaired evil-surround evil-nerd-commenter evil-mc evil-matchit evil-magit evil-exchange eshell-z eshell-prompt-extras esh-help dumb-jump doom-themes docker tablist diminish diff-hl counsel-projectile projectile counsel swiper ivy company-anaconda color-theme-modern coffee-mode clj-refactor hydra cider parseedn clojure-mode bind-key base16-theme auto-yasnippet aggressive-indent ace-window ace-pinyin ace-link avy elixir-mode eval-sexp-fu auctex company anzu iedit smartparens highlight evil undo-tree flx pos-tip flycheck yasnippet multiple-cursors skewer-mode simple-httpd lv markdown-mode dash-functional magit-popup magit git-commit with-editor transient async org-plus-contrib f js2-mode dash ws-butler web-beautify volatile-highlights vi-tilde-fringe tagedit systemd swift-mode smex slim-mode simpleclip sesman scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reveal-in-osx-finder restart-emacs rbenv rake queue pyvenv pyim-basedict pyenv-mode py-isort pug-mode prettier-js pip-requirements pbcopy parseclj paredit osx-trash open-junk-file ob-elixir nginx-mode move-text moe-theme mmm-mode magit-gitflow lsp-sourcekit livid-mode linum-relative launchctl json-mode js2-refactor js-doc inflections highlight-parentheses highlight-numbers highlight-indentation graphql-mode goto-chg google-translate gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-gutter-fringe+ gh-md fuzzy flycheck-pos-tip flycheck-credo flx-ido find-by-pinyin-dired fancy-battery evil-visualstar evil-visual-mark-mode evil-tutor evil-search-highlight-persist evil-numbers evil-lisp-state evil-indent-plus evil-iedit-state evil-escape evil-ediff evil-args evil-anzu erlang emmet-mode dockerfile-mode docker-tramp cython-mode company-web company-tern company-statistics company-auctex column-enforce-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby bundler basic-theme auto-highlight-symbol auctex-latexmk anaconda-mode alchemist afternoon-theme adaptive-wrap ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
    (toml-mode ron-mode racer helm-gtags ggtags flycheck-rust dap-mode posframe lsp-treemacs bui treemacs pfuture counsel-gtags cargo rust-mode pcre2el lsp-mode ht parent-mode haml-mode fringe-helper git-gutter+ format-sql fcitx json-snatcher json-reformat web-completion-data yapfify yaml-mode xterm-color winum which-key wgrep web-mode uuidgen use-package toc-org tide typescript-mode sql-indent spaceline powerline smeargle shell-pop request rainbow-delimiters pytest pyim xr popwin persp-mode paradox pangu-spacing osx-dictionary orgit org-bullets neotree multi-term minitest markdown-toc lorem-ipsum live-py-mode link-hint ivy-hydra indent-guide hy-mode hungry-delete hl-todo helm-make helm-ag helm helm-core golden-ratio git-messenger git-link git-gutter-fringe git-gutter flycheck-mix fill-column-indicator eyebrowse expand-region exec-path-from-shell evil-unimpaired evil-surround evil-nerd-commenter evil-mc evil-matchit evil-magit evil-exchange eshell-z eshell-prompt-extras esh-help dumb-jump doom-themes docker tablist diminish diff-hl counsel-projectile projectile counsel swiper ivy company-anaconda color-theme-modern coffee-mode clj-refactor hydra cider parseedn clojure-mode bind-key base16-theme auto-yasnippet aggressive-indent ace-window ace-pinyin ace-link avy elixir-mode eval-sexp-fu auctex company anzu iedit smartparens highlight evil undo-tree flx pos-tip flycheck yasnippet multiple-cursors skewer-mode simple-httpd lv markdown-mode dash-functional magit-popup magit git-commit with-editor transient async org-plus-contrib f js2-mode dash ws-butler web-beautify volatile-highlights vi-tilde-fringe tagedit systemd swift-mode smex slim-mode simpleclip sesman scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reveal-in-osx-finder restart-emacs rbenv rake queue pyvenv pyim-basedict pyenv-mode py-isort pug-mode prettier-js pip-requirements pbcopy parseclj paredit osx-trash open-junk-file ob-elixir nginx-mode move-text moe-theme mmm-mode magit-gitflow lsp-sourcekit livid-mode linum-relative launchctl json-mode js2-refactor js-doc inflections highlight-parentheses highlight-numbers highlight-indentation graphql-mode goto-chg google-translate gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-gutter-fringe+ gh-md fuzzy flycheck-pos-tip flycheck-credo flx-ido find-by-pinyin-dired fancy-battery evil-visualstar evil-visual-mark-mode evil-tutor evil-search-highlight-persist evil-numbers evil-lisp-state evil-indent-plus evil-iedit-state evil-escape evil-ediff evil-args evil-anzu erlang emmet-mode dockerfile-mode docker-tramp cython-mode company-web company-tern company-statistics company-auctex column-enforce-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu chruby bundler basic-theme auto-highlight-symbol auctex-latexmk anaconda-mode alchemist afternoon-theme adaptive-wrap ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)

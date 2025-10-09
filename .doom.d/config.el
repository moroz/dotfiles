;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun is-linux () (string-equal system-type "gnu/linux"))
(defun is-mac () (not (is-linux)))

(if (is-mac) (exec-path-from-shell-initialize))
(setq mac-command-modifier 'control)

(defun is-retina ()
  (and (is-linux) (string-equal (shell-command-to-string "gsettings get org.cinnamon.desktop.interface scaling-factor") "uint32 2\n"))
  )

(defun km/get-font-size ()
  (setq base-size 22)
  (setq scaling-factor (if (is-linux) 2 1))
  (* scaling-factor base-size)
  )

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq latin-font "JetBrainsMono Nerd Font")
(setq sans-font "Roboto")

(defun get-preferred-color-scheme()
  (let ((preferred (string-trim (shell-command-to-string "dconf read /org/gnome/desktop/interface/color-scheme"))))
    (cond ((string= preferred "'prefer-light'") (or (getenv "EMACS_LIGHT_COLORSCHEME") "modus-operandi-tinted"))
          ((string= preferred "'prefer-dark'") (or (getenv "EMACS_DARK_COLORSCHEME") "modus-vivendi-tinted"))
          (t (or (getenv "EMACS_COLORSCHEME") "doom-dracula"))
          )
    )
  )

(setq doom-theme (intern (get-preferred-color-scheme)))

(setq common-face (font-spec :family latin-font :size (km/get-font-size)))
(setq doom-font common-face)

(setq display-line-numbers-type t)

(setq neo-smart-open t)
(global-set-key (kbd "<f9>") 'neotree-toggle)
(global-set-key (kbd "C-1") 'neotree-toggle)
(global-set-key (kbd "M-<f12>") '+vterm/toggle)
(setq neo-theme 'ascii)

(global-set-key (kbd "<f10>") 'save-buffers-kill-terminal)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(global-set-key (kbd "C-s") 'save-buffer)
(global-visual-line-mode t)

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

(setq company-idle-delay 0.2)
(setq select-enable-clipboard nil)

;; Make evil mode recognize snake_case words
(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

(use-package! lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path (expand-file-name "~/elixir-ls"))
  )

(setq treemacs-width 30)
(if (is-linux) (toggle-frame-maximized)) ;; Maximize the window after starting
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 5000)

(add-hook 'elixir-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'typescript-mode-hook  'turn-on-ctags-auto-update-mode)

(defun set-latex-vars ()
  (setq-local TeX-master "main.tex")
  (company-mode nil)
  (turn-off-evil-snipe-mode)
  (turn-off-evil-snipe-override-mode)
  )

(add-hook 'LaTeX-mode-hook 'set-latex-vars)

(map! :after tex-mode
      :map LaTeX-mode-map
      :localleader
      :nv "b" #'latex/build
      )

(map! :after tex-mode
      :map LaTeX-mode-map
      :localleader
      :nv "v" #'TeX-view
      )

(setq-default TeX-engine 'xetex)
(setq-default TeX-master "main.tex")
(setq-default TeX-command-default "LaTeX")

(if (is-mac)
    (setq TeX-view-program-selection '((output-pdf "Skim")))
  )

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "The default command to use with `SPC m b'")

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))

(after! apheleia
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop))

(after! company
  (add-hook 'markdown-mode-hook (lambda () (company-mode -1))))

(use-package! xclip
  :config
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t))

(setq xclip-select-enable-clipboard t)

(remove-hook 'doom-first-input-hook
             #'evil-snipe-mode)

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun is-linux () (string-equal system-type "gnu/linux"))
(defun is-mac () (not (is-linux)))

(defun is-retina ()
  (and (is-linux) (string-equal (shell-command-to-string "gsettings get org.cinnamon.desktop.interface scaling-factor") "uint32 2\n"))
  )

(defun daytime-command ()
  (if (is-linux)
    "$HOME/.dotfiles/daytime"
    "$HOME/.dotfiles/daytime.Darwin"))

(defun is-daytime ()
  (string-equal (string-trim (shell-command-to-string (daytime-command)))
                "DAYTIME")
  )

(defun theme-by-daytime ()
  (if (is-daytime) #'vscode-dark-plus #'distinguished)
  )

(defun km/get-font-size ()
  (setq base-size 17)
  (setq scaling-factor (if (is-retina) 2 1))
  (* scaling-factor base-size)
  )

(setq latin-font "Monaco")
(setq cjk-font "Noto Sans CJK TC Medium")
(setq cjk-scaling-factor (if (is-retina) 0.315 0.630))
(setq cjk-font-size (* (km/get-font-size) cjk-scaling-factor))
(setq doom-theme (theme-by-daytime))
(setq common-face (font-spec :family latin-font :size (km/get-font-size)))
(setq cjk-face (font-spec :family cjk-font :size cjk-font-size))
(setq doom-font common-face
      doom-variable-pitch-font common-face
      doom-unicode-font cjk-face)

(setq display-line-numbers-type t)

(global-set-key (kbd "<f9>") 'neotree-toggle)
(global-set-key (kbd "<f10>") 'save-buffers-kill-terminal)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
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
  (add-to-list 'exec-path "/home/karol/elixir-ls/release")
  )

(setq treemacs-width 30)
(if (is-linux) (toggle-frame-maximized)) ;; Maximize the window after starting
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 5000)

(defun km/connect-to-fitness-dev ()
  "Connect to fitness_development running in Docker."
  (interactive)
  (setq sql-user "mmagym")
  (setq sql-password "mmagym")
  (setq sql-database "fitness_development")
  (setq sql-server "localhost")
  (setq sql-port 3307)
  (defalias 'sql-get-login 'ignore)
  (sql-mysql)
  )

(setq mac-command-modifier 'control)

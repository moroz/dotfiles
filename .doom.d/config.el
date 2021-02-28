;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun km/get-font-size ()
  (setq base-size 20)
  (setq is-retina (and (string-equal system-type "gnu/linux") (string-equal (shell-command-to-string "gsettings get org.cinnamon.desktop.interface scaling-factor") "uint32 2\n")))
  (setq scaling-factor (if is-retina 2 1))
  (* scaling-factor base-size)
  )

(setq latin-font "DejaVu Sans Mono")
(setq cjk-font "Microsoft JhengHei")
(setq doom-theme 'doom-tomorrow-night)
(setq common-face (font-spec :family latin-font :size (km/get-font-size)))
(setq doom-font common-face
      doom-variable-pitch-font common-face)

(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
    charset (font-spec :family cjk-font :size (km/get-font-size))))

(setq display-line-numbers-type t)

(global-set-key (kbd "<f9>") 'treemacs)
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
  (add-to-list 'exec-path "/home/karol/elixir-ls/release"))

(setq treemacs-width 30)
(toggle-frame-maximized) ;; Maximize the window after starting
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

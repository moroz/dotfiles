;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-ayu-mirage)
(setq common-face (font-spec :family "Ubuntu Mono" :size 23))
(setq doom-font common-face
      doom-variable-pitch-font common-face)

(setq display-line-numbers-type t)

(global-set-key (kbd "<f9>") 'treemacs)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-s") 'save-buffer)

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

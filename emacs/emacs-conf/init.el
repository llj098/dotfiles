;;;;; packages

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
         (package-install package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'iedit 'magit 'cider
			  'paredit 'clojure-mode
			  'lua-mode
			  'guru-mode
			  'restclient 'ace-jump-mode
			  'multiple-cursors 'expand-region
			  'window-numbering
			  'smart-mode-line
			  'smex
			  'helm
			  'highlight-current-line
			  'flymake 
			  'flymake-lua
			  'flymake-shell
			  'flycheck
			  'imenu-anywhere
			  'magit
			  'markdown-mode
			  'highlight-current-line
			  'org
			  'undo-tree
			  'rainbow-delimiters
			  'zenburn-theme)
(package-initialize)

;;; common config
(ido-mode t)
(load-theme 'zenburn t)
(global-linum-mode)
(global-undo-tree-mode)
(show-paren-mode 1)
(window-numbering-mode 1)
(sml/setup)
(hl-line-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defvar modes-to-hook '(paredit-mode
			rainbow-delimiters-mode
			undo-tree-mode))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(if (eq system-type 'windows-nt)
    (set-default-font "Consolas-14")
  (set-default-font "PragmataPro-13"))

(if (eq system-type 'darwin)
    (toggle-frame-maximized))

;;; clojure
(mapcar (lambda (m) (add-hook 'clojure-mode-hook m))
	modes-to-hook)
(mapcar (lambda (m) (add-hook 'cider-mode-hook m))
	(cons 'cider-turn-on-eldoc-mode modes-to-hook))
(mapcar (lambda (m) (add-hook 'cider-repl-mode-hook m))
	(cons 'cider-turn-on-eldoc-mode modes-to-hook))

;; Use ac-nrepl-popup-doc to show in-line docs in a clojure buffer
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Use ac-nrepl-popup-doc to show in-line docs in an nrepl buffer
(eval-after-load "cider"
  '(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(if (eq system-type 'darwin)
    (progn
      (toggle-fullscreen)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)))

(sml/setup)
(window-numbering-mode)


(setq lua-indent-level 2)

;;; clojure
(mapcar (lambda (m) (add-hook 'clojure-mode-hook m)) modes-to-hook)
(mapcar (lambda (m) (add-hook 'cider-mode-hook m)) (cons 'cider-turn-on-eldoc-mode modes-to-hook))
(mapcar (lambda (m) (add-hook 'cider-repl-mode-hook m)) (cons 'cider-turn-on-eldoc-mode modes-to-hook))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


;; Use ac-nrepl-popup-doc to show in-line docs in a clojure buffer
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Use ac-nrepl-popup-doc to show in-line docs in an nrepl buffer
(eval-after-load "cider"
  '(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(setq cider-show-error-buffer nil)

;; lua

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-default-application "/usr/local/bin/lua")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lua-indent-level 2))


;; eshell
(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)


;; w3m
(setq w3m-command "/usr/bin/w3m")

;; expand region
(require 'expand-region)
(define-key global-map (kbd "M-=") 'er/expand-region)
;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; imenu
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "imenu-") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))

(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (imenu-anywhere)))

(global-set-key (kbd "C-.") 'ido-imenu-push-mark)


;; helm
(global-set-key (kbd "C-c h") 'helm-mini)
(setq helm-grep-default-command "/usr/local/bin/grep")


(setq socks-noproxy '("127.0.0.1"))
(setq socks-server '("Default server" "127.0.0.1" 1080 5))
;;(setq url-gateway-method 'socks)


;;(setq url-proxy-services '(("no_proxy" . "work\\.com")
;;  ("http" . "ln.fatlj.me:8888")))


(set-face-background hl-line-face "gray13")
(set-face-attribute 'region nil :background "blue")
(provide 'init-local)
;;;

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
			  'flymake
			  'flymake-lua
			  'flymake-shell
			  'flycheck
			  'magit
			  'markdown-mode
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

(defvar modes-to-hook '(paredit-mode
			rainbow-delimiters-mode
			undo-tree-mode))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(if (eq system-type 'windows-nt)
    (set-default-font "Consolas-14")
  (set-default-font "PragmataPro-12"))

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
(Eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Use ac-nrepl-popup-doc to show in-line docs in an nrepl buffer
(eval-after-load "cider"
  '(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


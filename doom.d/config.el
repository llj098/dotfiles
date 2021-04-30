;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Lijin Liu"
      user-mail-address "llj098@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)
(setq doom-font "PragmataPro-14")
(if (eq system-type 'darwin)
    (setq doom-font "PragmataPro-18"))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(global-undo-tree-mode)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "hs" 'helm-swoop
  "k" 'kill-buffer
  "I" 'helm-imenu
  "pf" 'helm-projectile-find-file
  "pF" 'helm-projectile-find-file-dwim
  "ss" 'helm-swoop
  "gd" 'xref-find-definitions
  "gs" 'magit-status
  "r" 'er/expand-region
  "*"  'helm-projectile-ag
  "m" 'pop-global-mark
 )

(use-package! deft
  :config (setq
	   deft-default-extension "org"
	   deft-text-mode 'org-mode
	   deft-use-filename-as-title t
	   deft-directory "~/syncthing/deft"
	   deft-auto-save-interval 5.0
	   deft-use-filter-string-for-filename t))
;;
;; TODO: orgmode keymap, C-k....
;; https://github.com/hlissner/doom-emacs/issues/2403

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (lilypond . t)))



(after! evil-snipe
  (evil-snipe-mode -1))

(global-unset-key (kbd "C-SPC"))
(evil-define-key 'insert c-mode-map (kbd "C-SPC") nil) ;; company-active-map
(evil-define-key 'insert c-mode-map (kbd "C-SPC") nil) ;; company-active-map
(evil-define-key 'insert c-mode-map (kbd "M-SPC") 'company-active-map) ;; company-active-map


(elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

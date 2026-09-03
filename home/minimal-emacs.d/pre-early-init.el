;;; pre-early-init.el --- Earliest UI settings -*- no-byte-compile: t; lexical-binding: t; -*-

;; Keep intermediate startup states off-screen.  The final themed UI is drawn
;; once, after init.el and post-init.el have finished.
(setq minimal-emacs-inhibit-redisplay-during-startup t
      minimal-emacs-inhibit-message-during-startup t
      ;; Emacs 31's full `package-initialize' builds an expensive archive
      ;; compatibility table.  pre-init.el performs fast local activation.
      minimal-emacs-package-initialize-and-refresh nil
      ;; Prefer compiled pre/post init files when they are up to date.
      minimal-emacs-load-compiled-init-files t
      package-user-dir
      (expand-file-name "~/.local/share/emacs/minimal-emacs/elpa/")
      ;; There is no reason to hide and later restore the mode line while all
      ;; startup redisplay is already inhibited.
      minimal-emacs-disable-mode-line-during-startup nil
      menu-bar-mode nil)

;; Cover both the initial graphical frame and every subsequently created frame.
(add-to-list 'initial-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))

;; Native compilation products are machine- and Emacs-version-specific cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "~/.cache/emacs/minimal-emacs/eln-cache/")))

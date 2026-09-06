;;; init.el --- Omarchy theme synchronization -*- lexical-binding: t; -*-

(require 'filenotify)
(require 'subr-x)

(use-package modus-themes
  :ensure t
  :demand t)

(use-package omarchy
  :vc (:url "https://github.com/llj098/omarchy.el.git"
       :rev "6f2f2f33e46e70b7bfd244d0a35a91ff5e0922c9")
  :demand t)

(require 'omarchy-themes)

(defvar omarchy-default-theme)
(defvar omarchy-hooks-directory)
(declare-function omarchy-apply-theme "omarchy")
(declare-function omarchy-current-theme "omarchy")

(defconst lj/omarchy-theme-state-file
  (expand-file-name "omarchy/emacs-theme-name"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state/")))

(defconst lj/omarchy-theme-hook-source
  (expand-file-name "omarchy/theme-set-hook" user-emacs-directory))

(defvar lj/omarchy-theme-watch-descriptor nil)
(defvar lj/omarchy-theme-update-timer nil)

(defun lj/omarchy--file-contents (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun lj/omarchy-install-theme-hook ()
  "Install the file-based Omarchy theme hook when needed."
  (let* ((directory (expand-file-name "theme-set.d" omarchy-hooks-directory))
         (target (expand-file-name "emacs-theme" directory))
         (content (lj/omarchy--file-contents lj/omarchy-theme-hook-source)))
    (make-directory directory t)
    (unless (and (not (file-symlink-p target))
                 (file-readable-p target)
                 (string= content (lj/omarchy--file-contents target)))
      (let ((temporary
             (make-temp-file (expand-file-name ".emacs-theme-hook." directory))))
        (unwind-protect
            (progn
              (write-region content nil temporary nil 'silent)
              (set-file-modes temporary #o755)
              (rename-file temporary target t)
              (setq temporary nil))
          (when (and temporary (file-exists-p temporary))
            (delete-file temporary)))))
    (unless (= (file-modes target) #o755)
      (set-file-modes target #o755))))

(defun lj/omarchy--read-state-theme ()
  "Return the theme name recorded by the Omarchy hook."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents lj/omarchy-theme-state-file nil 0 256)
        (let ((theme (string-trim (buffer-string))))
          (unless (string-empty-p theme) theme)))
    (file-error nil)))

(defun lj/omarchy--apply-state-theme ()
  "Apply the latest theme recorded by the Omarchy hook."
  (setq lj/omarchy-theme-update-timer nil)
  (when-let* ((theme (lj/omarchy--read-state-theme)))
    (omarchy-apply-theme theme)))

(defun lj/omarchy--state-file-p (file)
  "Return non-nil when FILE names the Omarchy theme state file."
  (and (stringp file)
       (string= (expand-file-name file) lj/omarchy-theme-state-file)))

(defun lj/omarchy--theme-state-changed (event)
  "Schedule a theme update when file notification EVENT affects the state file."
  (when (or (lj/omarchy--state-file-p (nth 2 event))
            (lj/omarchy--state-file-p (nth 3 event)))
    (when (timerp lj/omarchy-theme-update-timer)
      (cancel-timer lj/omarchy-theme-update-timer))
    (setq lj/omarchy-theme-update-timer
          (run-with-timer 0.05 nil #'lj/omarchy--apply-state-theme))))

(defun lj/omarchy-start-theme-watch ()
  "Watch for theme names written by the Omarchy shell hook."
  (let ((directory (file-name-directory lj/omarchy-theme-state-file)))
    (make-directory directory t)
    (when lj/omarchy-theme-watch-descriptor
      (ignore-errors
        (file-notify-rm-watch lj/omarchy-theme-watch-descriptor)))
    (setq lj/omarchy-theme-watch-descriptor
          (file-notify-add-watch directory '(change attribute-change)
                                 #'lj/omarchy--theme-state-changed))))

(setq omarchy-default-theme 'rose-pine-dawn)
(lj/omarchy-install-theme-hook)
(lj/omarchy-start-theme-watch)
(when-let* ((theme (omarchy-current-theme)))
  (omarchy-apply-theme theme))

(provide 'lj-omarchy)
;;; init.el ends here

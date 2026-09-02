;;; desktop-theme.el --- Follow desktop light/dark mode -*- lexical-binding: t; -*-

(require 'subr-x)

(defun linux-setup/apply-nord-theme (mode source)
  "Apply one Nord theme for MODE and report SOURCE.
MODE is `light' or `dark'.  Disable previously enabled themes first so
repeated desktop transitions never stack color themes."
  (let* ((normalized (downcase (string-trim mode)))
         (theme (cond ((string= normalized "light") 'doom-nord-light)
                      ((string= normalized "dark") 'doom-nord))))
    (if (not theme)
        (message "%s returned unsupported mode: %S" source normalized)
      (unless (and (eq doom-theme theme)
                   (equal custom-enabled-themes (list theme)))
        (mapc #'disable-theme custom-enabled-themes)
        (setq doom-theme theme)
        (if (custom-theme-p theme)
            (enable-theme theme)
          (load-theme theme t)))
      (message "%s applied %s" source theme))
    theme))

(defun auto-darkman ()
  "Apply Nord light/dark according to Darkman, when available."
  (interactive)
  (when (executable-find "darkman")
    (linux-setup/apply-nord-theme
     (shell-command-to-string "darkman get")
     "Darkman")))

(defun auto-omarchy (&optional mode)
  "Apply Nord light/dark according to Omarchy.
A theme-set hook may pass MODE to avoid querying it again."
  (interactive)
  (when (or mode (executable-find "omarchy-theme-color"))
    (linux-setup/apply-nord-theme
     (or mode (shell-command-to-string "omarchy-theme-color mode"))
     "Omarchy")))

(cond ((executable-find "omarchy-theme-color") (auto-omarchy))
      ((executable-find "darkman") (auto-darkman)))

(provide 'desktop-theme)

;;; mode-line-style.el --- Flat, quiet mode line -*- lexical-binding: t; -*-

(require 'color)

(defun lj/mode-line-style (&rest _)
  "Keep the mode line flat and close to the current theme's background."
  (let* ((dark (eq (frame-parameter nil 'background-mode) 'dark))
         (bg (or (color-name-to-rgb (face-background 'default nil t))
                 (if dark '(0.08 0.08 0.08) '(0.97 0.97 0.97))))
         (fg (or (color-name-to-rgb (face-foreground 'default nil t))
                 (if dark '(0.85 0.85 0.85) '(0.15 0.15 0.15))))
         (blend (lambda (amount)
                  (apply #'color-rgb-to-hex
                         (append (cl-mapcar
                                  (lambda (b f) (+ b (* amount (- f b))))
                                  bg fg)
                                 '(2)))))
         (active-bg (funcall blend 0.07))
         (inactive-bg (funcall blend 0.025))
         (active-fg (funcall blend 0.85))
         (inactive-fg (funcall blend 0.50)))
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (when (facep face)
        (let ((inactive (eq face 'mode-line-inactive)))
          (set-face-attribute face nil
                              :background (if inactive inactive-bg active-bg)
                              :foreground (if inactive inactive-fg active-fg)
                              :box nil :inverse-video nil
                              :overline nil :underline nil
                              :weight 'normal :slant 'normal))))
    (setq-default mode-line-end-spaces '(" "))
    (force-mode-line-update t)))

(require 'cl-lib)
(add-hook 'enable-theme-functions #'lj/mode-line-style)
(add-hook 'disable-theme-functions #'lj/mode-line-style)
(add-hook 'window-setup-hook #'lj/mode-line-style)
(lj/mode-line-style)

(provide 'mode-line-style)
;;; mode-line-style.el ends here

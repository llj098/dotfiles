;;; post-early-init.el --- Final early settings -*- no-byte-compile: t; lexical-binding: t; -*-

;; A terminal's initial frame already exists by the time early-init.el runs.
;; `default-frame-alist' does not change that frame, so remove its menu bar
;; explicitly while redisplay is still inhibited.
(set-frame-parameter nil 'menu-bar-lines 0)

;; Evil 1.15.0 from NonGNU relies on `evil-mode-buffers', which Emacs 31 no
;; longer creates.  Use the current MELPA build containing the Emacs 31 fix.
(setq package-pinned-packages '((evil . "melpa")))

(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

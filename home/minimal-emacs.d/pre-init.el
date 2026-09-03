;;; pre-init.el --- Fast local package activation -*- no-byte-compile: t; lexical-binding: t; -*-

;; Emacs 31's `package-initialize' reads all archive metadata and builds a
;; compatibility table on every startup.  Activate installed packages without
;; that archive-only work; package metadata remains available for use-package.
(require 'package)
(setq package-alist nil)
(package-load-all-descriptors)
(setq package--initialized t)
(package-activate-all)
(require 'use-package)

;;; pre-init.el ends here

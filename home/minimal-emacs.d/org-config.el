;;; org-config.el --- Personal Org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org paths, capture, agenda, TODO workflow, Org-roam, Consult integration,
;; and Deft.  Existing Org files remain in ~/syncthing and are not moved.

;;; Code:

(require 'subr-x)

(defconst lj/org-directory
  (file-name-as-directory (file-truename "~/syncthing/deft/org/"))
  "Root directory for personal Org data.")
(defconst lj/org-roam-directory
  (file-name-as-directory (file-truename (expand-file-name "roam/" lj/org-directory)))
  "Root directory for Org-roam notes.")
(defconst lj/org-roam-projects-directory
  (file-name-as-directory
   (file-truename (expand-file-name "projects/" lj/org-roam-directory)))
  "Directory containing project notes.")
(defconst lj/org-inbox-file
  (expand-file-name "inbox.org" lj/org-directory)
  "Inbox used by Org Capture.")

(defun lj/org--ensure-file (file content)
  "Create FILE with CONTENT if it does not already exist."
  (make-directory (file-name-directory file) t)
  (unless (file-exists-p file)
    (with-temp-file file
      (insert content)))
  file)

(defun lj/org--today-daily-file ()
  "Return today's daily file, creating its standard headings if necessary."
  (lj/org--ensure-file
   (expand-file-name (format-time-string "%Y-%m-%d.org")
                     (expand-file-name "daily/" lj/org-roam-directory))
   (format "#+title: %s\n\n* Tasks\n\n* Log\n\n* Notes\n"
           (format-time-string "%Y-%m-%d"))))

(defun lj/org-inherited-priority (headline)
  "Return HEADLINE priority, inheriting it from parent headings when absent."
  (save-excursion
    (cond
     ((string-match org-priority-regexp headline)
      (* 1000 (- org-priority-lowest
                 (org-priority-to-value (match-string 2 headline)))))
     ((org-up-heading-safe)
      (lj/org-inherited-priority (org-get-heading)))
     (t
      (* 1000 (- org-priority-lowest org-priority-default))))))

(defun lj/org-capture--quit-columns (&rest _)
  "Turn off Columns View in buffers used by the active capture."
  (let* ((buffer (org-capture-get :buffer))
         (file (and (buffer-live-p buffer) (buffer-file-name buffer))))
    (dolist (candidate
             (if file
                 (seq-filter
                  (lambda (buf)
                    (and (buffer-file-name buf)
                         (file-equal-p (buffer-file-name buf) file)))
                  (buffer-list))
               (and buffer (list buffer))))
      (with-current-buffer candidate
        (when (bound-and-true-p org-columns-mode)
          (ignore-errors (org-columns-quit)))))))

(defun lj/org-capture-place-template-a (fn &rest args)
  "Run FN with ARGS after making the capture target writable."
  (lj/org-capture--quit-columns)
  (let ((inhibit-read-only t))
    (apply fn args)))

(use-package org
  :ensure nil
  :commands (org-agenda org-capture org-mode org-store-link org-todo-list)
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory lj/org-directory
        org-agenda-files
        (list (expand-file-name "roam/daily/" lj/org-directory)
              lj/org-inbox-file
              (expand-file-name "roam/projects/" lj/org-directory))
        ;; Doom's Org module used the current window instead of Org's default
        ;; `reorganize-frame', which deliberately creates a two-window layout.
        org-agenda-window-setup 'current-window
        ;; Use Org's built-in virtual indentation, matching Doom's defaults.
        org-startup-indented t
        org-hide-leading-stars t
        org-startup-folded t
        org-log-into-drawer "LOGBOOK"
        org-clock-into-drawer "LOGBOOK"
        org-log-refile 'time
        org-log-done 'time
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-repeat 'time
        org-todo-keywords
        '((sequence
           "TODO(t)" "STRT(s!)" "WAIT(w@/!)" "HOLD(h@/!)"
           "|"
           "DONE(d!)" "CANCELLED(c@)")))
  :config
  (setq org-priority-get-priority-function #'lj/org-inherited-priority)
  (when (boundp 'org-get-priority-function)
    (setq org-get-priority-function #'lj/org-inherited-priority))

  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " %i %-12:c %l")
          (tags   . " %i %-12:c %l")
          (search . " %i %-12:c %l"))
        org-agenda-custom-commands
        '(("D" "Deadlines (next 30 days)"
           agenda ""
           ((org-agenda-span 30)
            (org-agenda-entry-types '(:deadline))))
          ("W" "工作视图：隐藏所有 :home:（agenda + alltodo）"
           ((agenda "" ((org-agenda-span 'day)))
            (alltodo ""))
           ((org-agenda-tag-filter-preset '("-home"))
            (org-agenda-compact-blocks t)))))

  ;; Load Babel languages once, rather than once for every Org buffer.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org . t)
     (lilypond . t)))

  (setq org-capture-templates
        `(("t" "Today · Task" entry
           (file+headline lj/org--today-daily-file "Tasks")
           "** TODO %?\n%T\n"
           :prepend t :empty-lines 1)
          ("i" "Inbox · Task" entry
           (file+headline ,lj/org-inbox-file "Tasks")
           "* TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("p" "Project tasks")
          ("pt" "Trading · Task" entry
           (file+headline ,(expand-file-name "trading.org"
                                             lj/org-roam-projects-directory)
                          "Tasks")
           "** TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("pq" "Quant · Task" entry
           (file+headline ,(expand-file-name "quant.org"
                                             lj/org-roam-projects-directory)
                          "Tasks")
           "** TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("pk" "育儿 · Task" entry
           (file+headline ,(expand-file-name "parenting.org"
                                             lj/org-roam-projects-directory)
                          "Tasks")
           "** TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("ph" "健康 · Task" entry
           (file+headline ,(expand-file-name "health.org"
                                             lj/org-roam-projects-directory)
                          "Tasks")
           "** TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("pb" "Baking · Task" entry
           (file+headline ,(expand-file-name "baking.org"
                                             lj/org-roam-projects-directory)
                          "Tasks")
           "** TODO %?\n%U\n"
           :prepend t :empty-lines 1)
          ("W" "Market watch (daily)" entry
           (file+headline ,(expand-file-name "market-watch.org"
                                             lj/org-roam-projects-directory)
                          "Market Watch Log")
           "*** %^{Symbol}\n:PROPERTIES:\n:CREATED: %U\n:TYPE: %^{类型|Maj|LB}\n:STATUS: %^{状态|震荡|多头|空头|其他}\n:OPP: %^{机会|多|空|None}\n:ITV: %^{itv|1d|4h|1h|15m}\n:ACTION: %^{action|wait|watching}\n:REMARK:\n:END:\n\n%?"
           :prepend t :empty-lines 1)))

  (with-eval-after-load 'org-capture
    (unless (advice-member-p #'lj/org-capture-place-template-a
                             #'org-capture-place-template)
      (advice-add #'org-capture-place-template
                  :around #'lj/org-capture-place-template-a))))

;; Org's built-in structure templates, including <s TAB for source blocks.
(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam
  :ensure t
  ;; Standard Org-roam commands already have package-generated autoloads.
  :defer t
  :init
  (let ((cache-directory (expand-file-name "cache/" user-emacs-directory)))
    (make-directory cache-directory t)
    (setq org-roam-directory lj/org-roam-directory
          org-roam-dailies-directory "daily/"
          org-roam-db-location (expand-file-name "org-roam.db" cache-directory)))
  :config
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode 1)
  (require 'org-roam-mode)

  (defvar lj/org-roam-forward-link-target-id nil
    "ID of the Org-roam link currently previewed in the Roam buffer.")

  (defun lj/org-roam-forward-link-id-at-point ()
    "Return the target ID of the Org id link at point, or nil."
    (when-let* ((context (org-element-context))
                ((eq (org-element-type context) 'link))
                ((string= (org-element-property :type context) "id"))
                (path (org-element-property :path context)))
      (car (split-string path "::"))))

  (defun lj/org-roam-forward-link-section (_node)
    "Insert a top Roam section previewing the id link at point."
    (when-let* (((string= (buffer-name) org-roam-buffer))
                (id lj/org-roam-forward-link-target-id)
                (target (org-roam-node-from-id id)))
      (magit-insert-section (org-roam-forward-link)
        (magit-insert-heading "Forward link at point:")
        (org-roam-node-insert-section
         :source-node target
         :point (or (org-roam-node-point target) 1)
         :properties (list :outline (org-roam-node-olp target)))
        (insert ?\n))))

  (defun lj/org-roam-forward-link-refresh-h ()
    "Refresh the Roam buffer when point moves to a different id link."
    (when (and (derived-mode-p 'org-mode)
               (get-buffer-window org-roam-buffer 'visible))
      (let ((id (lj/org-roam-forward-link-id-at-point)))
        (unless (equal id lj/org-roam-forward-link-target-id)
          (setq lj/org-roam-forward-link-target-id id)
          (when-let* ((buffer (get-buffer org-roam-buffer)))
            (with-current-buffer buffer
              (when org-roam-buffer-current-node
                (org-roam-buffer-render-contents))))))))

  (defun lj/org-roam-forward-link-setup-h ()
    "Enable forward-link refresh in an Org-roam file buffer."
    (add-hook 'post-command-hook #'lj/org-roam-forward-link-refresh-h nil t))

  (add-to-list 'org-roam-mode-sections #'lj/org-roam-forward-link-section)
  (add-hook 'org-roam-find-file-hook #'lj/org-roam-forward-link-setup-h))

(use-package consult-org-roam
  :ensure t
  :commands (lj/consult-org-roam-backlinks
             lj/consult-org-roam-backlinks-unique)
  :defer t
  :init
  (setq consult-org-roam-grep-func #'consult-ripgrep
        consult-org-roam-buffer-narrow-key ?r
        consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  (consult-customize consult-org-roam-forward-links :preview-key "M-.")
  (require 'org-roam-mode)

  (defun lj/org-roam-backlink--outline (backlink)
    "Return the outline path recorded for BACKLINK."
    (if-let* ((outline (plist-get (org-roam-backlink-properties backlink)
                                  :outline)))
        (mapconcat #'org-link-display-format outline " > ")
      "Top"))

  (defun lj/org-roam-backlink--candidate (backlink)
    "Return a Consult candidate for BACKLINK."
    (let* ((source (org-roam-backlink-source-node backlink))
           (title (or (org-roam-node-title source) "Untitled"))
           (file (org-roam-node-file source))
           (relative (if file (file-relative-name file org-roam-directory) ""))
           (point (or (org-roam-backlink-point backlink) 1))
           (outline (lj/org-roam-backlink--outline backlink)))
      (cons (format "%s — %s  (%s@%d)" title outline relative point)
            backlink)))

  (defun lj/org-roam-backlink--goto-point (point)
    "Move to POINT in the current Org buffer and reveal its context."
    (widen)
    (goto-char (min (point-max) (max (point-min) point)))
    (when (and (derived-mode-p 'org-mode)
               (fboundp 'org-fold-show-context))
      (org-fold-show-context))
    (recenter))

  (defun lj/org-roam-backlink--visit (backlink &optional other-window)
    "Visit BACKLINK at its exact source position, optionally OTHER-WINDOW."
    (let ((file (org-roam-node-file (org-roam-backlink-source-node backlink)))
          (point (or (org-roam-backlink-point backlink) 1)))
      (org-roam-preview-visit file point other-window)))

  (defun lj/org-roam-backlink--preview-state (other-window)
    "Return a Consult preview state function honoring OTHER-WINDOW."
    (let ((open (consult--temporary-files))
          (preview (consult--buffer-preview))
          (window-state (window-state-get nil t)))
      (lambda (action backlink)
        (pcase action
          ('preview
           (if-let* ((backlink backlink)
                     (file (org-roam-node-file
                            (org-roam-backlink-source-node backlink)))
                     (buffer (funcall open file)))
               (progn
                 (funcall preview 'preview buffer)
                 (when-let* ((window (get-buffer-window buffer t)))
                   (with-selected-window window
                     (lj/org-roam-backlink--goto-point
                      (or (org-roam-backlink-point backlink) 1)))))
             (funcall preview 'preview nil)))
          ('exit
           (funcall preview 'exit nil)
           (window-state-put window-state)
           (funcall open))
          ('return
           (when backlink
             (lj/org-roam-backlink--visit backlink other-window)))))))

  (defun lj/consult-org-roam-backlinks (&optional other-window unique)
    "Select an exact backlink to the node at point with Consult.
With OTHER-WINDOW, visit it in another window.  With UNIQUE, return at most
one backlink per source node."
    (interactive "P")
    (let* ((node (org-roam-node-at-point 'assert))
           (backlinks (seq-sort #'org-roam-backlinks-sort
                                (org-roam-backlinks-get node :unique unique)))
           (candidates (mapcar #'lj/org-roam-backlink--candidate backlinks)))
      (unless candidates
        (user-error "No backlinks found"))
      (consult--read candidates
                     :prompt "Backlink: "
                     :category 'org-roam-backlink
                     :sort nil
                     :require-match t
                     :state (lj/org-roam-backlink--preview-state other-window)
                     :lookup #'consult--lookup-cdr)))

  (defun lj/consult-org-roam-backlinks-unique (&optional other-window)
    "Select one exact backlink per source node with Consult."
    (interactive "P")
    (lj/consult-org-roam-backlinks other-window t)))

(use-package deft
  :ensure t
  :commands deft
  :init
  (setq deft-default-extension "org"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-directory (expand-file-name "~/syncthing/deft/txt/")
        deft-auto-save-interval 5.0
        deft-use-filter-string-for-filename t
        deft-recursive nil))

(provide 'org-config)

;;; org-config.el ends here

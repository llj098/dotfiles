;;; keymaps.el --- Personal key bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil's built-in leader support, which-key labels, and personal commands.

;;; Code:

;; Leader prefix maps
(defvar lj/leader-map (make-sparse-keymap)
  "Top-level leader keymap.")
(defvar lj/leader-buffer-map (make-sparse-keymap)
  "Leader buffer keymap.")
(defvar lj/leader-code-map (make-sparse-keymap)
  "Leader code keymap.")
(defvar lj/leader-file-map (make-sparse-keymap)
  "Leader file keymap.")
(defvar lj/leader-git-map (make-sparse-keymap)
  "Leader Git keymap.")
(defvar lj/leader-open-map (make-sparse-keymap)
  "Leader open keymap.")
(defvar lj/leader-notes-map (make-sparse-keymap)
  "Leader notes keymap.")
(defvar lj/leader-roam-map (make-sparse-keymap)
  "Leader Org-roam keymap.")
(defvar lj/leader-roam-dailies-map (make-sparse-keymap)
  "Leader Org-roam dailies keymap.")
(defvar lj/leader-project-map (make-sparse-keymap)
  "Leader project keymap.")
(defvar lj/leader-quit-map (make-sparse-keymap)
  "Leader quit keymap.")
(defvar lj/leader-search-map (make-sparse-keymap)
  "Leader search keymap.")
(defvar lj/leader-toggle-map (make-sparse-keymap)
  "Leader toggle keymap.")

;; Personal commands migrated from the Doom configuration.
(defun lj/unix-timestamp-at-point ()
  "Display the Unix timestamp at point or in the active region."
  (interactive)
  (let ((value (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (unless (and value (string-match-p "\\`[0-9]+\\'" value))
      (user-error "No Unix timestamp at point"))
    (message "%s"
             (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (seconds-to-time (string-to-number value))))))

(defun lj/create-note-buffer ()
  "Visit a new timestamp-named Org file in the personal text directory."
  (interactive)
  (let ((directory (expand-file-name "~/syncthing/deft/txt/")))
    (make-directory directory t)
    (find-file (expand-file-name
                (format "%d.org" (time-convert (current-time) 'integer))
                directory))))

(defun lj/open-obsidian ()
  "Open the personal Obsidian directory in Dired."
  (interactive)
  (dired (expand-file-name "~/syncthing/obsidian/")))

;; Top-level groups
(keymap-set lj/leader-map "b" lj/leader-buffer-map)
(keymap-set lj/leader-map "c" lj/leader-code-map)
(keymap-set lj/leader-map "f" lj/leader-file-map)
(keymap-set lj/leader-map "g" lj/leader-git-map)
(keymap-set lj/leader-map "n" lj/leader-notes-map)
(keymap-set lj/leader-map "o" lj/leader-open-map)
(keymap-set lj/leader-map "p" lj/leader-project-map)
(keymap-set lj/leader-map "q" lj/leader-quit-map)
(keymap-set lj/leader-map "s" lj/leader-search-map)
(keymap-set lj/leader-map "t" lj/leader-toggle-map)

;; Top-level commands
(keymap-set lj/leader-map ";" #'pp-eval-expression)
(keymap-set lj/leader-map ":" #'execute-extended-command)
(keymap-set lj/leader-map "." #'find-file)
(keymap-set lj/leader-map "," #'consult-buffer)
(keymap-set lj/leader-map "RET" #'consult-bookmark)
(keymap-set lj/leader-map "u" #'universal-argument)
(keymap-set lj/leader-map "m" #'pop-global-mark)
(keymap-set lj/leader-map "/" #'consult-ripgrep)
(keymap-set lj/leader-map "X" #'org-capture)

;; Buffer: SPC b
(keymap-set lj/leader-buffer-map "[" #'previous-buffer)
(keymap-set lj/leader-buffer-map "]" #'next-buffer)
(keymap-set lj/leader-buffer-map "b" #'consult-buffer)
(keymap-set lj/leader-buffer-map "c" #'clone-indirect-buffer)
(keymap-set lj/leader-buffer-map "C" #'clone-indirect-buffer-other-window)
(keymap-set lj/leader-buffer-map "i" #'ibuffer)
(keymap-set lj/leader-buffer-map "k" #'kill-current-buffer)
(keymap-set lj/leader-buffer-map "m" #'bookmark-set)
(keymap-set lj/leader-buffer-map "M" #'bookmark-delete)
(keymap-set lj/leader-buffer-map "n" #'next-buffer)
(keymap-set lj/leader-buffer-map "N" #'evil-buffer-new)
(keymap-set lj/leader-buffer-map "p" #'previous-buffer)
(keymap-set lj/leader-buffer-map "r" #'revert-buffer)
(keymap-set lj/leader-buffer-map "R" #'rename-buffer)
(keymap-set lj/leader-buffer-map "s" #'save-buffer)
(keymap-set lj/leader-buffer-map "S" #'save-some-buffers)
(keymap-set lj/leader-buffer-map "z" #'bury-buffer)

;; File: SPC f
(keymap-set lj/leader-file-map "d" #'dired)
(keymap-set lj/leader-file-map "f" #'find-file)
(keymap-set lj/leader-file-map "l" #'locate)
(keymap-set lj/leader-file-map "r" #'consult-recent-file)
(keymap-set lj/leader-file-map "s" #'save-buffer)
(keymap-set lj/leader-file-map "S" #'write-file)

;; Search: SPC s
(keymap-set lj/leader-search-map "b" #'consult-line)
(keymap-set lj/leader-search-map "B" #'consult-line-multi)
(keymap-set lj/leader-search-map "d" #'consult-ripgrep)
(keymap-set lj/leader-search-map "f" #'consult-find)
(keymap-set lj/leader-search-map "g" #'consult-grep)
(keymap-set lj/leader-search-map "i" #'consult-imenu)
(keymap-set lj/leader-search-map "I" #'consult-imenu-multi)
(keymap-set lj/leader-search-map "m" #'consult-bookmark)
(keymap-set lj/leader-search-map "r" #'consult-mark)

;; Notes and Org-roam: SPC n
(keymap-set lj/leader-notes-map "a" #'org-agenda)
(keymap-set lj/leader-notes-map "d" #'deft)
(keymap-set lj/leader-notes-map "l" #'org-store-link)
(keymap-set lj/leader-notes-map "n" #'org-capture)
(keymap-set lj/leader-notes-map "r" lj/leader-roam-map)
(keymap-set lj/leader-notes-map "t" #'org-todo-list)

(keymap-set lj/leader-roam-map "a" #'org-roam-node-random)
(keymap-set lj/leader-roam-map "b" #'lj/consult-org-roam-backlinks)
(keymap-set lj/leader-roam-map "B" #'lj/consult-org-roam-backlinks-unique)
(keymap-set lj/leader-roam-map "d" lj/leader-roam-dailies-map)
(keymap-set lj/leader-roam-map "e" #'consult-org-roam-file-find)
(keymap-set lj/leader-roam-map "f" #'org-roam-node-find)
(keymap-set lj/leader-roam-map "g" #'org-roam-graph)
(keymap-set lj/leader-roam-map "i" #'org-roam-node-insert)
(keymap-set lj/leader-roam-map "l" #'consult-org-roam-forward-links)
(keymap-set lj/leader-roam-map "n" #'org-roam-capture)
(keymap-set lj/leader-roam-map "r" #'org-roam-buffer-toggle)
(keymap-set lj/leader-roam-map "R" #'org-roam-buffer-display-dedicated)
(keymap-set lj/leader-roam-map "s" #'org-roam-db-sync)
(keymap-set lj/leader-roam-map "S" #'consult-org-roam-search)

(keymap-set lj/leader-roam-dailies-map "b" #'org-roam-dailies-goto-previous-note)
(keymap-set lj/leader-roam-dailies-map "d" #'org-roam-dailies-goto-date)
(keymap-set lj/leader-roam-dailies-map "D" #'org-roam-dailies-capture-date)
(keymap-set lj/leader-roam-dailies-map "f" #'org-roam-dailies-goto-next-note)
(keymap-set lj/leader-roam-dailies-map "m" #'org-roam-dailies-goto-tomorrow)
(keymap-set lj/leader-roam-dailies-map "M" #'org-roam-dailies-capture-tomorrow)
(keymap-set lj/leader-roam-dailies-map "n" #'org-roam-dailies-capture-today)
(keymap-set lj/leader-roam-dailies-map "t" #'org-roam-dailies-goto-today)
(keymap-set lj/leader-roam-dailies-map "y" #'org-roam-dailies-goto-yesterday)
(keymap-set lj/leader-roam-dailies-map "Y" #'org-roam-dailies-capture-yesterday)

;; Project: SPC p (built-in project.el)
(keymap-set lj/leader-project-map "&" #'project-async-shell-command)
(keymap-set lj/leader-project-map "b" #'project-switch-to-buffer)
(keymap-set lj/leader-project-map "c" #'project-compile)
(keymap-set lj/leader-project-map "d" #'project-dired)
(keymap-set lj/leader-project-map "f" #'project-find-file)
(keymap-set lj/leader-project-map "k" #'project-kill-buffers)
(keymap-set lj/leader-project-map "p" #'project-switch-project)
(keymap-set lj/leader-project-map "s" #'project-shell-command)

;; Code and personal utilities: SPC c
(keymap-set lj/leader-code-map "b" #'lj/create-note-buffer)
(keymap-set lj/leader-code-map "c" #'compile)
(keymap-set lj/leader-code-map "C" #'recompile)
(keymap-set lj/leader-code-map "d" #'xref-find-definitions)
(keymap-set lj/leader-code-map "D" #'xref-find-references)
(keymap-set lj/leader-code-map "e" #'eval-buffer)
(keymap-set lj/leader-code-map "r" #'eval-region)
(keymap-set lj/leader-code-map "w" #'delete-trailing-whitespace)
(keymap-set lj/leader-code-map "x" #'flymake-show-buffer-diagnostics)

;; Git/Magit: SPC g
(keymap-set lj/leader-git-map "/" #'magit-dispatch)
(keymap-set lj/leader-git-map "." #'magit-file-dispatch)
(keymap-set lj/leader-git-map "b" #'magit-branch-checkout)
(keymap-set lj/leader-git-map "B" #'magit-blame-addition)
(keymap-set lj/leader-git-map "c" #'magit-commit-create)
(keymap-set lj/leader-git-map "C" #'magit-clone)
(keymap-set lj/leader-git-map "F" #'magit-fetch)
(keymap-set lj/leader-git-map "g" #'magit-status)
(keymap-set lj/leader-git-map "G" #'magit-status-here)
(keymap-set lj/leader-git-map "L" #'magit-log-buffer-file)
(keymap-set lj/leader-git-map "s" #'magit-file-stage)
(keymap-set lj/leader-git-map "u" #'magit-file-unstage)
(keymap-set lj/leader-git-map "R" #'vc-revert)

;; Open: SPC o
(keymap-set lj/leader-open-map "-" #'dired-jump)
(keymap-set lj/leader-open-map "O" #'lj/open-obsidian)
(keymap-set lj/leader-open-map "b" #'browse-url-of-file)
(keymap-set lj/leader-open-map "f" #'make-frame)

;; Quit/session: SPC q
(keymap-set lj/leader-quit-map "f" #'delete-frame)
(keymap-set lj/leader-quit-map "K" #'save-buffers-kill-emacs)
(keymap-set lj/leader-quit-map "q" #'save-buffers-kill-terminal)

;; Toggles and timestamp utility: SPC t
(keymap-set lj/leader-toggle-map "c" #'display-fill-column-indicator-mode)
(keymap-set lj/leader-toggle-map "f" #'flymake-mode)
(keymap-set lj/leader-toggle-map "l" #'display-line-numbers-mode)
(keymap-set lj/leader-toggle-map "r" #'read-only-mode)
(keymap-set lj/leader-toggle-map "s" #'lj/unix-timestamp-at-point)
(keymap-set lj/leader-toggle-map "v" #'visible-mode)
(keymap-set lj/leader-toggle-map "w" #'visual-line-mode)

;; Evil provides the virtual <leader> event. SPC is used in normal-like states;
;; M-SPC reaches the same map from insert and Emacs states.
(with-eval-after-load 'evil
  (keymap-set lj/leader-map "h" help-map)
  (keymap-set lj/leader-map "w" evil-window-map)
  ;; Use the function API so this file also compiles correctly when Evil's
  ;; macros are not present in the native compiler process.
  (dolist (state '(normal visual motion insert emacs))
    (evil-global-set-key state (kbd "<leader>") lj/leader-map))
  (evil-set-leader '(normal visual motion) (kbd "SPC"))
  (evil-set-leader '(insert emacs) (kbd "M-SPC")))

;; Emacs 31 includes which-key.
(use-package which-key
  :ensure nil
  :commands which-key-mode
  :init
  (setq which-key-idle-delay 0.4)
  (lj/register-after-ui-task 15 (lambda () (which-key-mode 1)))
  :config
  (which-key-add-keymap-based-replacements
    lj/leader-map
    "b" "buffer"
    "c" "code"
    "f" "file"
    "g" "git"
    "h" "help"
    "n" "notes"
    "o" "open"
    "p" "project"
    "q" "quit/session"
    "s" "search"
    "t" "toggle"
    "w" "window")
  (which-key-add-keymap-based-replacements
    lj/leader-notes-map
    "r" "roam")
  (which-key-add-keymap-based-replacements
    lj/leader-roam-map
    "d" "dailies"))

(provide 'keymaps)

;;; keymaps.el ends here

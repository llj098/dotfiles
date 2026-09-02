;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/syncthing/deft/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.




;; personal stuff begins
;;
;;
;;
(use-package evil
  :custom
  evil-disable-insert-state-bindings t
  )


(setq user-full-name "Lijin Liu"
      user-mail-address "llj098@gmail.com")

(setq doom-font (font-spec :family "JetBrainsMono" :size 12.0 ))

;; BEGIN linux-setup: Simplified Chinese Emacs font
(defun ljx-apply-cjk-font (&optional frame)
  "Use a light Simplified Chinese face in each graphical FRAME."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (dolist (script '(han cjk-misc))
        (set-fontset-font nil script
                          (font-spec :family "Noto Sans CJK SC"
                                     :weight 'light)
                          frame)))))
(add-hook 'after-setting-font-hook #'ljx-apply-cjk-font)
(add-hook 'after-make-frame-functions #'ljx-apply-cjk-font)
;; END linux-setup: Simplified Chinese Emacs font

(use-package! deft
  :config (setq
	   deft-default-extension "org"
	   deft-text-mode 'org-mode
	   deft-use-filename-as-title t
	   deft-directory "~/syncthing/deft/txt"
	   deft-auto-save-interval 5.0
	   deft-use-filter-string-for-filename t
           deft-recursive nil))


;;; Doom Vertico: preview key from C-SPC -> C-,

(after! consult
  ;; 你全局已经用 C-, 了，这里再确保一次（可留可删）
  (setq consult-preview-key "C-,")

  ;; consult 文档里点名的“在 consult-buffer 里常被单独设为手动预览”的 sources
  ;; 把它们的 :preview-key 统一改成 C-,
  (consult-customize
   consult-source-bookmark
   consult-source-file-register
   consult-source-recent-file
   consult-source-project-recent-file
   :preview-key "C-,"))


(after! vertico
  ;; Doom 文档提示：改 preview key 也要改 vertico-map 里的绑定 :contentReference[oaicite:1]{index=1}
  (let ((cmd (lookup-key vertico-map (kbd "C-SPC"))))
    (when (commandp cmd)
      (define-key vertico-map (kbd "C-,") cmd))
    ;; 避免 fcitx5 抢走后导致无效：在 vertico 会话里干脆解绑旧键
    (define-key vertico-map (kbd "C-SPC") nil)
    ;; 终端里 C-SPC 常等价于 C-@，顺手也解绑，兼容更好
    (define-key vertico-map (kbd "C-@") nil)))


;;
;; TODO: orgmode keymap, C-k....
;; https://github.com/hlissner/doom-emacs/issues/2403


;; (after! evil-snipe
;;   (evil-snipe-mode -1))


;;(global-unset-key (kbd "C-SPC"))
;;(evil-define-key 'insert c-mode-map (kbd "C-SPC") nil) ;; company-active-map
;;(evil-define-key 'insert c-mode-map (kbd "C-SPC") nil) ;; company-active-map
;;(evil-define-key 'insert c-mode-map (kbd "M-SPC") 'company-active-map) ;; company-active-map


;; python
;; (use-package! elpy
;;   :config (setq
;;            elpy-rpc-virtualenv-path "~/.venv"))
;; (add-hook! 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook! python-mode
  (setq elpy-rpc-virtualenv-path "~/.venv")
  (elpy-enable)
  ;; Use IPython for REPL
  (setq python-shell-interpreter "~/.venv/bin/jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
   (setq indent-tabs-mode nil
         tab-width 4))


;; flycheck
(add-hook! prog-mode 'flycheck-mode)

;; org
;;; --- 基础路径 ---
;;; ====== PATHS ======

;; ====== 必须“提前”生效的路径（不要放 after!）======
(setq! org-directory (file-truename "~/syncthing/deft/org/")
       org-roam-directory (file-truename (expand-file-name "roam/" org-directory))
       org-roam-dailies-directory (file-truename (expand-file-name "roam/daily" org-directory))
       org-roam-projects-directory (file-truename (expand-file-name "roam/projects" org-directory))
       ;; db 放本机 cache，别进 syncthing
       org-roam-db-location (expand-file-name "org-roam.db" doom-cache-dir)
       org-agenda-files (list (expand-file-name "roam/daily/" org-directory)
                              (expand-file-name "inbox.org" org-directory)
                              (expand-file-name "roam/projects/" org-directory)))

(after! org-roam
  ;;(org-roam-db-autosync-mode 1)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  (require 'org-roam-mode)

  (defvar lj/org-roam-forward-link-target-id nil
    "ID of the org-roam link currently previewed in `org-roam-buffer'.")

  (defun lj/org-roam-forward-link-id-at-point ()
    "Return the target ID of the Org id link at point, or nil."
    (when-let* ((context (org-element-context))
                ((eq (org-element-type context) 'link))
                ((string= (org-element-property :type context) "id"))
                (path (org-element-property :path context)))
      (car (split-string path "::"))))

  (defun lj/org-roam-forward-link-section (_node)
    "Insert a top `org-roam-buffer' section previewing the id link at point."
    (when-let* (((string= (buffer-name) org-roam-buffer))
                (id lj/org-roam-forward-link-target-id)
                (target (org-roam-node-from-id id)))
      (magit-insert-section (org-roam-forward-link)
        (magit-insert-heading "Forward link at point:")
        ;; Reuse Org-roam's own node section renderer, preview function, faces,
        ;; keymaps, and RET behavior instead of maintaining a parallel preview UI.
        (org-roam-node-insert-section
         :source-node target
         :point (or (org-roam-node-point target) 1)
         :properties (list :outline (org-roam-node-olp target)))
        (insert ?\n))))

  (defun lj/org-roam-forward-link-refresh-h ()
    "Refresh `org-roam-buffer' when point enters/leaves a different id link."
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
    "Enable forward-link preview refresh in Org-roam buffers."
    (add-hook 'post-command-hook #'lj/org-roam-forward-link-refresh-h nil t))

  (add-to-list 'org-roam-mode-sections #'lj/org-roam-forward-link-section)
  (add-hook 'org-roam-find-file-hook #'lj/org-roam-forward-link-setup-h))

(use-package! org-transclusion
  :after org
  :commands (org-transclusion-mode
             org-transclusion-add
             org-transclusion-add-all
             org-transclusion-make-from-link
             org-transclusion-refresh
             org-transclusion-open-source
             org-transclusion-move-to-source)
  :init
  (map! :leader
        (:prefix ("n T" . "transclusion")
         :desc "Toggle mode" "t" #'org-transclusion-mode
         :desc "Add at point" "a" #'org-transclusion-add
         :desc "Add all" "A" #'org-transclusion-add-all
         :desc "Make from link" "m" #'org-transclusion-make-from-link
         :desc "Refresh" "r" #'org-transclusion-refresh
         :desc "Open source" "o" #'org-transclusion-open-source
         :desc "Move to source" "g" #'org-transclusion-move-to-source)))

(use-package! org-tree-slide
  :after org
  :commands (org-tree-slide-mode
             org-tree-slide-move-next-tree
             org-tree-slide-move-previous-tree
             org-tree-slide-content
             org-tree-slide-presentation-profile
             org-tree-slide-simple-profile)
  :init
  (map! :leader
        :desc "Org tree slide" "n P" #'org-tree-slide-mode
        (:prefix ("n T" . "transclusion")
         :desc "Slide mode" "s" #'org-tree-slide-mode))
  :config
  ;; Only active while `org-tree-slide-mode' is on, so normal Org editing keeps
  ;; the usual arrow-key behavior.
  (map! :map org-tree-slide-mode-map
        "<right>" #'org-tree-slide-move-next-tree
        "<left>" #'org-tree-slide-move-previous-tree
        "q" #'org-tree-slide-mode
        "C-c C-c" #'org-tree-slide-content))

(use-package! consult-org-roam
  :after org-roam
  :commands (consult-org-roam-file-find
             consult-org-roam-backlinks
             consult-org-roam-backlinks-recursive
             consult-org-roam-forward-links
             consult-org-roam-search)
  :custom
  ;; Official README recommends ripgrep when available.
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; Keep Consult live preview for forward-link preview behind an explicit key.
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")

  (require 'org-roam-mode)

  (defun lj/org-roam-backlink--outline (backlink)
    "Return the outline path recorded for BACKLINK."
    (if-let* ((outline (plist-get (org-roam-backlink-properties backlink) :outline)))
        (mapconcat #'org-link-display-format outline " > ")
      "Top"))

  (defun lj/org-roam-backlink--candidate (backlink)
    "Return a Consult candidate for BACKLINK."
    (let* ((source (org-roam-backlink-source-node backlink))
           (title (or (org-roam-node-title source) "Untitled"))
           (file (org-roam-node-file source))
           (relfile (if file
                        (file-relative-name file org-roam-directory)
                      ""))
           (point (or (org-roam-backlink-point backlink) 1))
           (outline (lj/org-roam-backlink--outline backlink)))
      ;; Include the DB point to keep candidates unique when one source heading
      ;; links to the current node multiple times.
      (cons (format "%s — %s  (%s@%d)" title outline relfile point)
            backlink)))

  (defun lj/org-roam-backlink--goto-point (point)
    "Move to POINT in the current Org buffer and reveal context."
    (widen)
    (goto-char (min (point-max) (max (point-min) point)))
    (when (and (derived-mode-p 'org-mode)
               (fboundp 'org-fold-show-context))
      (org-fold-show-context))
    (recenter))

  (defun lj/org-roam-backlink--visit (backlink &optional other-window)
    "Visit BACKLINK at its exact source position."
    (let ((file (org-roam-node-file (org-roam-backlink-source-node backlink)))
          (point (or (org-roam-backlink-point backlink) 1)))
      (org-roam-preview-visit file point other-window)))

  (defun lj/org-roam-backlink--preview-state (other-window)
    "Return a Consult state function for previewing Org-roam backlinks."
    (let ((open (consult--temporary-files))
          (preview (consult--buffer-preview))
          (window-state (window-state-get nil t)))
      (lambda (action backlink)
        (pcase action
          ('preview
           (if-let* ((backlink backlink)
                     (file (org-roam-node-file (org-roam-backlink-source-node backlink)))
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
    "Select an exact Org-roam backlink to the node at point with Consult.

Unlike `consult-org-roam-backlinks', this uses Org-roam's native
`org-roam-backlinks-get', so the candidate set matches the authoritative
`org-roam-buffer-toggle' backlink data, including individual link positions."
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
    "Select an exact Org-roam backlink, limited to one backlink per source node."
    (interactive "P")
    (lj/consult-org-roam-backlinks other-window t))

  (map! :leader
        (:prefix ("n r" . "roam")
         :desc "Toggle roam buffer" "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer" "R" #'org-roam-buffer-display-dedicated
         :desc "Consult backlinks" "b" #'lj/consult-org-roam-backlinks
         :desc "Consult backlinks unique" "B" #'lj/consult-org-roam-backlinks-unique
         :desc "Forward links" "l" #'consult-org-roam-forward-links
         :desc "Find roam file" "e" #'consult-org-roam-file-find
         :desc "Search roam" "S" #'consult-org-roam-search)))

(defun lj/org-inherited-priority (headline)
  "Return priority for HEADLINE, inheriting from parents when no cookie is present."
  (save-excursion
    (cond
     ;; 当前 headline 自己带 [#A]/[#B]/[#C]
     ((string-match org-priority-regexp headline)
      (* 1000 (- org-priority-lowest
                 (org-priority-to-value (match-string 2 headline)))))

     ;; 没有优先级 cookie，向上找父节点
     ((org-up-heading-safe)
      (lj/org-inherited-priority (org-get-heading)))

     ;; 到顶了还没找到，用默认优先级（无 cookie 等价默认，通常是 B）
     (t
      (* 1000 (- org-priority-lowest org-priority-default))))))


(after! org
  (setq org-agenda-custom-commands
        '(
          ("D" "Deadlines (next 30 days)"
           agenda ""
           ((org-agenda-span 30)
            (org-agenda-entry-types '(:deadline))))

          ("W" "工作视图：隐藏所有 :home:（agenda + alltodo）"
            ((agenda "" ((org-agenda-span 'day)))  ; 你也可以改成 'week
             (alltodo ""))
            ;; 注意：这里是 block agenda 的“全局 options”，对整个视图生效
            ;; 过滤器格式：每个元素是 \"+tag\" 或 \"-tag\"
            ((org-agenda-tag-filter-preset '("-home"))
             (org-agenda-compact-blocks t)))))

  (setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c %l")   ;; <- 关键：%l = 按层级缩进
        (tags   . " %i %-12:c %l")
        (search . " %i %-12:c %l")))

  ;; 新变量名（Org 较新版本）
  (setq org-priority-get-priority-function #'lj/org-inherited-priority)

  ;; 兼容旧版本（有些环境仍用 org-get-priority-function 这个名字）
  (when (boundp 'org-get-priority-function)
    (setq org-get-priority-function #'lj/org-inherited-priority))

  )



;;; ===== 基础路径（和你真实目录一致）=====
(defconst my/org-roam-dir      (expand-file-name "roam/" org-directory))
(defconst my/org-inbox-file    (expand-file-name "inbox.org" org-directory))

(defun my/org--ensure-file (file content)
  (make-directory (file-name-directory file) t)
  (unless (file-exists-p file)
    (with-temp-buffer
      (insert content)
      (write-file file))))

(defun my/org--today-daily-file ()
  (let ((f (expand-file-name (format-time-string "%Y-%m-%d.org") org-roam-dailies-directory)))
    (my/org--ensure-file
     f (format "#+title: %s\n\n* Tasks\n\n* Log\n\n* Notes\n"
               (format-time-string "%Y-%m-%d")))
    f))

(defun my/org--ensure-inbox ()
  (my/org--ensure-file
   my/org-inbox-file
   "#+title: Inbox\n#+filetags: :inbox:\n\n* Tasks\n\n* Notes\n"))

(defun my/org--ensure-project-hub (slug title)
  "如果项目文件不存在就创建一个最小 Hub（含 Tasks/Log）。"
  (let ((file (expand-file-name (concat slug ".org") org-roam-projects-directory)))
    (my/org--ensure-file
     file (format "#+title: %s\n#+category: %s\n\n* Overview\n\n* Tasks\n\n* Log\n\n* Links\n"
                  title title))
    file))

;; ====== 你的项目表：加一行就多一个项目快捷键 ======
;; key 是第二个按键：比如 pt=交易，pk=育儿
(defvar my/org-project-specs
  '(("t" "trading"   "交易")
    ("q" "quant"     "量化")
    ("p" "piano"     "钢琴")
    ("k" "parenting" "育儿")
    ("b" "baking"    "烘焙")))

(defun my/org--project-capture-templates ()
  (mapcan
   (lambda (spec)
     (pcase-let ((`(,k ,slug ,title) spec))
       (let ((file (my/org--ensure-project-hub slug title)))
         (list
          ;; 方案B：双键直达项目 Tasks（pt/pk/pq...）
          `(,(concat "p" k) ,(format "%s · Task" title) entry
            (file+headline ,file "Tasks")
            "** TODO %?\n%U\n%a\n"
            :prepend t :empty-lines 1)

          ;; 同项目的“记录/想法”（Pt/Pk/Pq...）
          `(,(concat "P" k) ,(format "%s · Note" title) entry
            (file+headline ,file "Log")
            "** %U %?\n%a\n"
            :prepend t :empty-lines 1)))))
   my/org-project-specs))

(after! org-capture
  (my/org--ensure-inbox)
  (setq org-capture-templates
        (append
         `(
           ("t" "Today · Task" entry
            (file+headline (lambda() (my/org--today-daily-file)) "Tasks")
            "** TODO %?\n%T\n" :prepend t :empty-lines 1)

           ("i" "Inbox · Task" entry
            (file+headline my/org-inbox-file "Tasks")
            "* TODO %?\n%U\n"
            :prepend t :empty-lines 1)

           ("p" "Project templates")

           ("pt" "trading · Task" entry
            (file+headline (lambda() (expand-file-name "trading.org" org-roam-projects-directory))  "Tasks")
            "** TODO %?\n%U\n"
            :prepend t :empty-lines 1)

           ("W" "Market watch (daily)" entry
            (file+olp (lambda() (expand-file-name "market-watch.org" org-roam-projects-directory))  "Market Watch Log")
            ;;"*** %^{Symbol}\n:PROPERTIES:\n:CREATED: %U\n:STATUS: %^{状态|震荡|多头|空头|其他}\n:OPP: %^{机会|多|空|None}\n:ITV: %^{itv|1d|4h|1h|15m}\n:END:\n\n"
            (file "~/.config/doom/t.org")
            :prepend t :empty-lines 1)

           ("pk" "育儿 · Task" entry
            (file+headline (lambda() (expand-file-name "parenting.org" org-roam-projects-directory))  "Tasks")
            "** TODO %?\n%U\n"
            :prepend t :empty-lines 1)

           ("ph" "健康 · Task" entry
            (file+headline (lambda() (expand-file-name "health.org" org-roam-projects-directory))  "Tasks")
            "** TODO %?\n%U\n"
            :prepend t :empty-lines 1)

           ("pb" "baking · Task" entry
            (file+headline (lambda() (expand-file-name "baking.org" org-roam-projects-directory))  "Tasks")
            "** TODO %?\n%U\n"
            :prepend t :empty-lines 1)

           ("pq" "quant · Task" entry
            (file+headline (lambda() (expand-file-name "quant.org" org-roam-projects-directory))  "Tasks")
            "** TODO %?\n%U\n"
            :prepend t :empty-lines 1)

         )))
  )

;; 解决 org-capture 在目标开启 columns-view 写入失败问题：先关闭 colunms-view
(after! org-capture
  (defun my/org-capture--quit-columns (&rest _)
    (let* ((buf  (org-capture-get :buffer))
           (file (and (buffer-live-p buf) (buffer-file-name buf))))
      (dolist (b (if file
                     (cl-remove-if-not
                      (lambda (x)
                        (and (buffer-file-name x)
                             (file-equal-p (buffer-file-name x) file)))
                      (buffer-list))
                   (list buf)))
        (with-current-buffer b
          (when (bound-and-true-p org-columns-mode)
            (ignore-errors (org-columns-quit)))))))

  (advice-add #'org-capture-place-template :before #'my/org-capture--quit-columns)
  (advice-add #'org-capture-place-template :around
              (lambda (fn &rest args)
                (let ((inhibit-read-only t))
                  (apply fn args)))))



(after! org
  (add-to-list 'org-capture-templates
               '("mw" "Market watch (daily)" entry
                 (file+olp "~/org/trading.org" "Market Watch Log")
                 "* %<%Y-%m-%d %a>\n:PROPERTIES:\nREGIME: %^{Regime|trend|range|risk-on|risk-off}\n:ACTION: %^{Action|watch|long|short|reduce|no-trade}\n:END:\n\n- LB notes:\n  %?\n- MS notes:\n\n")))


;; raw org-todo-keywords
;;((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)") (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)") (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
(after! org
  (setq org-log-into-drawer "LOGBOOK"
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
           "DONE(d!)" "CANCELLED(c@)"))))



 (add-hook! org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org . t)
     (lilypond . t))))

(setq org-startup-folded t)





;; (use-package! company
;;   :config
;;   (general-define-key
;;    :keymaps   'company-mode-map
;;    "C-;"      'company-complete)

;;   (general-define-key
;;    :keymaps   'company-active-map
;;    "C-;"      'company-complete
;;    "M-1"      'company-complete-number
;;    "M-2"      'company-complete-number
;;    "M-3"      'company-complete-number
;;    "M-4"      'company-complete-number
;;    "M-5"      'company-complete-number
;;    "M-6"      'company-complete-number
;;    "M-7"      'company-complete-number
;;    "M-8"      'company-complete-number
;;    "M-9"      'company-complete-number
;;    "M-0"      'company-complete-number)

;;     (setq company-show-numbers t
;;         company-idle-delay 0.3
;;         company-tooltip-limit 10
;;         company-auto-commit nil
;;         company-auto-commit-chars '(46)
;;         company-dabbrev-other-buffers t
;;         company-selection-wrap-around t
;;         company-minimum-prefix-length 2
;;         company-dabbrev-code-everywhere nil
;;         company-dabbrev-downcase nil
;;         company-dabbrev-ignore-case 'keep-prefix
;;         company-dabbrev-code-ignore-case nil
;;         company-dabbrev-ignore-buffers "\\`[ *]"))



(defun unix-timestamp2 ()
  "convert cursor words to time"
  (interactive)
  (let ((word (string-to-number (thing-at-point 'word))))
    (message (unix-timestamp-to-string word))))


(defun unix-timestamp-to-string (unix-timestamp &optional timezone)
 "Convert UNIX-TIMESTAMP to a human-readable string.
TIMEZONE is an optional argument to specify the time zone."
 (let ((time (seconds-to-time unix-timestamp)))
    (format-time-string "%Y-%m-%d %H:%M:%S" time timezone)))


(defun convert-selected-timestamp-to-string ()
 "Convert selected Unix timestamp to a string."
 (interactive)
 (when (use-region-p)
    (let ((timestamp (string-to-number (buffer-substring-no-properties (region-beginning) (region-end)))))
      (message (unix-timestamp-to-string timestamp)))))


(defun create-buffer-with-random-file-name ()
 "Create a new buffer with a random file name."
 (interactive)
 (let* ((buffer (generate-new-buffer "*scratch*"))
         (random-file-name (concat (expand-file-name "~/syncthing/deft/txt/")
                                   (number-to-string (time-convert (current-time) 'integer))
                                   ".org")))
    (with-current-buffer buffer
      (set-visited-file-name random-file-name)
        (switch-to-buffer buffer))))


;; Follow the active desktop's light/dark mode without polling.  Darkman and
;; Omarchy invoke the functions in this module from their native change hooks.
(load! "desktop-theme")

(defun open-obsidian()
  (interactive)
  (dired (expand-file-name "~/syncthing/obsidian")))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "hs" 'helm-swoop
  "k" 'kill-buffer
  "I" 'consult-imenu
  ;;"I" 'helm-imenu
  ;;"I" 'ivy-imenu-anywhere
  "pf" 'helm-projectile-find-file
  "pF" 'helm-projectile-find-file-dwim
  ;;"ss" 'helm-swoop
  "gd" 'xref-find-definitions
  "gs" 'magit-status
  "r" 'er/expand-region
  "*"  'helm-projectile-ag
  "m" 'pop-global-mark
  "ts" 'unix-timestamp2
  "cb" 'create-buffer-with-random-file-name
  "gO" 'open-obsidian)


;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)
;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


(defun lj/org-headings(content)
  (-filter (lambda (x) (s-starts-with? "*" x))
           (-remove #'s-blank?  (s-split "\n" content))))


(defun lj/mv-org-file (buf-fname)
  "A function that requires user confirmation before execution."
  (interactive)
  (if (and (s-ends-with? "deft/txt/" (file-name-directory buf-fname))
           (yes-or-no-p "Do you want to move file to bak dir ?"))
      (let ((fname (file-name-nondirectory buf-fname) ))
        (f-move fname (format "bak/%s" fname))
        (message "file %s moved to bak dir" fname))
    (message "Operation canceled.")))

(defun lj/-org-fn(fname)
  (s-join "-" (s-split-words (s-replace "*" "" fname))))

(defun lj/rename-org-file()
  (interactive)
  (let ((fname (nth 0  (lj/org-headings (buffer-string)) ))
        (buf-fname buffer-file-name))
    (message (format "%s.org" (lj/-org-fn fname)))
    (save-buffer)
    (write-file (format "%s.org"  (lj/-org-fn fname)) 't)
    (lj/mv-org-file buf-fname)
    ))

(defun lj/markdown-to-org ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (format "pandoc -f markdown -t org -o %s"
           (concat (file-name-sans-extension (buffer-file-name)) ".org"))))


;; ============================================================
;; TypeScript / JavaScript IDE 配置
;; ============================================================

;; Tree-sitter 语法源（确保 grammar 可自动安装）
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (yaml       "https://github.com/tree-sitter/tree-sitter-yaml")))

;; 不要在启动时自动安装缺失 grammar；网络抖动会把 Emacs 启动卡到几十秒。
;; 需要时手动执行：M-x lj/install-missing-treesit-grammars
(defun lj/install-missing-treesit-grammars ()
  "Install missing tree-sitter grammars on demand."
  (interactive)
  (dolist (lang '(typescript tsx javascript json css html yaml))
    (unless (treesit-language-available-p lang)
      (message "Installing tree-sitter grammar: %s" lang)
      (treesit-install-language-grammar lang))))

;; LSP 性能优化
(after! lsp-mode
  (setq lsp-idle-delay 0.5
        lsp-log-io nil                  ; 关闭 IO 日志，提升性能
        lsp-completion-provider :capf   ; 用 corfu/capf
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-diagnostics-enable t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil    ; 用 K 手动看
        lsp-ui-doc-show-with-mouse t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t))

;; TypeScript 专项：确保所有 TS/JS 模式（含 tree-sitter 变体）自动启动 LSP
(add-hook! '(typescript-ts-mode-hook
             typescript-mode-hook
             tsx-ts-mode-hook
             js-ts-mode-hook
             js-mode-hook)
  #'lsp-deferred)

(after! lsp-mode
  (setq lsp-clients-typescript-server-args '("--stdio"))

  (defun +lsp/organize-imports ()
    (interactive)
    (lsp-organize-imports))

  (add-hook! '(typescript-ts-mode-hook typescript-mode-hook
              tsx-ts-mode-hook js-ts-mode-hook js-mode-hook)
    (add-hook 'before-save-hook #'+lsp/organize-imports nil t)))

;; Prettier 作为 JS/TS 格式化工具
(after! format
  (set-formatter! 'prettier
    '("prettier" "--stdin-filepath" filepath)
    :modes '(typescript-mode typescript-tsx-mode js-mode js2-mode json-mode css-mode html-mode yaml-mode)))

;; 修复 ielm 中 RET 被 evil-collection 劫持为 comint-send-input 导致不求值的问题
;; （evil-collection 对 comint-mode-map 的 insert-state 绑定优先于 ielm-map 本体）
(after! ielm
  (evil-define-key 'insert ielm-map
    (kbd "RET") #'ielm-return
    (kbd "<return>") #'ielm-return))

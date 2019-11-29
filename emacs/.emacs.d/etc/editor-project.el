;; Globally enable save place mode
(save-place-mode 1)

;; Write backup ffiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; Magit
(straight-use-package 'magit)
(global-set-key (kbd "C-x g g") 'magit-status)
(global-set-key (kbd "C-x g f") 'magit-find-file)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "C-c M-g") 'magit-file-dispatch)

(straight-use-package 'magit-todos)

(straight-use-package 'diff-hl)
(global-diff-hl-mode)

;; Highlight changed files in the fringe of Dired
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; Fall back to the display margin, if the fringe is unavailable
(unless (display-graphic-p)
  (diff-hl-margin-mode))

;; Refresh diff-hl after Magit operations
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

(straight-use-package 'undo-tree)
(setq undo-tree-auto-save-history t)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(straight-use-package 'projectile)
(setq projectile-dynamic-mode-line nil
      projectile-enable-caching t
      projectile-mode-line-prefix ""
      projectile-sort-order 'recentf
      projectile-project-compilation-cmd "make "
      projectile-completion-system 'ivy
      )
(projectile-mode)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

(straight-use-package 'counsel-projectile)
(counsel-projectile-mode)

(straight-use-package 'go-projectile)
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  (go-eldoc-setup)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (add-to-list 'company-backends 'company-go))
(add-hook 'go-mode-hook 'my-go-mode-hook)

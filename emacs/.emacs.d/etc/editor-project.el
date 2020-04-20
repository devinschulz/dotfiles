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

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :init
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (global-magit-file-mode))

(use-package magit-todos
  :straight t)

;; Package `forge` provides a Github interface directly within magit
(use-package forge)

(use-package git-commit
  :config
  (setq git-commit-summary-max-length 50))

;; Package `git-link` provides a simple function M-x git-link which
;; copies to the kill ring a link to the current line of code or
;; selection on Github
(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package diff-hl
  :after magit

  :hook (
         (vc-dir-mode . turn-on-diff-hl-mode)
         ;; Since diff-hl only updates highlights whenever the files
         ;; has saved, flydiff-mode offers highlighting when the file
         ;; has yet to be saved
         ((dired-mode prog-mode vc-dir-mode) . diff-hl-flydiff-mode)

         ;; Refresh diff-hl after Magit operations
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("M-/" . undo-tree-redo))
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil))

(use-package projectile
  :bind-keymap* (("C-x p" . projectile-command-map))
  :config

  (setq projectile-indexing-method 'alien)

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  (setq projectile-switch-project-action 'projectile-commander)

  (projectile-mode +1))

(use-package go-projectile
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode)                    ; highlight identifiers
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

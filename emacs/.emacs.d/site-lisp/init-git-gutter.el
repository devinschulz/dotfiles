;; (use-package git-gutter
;;   :init (global-git-gutter-mode)
;;   :config (setq git-gutter:lighter "")
;;   (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)
;;   :bind
;;   ("C-x g p" . git-gutter:previous-hunk)
;;   ("C-x g n" . git-gutter:next-hunk)
;;   ("C-x g s" . git-gutter:stage-hunk)
;;   ("C-x g r" . git-gutter:revert-hunk))

;; (use-package diff-hl
;;   :config
;;   (global-diff-hl-mode))

(use-package diff-hl
  :commands (diff-hl-magit-post-refresh global-diff-hl-mode)
  :functions (diff-hl-flydiff-mode diff-hl-margin-mode)
  :defines diff-hl-margin-symbols-alist
  :init
  (progn
    (setq diff-hl-margin-symbols-alist
           '((insert . "+") (delete . "-") (change . "~")
             (unknown . "?") (ignored . "i")))
    (global-diff-hl-mode)
    (diff-hl-margin-mode)
    (diff-hl-flydiff-mode)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'init-git-gutter)

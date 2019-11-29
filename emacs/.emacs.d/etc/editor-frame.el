;; disable splash screen
(setq inhibit-startup-message t)

;; Disable flashing for beeps
(setq ring-bell-function 'ignore)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the scroll bar
(toggle-scroll-bar -1)

;; Disable the toolbar
(tool-bar-mode -1)

(use-package avy
  :bind (("C-c f" . avy-goto-char)
         ("C-c h" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c w" . avy-goto-word-1)
         ("C-c o" . avy-org-goto-heading-timer)
         ("M-g a" . avy-copy-line)
         ("M-g r" . avy-copy-region)
         ("M-g y" . avy-kill-ring-save-whole-line)
         ("M-g w" . avy-kill-whole-line)
         ("M-g l" . avy-move-line)
         ("M-g c" . avy-kill-ring-save-region)
         ("M-g m" . avy-move-region)
         ("M-g k" . avy-kill-region)))

(use-package move-text
  :bind (("M-n" . move-text-down)
         ("M-p" . move-text-up)))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/engine 'emacs))

(use-package goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

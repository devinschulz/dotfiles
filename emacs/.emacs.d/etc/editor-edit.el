(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)

;; Auto-pair
(electric-pair-mode 1)

(defun display-startup-echo-area-message ()
  (message ""))

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq delete-by-moving-to-trash t)
(setq track-eol t) ; Keep cursor at end of lines
(setq line-move-visual nil) ; To be required by track-eol

(setq-default kill-whole-line t) ; Kill line including '\n'
(setq-default indent-tabs-mode nil) ; use space

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package move-text
  :bind
  ("M-g <up>" . move-text-up)
  ("M-g <down>" . move-text-down))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Highlights changes to the buffer caused by commands
;; such as ‘undo’, ‘yank’/’yank-pop’, etc.
(use-package volatile-highlights
  :demand t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; Automatically reload files that was modified by an external program
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete selection when inserting someting
(use-package delsel
  :config (delete-selection-mode))

(use-package hungry-delete
  :config (global-hungry-delete-mode))

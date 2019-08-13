;; Based on https://sam217pa.github.io/2016/08/30/how-to-make-your-own-spacemacs/#fnref:3
(use-package ivy
  :init
  (ivy-mode 1) ; globally at startup
  :bind
  ("C-x C-r" . ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 20
        ivy-count-format "%d/%d "))

(use-package counsel
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package swiper
  :ensure t
  :config
  (defun swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol))) (swiper sym))
  :bind ("M-s w" . swiper-at-point))

(provide 'init-ivy)

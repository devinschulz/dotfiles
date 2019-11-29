(use-package flycheck
  :diminish flycheck-mode
  :hook ((after-init . global-flycheck-mode)
         (flycheck-mode . add-node-modules-path))
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  (setq flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flyspell-correct
  :diminish flyspell-correct-auto-mode
  :bind (("C-c i n" . flyspell-correct-next)
         ("C-c i p" . flyspell-correct-previous)
         ("C-c i w" . flyspell-correct-wrapper))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-correct-ivy
  :requires (flyspell-correct)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package ispell
  :init
  (setq ispell-program-name "aspell")
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)
         ("C-c i w" . ispell-word)))

(use-package flyspell                   ;
  :diminish flyspell-mode
  :requires (ispell)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . turn-on-flyspell)
         (org-mode . flyspell-mode))
  :commands (flyspell-mode flyspell-prog-mode))

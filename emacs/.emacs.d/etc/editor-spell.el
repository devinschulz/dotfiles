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

(use-package lorem-ipsum
  :ensure t
  :bind (("C-c C-l s" . lorem-ipsum-insert-sentences)
         ("C-c C-l p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-l l" . lorem-ipsum-insert-list)))

(use-package flyspell-correct
    :ensure t
    :diminish flyspell-correct-auto-mode
    :bind (("C-c i n" . flyspell-correct-next)
           ("C-c i p" . flyspell-correct-previous))
    :config
    (defun frog-menu-flyspell-correct (candidates word)
      (let* ((corrects (if flyspell-sort-corrections
                       (sort candidates 'string<)
                     candidates))
         (actions `(("C-s" "Save word"         (save    . ,word))
                    ("C-a" "Accept (session)"  (session . ,word))
                    ("C-b" "Accept (buffer)"   (buffer  . ,word))
                    ("C-c" "Skip"              (skip    . ,word))))
         (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                   ispell-dictionary
                                                   "default")))
         (res      (frog-menu-read prompt corrects actions)))
        (unless res
          (error "Quit"))
        res))
    (setq flyspell-correct-interface #'frog-menu-flyspell-correct))

(use-package ispell
    :ensure nil
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)
           ("C-c i w" . ispell-word)))

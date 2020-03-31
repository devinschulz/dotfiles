(use-package flycheck
  :defer 4
  :hook ((after-init . global-flycheck-mode)
         (flycheck-mode . add-node-modules-path))
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. See
  ;; https://github.com/flycheck/flycheck/pull/1308.
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to only briefly. This allows "refreshing" the syntax
  ;; check state for several buffers quickly after e.g. changing a
  ;; config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors in the echo area after only 0.2 seconds, not 0.9.
  (setq flycheck-display-errors-delay 0.2)

  (global-flycheck-mode +1))

(use-package flyspell-correct
  :diminish flyspell-correct-auto-mode
  :bind (("C-c i n" . flyspell-correct-next)
         ("C-c i p" . flyspell-correct-previous)
         ("C-c i w" . flyspell-correct-wrapper))
  :config
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
  :requires (ispell)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . turn-on-flyspell)
         (org-mode . flyspell-mode))
  :commands (flyspell-mode flyspell-prog-mode))

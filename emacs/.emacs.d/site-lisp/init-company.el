;; company
(use-package company
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

;; Complete for web,html,emmet,jade,slim modes
(use-package company-web
  :after company
  :config (add-to-list 'company-backends 'company-web-html))

(provide 'init-company)

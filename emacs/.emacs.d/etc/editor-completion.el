(use-package company
  :init
  :config
  (setq company-idle-delay 0.15
        company-tooltop-minimum company-tooltip-limit
        company-frontends '(company-pseudo-tooltip-frontend)
        company-show-numbers t
        company-require-match #'company-explicit-action-p
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t)
  (global-company-mode +1))

(use-package company-lsp)

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

;; Complete for web,html,emmet,jade,slim modes
(use-package company-web
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package company-go)

(use-package which-key
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-max-height 0.25
      which-key-idle-delay 0.25)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

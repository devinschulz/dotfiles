(use-package company
  :diminish
  :init (global-company-mode 1)
  :commands (company-mode)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ([tab] . company-select-next)
              ([backtab] . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

;; Complete for web,html,emmet,jade,slim modes
(use-package company-web
  :after company
  :config (add-to-list 'company-backends 'company-web-html))

(use-package company-go
  :after company)

(use-package which-key
  :defer 5
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.25)
  :commands which-key-mode)

 (use-package prescient
   :config
   (progn
     (prescient-persist-mode t)))

 (use-package company-prescient
   :after company
   :config
   (progn
     (company-prescient-mode t)))

 (use-package ivy-prescient
   :after ivy
   :config
   (progn
     (ivy-prescient-mode t)))

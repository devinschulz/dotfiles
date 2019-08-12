(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.05)
  :diminish which-key-mode)
(provide 'init-which-key)

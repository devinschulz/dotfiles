(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq ns-auto-hide-menu-bar nil)

  (setq initial-frame-alist
     (append
      '((ns-transparent-titlebar . t)
        (ns-appearance . dark)
        (vertical-scroll-bars . nil)
        (internal-border-width . 0)))))

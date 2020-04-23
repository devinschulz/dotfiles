(use-package company
  :config

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

 ;; Make completions display as soon as possible
  (setq company-idle-delay 0.0)

  ;; Make completions display after only typed one character, instead
  ;; of three.
  (setq company-minimum-prefix-length 1)

    ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

   ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

    ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

    ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (global-company-mode +1))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode +1))

;; Complete for web,html,emmet,jade,slim modes
(use-package company-web
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package company-go)

(use-package which-key
  :defer 2
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-idle-delay 0.25)
  :config
  (which-key-mode))

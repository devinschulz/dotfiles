(use-package org
  :init
  (setq org-directory "~/Google Drive/My Notes")
  (setq org-hide-emphasis-markers nil)
  (setq org-pretty-entities nil))

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%Y-%m-%d, %A"))

(use-package deft
  :commands deft
  :bind ("<f8>" . deft)
  :config
  (setq zettel-indicator "§")
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-default-extension "txt")
  (setq deft-directory (concat org-directory "/braindump"))
  (setq deft-recursive t))

(use-package zetteldeft
  :after deft)

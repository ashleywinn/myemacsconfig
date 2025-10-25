;;; awinn-orgmode.el --- my org-mode config          -*- lexical-binding: t; -*-

;; /.../ for italic
;; *...* for bold
;; =...= for verbatim
;; ~...~ for code
(setq org-hide-emphasis-markers t)

;; Change '-' to bullets
;; *** this doesn't seem like it works
(add-hook 'org-mode-hook
  (lambda () 
     (font-lock-add-keywords 'org-mode
        '(("^ *\\([-])\\) "
           (0 (prog1 ()
                (compose-region (match-beginning 1)
                                (match-end 1) "•"))))))))

;; Change list of * to unique unicode bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold )))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


;; Org-roam setup
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/notes"))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r g" . org-roam-graph)
         ("C-c r c" . org-roam-capture)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c r t" . org-id-get-create)
         )
  :config
  (org-roam-db-autosync-mode))

(provide 'awinn-orgmode)

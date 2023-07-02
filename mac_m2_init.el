;; init.el

;; recently switched to 'use-package' which automatically downloads
;; required packages

;; *** for the doom-modeline icons, the first time Emacs is started
;;     need to run:
;;     > M-x all-the-icons-install-fonts

;; I can't figure out how to use this in my mode hooks
(defvar my-font-mono-height 120 "default height for my mono fonts")

(setq inhibit-startup-message t)

(setq initial-frame-alist
          '((top . 30) (left . 50) (width . 180) (height . 55)))

;; mac-specific key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; Set the fixed pitch face
(set-face-attribute 'default nil :font "SF Mono" :height 130)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 130)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "SF Pro" :height 130 :weight 'regular)

;; Program-mode hook
(defun arw/prog-mode-font-hook ()
  (setq buffer-face-mode-face '(:family "Fira Code" :height 130))
  (buffer-face-mode))

(add-hook 'prog-mode-hook 'arw/prog-mode-font-hook)

;; ----------------------------
;; External Packages setup

;; solved an error where emacs continued to use older compiled byte code
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; ---------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpa-Mirror : create local mirror elpa for using on firewalled machine
;;; run: elpamr-create-mirror-for-installed
(use-package elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/.emacs.d/local_elpa_mirror"))

;; use spaces for tabs
(setq-default indent-tabs-mode nil)

;; Turn off the top window buttons
(tool-bar-mode 0)

;; Save history between sessions
(savehist-mode 1)

;; Show column numbers
(setq column-number-mode t)

;; line numbers
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))

;; highlight column characters beyond this width
(setq whitespace-line-column 90)

;; switch buffers
(global-set-key (kbd "M-o") 'other-window)

;; parenthesis, brackets, etc. have matching colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; jump between matching symbols
(use-package smartscan
  :config
  (add-hook 'prog-mode-hook (lambda () (smartscan-mode 1))))

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "M-r") 'hippie-expand)

;; use the cool ligatures in Fira Code
(mac-auto-operator-composition-mode)

;;;; My custom commands will all use C-l

;; I want to use C-l for my custom comands
(global-set-key (kbd "C-l") nil)

;; by default C-l, recenters this is now C-l l
(global-set-key (kbd "C-l l") 'recenter-top-bottom)

;; truncate lines switch
(global-set-key (kbd "C-l t") 'toggle-truncate-lines)

;; swap buffers in windows
(global-set-key (kbd "C-l b") 'window-swap-states)


(use-package zenburn-theme)

;; Scrolling
;; I was using 'C-u' and 'C-j' for a long time, but kept learning about
;; other important commands that those bindings interfere with.
;; So I wanted to use 'C-i' and 'C-m' directly, that but that
;; causes all sorts of trouble.
;;
;; Smartrep: smart repeat allows smart repeat key bindings
(use-package smartrep
  :config
  (smartrep-define-key
      global-map "C-l" '(("C-i" . (scroll-down-command 5))
                    ("C-k" . (scroll-up-command 5))
                    ("C-u" . (scroll-down-command 10))
                    ("C-j" . (scroll-up-command 10))
                    ("C-l" . (recenter-top-bottom))
                    ("C-v" . (scroll-up-command))
                    ("M-v" . (scroll-down-command)))))

(use-package avy
  :bind
  ("C-;" . avy-goto-char-timer))

(global-set-key (kbd "C-'")
                (lambda () (interactive)
                  (let ((current-prefix-arg 4))
                    (call-interactively #'set-mark-command))))

;; Scroll with the 'U' and 'J' keys
;; (global-set-key "\C-u" 'scroll-down-five)
;; (global-set-key "\C-j" 'scroll-up-five)
;; (defun scroll-up-five ()
;;   (interactive)
;;   (scroll-up-command 5))
;; (defun scroll-down-five ()
;;   (interactive)
;;   (scroll-down-command 5))

(global-set-key [f6]   'revert-buffer)
(global-set-key (kbd "C-l r") 'revert-buffer)

(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)


;;; These stopped working when I started adding a bunch of new packages
;;; I'm not quite sure why but previous-buffer seems like a better option anyway
;; (defun buffer-ring ()
;;   (interactive) (bury-buffer (current-buffer))
;;   (switch-to-buffer (other-buffer))
;; )

;; (defun buffer-ring-previous ()
;;   (interactive)
;;   (switch-to-buffer (car (reverse (buffer-list))))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame presets

(setq frame-resize-pixelwise t)

;; https://christiantietze.de/posts/2022/04/emacs-center-window-current-monitor-simplified/
(defun ash/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(defun set-frame-m2air ()
  (interactive)
  (set-frame-font (font-spec :size 13))
  (set-frame-size (selected-frame) 1400 800 t)
  (set-frame-position (selected-frame) 15 45)
  )

(defun set-frame-work-external ()
  (interactive)
  (set-frame-font (font-spec :size 15))
  (set-frame-size (selected-frame) 1800 1200 t)
  (ash/frame-recenter (selected-frame))
  )

(defun set-frame-home-external ()
  (interactive)
  (set-frame-font (font-spec :size 15))
  (modify-frame-parameters
   (selected-frame) '((user-position . t) (top + -1321) (left + -200)))
  (set-frame-size (selected-frame) 1800 1200 t)
  )

(defun set-frame-95 ()
  (interactive)
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.95) (height . 0.95)))
  (ash/frame-recenter (selected-frame))
  )

(defun set-frame-top-right ()
  (interactive)
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.33) (height . 0.70)
                      (user-position . t) (top . 0.05) (left . 0.98)))
  )

(defun set-frame-left-pane ()
  (interactive)
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.37) (height . 0.90)
                      (user-position . t) (top . 0.4) (left . 0.25)))
  )

(defun set-frame-right-pane ()
  (interactive)
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.37) (height . 0.90)
                      (user-position . t) (top . 0.4) (left . 0.8)))
  )

(global-set-key (kbd "C-l 1") 'set-frame-m2air)
(global-set-key (kbd "C-l 2") 'set-frame-work-external)
(global-set-key (kbd "C-l 3") 'set-frame-home-external)
(global-set-key (kbd "C-l 4") 'set-frame-left-pane)
(global-set-key (kbd "C-l 5") 'set-frame-95)
(global-set-key (kbd "C-l 6") 'set-frame-right-pane)
(global-set-key (kbd "C-l 0") 'set-frame-top-right)




(global-set-key (kbd "C-l w") 'delete-trailing-whitespace)

(use-package recentf
      :bind ("C-x C-r" . recentf-open-files)
      :config
      (setq recentf-max-menu-items 15
            recentf-max-saved-items 1000
       )
      :hook (after-init . recentf-mode))

(use-package init-open-recentf
      :after recentf
      :config (init-open-recentf))

(use-package ivy
  :diminish
  :bind (("C-q" . swiper)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         ("M-b" . ivy-backward-delete-char))
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; don't start M-x with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :custom
  (ivy-rich-parse-remote-buffer nil))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-file-exists-remote-cache-expire nil)
    (setq projectile-globally-ignored-directories
          (quote
           (".git" ".tox" "compile" "sim" "build" "target")))
    (setq projectile-globally-ignored-file-suffixes '("~" ".log")))
  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; trying to use a remote directory here was a bad idea
  ;; :init
  ;; (when (file-directory-p "/scp:sha-fpga01:/auto/crdc_iot_fpga/users/aswinn")
  ;;   (setq projectile-project-search-path
  ;;         '("/scp:sha-fpga01:/auto/crdc_iot_fpga/users/aswinn")))
  ;; (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
   :config (counsel-projectile-mode))

;; ----------------------------------------
;; Dired
(setq dired-dwim-target t)


;; ------------------------------------------
;; Org Mode
(defun arw/org-mode-setup ()
;;  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode 0))


(defun arw/org-font-setup ()
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook
  (org-mode . arw/org-mode-setup)
  (org-mode . arw/org-font-setup)
  :bind (("C-l a" . org-agenda)
         ("C-l c" . org-capture))
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun arw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . arw/org-mode-visual-fill))


(setq org-capture-templates
    `(("t" "Work Tasks")
      ("tt" "Task" entry
       (file+olp "~/icloud_docs/Documents/diary/work.org" "New")
       "* TODO %?\n%U\n%a\n%i" :empty-lines 1)
      ("tf" "Fix-Me" entry
       (file+olp "~/icloud_docs/Documents/diary/work.org" "Stuff to Fix")
       "* TODO %?\n:fixme:\n%U\n%a\n%i" :empty-lines 1)
      ("p" "Personal Task" entry
       (file+olp "~/icloud_docs/Documents/diary/personal.org" "New")
       "* TODO %?\n%U\n%i" :empty-lines 1)
      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/icloud_docs/Documents/diary/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/icloud_docs/Documents/diary/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)))


;; Org-mode stuff
;;(use-package ox-md)

;; PlantUML for org mode
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml.jar"))


(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)


;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))


(setq load-path (append load-path (list "~/elisp")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

; (require 'yaml-mode)
; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))


;; don't wrap lines
;;(setq-default truncate-lines t)
;;(setq-default truncate-partial-width-windows t)

;; turn off ElDoc mode
;(global-eldoc-mode -1)

;; frequent commands
(global-set-key [f2]      'compile)
(global-set-key [f3]      'shell)

;; multiple cursors
(use-package mc-extras
     :bind (("C-c m c" . mc/edit-lines)
            ("C->" . mc/mark-next-like-this)
            ("C-<" . mc/mark-previous-like-this)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
(use-package diff-hl
     :config
     (global-diff-hl-mode))

(use-package magit
     :bind (("C-c g" . magit-file-dispatch))
     :config
;; display status in the gutter
     (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Set a unique remote path
(connection-local-set-profile-variables 'remote-path-with-cisco-bin
              '((tramp-remote-path . ("/usr/cisco/bin" tramp-default-remote-path))))

(connection-local-set-profiles
             '(:application tramp :machine "sha-iot-edit1") 'remote-path-with-cisco-bin)

(connection-local-set-profiles
             '(:application tramp :machine "sha-iot-fpga01") 'remote-path-with-cisco-bin)

;; ssh config alias
(connection-local-set-profiles
             '(:application tramp :machine "sha-fpga01") 'remote-path-with-cisco-bin)


;; Python
(add-to-list 'exec-path "/Users/aswinn/.pyenv/shims")

;; these overrides are no longer necessary since all my shortcuts
;; are now prefixed with C-l. but keeping the code to remember
;; (add-hook 'org-mode-hook
;;           '(lambda ( )
;;              (define-key org-mode-map (kbd "C-j") nil)))

;; (add-hook 'magit-status-mode-hook
;;           '(lambda ( )
;;              (define-key magit-status-mode-map (kbd "C-j") nil)))

(add-hook 'verilog-mode-hook
          #'(lambda ( )
             (define-key verilog-mode-map (kbd "C-?") nil)))

(add-hook 'verilog-mode-hook
          #'(lambda ( )
             (define-key verilog-mode-map (kbd "C-;") nil)))

(add-hook 'flyspell-mode-hook
          #'(lambda ( )
              (define-key flyspell-mode-map (kbd "C-;") nil)
              (define-key flyspell-mode-map (kbd "C-,") nil)))


;; Markdown
(defun arw/markdown-mode-font-hook ()
  (setq buffer-face-mode-face '(:family "Fira Code" :height 130))
  (buffer-face-mode)
  (visual-line-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command
              (concat
               "pandoc"
               " --from=markdown --to=html"
               " --standalone --mathjax --highlight-style=pygments"))
  (setq markdown-open-command "/usr/local/bin/mark")
  :config
  (add-hook 'markdown-mode-hook 'arw/markdown-mode-font-hook))


(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bazel Build
(use-package bazel
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doom-Modeline

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (doom-modeline-buffer-encoding nil)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(default-frame-alist nil)
 '(doom-modeline-buffer-file-name-style 'truncate-with-project)
 '(fci-rule-color "#383838")
 '(load-home-init-file t t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/icloud_docs/Documents/diary/"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (calc . t)
     (R . t)
     (dot . t)
     (js . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/icloud_docs/Documents/diary/tasks.org")
 '(org-export-with-sub-superscripts '{})
 '(org-image-actual-width nil)
 '(package-selected-packages
   '(bazel fira-code-mode elpa-mirror visual-fill-column org-bullets avy counsel-projectile projectile helpful ivy-rich counsel ivy fido which-key rainbow-delimiters use-package le-thesaurus smartrep plantuml-mode mc-extras diff-hl magit zenburn-theme smartscan pikchr-mode nov jinja2-mode htmlize multiple-cursors toml ess-view-data ess ox-gfm markdown-preview-mode markdown-mode rust-mode yaml-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-indent-on-newline nil)
 '(verilog-auto-newline nil)
 '(whitespace-style
   '(face trailing tabs lines newline empty indentation space-after-tab space-before-tab tab-mark newline-mark))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil))))
(put 'scroll-left 'disabled nil)

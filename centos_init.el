
(setq initial-frame-alist
          '((top . 30) (left . 50) (width . 170) (height . 50)))

;; ----------------------------
;; External Packages setup

;; solved an error where emacs continued to use older compiled byte code
(setq load-prefer-newer t)

(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("elpa" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; localelpa is the ONLY repository now, dont forget trailing slash in the directory
(setq package-archives '(("local-elpa" . "~/.emacs.d/local_elpa_mirror/")))

(package-initialize)


(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; ---------------------------------------


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


;;;; My custom commands will all use C-l

;; I want to use C-l for my custom comands
(global-set-key (kbd "C-l") nil)

;; by default C-l, recenters this is now C-l l
(global-set-key (kbd "C-l l") 'recenter-top-bottom)

;; truncate lines switch
(global-set-key (kbd "C-l t") 'toggle-truncate-lines)

;; swap buffers in windows
(global-set-key (kbd "C-l b") 'window-swap-states)

;; Reload the file
(global-set-key (kbd "C-l r") 'revert-buffer)

;; clean out the extra whitespace
(global-set-key (kbd "C-l w") 'delete-trailing-whitespace)

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

(setq frame-resize-pixelwise t)

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
  (set-frame-position (selected-frame) 350 100)
  )

(defun set-frame-home-external ()
  (interactive)
  (set-frame-font (font-spec :size 13))
  (set-frame-size (selected-frame) 1500 925 t)
  (set-frame-position (selected-frame) 225 75)
  )

(global-set-key (kbd "C-l 1") 'set-frame-m2air)
(global-set-key (kbd "C-l 2") 'set-frame-work-external)
(global-set-key (kbd "C-l 3") 'set-frame-home-external)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
(setq dired-dwim-target t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent-F : recent files still available as buffers after restart
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avy
(use-package avy
  :bind
  ("C-;" . avy-goto-char-2))

(global-set-key (kbd "C-'")
                (lambda () (interactive)
                  (let ((current-prefix-arg 4))
                    (call-interactively #'set-mark-command))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parenthesis, brackets, etc. have matching colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which-Key : show help after a key press
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie Expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "M-r") 'hippie-expand)



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors
(use-package mc-extras
     :bind (("C-c m c" . mc/edit-lines)
            ("C->" . mc/mark-next-like-this)
            ("C-<" . mc/mark-previous-like-this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful - better help docs
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy / Counsel
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
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

(use-package counsel-projectile
   :config (counsel-projectile-mode))


;; Turn off some verilog mode key bindings that I don't like
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


(use-package zenburn-theme)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)
           (doom-modeline-buffer-encoding nil)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(package-selected-packages '(all-the-icons use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

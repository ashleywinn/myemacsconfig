
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/opensource/crafted-emacs/modules/crafted-init-config")

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(require 'crafted-completion-packages)
(require 'crafted-ui-packages)
(require 'crafted-ide-packages)

(add-to-list 'package-selected-packages 'avy)
(add-to-list 'package-selected-packages 'web-mode)
(add-to-list 'package-selected-packages 'elixir-ts-mode)
(add-to-list 'package-selected-packages 'consult-lsp)

;; ;; Manually select "ef-themes" package
;; (add-to-list 'package-selected-packages 'ef-themes)
;; (crafted-tree-sitter-load 'python)
;; (crafted-tree-sitter-load 'ruby)

;; Install selected packages
(package-install-selected-packages :noconfirm)

;; Load configuration for the completion module
(require 'crafted-completion-config)
(require 'crafted-ui-config)
(require 'crafted-osx-config)
(require 'crafted-ide-config)

;; Consult mode
(global-set-key (kbd "C-x b")   #'consult-buffer)
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)

;; install all language grammars
(crafted-ide-configure-tree-sitter)

(keymap-global-set "C-s" 'isearch-forward)

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; Avy
(global-set-key (kbd "C-;") 'avy-goto-char-2)

(global-set-key (kbd "C-'")
                (lambda () (interactive)
                  (let ((current-prefix-arg 4))
                    (call-interactively #'set-mark-command))))

(load-theme 'zenburn)

;; mac-specific key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

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

(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)

;; I want to use C-l for my custom comands
(global-set-key (kbd "C-l") nil)

;; by default C-l, recenters this is now C-l l
(global-set-key (kbd "C-l l") 'recenter-top-bottom)

(global-set-key (kbd "C-l c") 'compile)

;; truncate lines switch
(global-set-key (kbd "C-l t") 'toggle-truncate-lines)

;; visual line wrap mode
(global-set-key (kbd "C-l y") 'visual-line-mode)

;; swap buffers in windows
(global-set-key (kbd "C-l b") 'window-swap-states)


(global-set-key (kbd "C-l w") 'delete-trailing-whitespace)

;; Reload the file
(defun revert-buffer-confirm-if-change ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t (not (buffer-modified-p)) t)
    (message "buffer reverted"))

(global-set-key (kbd "C-l r") 'revert-buffer-confirm-if-change)

;; parenthesis, brackets, etc. have matching colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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

;; -------------------------------------------
;; Dired
(setq dired-dwim-target t)

;; Eglot

;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

;; Elixir LSP
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((elixir-ts-mode heex-ts-mode) .
                 ,(if (and (fboundp 'w32-shell-dos-semantics)
                           (w32-shell-dos-semantics))
                      '("language_server.bat")
                    (eglot-alternatives
                     '("language_server.sh" "/Users/ashleywinn/opensource/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))))))

(add-hook 'elixir-ts-mode 'eglot-ensure)
(add-hook 'heex-ts-mode 'eglot-ensure)

;; Eglot - Lexical for Elixir

;; To enable Dumb Jump, add the following to your initialisation file:

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Now pressing M-. on an identifier should open a buffer at the place
;; where it is defined, or a list of candidates if uncertain. This
;; list can be navigated using M-g M-n (next-error) and M-g M-p
;; (previous-error).


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


;; Rectangular edit
;; multiple cursors
(use-package mc-extras
     :bind (("C-c m c" . mc/edit-lines)
            ("C->" . mc/mark-next-like-this)
            ("C-<" . mc/mark-previous-like-this)))

;; Magit
(use-package diff-hl
     :config
     (global-diff-hl-mode))

(use-package magit
     :bind (("C-c g" . magit-file-dispatch))
     :config
;; display status in the gutter
     (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))



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

(defun set-frame-m1air ()
  (interactive)
  (set-frame-font (font-spec :size 13))
  (set-frame-size (selected-frame) 1380 800 t)
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

(defun set-frame-laptop-right-pane ()
  (interactive)
  (set-frame-font (font-spec :size 13))
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.47) (height . 0.95)
                      (user-position . t) (top . 0.3) (left . 1.0)))
  )

(defun set-frame-laptop-left-pane ()
  (interactive)
  (set-frame-font (font-spec :size 13))
  (modify-frame-parameters
   (selected-frame) '((user-size . t) (width . 0.47) (height . 0.95)
                      (user-position . t) (top . 0.3) (left . 0.02)))
  )

(global-set-key (kbd "C-l 1") 'set-frame-m1air)
(global-set-key (kbd "C-l 2") 'set-frame-work-external)
(global-set-key (kbd "C-l 3") 'set-frame-laptop-left-pane)
(global-set-key (kbd "C-l 4") 'set-frame-left-pane)
(global-set-key (kbd "C-l 5") 'set-frame-95)
(global-set-key (kbd "C-l 6") 'set-frame-right-pane)
(global-set-key (kbd "C-l 7") 'set-frame-laptop-right-pane)
(global-set-key (kbd "C-l 0") 'set-frame-top-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verilog Mode
(add-to-list 'auto-mode-alist '("\\.[ds]?va?h?\\'" . verilog-mode))

;; Turn off some verilog mode key bindings that I don't like
(add-hook 'verilog-mode-hook
          #'(lambda ( )
             (define-key verilog-mode-map (kbd "C-?") nil)))

(add-hook 'verilog-mode-hook
          #'(lambda ( )
             (define-key verilog-mode-map (kbd "C-;") nil)))

(add-hook 'verilog-mode-hook
          #'(lambda ( )
             (define-key verilog-mode-map (kbd "C-c C-r") nil)))




;;; awinn-magit.el --- My magit config               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ashley Winn

;; Author: Ashley Winn <ashleywinn@Weishans-macbook-air.local>
;; Keywords: vc

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

;; ---------------------------
;; Show Magit on right side, widen frame if room, 
;; Animated widen for Magit side window, then restore on buffer kill
;; ---------------------------

(defvar my/magit-side-window-width 40
  "Fraction of frame width to use for Magit side window (0.0 - 1.0).
If you prefer absolute columns, set to an integer >= 1 and it's treated as columns.")

(defvar my/magit-widen-columns 40
  "Number of columns to try to add to frame width when Magit opens, if there's room.")

(defun my/frame-available-pixels-right ()
  "Return number of extra pixels available to the right of FRAME on the display."
  (let* ((display-w (display-pixel-width))
         (frame-w (frame-pixel-width (selected-frame))))
    (max 0 (- display-w frame-w))))

(defun my/compute-columns-to-add-for-side (frame)
  "Compute how many columns must be added to FRAME so the side window reaches the desired size.
Returns an integer number of columns to add (>=0)."
  (let* ((orig-cols (frame-width frame))
         (f my/magit-side-window-width))
    (cond
     ;; fraction case: solve S = f * (W + S) -> S = (f * W) / (1 - f)
     ((and (numberp f) (> f 0) (< f 1))
      (let ((desired-side (max 1 (round (/ (* f orig-cols) (- 1 f))))))
        ;; columns to add equals desired-side (so final frame = orig + desired-side)
        (max 0 desired-side)))
     ;; absolute columns specified
     ((and (numberp f) (>= f 1))
      (max 0 (round f)))
     ;; fallback
     (t my/magit-widen-columns))))

(defun my/try-widen-frame-for-magit ()
  "If there is room, widen the selected frame so Magit side window fits.
Records the original width in frame parameter 'my/original-width so it can be restored."
  (let* ((frame (selected-frame))
         (cols-to-add (my/compute-columns-to-add-for-side frame))
         (char-px (frame-char-width frame))
         (needed-px (* cols-to-add (or char-px 0)))
         (available (my/frame-available-pixels-right))
         (already (frame-parameter frame 'my/original-width)))
    (when (and (> cols-to-add 0) (not already) (>= available needed-px))
      (set-frame-parameter frame 'my/original-width (frame-width frame))
      (set-frame-width frame (+ (frame-width frame) cols-to-add)))))

(defun my/restore-frame-width-for-magit (&optional frame)
  "Restore FRAME width to previously-saved 'my/original-width, if present."
  (let* ((frame (or frame (selected-frame)))
         (orig (frame-parameter frame 'my/original-width)))
    (when (and orig (integerp orig) (frame-live-p frame))
      (set-frame-width frame orig)
      (set-frame-parameter frame 'my/original-width nil))))

;; named helper invoked on buffer kill so we can add/remove it safely
(defun my/magit-buffer-kill-handler ()
  "Restore frame width when this magit buffer is killed. Uses `selected-frame' at creation time."
  (let ((frame (or (frame-parameter nil 'my/resize-origin-frame) (selected-frame))))
    (my/restore-frame-width-for-magit frame)))

;; Main display handler: widen (animated), show right side window, mark dedicated,
;; and install a buffer-local kill-buffer hook to restore the frame when buffer dies.
(defun my/create-magit-side-window (buffer frame)
  "Create the Magit side window for BUFFER on FRAME, mark dedicated and add kill-hook."
  (when (frame-live-p frame)
    (let ((window
           (with-selected-frame frame
             (display-buffer-in-side-window
              buffer
              `((side . right)
                (slot . 0)
                (window-width . ,my/magit-side-window-width)
                (window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t))))))))
      (when (windowp window)
        (set-window-dedicated-p window t)
        ;; record the frame on which we widened (so the kill handler can use it)
        (set-frame-parameter frame 'my/resize-origin-frame frame)
        ;; add a named buffer-local hook: when the magit buffer is killed, restore the frame
        (with-current-buffer buffer
          ;; remove any previous local hook to avoid duplicates
          (remove-hook 'kill-buffer-hook #'my/magit-buffer-kill-handler t)
          ;; add buffer-local kill hook
          (add-hook 'kill-buffer-hook #'my/magit-buffer-kill-handler nil t))
        window))))


;; Predicate that matches Magit buffers by major-mode.
;; Adjust the list if you want more/less magit-related modes.
(defun my/magit-buffer-p (buffer _alist)
  "Return non-nil if BUFFER is a Magit buffer.
Used as a predicate for `display-buffer-alist'."
  (with-current-buffer buffer
    (or (derived-mode-p 'magit-mode)
        (derived-mode-p 'magit-status-mode)
        (derived-mode-p 'magit-log-mode)
        (derived-mode-p 'magit-revision-mode)
        (derived-mode-p 'magit-diff-mode)
        (derived-mode-p 'magit-process-mode)
        (derived-mode-p 'magit-stash-mode)
        (derived-mode-p 'magit-popup-mode)))) ;; magit-popup used by older magit helpers

(defun my/display-magit-right-side-window (buffer _alist)
  "Handler for `display-buffer' to show Magit buffers on the right and widen immediately if needed."
  (let ((frame (selected-frame)))
    ;; Try to widen 
    (my/try-widen-frame-for-magit)
    ;; Then create the side window (will use final frame width)
    (my/create-magit-side-window buffer frame)))

;; Install the rule
(add-to-list 'display-buffer-alist
             `(my/magit-buffer-p . (my/display-magit-right-side-window)))


(provide 'awinn-magit)


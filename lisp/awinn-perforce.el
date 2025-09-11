;;; awinn-perforce.el --- my perforce config         -*- lexical-binding: t; -*-

;; Author: Ashley Winn <awinn@atletx7-reg039>
;; Keywords: vc, 


(add-to-list 'load-path "~/opensource/emacs-packages/vc-p4/")
(require 'vc-p4)
(add-to-list 'vc-handled-backends 'P4)

(setenv "P4PORT" "atlvp4p01.amd.com:1711")
(setenv "P4USER" "awinn")
(setenv "P4CLIENT" "awinn_atl_mkwa_proj_df_verif_bdc_scratch9_awinn_magnus_first")

(defun vc-p4-root (dir)
  "Find the root of the Perforce workspace starting from DIR."
  (locate-dominating-file dir "P4CONFIG"))

;; (defun awinn/p4-project-find-function (dir)
;;   (when (vc-p4-root dir)
;;     (cons 'vc (vc-p4-root dir))))

(defun awinn/p4-project-find-function (dir)
  "Custom project.el hook to detect Perforce project root"
  (let ((root (vc-p4-root dir)))
    (when root
      (list 'vc 'P4 root))))


(add-hook 'project-find-functions #'awinn/p4-project-find-function)


(provide 'awinn-perforce)

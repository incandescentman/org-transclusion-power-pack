;;; Org-transclusion-power-pack.el --- Enhancements for org-transclusion package -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jay Dixit

;; Author: Jay Dixit <jaydixit.work@gmail.com>

;;; Commentary:

;; This file provides enhancements for the org-transclusion package,
;; streamlining the process of working with transclusions in Org documents
;; and making it more intuitive and efficient.

;;; Code:

;; Toggle transclusion on or off
(defun tr-toggle-transclusion ()
  "Toggle org transclusion on or off based on the current context.
If the point is on a transclusion, remove it.
If the point is on a line starting with #+transclude:, add it."
  (interactive)
  (if (org-transclusion-within-transclusion-p)
      (org-transclusion-remove)
    (let ((line (thing-at-point 'line t)))
      (if (string-prefix-p "#+transclude:" line)
          (org-transclusion-add)
        (message "Not on a transclusion or a #+transclude: keyword.")))))

;; Define user-friendly aliases for common transclusion functions
(defalias 'tr-expand 'org-transclusion-add
  "Alias for `org-transclusion-add'.")

(defalias 'tr-collapse 'org-transclusion-remove
  "Alias for `org-transclusion-remove'.")

(defalias 'tr-duplicate-link-as-transclusion 'org-transclusion-make-from-link
  "Alias for `org-transclusion-make-from-link'.")

(defalias 'tr-expand-all 'org-transclusion-add-all
  "Alias for `org-transclusion-add-all'.")

(defalias 'tr-expand-or-collapse-tranclusion 'tr-toggle-transclusion
  "Alias for `tr-toggle-transclusion'.")

;; Function to insert a transclusion org-roam link
(defun tr-insert-transclusion ()
  "Insert a transclusion org-roam link."
  (interactive)
  (org-roam-node-insert)
  (beginning-of-line)
  (insert "#+transclude: ")
  (beginning-of-line))

;; Function to convert a link to a transclusion
(defun tr-convert-link-to-transclusion ()
 "Convert an Org-mode or Org-roam link to a transclusion link if the point is on a link."
 (interactive)
 (let ((link (org-element-context)))
  (if (eq (car link) 'link)
    (progn
     (goto-char (org-element-property :begin link))
     (insert "#+transclude: ")
     (beginning-of-line))
   (message "Point is not on a valid Org-mode or Org-roam link."))))

(define-key org-mode-map (kbd "s-M") 'tr-toggle-transclusion)
(define-key org-mode-map (kbd "S-s-<down>") 'tr-insert-transclusion)

(provide 'org-transclusion-power-pack)
;;; org-transclusion-power-pack.el ends here

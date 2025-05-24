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

;; helper function for "match level" functions below
(defun insert-current-org-heading-level-plus-one ()
  "Return the current org-heading level plus one as an integer.
If the point is before the first heading, return 1.
Insert the level into the buffer after the word \":level \"."
  (interactive)
  (let ((level (save-excursion
                 (if (re-search-backward org-heading-regexp nil t)
                     (+ 1 (org-current-level))
                   1))))
    (insert (format " :level %d" level))
    level))

;; Function to insert a transclusion org-roam link
(defun tr-insert-transclusion ()
  "Insert a transclusion org-roam link."
  (interactive)
  (org-roam-node-insert)
  (beginning-of-line)
  (insert "#+transclude: ")
  (beginning-of-line))

(defun tr-insert-transclusion-match-level ()
  "Insert a transclusion org-roam link at the appropriate heading level."
  (interactive)
  ;; Ensure org-roam is available before proceeding.
  (if (fboundp 'org-roam-node-insert)
      (progn
        (org-roam-node-insert)
        (let ((link (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format "#+transclude: %s" link))
          (end-of-line)
          (insert-current-org-heading-level-plus-one)
          (beginning-of-line))
        (message "Transclusion link inserted with matched heading level."))
    (message "org-roam not available. Please ensure org-roam is installed and loaded."))
  )


(defun tr-insert-transclusion-match-level ()
  "Insert a transclusion org-roam link at the appropriate heading level."
  (interactive)
  ;; Display the function name to distinguish it from similar functions
  (message "Please select a node for transclusion")
  (sit-for .5)  ;; Pause for 1 second
  ;; Ensure org-roam is available before proceeding.
  (if (fboundp 'org-roam-node-insert)
      (progn
        (org-roam-node-insert)
        (let ((link (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format "#+transclude: %s" link))
          (end-of-line)
          (insert-current-org-heading-level-plus-one)
          (beginning-of-line))
        (message "Transclusion link inserted with matched heading level."))
    (message "org-roam not available. Please ensure org-roam is installed and loaded.")))



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

;; Function to convert a link to a transclusion and match level
(defun tr-convert-link-to-transclusion-match-level ()
  "Convert an Org-mode or Org-roam link to a transclusion link if the point is on a link."
  (interactive)
  (let ((link (org-element-context)))
    (if (eq (car link) 'link)
        (progn
          (goto-char (org-element-property :begin link))
          (insert "#+transclude: ")
          (end-of-line)
          (insert-current-org-heading-level-plus-one)
          (beginning-of-line))
      (message "Point is not on a valid Org-mode or Org-roam link."))))



(define-key org-mode-map (kbd "s-M") 'tr-toggle-transclusion)
(define-key org-mode-map (kbd "S-s-<down>") 'tr-insert-transclusion-match-level)

(provide 'org-transclusion-power-pack)
;;; org-transclusion-power-pack.el ends here

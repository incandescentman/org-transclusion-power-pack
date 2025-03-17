;;; Org-transclusion-power-pack.el --- Enhancements for org-transclusion package -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jay Dixit

;; Author: Jay Dixit <jaydixit.work@gmail.com>

;;; Commentary:

;; This file provides enhancements for the org-transclusion package,
;; streamlining the process of working with transclusions in Org documents
;; and making it more intuitive and efficient.

;;; Code:

;; patch this

(defun org-transclusion-content-org-buffer-or-element (only-element plist)
  "Return a list of payload for transclusion..."
  (let* ((el (org-element-context))
         (type (org-element-type el))
         (beg (org-element-property :begin el))
         (end (org-element-property :end el))
         (only-contents (plist-get plist :only-contents))
         (exclude-elements (org-transclusion-keyword-plist-to-exclude-elements plist))
         (expand-links (plist-get plist :expand-links)))
    ;; If we can't find a valid element, just do the entire buffer
    (if (or (null el) (null beg) (null end))
        ;; The entire buffer:
        (progn
          (message "Could not find an Org element at point. Fallback to entire buffer.")
          (list :src-content (buffer-string)
                :src-buf (current-buffer)
                :src-beg (point-min)
                :src-end (point-max)))
      ;; Else do the normal logic...
      (when only-element
        (narrow-to-region beg end))
      ;; ... rest of your code for exclude-elements, only-contents, etc.
      )))

(defun org-transclusion-content-org-marker (marker plist)
  "Return a list of payload from MARKER and PLIST."
  (if (and marker (marker-buffer marker) (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char marker)
         ;; If we land in a property drawer, climb up to the heading
         (while (and (org-in-property-drawer-p)
                     (org-up-heading-safe)))
         ;; Or, if we want to be more thorough, we can walk up until we get a
         ;; headline or top-level element
         (let* ((el   (org-element-context))
                (type (org-element-type el)))
           ;; If we still can’t get a valid element with :begin, fallback
           (if (or (null el) (null (org-element-property :begin el)))
               ;; fallback: entire buffer
               (org-transclusion-content-org-buffer-or-element nil plist)
             ;; else do the usual
             (if (org-before-first-heading-p)
                 (org-transclusion-content-org-buffer-or-element nil plist)
               (org-transclusion-content-org-buffer-or-element 'only-element plist))))))
    (message "Nothing done. Cannot find marker for the ID.")
    nil))


(defun tr-toggle-transclusion ()
  "Toggle org transclusion at point.

- If point is inside an active transclusion, remove it.
- If point is on a line starting with `#+transclude:`, add the transclusion.
- Otherwise, show a helpful message."
  (interactive)
  (cond
   ;; Case 1: inside a transcluded region
   ((org-transclusion-within-transclusion-p)
    (org-transclusion-remove))
   ;; Case 2: on a #+transclude: line
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*#\\+transclude:"))
    (org-transclusion-add))
   ;; Otherwise: fallback message
   (t
    (message "Point is not in a transclusion or on a #+transclude: line."))))

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

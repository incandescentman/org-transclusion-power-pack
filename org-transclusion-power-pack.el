;;; Org-transclusion-power-pack.el --- Enhancements for org-transclusion package -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jay Dixit

;; Author: Jay Dixit <jaydixit.work@gmail.com>

;;; Commentary:

;; This file provides enhancements for the org-transclusion package,
;; streamlining the process of working with transclusions in Org documents
;; and making it more intuitive and efficient.

;;; Code:

;; 0) Define fallback for older Org if `org-in-property-drawer-p` is missing:
(unless (fboundp 'org-in-property-drawer-p)
  (defun org-in-property-drawer-p ()
    "Return non-nil if point is in a property drawer.
This is a fallback for older Org versions (<9.6)."
    (let* ((elem (org-element-at-point))
           (etype (org-element-type elem)))
      (eq etype 'property-drawer))))



;; Your other patched functions here...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) A patched version of `org-transclusion-content-org-buffer-or-element`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org-transclusion
  ;; put the two patched defuns here
  (defun org-transclusion-content-org-buffer-or-element (only-element plist)
    "Return a list of payload for transclusion.
This function is a patched version of the original. It:
- Falls back to entire buffer if we can't find a valid element.
- Otherwise processes an Org element and respects :only-contents, :exclude-elements, etc."
    (let* ((el (org-element-context))
           (type (when el (org-element-type el)))
           (beg (org-element-property :begin el))
           (end (org-element-property :end el))
           (only-contents (plist-get plist :only-contents))
           (exclude-elements (org-transclusion-keyword-plist-to-exclude-elements plist))
           (expand-links (plist-get plist :expand-links)))
      ;; If we can't find a valid element, just do the entire buffer
      (if (or (null el) (null beg) (null end))
          (progn
            (message "org-transclusion: no valid element => fallback to entire buffer.")
            (list :src-content (buffer-string)
                  :src-buf (current-buffer)
                  :src-beg (point-min)
                  :src-end (point-max)))

        ;; We do have a valid element. Optionally narrow to *just* that element:
        (when only-element
          (narrow-to-region beg end))

        ;; Now parse the buffer (or region) to apply filters, expansions, etc.
        (let ((obj (org-element-parse-buffer)))
          ;; 1) Exclude certain elements
          (let ((org-transclusion-exclude-elements
                 (append exclude-elements org-transclusion-exclude-elements)))
            (setq obj (org-element-map obj org-element-all-elements
                        #'org-transclusion-content-filter-org-exclude-elements
                        nil nil org-element-all-elements nil)))

          ;; 2) If not only-element, also consider skipping the “first section,”
          ;;    unless :org-transclusion-include-first-section is set (the user can adapt).
          (unless only-element
            (setq obj (org-element-map obj org-element-all-elements
                        #'org-transclusion-content-filter-org-first-section
                        nil nil org-element-all-elements nil)))

          ;; 3) If :only-contents is specified, remove headlines
          (when only-contents
            (setq obj (org-element-map obj org-element-all-elements
                        #'org-transclusion-content-filter-org-only-contents
                        nil nil '(section) nil)))

          ;; 4) Expand links if asked
          (when expand-links
            (org-element-map obj 'link
              #'org-transclusion-content-filter-expand-links))

          ;; Finally interpret the parse tree back to a string
          (let ((transcluded-text (org-element-interpret-data obj)))
            (list :src-content transcluded-text
                  :src-buf (current-buffer)
                  :src-beg (point-min)
                  :src-end (point-max)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 2) A patched version of `org-transclusion-content-org-marker`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun org-transclusion-content-org-marker (marker plist)
    "Return a payload from MARKER and PLIST, opening or climbing out if needed.
1) If MARKER is (file . pos), open the file and convert to a real marker.
2) If inside a property drawer, climb up to a heading.
3) If still before first heading, transclude the entire buffer; otherwise just the element."
    (cond
     ((null marker)
      (message "org-transclusion: no marker => no transclusion.")
      nil)

     ;; If we have a (file . pos) but not a live marker, open the file and move there:
     ((consp marker)
      (let* ((filename (car marker))
             (pos (cdr marker))
             (buf (find-file-noselect filename)))
        (with-current-buffer buf
          (goto-char pos)
          (org-transclusion-content-org-marker (point-marker) plist))))

     ;; If we do have a real marker:
     ((markerp marker)
      (let ((src-buf (marker-buffer marker)))
        (if (not (buffer-live-p src-buf))
            (progn
              (message "org-transclusion: marker buffer is dead => no transclusion.")
              nil)
          (with-current-buffer src-buf
            (org-with-wide-buffer
             (goto-char marker)
             ;; If we land in a property drawer, climb up to the heading
             (while (and (org-in-property-drawer-p)
                         (org-up-heading-safe)))
             ;; Now check if we're before the first heading
             (if (org-before-first-heading-p)
                 ;; Transclude the entire buffer, or the first section, depending on your config
                 (org-transclusion-content-org-buffer-or-element nil plist)
               ;; Otherwise do 'only-element'
               (org-transclusion-content-org-buffer-or-element 'only-element plist)))))))

     (t
      (message "org-transclusion: unknown marker type => no transclusion.")
      nil)))
  )



;; Add a fallback implementation of org-in-property-drawer-p
(unless (fboundp 'org-in-property-drawer-p)
  (defun org-in-property-drawer-p ()
    "Return non-nil if point is in a property drawer.
This is a fallback implementation for older Org versions."
    (let ((element (org-element-at-point)))
      (and element
           (eq (org-element-type element) 'node-property)
           (eq (org-element-type (org-element-property :parent element)) 'property-drawer)))))


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

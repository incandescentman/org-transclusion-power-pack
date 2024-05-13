
(defun return-current-org-heading-plus-one ()
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


(defun tr-insert-transclusion-match-heading-level ()
"Insert a transclusion org-roam link at the appropriate heading level."
  (interactive)
  ;; Step 1: Call org-roam-node-insert and capture the inserted link
  (let ((link (org-roam-node-insert nil t)))  ; 't' returns the description instead of inserting
    ;; Step 2: Add '#+transclude:' at the beginning of the current line
    (beginning-of-line)
    (insert (format "#+transclude: %s" link))
    ;; Step 3: Move to the end of the line
    (end-of-line)
    ;; Step 5: Add ':level x' where x is the current org-heading level plus one
          (return-current-org-heading-plus-one)
          ))
  ;; Ensure formatting is proper, move back to the heading
  (org-back-to-heading))

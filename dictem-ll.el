;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                              ;;;;;

(defun get-line ()
  "Replacement for (thing-at-point 'line)"
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point)))))

(defun list2alist (l)
  (cond
   ((null l) nil)
   (t (cons
       (list (car l) nil)
       (list2alist (cdr l))))))

(defun dictem-replace-spaces (str)
  (while (string-match "  +" str)
    (setq str (replace-match " " t t str)))
  (if (string-match "^ +" str)
      (setq str (replace-match "" t t str)))
  (if (string-match " +$" str)
      (setq str (replace-match "" t t str)))
  str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      Low Level Functions     ;;;;;

(defun dictem-select (prompt alist default history)
  (let
      ((completion-ignore-case t))
    (completing-read
     (concat prompt " (" default "): ")
     alist
     nil
     t
     nil
     history
     default)))

(defun dictem-tokenize (s)
  (if (string-match "\"[^\"]+\"\\|[^ \"]+" s )
;	(substring s (match-beginning 0) (match-end 0))
      (cons (substring s (match-beginning 0) (match-end 0)) 
	    (dictem-tokenize (substring s (match-end 0))))
    nil))

(defun search-forward-regexp-cs (REGEXP &optional BOUND NOERROR COUNT)
  "Case-sensitive variant for search-forward-regexp"
  (let ((case-replace nil)
	(case-fold-search nil))
    (search-forward-regexp REGEXP BOUND NOERROR COUNT)))

(defun replace-match-cs (NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
  "Case-sensitive variant for replace-match"
  (let ((case-replace nil)
	(case-fold-search nil))
    (replace-match NEWTEXT FIXEDCASE LITERAL STRING SUBEXP)))

(provide 'dictem-ll)

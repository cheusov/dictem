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

(defun dictem-remove-value-from-alist (l)
  (cond
   ((symbolp l) l)
   (t (cons (list (caar l))
	    (dictem-remove-value-from-alist (cdr l))))))

(defun dictem-select (prompt alist default history)
  (let*
      ((completion-ignore-case t)
       (str (completing-read
	     (concat prompt " (" default "): ")
	     alist
	     nil
	     t
	     nil
	     history
	     default))
       (str-cons (assoc str alist)))
;    str-cons))
    (cond
     ((and str-cons (cdr str-cons))
      (cdr str-cons))
     ((and str-cons (null (cdr str-cons)))
      (car str-cons))
     (t nil))))

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

(defun dictem-get-port ()
  (cond
   ((stringp dictem-port) dictem-port)
   ((numberp dictem-port) (number-to-string dictem-port))
   (t (error "The value of dictem-port variable should be \
either a string or a number"))
   ))

(defun dictem-get-server ()
  (cond
   ((stringp dictem-server) dictem-server)
   (t (error "The value of dictem-server variable should be \
either a string or a number"))
   ))

(provide 'dictem-ll)

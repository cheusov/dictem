;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;            Faces             ;;;;;

(defface dictem-reference-definition-face
  '((((type x)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((type tty)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:underline t)))

  "The face that is used for displaying a reference to
a phrase in a DEFINE search."
  :group 'dictem-faces)

(defface dictem-reference-m1-face
  '((((type x)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((type tty)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:underline t)))

  "The face that is used for displaying a reference to
a phrase in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-m2-face
  '((((type x)
      (class color)
      (background dark))
     (:foreground "green"))
    (((type tty)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:underline t)))

  "The face that is used for displaying a reference to
a single word in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-dbname-face
  '((((type x)
      (class color)
      (background dark))
     (:foreground "white"))
    (((type tty)
      (class color)
      (background dark))
     (:foreground "white"))
    (((class color)
      (background light))
     (:foreground "white"))
    (t
     (:underline t)))

  "The face that is used for displaying a reference to database"
  :group 'dictem-faces)

(defface dictem-database-description-face
  '((((type x)
      (class color)
      (background dark))
;     (:underline t)
     (:foreground "dark green")
     (:weight bold)
     )
    (((type tty)
      (class color)
      (background dark))
     (:foreground "white"))
    (((class color)
      (background light))
     (:foreground "white"))
    (t
     (:underline t)))

  "The face that is used for displaying a database description"
  :group 'dictem-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      Low Level Functions     ;;;;;

(defun link-create-link (start end face function &optional data help)
  "Create a link in the current buffer starting from `start' going to `end'.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((properties
	 (list 'face face
	       'mouse-face 'highlight
	       'link t
	       'link-data data
	       'link-function function)
	  ))
    (remove-text-properties start end properties)
    (add-text-properties start end properties)))

;;;;;;;   Coloring Functions     ;;;;;;;

(defun dictem-postprocess-definition-separator ()
  (save-excursion
    (beginning-of-buffer)
    (let ((regexp "^\\(From\\)\\( [^\n]+\\)\\(\\[[^\n]+\\]\\)"))

      (while (search-forward-regexp regexp nil t)
	(let ((beg (match-beginning 1))
	      (end (match-end 1))
	      (beg-dbdescr (match-beginning 2))
	      (end-dbdescr (match-end 2))
	      (beg-dbname (match-beginning 3))
	      (end-dbname (match-end 3))
	      )
	  (put-text-property beg end
			     'face 'dictem-database-description-face)
	  (put-text-property beg-dbdescr end-dbdescr
			     'face 'dictem-database-description-face)
	  (setq dictem-current-dbname
		(dictem-replace-spaces
		 (buffer-substring-no-properties
		  (+ beg-dbname 1) (- end-dbname 1))))
	  (link-create-link
	   beg-dbname end-dbname
	   'dictem-reference-dbname-face
	   'dictem-dbinfo-base
	   (list (cons 'dbname dictem-current-dbname))))
	))))

(defun dictem-postprocess-definition-hyperlinks ()
  (save-excursion
    (beginning-of-buffer)
    (let ((regexp "[{]\\([^{}\n]+\\)[}]\\|^From [^\n]+\\[\\([^\n]+\\)\\]"))

      (while (search-forward-regexp regexp nil t)
	(if (match-beginning 1)
	    (let* ((beg (match-beginning 1))
		   (end (match-end 1))
		   (word
		    (dictem-replace-spaces
		     (buffer-substring-no-properties beg end))))
	      (replace-match "\\1")
	      (link-create-link
	       (- beg 1) (- end 1)
	       'dictem-reference-definition-face
	       'dictem-define-base
	       (list (cons 'word word)
		     (cons 'dbname dictem-current-dbname))
	       ))
	  (setq dictem-current-dbname
		(dictem-replace-spaces
		 (buffer-substring-no-properties (match-beginning 2)
						 (match-end 2))))
	  )))))

(defun dictem-postprocess-match ()
  (goto-char (point-min))
  (let ((last-database dictem-last-database)
	(regexp "\\(\"[^\"\n]+\"\\)\\|\\([^ \"\n]+\\)"))

;    (forward-line-nomark)
    (while (search-forward-regexp regexp nil t)
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (first-char (buffer-substring-no-properties beg beg)))
	(cond
	 ((save-excursion (goto-char beg) (= 0 (current-column)))
	  (setq last-database
		(dictem-replace-spaces
		 (buffer-substring-no-properties beg (- end 1))))
	  (link-create-link
	   beg (- end 1)
	   'dictem-reference-dbname-face 'dictem-dbinfo-base
	   (list (cons 'dbname last-database))))
	 ((match-beginning 1)
	  (link-create-link
	   beg end
	   'dictem-reference-m1-face 'dictem-define-base
	   (list (cons 'word
		       (dictem-replace-spaces
			(buffer-substring-no-properties
			 (+ beg 1) (- end 1))))
		 (cons 'dbname last-database))))
	 (t
	  (link-create-link
	   beg end
	   'dictem-reference-m2-face 'dictem-define-base
	   (list (cons 'word
		       (dictem-replace-spaces
			(buffer-substring-no-properties
			 beg end )))
		 (cons 'dbname last-database))))
	 )))))

;;;;;       On-Click Functions     ;;;;;

(defun dictem-define-on-click (event)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (let* (
	 (properties (text-properties-at (point)))
	 (data (plist-get properties 'link-data))
	 (fun  (plist-get properties 'link-function))
	 (word   (assq 'word data))
	 (dbname (assq 'dbname data))
	 )
    (if (or word dbname)
	(dictem-run fun
		    (if dbname (cdr dbname) dictem-last-database)
		    (if word (cdr word) nil)
		    nil))))

;(defun dictem-define-with-db-on-click (event)
;  "Is called upon clicking the link."
;  (interactive "@e")
;
;  (mouse-set-point event)
;  (let* (
;	 (properties (text-properties-at (point)))
;	 (word (plist-get properties 'link-data)))
;    (if word
;	(dictem-run 'dictem-define-base (dictem-select-database) word nil))))

(define-key dictem-mode-map [mouse-2]
  'dictem-define-on-click)

;(define-key dictem-mode-map [C-down-mouse-2]
;  'dictem-define-with-db-on-click)


;;;     Function for "narrowing" definitions ;;;;;

(defcustom dictem-postprocess-each-definition-hook
  nil
  "Hook run in dictem mode buffers containing SHOW SERVER result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-separator
	     dictem-postprocess-definition-hyperlinks))

(defun dictem-postprocess-each-definition ()
  (goto-char (point-min))
  (let ((regexp-from-dbname "^From [^\n]+\\[\\([^\n]+\\)\\]")
	(beg nil)
	(end (make-marker))
	(dbname nil))
    (if (search-forward-regexp regexp-from-dbname nil t)
	(let ((dictem-current-dbname
	       (buffer-substring-no-properties
		(match-beginning 1) (match-end 1))))
	  (setq beg (match-beginning 0))
	  (while (search-forward-regexp regexp-from-dbname nil t)
	    (set-marker end (match-beginning 0))
;	    (set-marker marker (match-end 0))
	    (setq dbname
		  (buffer-substring-no-properties
		   (match-beginning 1) (match-end 1)))

	    (save-excursion
	      (narrow-to-region beg (marker-position end))
	      (run-hooks 'dictem-postprocess-each-definition-hook)
	      (widen))

	    (setq dictem-current-dbname dbname)
	    (goto-char end)
	    (forward-char)
	    (setq beg (marker-position end))
	    )
	  (save-excursion
	    (narrow-to-region beg (point-max))
	    (run-hooks 'dictem-postprocess-each-definition-hook)
	    (widen))
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dictem-opt)

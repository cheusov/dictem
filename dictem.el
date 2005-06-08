; dictem.el - DICT protocol client (rfc-2229) for [X]Emacs

; This code was initially based on
; dictionary.el written by Torsten Hilbrich <Torsten.Hilbrich@gmx.net>
; but now probably doesn't contain original code.
; Most of the code has been written
; from scratch by Aleksey Cheusov <vle@gmx.net>, 2004-2005.
;
; DictEm is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; DictEm is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
; 02111-1307, USA

;
; Note! Documentation is in README file.
;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Custom Things        ;;;;;

(defgroup dictem nil
  "Client for accessing the DICT server."
  :tag "DictEm"
  :group 'help
  :group 'hypermedia)

(defgroup dictem-faces nil
  "Face options for dictem DICT client."
  :tag "DictEm faces"
  :group 'dictem
  :group 'faces)

(defcustom dictem-server "dict.org"
  "The DICT server"
  :group 'dictem
  :type 'string)

(defcustom dictem-port 2628
  "The port of the DICT server"
  :group 'dictem
  :type 'number)

(defcustom dictem-client-prog "dict"
  "The command line DICT client.
dictem accesses DICT server through this executable.
dict-1.9.14 or later (or compatible) is recomented."
  :group 'dictem
  :type 'string)

(defcustom dictem-default-strategy nil
  "The default search strategy."
  :group 'dictem
  :group 'string)

(defcustom dictem-default-database nil
  "The default database name."
  :group 'dictem
  :group 'string)

(defcustom dictem-user-databases-alist
  nil
  "ALIST of user's \"virtual\"databases.
Valid value looks like this:
'((\"en-ru\" .  (\"mueller7\" \"korolew_en-ru\"))
  ((\"en-en\" . (\"foldoc\" \"gcide\" \"wn\")))
  ((\"gazetteer\" . \"gaz\")))
"
  :group 'dictem
  :type '(alist :key-type string))

(defcustom dictem-use-user-databases-only
  nil
  "If `t', only user's dictionaries from dictem-user-databases-alist
will be used by dictem-select-database"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-mode-hook
  nil
  "Hook run in dictem mode buffers."
  :group 'dictem
  :type 'hook)

(defcustom dictem-use-existing-buffer
  nil
  "If `t' the `dictem-run' function will not create new *dictem* buffer.
Instead, existing buffer will be erased and used to show results.
"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-empty-initial-input
  nil
  "If `t' the `dictem-read-query' leave initial input empty"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-use-content-history t
  "If not nil and dictem-use-existing-buffer is also not nil,
buffer content and (point) is saved in dictem-content-history variable
when DEFINE hyperlinks are accessed.
It is restored by dictem-last function.
On slow machines it may better to set this variable to nil"
  :group 'dictem)

;;;;;            Faces             ;;;;;

(defface dictem-reference-definition-face
  '((((background light)) (:foreground "blue"))
    (((background dark))  (:foreground "cyan")))

  "The face that is used for displaying a reference to
a phrase in a DEFINE search."
  :group 'dictem-faces)

(defface dictem-reference-m1-face
  '((((background light)) (:foreground "darkgreen"))
    (((background dark))  (:foreground "lightblue")))

  "The face that is used for displaying a reference to
a phrase in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-m2-face
  '((((background light)) (:foreground "blue"))
    (((background dark))  (:bold true :foreground "gray")))

  "The face that is used for displaying a reference to
a single word in a MATCH search."
  :group 'dictem-faces)

(defface dictem-reference-dbname-face
  '((((background light)) (:foreground "darkgreen"))
    (((background dark))  (:bold t :foreground "white")))

  "The face that is used for displaying a reference to database"
  :group 'dictem-faces)

(defface dictem-database-description-face
  '((((background light)) (:bold t :foreground "darkblue"))
    (((background dark))  (:bold t :foreground "white")))

  "The face that is used for displaying a database description"
  :group 'dictem-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;           Variables          ;;;;;

(defvar dictem-version
  "0.0.5"
  "DictEm version information.")

(defvar dictem-strategy-alist
  '(("word"    nil)
    ("exact"     nil)
    ("prefix"    nil)
    ("substring" nil)
    ("suffix"  nil)
    ("re"      nil)
    ("regexp"  nil)
    ("soundex" nil)
    ("lev"     nil)
    )

  "ALIST of search strategies")

(defvar dictem-database-alist
  '(("elements" nil )
    ("web1913" nil )
    ("wn" nil )
    ("gazetteer" nil )
    ("jargon" nil )
    ("foldoc" nil )
    ("easton" nil )
    ("hitchcock" nil )
    ("devils" nil )
    ("world02" nil )
    ("vera" nil )
    )

  "ALIST of databases")

(defvar dictem-strategy-history
  nil
  "List of strategies entered from minibuffer")

(defvar dictem-database-history
  nil
  "List of database names entered from minibuffer")

(defvar dictem-query-history
  nil
  "List of queries entered from minibuffer")

(defvar dictem-last-database
  "*"
  "Last used database name")

(defvar dictem-last-strategy
  "."
  "Last used strategy name")

(defvar dictem-mode-map
  nil
  "Keymap for dictem mode")

(defvar dictem-temp-buffer-name
  "*dict-temp*"
  "Temporary buffer name")

(defvar dictem-current-dbname
  nil
  "This variable keeps a database name of the definition
currently processed
by functions run from dictem-postprocess-each-definition-hook.")

(defvar dictem-error-messages
  nil
  "A list of error messages collected by dictem-run")

(defvar dictem-hyperlinks-alist
  nil
  "ALIST of hyperlinks collected from dictem buffer by
the function dictem-postprocess-collect-hyperlinks
(add this function to the hook dictem-postprocess-definition-hook).
This variable is local to buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-prepand-special-strats (l)
  (cons '(".") l))

(defun dictem-prepand-special-dbs (l)
  (cons '("*") (cons '("!") l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Functions         ;;;;;;

(defmacro save-dictem (&rest funs)
  `(let ((dictem-port                    2628)
	 (dictem-server                  nil)
	 (dictem-database-alist          nil)
	 (dictem-strategy-alist          nil)
	 (dictem-use-user-databases-only nil)
	 (dictem-user-databases-alist    nil)
	 )
     (progn ,@funs)
     ))

(defun dictem-client-text ()
  "Returns a portion of text sent to the server for identifying a client"
  (concat "dictem " dictem-version ", DICT client for emacs"))

;; Functions related to error object  ;;

(defun dictem-make-error (error_status &optional buffer-or-string)
  "Creates dictem error object"
  (cond
   ((stringp buffer-or-string)
    (list 'dictem-error error_status buffer-or-string))
   ((bufferp buffer-or-string)
    (dictem-make-error
     error_status
     (save-excursion
       (set-buffer buffer-or-string)
       (goto-char (point-min))
       (dictem-get-line)
       )))
   ((eq nil buffer-or-string)
    (list 'dictem-error error_status buffer-or-string))
   (t
    (error "Invalid type of argument"))
   ))

(defun dictem-error-p (OBJECT)
  "Returns t if OBJECT is the dictem error object"
  (and
   (listp OBJECT)
   (eq (car OBJECT) 'dictem-error)
   ))

(defun dictem-error-message (err)
  "Extract error message from dictem error object"
  (cond
   ((dictem-error-p err)
    (nth 2 err))
   (t
    (error "Invalid type of argument"))
   ))

(defun dictem-error-status (err)
  "Extract error status from dictem error object"
  (cond
   ((dictem-error-p err)
    (nth 1 err))
   (t
    (error "Invalid type of argument"))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-collect-matches ()
  ; nreverse, setcar and nconc are used to reduce a number of cons
  (goto-char (point-min))
  (let ((dictem-temp nil))
    (loop
     (let ((line (dictem-get-line)))
       (if (string-match "^[^ ]+" line)
	   (progn
	     (if (consp dictem-temp)
		 (setcar (cdar dictem-temp)
			 (nreverse (cadar dictem-temp))))
	     (setq
	      dictem-temp
	      (cons
	       (list
		(substring line (match-beginning 0) (match-end 0))
		(nreverse 
		 (dictem-tokenize (substring line (match-end 0)))))
	       dictem-temp)))
	 (if (consp dictem-temp)
	     (setcar (cdar dictem-temp)
		     (nconc (nreverse (dictem-tokenize line))
			    (cadar dictem-temp))
		     ))
	 ))
     (if (or (> (forward-line 1) 0)
	     (> (current-column) 0))
	 (return (nreverse dictem-temp)))
     )))

;;;;;        GET Functions         ;;;;;

(defun dictem-get-matches (query &optional database strategy server port)
  "Returns ALIST of matches"
  (let ((exit_status
	 (call-process
	  dictem-client-prog nil
	  (get-buffer-create dictem-temp-buffer-name) nil
	  "-P" "-" "-m"
	  "-d" (if database database "*")
	  "-s" (if strategy strategy ".")
	  "-h" (if server server dictem-server)
	  "-p" (dictem-get-port port)
	  "--client" (dictem-client-text)
	  query)))
    (cond
     ((= exit_status 20) ;20 means "no matches found", See dict(1)
      (kill-buffer dictem-temp-buffer-name)
      nil)
     ((= exit_status 0)
      (progn
	(save-excursion
	  (set-buffer dictem-temp-buffer-name)
	  (let ((matches (dictem-collect-matches)))
	    (kill-buffer dictem-temp-buffer-name)
	    matches))))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
     )))

(defun dictem-get-strategies (&optional server port)
  "Obtains strategy ALIST from a DICT server
and returns alist containing strategies and their descriptions"
  (let ((exit_status
	 (call-process
	  dictem-client-prog nil
	  (get-buffer-create dictem-temp-buffer-name) nil
	  "-P" "-" "-S"
	  "-h" (if server server dictem-server)
	  "-p" (dictem-get-port port)
	  "--client" (dictem-client-text))
	 ))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(goto-char (point-min))
	(let ((regexp "^ \\([^ ]+\\) +\\(.*\\)$")
	      (l nil))
	  (while (search-forward-regexp regexp nil t)
	    (setq l (cons
		     (list
		      (buffer-substring-no-properties
		       (match-beginning 1) (match-end 1))
		      (buffer-substring-no-properties
		       (match-beginning 2) (match-end 2)))
		     l)))
	  (kill-buffer dictem-temp-buffer-name)
	  l)))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
    )))

(defun dictem-get-databases (&optional server port)
  "Obtains database ALIST from a DICT server
and returns alist containing database names and descriptions"
  (let ((exit_status
	 (call-process
	  dictem-client-prog nil
	  (get-buffer-create dictem-temp-buffer-name) nil
	  "-P" "-" "-D"
	  "-h" (if server server dictem-server)
	  "-p" (dictem-get-port port)
	  "--client" (dictem-client-text))
	 ))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(goto-char (point-min))
	(let ((regexp "^ \\([^ ]+\\) +\\(.*\\)$")
	      (l nil))
	  (while (search-forward-regexp regexp nil t)
	    (let ((dbname (buffer-substring-no-properties
			   (match-beginning 1) (match-end 1)))
		  (dbdescr (buffer-substring-no-properties
			    (match-beginning 2) (match-end 2))))
	      (if (not (string= "--exit--" dbname))
		  (setq l (cons (list dbname dbdescr) l)))))
	  (kill-buffer dictem-temp-buffer-name)
	  l)))
     (t
      (let
	  ((err (dictem-make-error exit_status
				   (get-buffer dictem-temp-buffer-name))))
	(kill-buffer dictem-temp-buffer-name)
	err))
     )))

(defun dictem-get-default-strategy (&optional def-strat)
  "Gets the default search strategy"
  (if def-strat
      def-strat
    (if dictem-default-strategy
	dictem-default-strategy
      (if dictem-last-strategy
	  dictem-last-strategy
	"."))))

(defun dictem-get-default-database (&optional def-db)
  "Returns the default database"
  (defun dictem-extract-dbname (database)
    (cond
     ((consp database) (dictem-extract-dbname (car database)))
     ((stringp database) database)
     (t (error "The database should be either stringp or consp"))
     ))

  (if def-db
      (dictem-extract-dbname def-db)
    (if dictem-default-database
	(dictem-extract-dbname dictem-default-database)
      (if dictem-last-database
	  (dictem-extract-dbname dictem-last-database)
	"*"))))

;;;;;      Low Level Functions     ;;;;;

(defun dictem-get-line ()
  "Replacement for (thing-at-point 'line)"
  (save-excursion
    (buffer-substring-no-properties
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point)))))

(defun dictem-list2alist (l)
  (cond
   ((null l) nil)
   (t (cons
       (list (car l) nil)
       (dictem-list2alist (cdr l))))))

(defun dictem-replace-spaces (str)
  (while (string-match "[ \n][ \n]+" str)
    (setq str (replace-match " " t t str)))
  (if (string-match "^ +" str)
      (setq str (replace-match "" t t str)))
  (if (string-match " +$" str)
      (setq str (replace-match "" t t str)))
  str)

(defun dictem-remove-value-from-alist (l)
  (let ((ret nil))
    (dolist (i l)
      (setq ret (cons (list (car i)) ret)))
    (reverse ret)
    ))
;(defun dictem-remove-value-from-alist (l)
;  (cond
;   ((symbolp l) l)
;   (t (cons (list (caar l))
;	    (dictem-remove-value-from-alist (cdr l))))))

(defun dictem-select (prompt alist default history)
  (let*
      ((completion-ignore-case t)
       (str (completing-read
	     (concat prompt " [" default "]: ")
	     alist nil t nil history default))
       (str-cons (assoc str alist)))
    (cond
     ((and str-cons (consp str-cons) (cdr str-cons))
      str-cons)
     ((and str-cons (consp str-cons))
      (car str-cons))
     (t nil))))

(defun dictem-tokenize (s)
  (if (string-match "\"[^\"]+\"\\|[^ \"]+" s )
;	(substring s (match-beginning 0) (match-end 0))
      (cons (substring s (match-beginning 0) (match-end 0)) 
	    (dictem-tokenize (substring s (match-end 0))))
    nil))

;(defun dictem-search-forward-regexp-cs (REGEXP &optional BOUND NOERROR COUNT)
;  "Case-sensitive variant for search-forward-regexp"
;  (let ((case-replace nil)
;	(case-fold-search nil))
;    (search-forward-regexp REGEXP BOUND NOERROR COUNT)))

;(defun dictem-replace-match-cs (NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
;  "Case-sensitive variant for replace-match"
;  (let ((case-replace nil)
;	(case-fold-search nil))
;    (replace-match NEWTEXT FIXEDCASE LITERAL STRING SUBEXP)))

(defun dictem-get-port (&optional port)
  (let ((p (if port port dictem-port)))
    (cond
     ((stringp p) p)
     ((numberp p) (number-to-string p))
     (t (error "The value of dictem-port variable should be \
either a string or a number"))
     )))

(defun dictem-get-server ()
  (cond
   ((stringp dictem-server) dictem-server)
   (t (error "The value of dictem-server variable should be \
either a string or a number"))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Main Functions        ;;;;;

;;;;;; Functions for Initializing ;;;;;;

(defun dictem-initialize-strategies-alist (&optional server port)
  "Obtain strategy ALIST from a DICT server
and sets dictem-strategy-alist variable."
  (interactive)
  (setq dictem-strategy-alist (dictem-get-strategies
			       server
			       (dictem-get-port port))))

(defun dictem-initialize-databases-alist (&optional server port)
  "Obtain database ALIST from a DICT server
and sets dictem-database-alist variable."
  (interactive)
  (setq dictem-database-alist (dictem-get-databases
			       server
			       (dictem-get-port port))))

(defun dictem-initialize ()
  "Initializes dictem, i.e. obtains
a list of available databases and strategiss from DICT server
and makes other tasks."
  (interactive)
  (let ((dbs (dictem-initialize-databases-alist)))
    (if (dictem-error-p dbs)
	dbs
      (dictem-initialize-strategies-alist))))

;;; Functions related to Minibuffer ;;;;

(defun dictem-select-strategy (&optional default-strat)
  "Switches to minibuffer and asks the user
to enter a search strategy."
  (interactive)
  (if (dictem-error-p dictem-strategy-alist)
      (error "A list of strategies was not initialized properly"))
  (dictem-select
   "strategy"
   (dictem-prepand-special-strats
    (dictem-remove-value-from-alist dictem-strategy-alist))
   (dictem-get-default-strategy default-strat)
   'dictem-strategy-history))

(defun dictem-select-database (spec-dbs user-dbs &optional default-db)
  "Switches to minibuffer and asks user
to enter a database name."
  (interactive)
  (if (dictem-error-p dictem-database-alist)
      (error "A list of databases was not initialized properly"))
  (let* ((dbs (dictem-remove-value-from-alist dictem-database-alist))
	 (dbs2 (if user-dbs
		   (if dictem-use-user-databases-only
		       dictem-user-databases-alist
		     (append dictem-user-databases-alist dbs)
		     )
		 dbs)))
    (dictem-select
     "db"
     (if spec-dbs (dictem-prepand-special-dbs dbs2) dbs2)
     (dictem-get-default-database default-db)
     'dictem-database-history)))

(defun dictem-read-query (&optional default-query)
  "Switches to minibuffer and asks user to enter a query."
  (interactive)
  (if (featurep 'xemacs)
      (read-string
       (concat "query [" default-query "]: ")
       nil 'dictem-query-history default-query)
    (read-string
     (concat "query [" default-query "]: ")
     (if dictem-empty-initial-input nil default-query)
     'dictem-query-history default-query t)))


;;;;;;;;           Hooks        ;;;;;;;;

(defcustom dictem-postprocess-definition-hook
  nil
  "Hook run in dictem mode buffers containing DEFINE result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-separator
	     dictem-postprocess-definition-hyperlinks
	     dictem-postprocess-each-definition
	     dictem-postprocess-definition-remove-header
	     dictem-postprocess-collect-hyperlinks))

(defcustom dictem-postprocess-match-hook
  nil
  "Hook run in dictem mode buffers containing MATCH result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-match))

(defcustom dictem-postprocess-show-info-hook
  nil
  "Hook run in dictem mode buffers containing SHOW INFO result."
  :group 'dictem
  :type 'hook
  :options '(dictem-postprocess-definition-hyperlinks
	     dictem-postprocess-collect-hyperlinks))

(defcustom dictem-postprocess-show-server-hook
  nil
  "Hook run in dictem mode buffers containing SHOW SERVER result."
  :group 'dictem
  :type 'hook)

;;;;;;;;    Search Functions     ;;;;;;;

(defun dictem-call-dict-internal (fun databases)
  (let ((exit-status -1))

    (defun dictem-local-call-dict-internal-iter (fun databases)
      (if databases
	  (let ((ex_st (funcall fun (car databases))))
	    (cond
	     ((= ex_st 0)
	      (setq exit-status 0))
	     (t (if (/= 0 exit-status)
		    (setq exit-status ex_st)))
	     )
	    (dictem-local-call-dict-internal-iter fun (cdr databases)))))

    (cond
     ((null databases) 0)
     ((stringp databases)
      (dictem-local-call-dict-internal-iter fun (cons databases nil)))
     ((consp databases)
      (dictem-local-call-dict-internal-iter fun (cdr databases)))
     (t (error "wrong type of argument"))
     )

    (if (= exit-status -1) 0 exit-status)
    ))

(defun dictem-make-url (host port database define_or_match query &optional strategy)
  "Returns dict:// URL"
  (concat
   "dict://" host ":"
   (dictem-get-port (if port port "2628"))
   "/" (if define_or_match "d" "m") ":" query ":" database
   (if (null define_or_match) (concat ":" (if strategy strategy ".")))
   ))

(defun dictem-base-show-databases (a b c)
  "Show a list of databases"
  (interactive)

  (let* ((beg (point))
	 (exit_status
	  (call-process
	   dictem-client-prog nil (current-buffer) nil
	   "-P" "-" "-D"
	   "-h" (dictem-get-server) "-p" (dictem-get-port)
	   "--client" (dictem-client-text)
	   )))

    (cond ((= 0 exit_status)
	   nil)
	  (t
	   (if (/= beg (point))
	       (setq dictem-error-messages
		     (append
		      (list
		       (dictem-make-url (dictem-get-server)
					(dictem-get-port) "" t "")
		       (buffer-substring-no-properties beg (point)))
		      dictem-error-messages)))
	   (kill-region beg (point))))))

(defun dictem-base-search (databases query strategy)
  "dictem search: MATCH + DEFINE"
  (interactive)

  (let ((ex_status -1))
  (defun dictem-local-run-dict-search (database)
    (let ((splitted-url nil))
      (if (setq splitted-url (dictem-parse-url database))
	  (save-dictem (setq dictem-server (nth 1 splitted-url))
		       (setq dictem-port (dictem-get-port (nth 2 splitted-url)))
		       (setq database      (nth 3 splitted-url))
		       (dictem-initialize)
		       (dictem-local-run-dict-search database)
		       )
	(let* ((beg (point))
	       (exit_status
		(call-process
		 dictem-client-prog nil (current-buffer) nil
		 "-P" "-" "-d" database "-s" strategy
		 "-h" (dictem-get-server) "-p" (dictem-get-port)
		 "--client" (dictem-client-text)
		 query)))

	  (cond ((= 0 exit_status)
		 (setq ex_status 0)
		 (save-excursion
		   (narrow-to-region beg (point))
		   (run-hooks 'dictem-postprocess-definition-hook)
		   (widen)))
		((= 21 exit_status)
		 (if (= -1 ex_status)
		     (setq ex_status exit_status))
		 (save-excursion
		   (narrow-to-region beg (point))
		   (run-hooks 'dictem-postprocess-match-hook)
		   (widen)))
		(t
		 (if (= -1 ex_status)
		     (setq ex_status exit_status))
		 (if (/= beg (point))
		     (setq dictem-error-messages
			   (append
			    (list
			     (dictem-make-url (dictem-get-server)
					      (dictem-get-port) query t database)
			     (buffer-substring-no-properties beg (point)))
			    dictem-error-messages)))
		 (kill-region beg (point))))
	  ex_status))))

  (setq dictem-last-database database)
  (setq dictem-last-strategy strategy)

  (dictem-call-dict-internal 'dictem-local-run-dict-search databases)))

(defun dictem-base-define (databases query strategy)
  "dictem search: DEFINE"
  (interactive)

  (let ((ex_status -1))
    (defun dictem-local-run-dict-define (database)
      (let ((splitted-url nil))
	(if (setq splitted-url (dictem-parse-url database))
	    (save-dictem (setq dictem-server (nth 1 splitted-url))
			 (setq dictem-port (dictem-get-port (nth 2 splitted-url)))
			 (setq database      (nth 3 splitted-url))
			 (dictem-initialize)
			 (dictem-local-run-dict-define database)
			 )
	  (let* ((beg (point))
		 (exit_status
		  (call-process
		   dictem-client-prog nil (current-buffer) nil
		   "-P" "-" "-d" database
		   "-h" (dictem-get-server) "-p" (dictem-get-port)
		   "--client" (dictem-client-text)
		   query)))

	    (cond ((= 0 exit_status)
		   (save-excursion
		     (narrow-to-region beg (point))
		     (run-hooks 'dictem-postprocess-definition-hook)
		     (widen)))
		  ((= 21 exit_status)
		   (save-excursion
		     (narrow-to-region beg (point))
		     (run-hooks 'dictem-postprocess-match-hook)
		     (widen)))
		  (t
		   (if (= -1 ex_status)
		       (setq ex_status exit_status))
		   (if (/= beg (point))
		       (setq dictem-error-messages
			     (append
			      (list
			       (dictem-make-url (dictem-get-server)
						(dictem-get-port) query t database)
			       (buffer-substring-no-properties beg (point)))
			      dictem-error-messages)))
		   (kill-region beg (point))))
	    ex_status))))

    (setq dictem-last-database database)
    (dictem-call-dict-internal 'dictem-local-run-dict-define databases)))

(defun dictem-base-match (databases query strategy)
  "dictem search: MATCH"
  (interactive)

  (let ((ex_status -1))
    (defun dictem-local-run-dict-match (database)
      (let ((splitted-url nil))
	(if (setq splitted-url (dictem-parse-url database))
	    (save-dictem (setq dictem-server (nth 1 splitted-url))
			 (setq dictem-port (dictem-get-port (nth 2 splitted-url)))
			 (setq database      (nth 3 splitted-url))
			 (dictem-initialize)
			 (dictem-local-run-dict-match database)
			 )
	  (let* ((beg (point))
		 (exit_status
		  (call-process
		   dictem-client-prog nil (current-buffer) nil
		   "-P" "-" "-d" database "-s" strategy
		   "-h" (dictem-get-server) "-p" (dictem-get-port) "-m"
		   "--client" (dictem-client-text)
		   query)))
	    (cond ((= 0 exit_status)
		   (save-excursion
		     (narrow-to-region beg (point))
		     (run-hooks 'dictem-postprocess-match-hook)
		     (widen)))
		  (t
		   (if (= -1 ex_status)
		       (setq ex_status exit_status))
		   (if (/= beg (point))
		       (setq dictem-error-messages
			     (append
			      (list
			       (dictem-make-url (dictem-get-server)
						(dictem-get-port) query t database)
			       (buffer-substring-no-properties beg (point)))
			      dictem-error-messages)))
		   (kill-region beg (point))))
	    ex_status))))

    (setq dictem-last-database database)
    (setq dictem-last-strategy strategy)
    (dictem-call-dict-internal 'dictem-local-run-dict-match databases)))

(defun dictem-base-show-info (databases b c)
  "dictem: SHOW INFO command"
  (interactive)

  (let ((ex_status -1))
    (defun run-dict-show-info (database)
      (let* ((beg (point))
	     (exit_status
	      (call-process
	       dictem-client-prog nil (current-buffer) nil
	       "-P" "-" "-i" database
	       "-h" (dictem-get-server) "-p" (dictem-get-port)
	       "--client" (dictem-client-text)
	       )))
	(cond ((= 0 exit_status)
	       (save-excursion
		 (narrow-to-region beg (point))
		 (run-hooks 'dictem-postprocess-show-info-hook)
		 (widen)))
	      (t
	       (if (= -1 ex_status)
		   (setq ex_status exit_status))
	       (if (/= beg (point))
		   (setq dictem-error-messages
			 (append
			  (list
			   (dictem-make-url (dictem-get-server)
					    (dictem-get-port) "" t database)
			   (buffer-substring-no-properties beg (point)))
			  dictem-error-messages)))
	       (kill-region beg (point))))
	ex_status))

    (setq dictem-last-database database)
    (dictem-call-dict-internal 'run-dict-show-info databases)))

(defun dictem-base-show-server (a b c)
  "dictem: SHOW SERVER command"
  (interactive)

  (let* ((beg (point))
	 (exit_status
	  (call-process
	   dictem-client-prog nil (current-buffer) nil
	   "-P" "-" "-I"
	   "-h" (dictem-get-server) "-p" (dictem-get-port)
	   "--client" (dictem-client-text)
	   )))
    (cond ((= 0 exit_status)
	   (save-excursion
	     (narrow-to-region beg (point))
	     (run-hooks 'dictem-postprocess-show-server-hook)
	     (widen))))
    exit_status))

(defun dictem-get-error-message (exit_status)
  (cond
   ((= exit_status 0) "All is fine")
   ((= exit_status 20) "No matches found")
   ((= exit_status 21) "Approximate matches found")
   ((= exit_status 22) "No databases available")
   ((= exit_status 23) "No strategies available")

   ((= exit_status 30) "Unexpected response code from server")
   ((= exit_status 31) "Server is temporarily unavailable")
   ((= exit_status 32) "Server is shutting down")
   ((= exit_status 33) "Syntax error, command not recognized")
   ((= exit_status 34) "Syntax error, illegal parameters")
   ((= exit_status 35) "Command not implemented")
   ((= exit_status 36) "Command parameter not implemented")
   ((= exit_status 37) "Access denied")
   ((= exit_status 38) "Authentication failed")
   ((= exit_status 39) "Invalid database name")
   ((= exit_status 40) "Invalid strategy name")
   ((= exit_status 41) "Connection to server failed")
   (t                  (concat "Ooops!" (number-to-string exit_status)))
   ))

(defun dictem-generate-full-error-message (exit_status)
  (defun dictem-local-internal (err-msgs exit_status)
    (if err-msgs
	(concat (car err-msgs) "\n"
		(cadr err-msgs)
		"\n"
		(dictem-local-internal
		 (cddr err-msgs)
		 nil)
		)
      (if exit_status
	  (dictem-get-error-message exit_status)
	nil)))

  (concat "Error messages:\n\n"
	  (dictem-local-internal dictem-error-messages exit_status)))

(defun dictem-run (search-fun &optional database query strategy)
  "Creates new *dictem* buffer and run search-fun"
  (interactive)

  (let ((ex_status -1))

    (defun dictem-local-run-functions (funs database query strategy)
      (cond
       ((functionp funs)
	(let ((ex_st (funcall funs database query strategy)))
	  (if (/= ex_status 0)
	      (setq ex_status ex_st))))
       ((and (consp funs) (functionp (car funs)))
	(dictem-local-run-functions (car funs) database query strategy)
	(dictem-local-run-functions (cdr funs) database query strategy))
       ((null funs)
	nil)
       (t (error "wrong argument type"))
       )
      ex_status)

    (let ((coding-system nil))
      (if (and (functionp 'coding-system-list)
	       (member 'utf-8 (coding-system-list)))
	  (setq coding-system 'utf-8))
      (let ((selected-window (frame-selected-window))
	    (coding-system-for-read coding-system)
	    (coding-system-for-write coding-system)
	    ; here we remember values of variables local to buffer
	    (server dictem-server)
	    (port   dictem-port)
	    (dbs    dictem-database-alist)
	    (strats dictem-strategy-alist)
	    (user-dbs  dictem-user-databases-alist)
	    (user-only dictem-use-user-databases-only)
	    (use-existing-buf dictem-use-existing-buffer)
	    )
	(if dictem-use-existing-buffer
	    (dictem-ensure-buffer)
	  (dictem))
;	(set-buffer-file-coding-system coding-system)
	(make-local-variable 'dictem-default-strategy)
	(make-local-variable 'dictem-default-database)
	(make-local-variable 'case-replace)
	(make-local-variable 'case-fold-search)

	; the following lines are to inherit values local to buffer
	(set (make-local-variable 'dictem-server) server)
	(set (make-local-variable 'dictem-port)   port)
	(set (make-local-variable 'dictem-database-alist) dbs)
	(set (make-local-variable 'dictem-strategy-alist) strats)
	(set (make-local-variable 'dictem-user-databases-alist) user-dbs)
	(set (make-local-variable 'dictem-use-user-databases-only) user-only)
	(set (make-local-variable 'dictem-use-existing-buffer) use-existing-buf)

	(set (make-local-variable 'dictem-hyperlinks-alist) nil)

	;;;;;;;;;;;;;;
	(setq case-replace nil)
	(setq case-fold-search nil)
	(setq dictem-error-messages nil)
	(dictem-local-run-functions search-fun database query strategy)
	(if (and (not (equal ex_status 0)) (= (point-min) (point-max)))
	    (insert (dictem-generate-full-error-message ex_status)))
	(goto-char (point-min))
	(setq buffer-read-only t)
	ex_status
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictem-next-section ()
  "Move point to the next definition"
  (interactive)
  (forward-char)
  (if (search-forward-regexp "^From " nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun dictem-previous-section ()
  "Move point to the previous definition"
  (interactive)
  (backward-char)
  (if (search-backward-regexp "^From " nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun dictem-hyperlinks-menu ()
  "Hyperlinks menu with autocompletion"
  (interactive)
  (let ((link (completing-read "Go to:" dictem-hyperlinks-alist)))
    (if (and link (setq link (assoc link dictem-hyperlinks-alist)))
	(dictem-run-define
	 (cadr link)
	 dictem-last-database))
    ))

(defun dictem-next-link ()
  "Move point to the next hyperlink"
  (interactive)
  (let ((pt nil)
	(limit (point-max)))
    (if (and (setq pt (next-single-property-change
		       (point) 'link nil limit))
	     (/= limit pt))
	(if (get-char-property pt 'link)
	    (goto-char pt)
	  (goto-char (next-single-property-change pt 'link nil limit))))
    ))

(defun dictem-previous-link ()
  "Move point to the previous hyperlink"
  (interactive)
  (let ((pt nil)
	(limit (point-min)))
    (if (and (setq pt (previous-single-property-change
		       (point) 'link nil limit))
	     (/= limit pt))
	(if (get-char-property pt 'link)
	    (goto-char pt)
	  (goto-char (previous-single-property-change pt 'link nil limit))))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dictem-help ()
  "Display a dictem help"
  (interactive)
  (describe-function 'dictem-mode))

(defun dictem-mode ()
  "This is a mode for dict client implementing
the protocol defined in RFC 2229.

The default key bindings:

  q         bury the dictem buffer
  k         kill the dictem buffer
  h         display the help information

  s         make a new SEARCH, i.e. ask for a database, strategy and query
            and show definitions
  d         make a new DEFINE, i.e. ask for a database and query
            and show definitions
  m         make a new MATCH, i.e. ask for database, strategy and query
            and show matches
  r         show information about DICT server
  i         ask for a database and show information about it
  n         move point to the next definition
  p         move point to the previous definition
  SPC                 scroll dictem buffer up
  DEL                 scroll dictem buffer down
  mouse-2 or RET      visit a link (DEFINE using all dictionaries)
"
;  SPC       search the marked region (DEFINE) in all dictionaries

  (interactive)

  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map dictem-mode-map)
  (setq major-mode 'dictem-mode)
  (setq mode-name "dictem")

  (add-hook 'kill-buffer-hook 'dictem-kill t t)
  (run-hooks 'dictem-mode-hook)
  )

(defvar dictem-window-configuration
  nil
  "The window configuration to be restored upon closing the buffer")

(defvar dictem-selected-window
  nil
  "The currently selected window")

(defvar dictem-content-history
  nil
  "A list of lists (buffer_content point)")

(defconst dictem-buffer-name
  "*dictem buffer*")

(defconst dictem-url-regexp
  "^\\(dict\\)://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*\\)$")

(defun dictem-parse-url (url)
  "Parses string like dict://dict.org:2628/foldoc
and returns a list containing protocol, server, port and path on nil if fails"
  (if (string-match dictem-url-regexp url)
      (list
       (match-string 1 url )
       (match-string 2 url)
       (match-string 4 url)
       (match-string 5 url)
       )
    nil))

(defun dictem ()
  "Create a new dictem buffer and install dictem-mode"
  (interactive)

  (let (
	(buffer (generate-new-buffer dictem-buffer-name))
	(window-configuration (current-window-configuration))
	(selected-window (frame-selected-window)))
    (switch-to-buffer-other-window buffer)
    (dictem-mode)

    (make-local-variable 'dictem-window-configuration)
    (make-local-variable 'dictem-selected-window)
    (make-local-variable 'dictem-content-history)
    (setq dictem-window-configuration window-configuration)
    (setq dictem-selected-window selected-window)
    ))

;(unless dictem-mode-map
(setq dictem-mode-map (make-sparse-keymap))
(suppress-keymap dictem-mode-map)

; Kill the buffer
(define-key dictem-mode-map "k" 'dictem-kill)

; Kill all dictem buffers
(define-key dictem-mode-map "x" 'dictem-kill-all-buffers)

; Bury the buffer
(define-key dictem-mode-map "q" 'dictem-quit)

; LAST, works like in Info-mode
(define-key dictem-mode-map "l" 'dictem-last)

; Show help message
(define-key dictem-mode-map "h" 'dictem-help)

; SEARCH = MATCH + DEFINE
(define-key dictem-mode-map "s" 'dictem-run-search)

; MATCH
(define-key dictem-mode-map "m" 'dictem-run-match)

; DEFINE
(define-key dictem-mode-map "d" 'dictem-run-define)

; SHOW SERVER
(define-key dictem-mode-map "r" 'dictem-run-show-server)

; SHOW INFO
(define-key dictem-mode-map "i" 'dictem-run-show-info)

; Move point to the next DEFINITION
(define-key dictem-mode-map "n" 'dictem-next-section)

; Move point to the previous DEFINITION
(define-key dictem-mode-map "p" 'dictem-previous-section)

; Move point to the next HYPER LINK
(define-key dictem-mode-map "\M-n" 'dictem-next-link)

; Move point to the previous HYPER LINK
(define-key dictem-mode-map "\M-p" 'dictem-previous-link)

; Hyperlinks menu
(define-key dictem-mode-map "e" 'dictem-hyperlinks-menu)

; Scroll up dictem buffer
(define-key dictem-mode-map " " 'scroll-up)

; Scroll down dictem buffer
(define-key dictem-mode-map "\177" 'scroll-down)

; Define on click
(if (featurep 'xemacs)
    (define-key dictem-mode-map [button2]
      'dictem-define-on-click)
  (define-key dictem-mode-map [mouse-2]
    'dictem-define-on-click))

(define-key dictem-mode-map "\C-m"
  'dictem-define-on-press)

(defun dictem-mode-p ()
  "Return non-nil if current buffer has dictem-mode"
  (eq major-mode 'dictem-mode))

(defun dictem-ensure-buffer ()
  "If current buffer is not a dictem buffer, create a new one."
  (if (dictem-mode-p)
      (progn
	(if dictem-use-content-history
	    (setq dictem-content-history
		  (cons (list (buffer-substring
			       (point-min) (point-max))
			      (point)) dictem-content-history)))
	(setq buffer-read-only nil)
	(erase-buffer))
    (dictem)))

(defun dictem-quit ()
  "Bury the current dictem buffer."
  (interactive)
  (if (featurep 'xemacs)
      (bury-buffer)
    (quit-window)))

(defun dictem-kill ()
  "Kill the current dictem buffer."
  (interactive)

  (if (eq major-mode 'dictem-mode)
      (progn
	(setq major-mode nil)
	(let ((configuration dictem-window-configuration)
	      (selected-window dictem-selected-window))
	  (kill-buffer (current-buffer))
	  (if (window-live-p selected-window)
	      (progn
		(select-window selected-window)
		(set-window-configuration configuration)))))))

(defun dictem-last ()
  "Go back to the last buffer visited visited."
  (interactive)
  (if (eq major-mode 'dictem-mode)
      (if dictem-content-history
	  (progn
	    (setq buffer-read-only nil)
	    (delete-region (point-min) (point-max))
	    (insert-string (car (car dictem-content-history)))
	    (goto-char (cadr (car dictem-content-history)))
	    (setq dictem-content-history (cdr dictem-content-history))
	    )
	  )
    ))

(defun dictem-kill-all-buffers ()
  "Kill all dictem buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buf-name (buffer-name buffer)))
      (if (and (<= (length dictem-buffer-name) (length buf-name))
	       (string= dictem-buffer-name
			(substring buf-name 0 (length dictem-buffer-name))))
	  (kill-buffer buf-name))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Top-level Functions     ;;;;;;

(defun dictem-run-match (query database strat)
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows matches in it."
  (interactive
   (list 
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))
    (dictem-select-strategy (dictem-get-default-strategy))))
  (dictem-run 'dictem-base-match database query strat))

(defun dictem-run-define (query database)
  "Asks a user about database name and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive
   (list
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))))
  (dictem-run 'dictem-base-define database query nil))

(defun dictem-run-search (query database strat)
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive
   (list
    (dictem-read-query (thing-at-point 'word))
    (dictem-select-database t t (dictem-get-default-database))
    (dictem-select-strategy (dictem-get-default-strategy))))
  (dictem-run 'dictem-base-search database query strat))

(defun dictem-run-show-info (database)
  "Asks a user about database name
creates new *dictem* buffer and
shows information about it."
  (interactive (list
		(dictem-select-database
		 nil nil
		 (dictem-get-default-database))))
  (dictem-run 'dictem-base-show-info database))

(defun dictem-run-show-server ()
  "Creates new *dictem* buffer and
shows information about DICT server in it."
  (interactive)
  (dictem-run 'dictem-base-show-server))

(defun dictem-run-show-databases ()
  "Creates new *dictem* buffer and
shows information about databases provided by DICT."
  (interactive)
  (dictem-run 'dictem-base-show-databases))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-menu-define
 dictem-menu
 dictem-mode-map
 "DictEm Menu"
 `("DictEm"
   ["DictEm..." dictem-help t]
   "--"
   ["Next Section"     dictem-next-section t]
   ["Previous Section" dictem-previous-section t]
   "--"
   ["Match"            dictem-run-match t]
   ["Definition"       dictem-run-define t]
   ["Search"           dictem-run-search t]
   "--"
   ["Information about server"   dictem-run-show-server t]
   ["Information about database" dictem-run-show-info t]
   ["A list of available databases" dictem-run-show-databases t]
   "--"
   ["Bury Dictem Buffer" dictem-quit t]
   ["Kill Dictem Buffer" dictem-kill t]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      Optional Features       ;;;;;
(defun dictem-create-link (start end face function &optional data add-props)
  "Create a link in the current buffer starting from `start' going to `end'.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((properties
	 (append (list 'face face
		       'mouse-face 'highlight
		       'link-data data
		       'link-function function
		       'dictem-server dictem-server
		       'dictem-port   dictem-port)
	 add-props)))
    (remove-text-properties start end properties)
    (add-text-properties start end properties)))

;;;;;;;   Postprocessing Functions     ;;;;;;;

(defun dictem-postprocess-definition-separator ()
  (save-excursion
    (goto-char (point-min))
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
	  (dictem-create-link
	   beg-dbname end-dbname
	   'dictem-reference-dbname-face
	   'dictem-base-show-info
	   (list (cons 'dbname dictem-current-dbname))))
	))))

(defvar dictem-hyperlink-beginning
  "{"
  "String that begins hyperlink.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defvar dictem-hyperlink-end
  "}"
  "String that ends hyperlink.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defvar dictem-hyperlink-define-func
  'dictem-base-define
  "Function called when user clicks on hyperlinks inside the definition.
This variable is used by
the function 'dictem-postprocess-definition-hyperlinks'")

(defun dictem-postprocess-collect-hyperlinks ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\(" dictem-hyperlink-beginning "\\([^{}|]+\\)"
		  dictem-hyperlink-end
		  "\\|\\(" dictem-hyperlink-beginning
		  "\\([^{}|\n]+\\)|\\([^{}|\n]+\\)" dictem-hyperlink-end
		  "\\)\\)")))

      (while (search-forward-regexp regexp nil t)
	(cond ((match-beginning 2)
	       (let* ((word (dictem-replace-spaces
			     (buffer-substring-no-properties
			      (match-beginning 2)
			      (match-end 2)))))
		 (setq dictem-hyperlinks-alist
		       (cons (list word word) dictem-hyperlinks-alist))
		 ))
	      ((match-beginning 3)
	       (let* ((word-beg (match-beginning 4))
		      (word-end (match-end 4))
		      (link-beg (match-beginning 5))
		      (link-end (match-end 5))
		      (word (dictem-replace-spaces
			     (buffer-substring-no-properties
			      word-beg word-end)))
		      (link (dictem-replace-spaces
			     (buffer-substring-no-properties
			      link-beg link-end)))
		      )
		 (setq dictem-hyperlinks-alist
		       (cons (list word link) dictem-hyperlinks-alist))
		 )))))
    ))

(defun dictem-postprocess-definition-hyperlinks ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp
	   (concat dictem-hyperlink-beginning "\\([^{}|]+\\)" dictem-hyperlink-end
		   "\\|"
		   "^From [^\n]+\\[\\([^\n]+\\)\\]"
		   "\\|"
		   "\\(" dictem-hyperlink-beginning "\\([^{}|\n]+\\)|\\([^{}|\n]+\\)"
		   dictem-hyperlink-end "\\)")))

      (while (search-forward-regexp regexp nil t)
	(cond ((match-beginning 1)
	       (let* ((beg (match-beginning 1))
		      (end (match-end 1))
		      (match-beg (match-beginning 0))
		      (word (buffer-substring-no-properties beg end)))
		 (replace-match word t t)
		 (dictem-create-link
		  match-beg (+ match-beg (length word))
		  'dictem-reference-definition-face
		  dictem-hyperlink-define-func
		  (list (cons 'word (dictem-replace-spaces word))
			(cons 'dbname dictem-current-dbname))
		  '(link t))
		 ))
	      ((match-beginning 2)
	       (if (null dictem-current-dbname)
		   (setq dictem-current-dbname
			 (dictem-replace-spaces
			  (buffer-substring-no-properties (match-beginning 2)
							  (match-end 2))))))
	      ((match-beginning 3)
	       (let* ((beg (match-beginning 5))
		      (end (match-end 5))
		      (match-beg (match-beginning 3))
	              (repl-beg (match-beginning 4))
		      (repl-end (match-end 4))
		      (repl (buffer-substring-no-properties repl-beg repl-end))
		      (word (buffer-substring-no-properties beg end)))
		 (replace-match repl t t)
		 (dictem-create-link
		  match-beg (+ match-beg (length repl))
		  'dictem-reference-definition-face
		  dictem-hyperlink-define-func
		  (list (cons 'word (dictem-replace-spaces word))
			(cons 'dbname dictem-current-dbname))
		  '(link t))))
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
	  (dictem-create-link
	   beg (- end 1)
	   'dictem-reference-dbname-face 'dictem-base-show-info
	   (list (cons 'dbname last-database))))
	 ((match-beginning 1)
	  (dictem-create-link
	   beg end
	   'dictem-reference-m1-face 'dictem-base-define
	   (list (cons 'word
		       (dictem-replace-spaces
			(buffer-substring-no-properties
			 (+ beg 1) (- end 1))))
		 (cons 'dbname last-database))))
	 (t
	  (dictem-create-link
	   beg end
	   'dictem-reference-m2-face 'dictem-base-define
	   (list (cons 'word
		       (dictem-replace-spaces
			(buffer-substring-no-properties
			 beg end )))
		 (cons 'dbname last-database))))
	 )))))

(defun dictem-postprocess-definition-remove-header ()
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (let (eol (point))
      (goto-char (point-min))
      (if (search-forward-regexp "definitions? found" eol t)
	  (progn
	    (goto-char (point-min))
	    (kill-line 2)
	    )
	))))

;;;;;       On-Click Functions     ;;;;;

(defun dictem-define-on-press ()
  "Is called upon pressing Enter."
  (interactive)

  (let* (
	 (properties (text-properties-at (point)))
	 (data (plist-get properties 'link-data))
	 (fun  (plist-get properties 'link-function))
	 (dictem-server (plist-get properties 'dictem-server))
	 (dictem-port   (plist-get properties 'dictem-port))
	 (word   (assq 'word data))
	 (dbname (assq 'dbname data))
	 )
    (if (or word dbname)
	(dictem-run fun
		    (if dbname (cdr dbname) dictem-last-database)
		    (if word (cdr word) nil)
		    nil))))

(defun dictem-define-on-click (event)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (dictem-define-on-press))

;(defun dictem-define-with-db-on-click (event)
;  "Is called upon clicking the link."
;  (interactive "@e")
;
;  (mouse-set-point event)
;  (let* (
;	 (properties (text-properties-at (point)))
;	 (word (plist-get properties 'link-data)))
;    (if word
;	(dictem-run 'dictem-base-define (dictem-select-database) word nil))))

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

(provide 'dictem)

(defgroup dictem nil
  "Client for accessing the DICT server."
  :group 'help
  :group 'hypermedia
  )

(defcustom dictem-server "dict.org"
  "The DICT server"
  :group 'dictem
  :type 'string
  )

(defcustom dictem-port "2628"
  "The port of the DICT server"
  :group 'dictem
  :type 'string
  )

(defcustom dictem-client-prog "dict"
  "The command line DICT client.
dictem accesses DICT server through this executable."
  :group 'dictem
  :type 'string
  )

(defvar dictem-strategy-list
  '(("."         nil)
;    ("word"    nil)
    ("exact"     nil)
    ("prefix"    nil)
    ("substring" nil)
    ("suffix"  nil)
    ("re"      nil)
    ("regexp"  nil)
    ("soundex" nil)
    ("lev"     nil)
    )
  )

(defvar dictem-database-list
  '(( "*"   nil )
    ( "elements" nil )
    ( "web1913" nil )
    ( "wn" nil )
    ( "gazetteer" nil )
    ( "jargon" nil )
    ( "foldoc" nil )
    ( "easton" nil )
    ( "hitchcock" nil )
    ( "devils" nil )
    ( "world02" nil )
    ( "vera" nil )
    )
  )

(defcustom dictem-default-strategy "."
  "The default search strategy."
  :group 'dictem
  :group 'string
  )

(defcustom dictem-default-database "*"
  "The default database name."
  :group 'dictem
  :group 'string
  )

(defvar dictem-strategy-history nil)
(defvar dictem-database-history nil)
(defvar dictem-query-history nil)
(defvar dictem-last-database
  "Last used database name"
  "*"
  )

(defvar dictem-last-strategy
  "Last used strategy name"
  "."
  )

(defvar dictem-mode-map
  nil
  "Keymap for dictem mode")

(defun list2alist (list)
  (if
   list
   (cons
    (list (car list) nil)
    (list2alist (cdr list))
    )
   nil
   )
  )

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
     default
     )
    )
  )

(defun get-first-token ()
  (let
      ((str (thing-at-point 'line)))
    (if
     (string-match "^ [^ ][^ ]*" str )
     (list (substring str ( + (match-beginning 0) 1) (match-end 0)))
     )
    )
  )

(defun get-first-tokens-from-temp-buffer ()
;    (switch-to-buffer "*dict-temp*")
  (set-buffer "*dict-temp*")
  (beginning-of-buffer)
  (let ((list-of-first-tokens nil )) ;(get-first-tokens)))
    (while (= (forward-line 1) 0)
      (setq list-of-first-tokens (append (get-first-token) list-of-first-tokens))
      )
    list-of-first-tokens
    )
  )

(defun dictem-set-strategies ()
  "Obtain strategy list from a DICT server
and sets dictem-strategy-list variable."
  (interactive)
;  (if (buffer-live-p "*dict-temp*")
;      (kill-buffer "*dict-temp*"))
  (if
   (eq 0 (call-process "dict" nil "*dict-temp*" nil "-P" "-" "-S" "-h" dictem-server "-p" dictem-port))
   (setq dictem-strategy-list
	 (cons
	  (list "." nil)
	  (nreverse (list2alist (get-first-tokens-from-temp-buffer) ) )
	  )
	 )
   )
  (kill-buffer "*dict-temp*")
  )

(defun dictem-set-databases ()
  "Obtain database list from a DICT server
and sets dictem-database-list variable."
  (interactive)
;  (if (buffer-live-p "*dict-temp*")
;      (kill-buffer "*dict-temp*"))
  (if
   (eq 0 (call-process "dict" nil "*dict-temp*" nil "-P" "-" "-D" "-h" dictem-server "-p" dictem-port))
   (setq dictem-database-list
	 (cons
	  (list "*" nil)
	  (nreverse (list2alist (get-first-tokens-from-temp-buffer)))
	  )
	 )
   )
  (kill-buffer "*dict-temp*")
  )

(defun dictem-help ()
  "Display a dictem help"
  (interactive)
  (describe-function 'dictem-mode)
  )

(defun dictem-select-strategy (&optional default-strat)
  "Switches to minibuffer and ask user
to enter a search strategy."
  (interactive)
  (setq dictem-last-strategy
	(dictem-select
	 "strategy"
	 dictem-strategy-list
	 (if
	  default-strat
	  default-strat
	  (if dictem-strategy-history
	      (car dictem-strategy-history)
	      "exact"
	      )
	  )
	 'dictem-strategy-history
	 )
	)
  )

(defun dictem-select-database (&optional default-db)
  "Switches to minibuffer and ask user
to enter a database name."
  (interactive)
  (setq dictem-last-database
	(dictem-select
	 "db"
	 dictem-database-list
	 (if
	  default-db
	  default-db
	  (if dictem-database-history
	      (car dictem-database-history)
	      "*"
	      )
	  )
	 'dictem-database-history
	 )
	)
  )

(defun dictem-read-query (&optional default-query)
  "Switches to minibuffer and ask user
to enter a query."
  (interactive)
  (read-string
   (concat "query:(" default-query ") ")
   nil
   'dictem-query-history
   default-query
   t)
  )

(defun dictem-replace-spaces (str)
  (while (string-match "  +" str)
    (setq str (replace-match " " t t str)))
  (if (string-match "^ +" str)
      (setq str (replace-match "" t t str)))
  (if (string-match " +$" str)
      (setq str (replace-match "" t t str)))
  str
  )

;(dictem-replace-spaces " qwe   ertrwww   ")

(defface dictem-reference-define-face
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
  :group 'dictem)

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
  :group 'dictem)

(defface dictem-reference-m2-face
  nil

  "The face that is used for displaying a reference to
a single word in a MATCH search."
  :group 'dictem)

(defun dictem-define-on-click (event)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (let* (
	 (properties (text-properties-at (point)))
	 (word (plist-get properties 'link-data)))
    (if word
	(dictem-run 'dictem-define-base dictem-last-database word nil)
	)
    )
  )

(defun dictem-define-with-db-on-click (event)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (let* (
	 (properties (text-properties-at (point)))
	 (word (plist-get properties 'link-data)))
    (if word
	(dictem-run 'dictem-define-base (dictem-select-database) word nil)
	)
    )
  )

(defun link-create-link (start end face function &optional data help)
  "Create a link in the current buffer starting from `start' going to `end'.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((properties `(face ,face
	              mouse-face highlight
		      link t
		      link-data ,data)
;		      help-echo ,help
;		      link-function ,function)
	  )
	)
    (remove-text-properties start end properties)
    (add-text-properties start end properties)))

(defun dictem-new-search (word &optional all)
;  (interactive)
  (dictem-run
   'dictem-define-base
   dictem-last-database
   word
   "exact"
   )
  )

(defun dictem-colorit-define ()
;  (interactive)
  (let ((regexp "\\({\\)\\([^}]*\\)\\(}\\)"))
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (search-forward-regexp regexp nil t)
	  (progn
	    (let* (
		   (match-length (- (match-end 2) (match-beginning 2)))
		   (match-string (match-string 2))
		   (match-start (match-beginning 1))
		   (match-finish (+ (match-beginning 1) match-length))
		   )
	      (replace-match "\\2")
	      (link-create-link
	       match-start
	       match-finish
	       'dictem-reference-define-face
	       'dictem-new-search
	       (dictem-replace-spaces
		(buffer-substring-no-properties match-start match-finish))
	       )
	      )
	    )
	  (goto-char (point-max))
	  )
      )
    )
  (beginning-of-buffer)
  )

(defun dictem-colorit-match ()
  (interactive)
  (let ((regexp1 "\"[^\"\n]*\"") (regexp2 "[^ \n][^ \n]*"))
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (search-forward-regexp regexp1 nil t)
	  (link-create-link
	   (match-beginning 0)
	   (match-end 0)
	   'dictem-reference-m1-face
	   'dictem-new-search
	   (dictem-replace-spaces
	    (buffer-substring-no-properties
	     (+ (match-beginning 0) 1)
	     (- (match-end 0) 1)))
	   )
	  (goto-char (point-max))
	  )
      )
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (search-forward-regexp regexp2 nil t)
	  (unless
	      (or
	       (equal 0 (match-beginning 0))
	       (get-text-property (match-beginning 0) 'link-data)
	       )
	    (link-create-link
	     (match-beginning 0)
	     (match-end 0)
	     'dictem-reference-m2-face
	     'dictem-new-search
	     (dictem-replace-spaces
	      (buffer-substring-no-properties
	       (match-beginning 0)
	       (match-end 0)))
	     )
	    )
	(goto-char (point-max))
	)
      )
    )
  (beginning-of-buffer)
  )

(defcustom dictem-mode-hook
  nil
  "Hook run in dictem mode buffers.")

(defun dictem-mode ()
  "This is a mode for dict client implementing
the protocol defined in RFC 2229.

The default key bindings:

  q         close the dictem buffer
  h         display the help information

  s         make a new SEARCH, i.e. ask for a database, strategy and query
            and show definitions
  d         make a new DEFINE, i.e. ask for a database and query
            and show definitions
  m         make a new MATCH, i.e. ask for database, strategy and query
            and show matches
  r         show information about DICT server
  i         ask for a database and show information about it
  mouse-2   visit a link (DEFINE using all dictionaries)
  C-mouse-2 visit a link (DEFINE using asked dictionaries)

  SPC       search the marked region (DEFINE) in all dictionaries
"

  (interactive)

  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map dictem-mode-map)
  (setq major-mode 'dictem-mode)
  (setq mode-name "dictem")

;  (toggle-read-only t)

  (add-hook 'kill-buffer-hook 'dictem-close t t)
  (run-hooks 'dictem-mode-hook)
  )

(defvar dictem-window-configuration
  nil
  "The window configuration to be restored upon closing the buffer")

(defvar dictem-selected-window
  nil
  "The currently selected window")

(defun dictem ()
  "Create a new dictem buffer and install dictem-mode"
  (interactive)

  (let (
	(buffer (generate-new-buffer "*dictem buffer*"))
	(window-configuration (current-window-configuration))
	(selected-window (frame-selected-window))
	)
    (switch-to-buffer-other-window buffer)
    (dictem-mode)

    (make-local-variable 'dictem-window-configuration)
    (make-local-variable 'dictem-selected-window)
    (setq dictem-window-configuration window-configuration)
    (setq dictem-selected-window selected-window)
    )
  )

;(unless dictem-mode-map
(setq dictem-mode-map (make-sparse-keymap))
(suppress-keymap dictem-mode-map)

(define-key dictem-mode-map "q"
  'dictem-close)

(define-key dictem-mode-map "h"
  'dictem-help)

(define-key dictem-mode-map [mouse-2]
  'dictem-define-on-click)

(define-key dictem-mode-map [C-down-mouse-2]
  'dictem-define-with-db-on-click)

; SEARCH = MATCH + DEFINE
(define-key dictem-mode-map "s"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-search-base
     (dictem-select-database)
     (dictem-read-query)
     (dictem-select-strategy)
     )
    )
  )

; MATCH
(define-key dictem-mode-map "m"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-match-base
     (dictem-select-database)
     (dictem-read-query)
     (dictem-select-strategy)
     )
    )
  )

; DEFINE
(define-key dictem-mode-map "d"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     (dictem-select-database)
     (dictem-read-query)
     nil
     )
    )
  )

; SHOW SERVER
(define-key dictem-mode-map "i"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-showinfo-base
     (dictem-select-database)
     nil
     nil
     )
    )
  )

; SHOW INFO
(define-key dictem-mode-map "r"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-showserver-base
     nil
     nil
     nil
     )
    )
  )

; DEFINE for the selected region
(define-key dictem-mode-map " "
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     "*"
     (thing-at-point 'word)
     nil
     )
    )
  )

; DEFINE for the selected region
(define-key dictem-mode-map [C-SPC]
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     (dictem-select-database dictem-last-database)
     (thing-at-point 'word)
     nil
     )
    )
  )

;  (link-initialize-keymap dictem-mode-map)

(defun dictem-mode-p ()
  "Return non-nil if current buffer has dictem-mode"
  (eq major-mode 'dictem-mode))

(defun dictem-ensure-buffer ()
  "If current buffer is not a dictem buffer, create a new one."
  (unless (dictem-mode-p)
    (dictem)
    )
  )

(defun dictem-close ()
  "Close the current dictem buffer"
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
		(set-window-configuration configuration)))
	  )
	)
      )
  )

(defun dictem-search-base (database query strategy)
  "dictem search: MATCH + DEFINE"
  (interactive)

  (call-process
   dictem-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database "-s" strategy
   "-h" dictem-server "-p" dictem-port
   query
   )
  (dictem-colorit-define)
  )

(defun dictem-define-base (database query strategy)
  "dictem search: DEFINE"
  (interactive)

  (call-process
   dictem-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database
   "-h" dictem-server "-p" dictem-port
   query
   )
  (dictem-colorit-define)
  )

(defun dictem-match-base (database query strategy)
  "dictem search: MATCH"
  (interactive)

  (call-process
   dictem-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database "-s" strategy
   "-h" dictem-server "-p" dictem-port "-m"
   query
   )
  (dictem-colorit-match)
  )

(defun dictem-showinfo-base (database b c)
  "dictem: SHOW SERVER command"
  (interactive)

  (call-process
   dictem-client-prog nil (current-buffer) nil
   "-P" "-" "-i" database
   "-h" dictem-server "-p" dictem-port
   )
  )

(defun dictem-showserver-base (a b c)
  "dictem: SHOW DB command"
  (interactive)

  (call-process
   dictem-client-prog nil (current-buffer) nil
   "-P" "-" "-I"
   "-h" dictem-server "-p" dictem-port
   )
  )

; search type may be "", 'dictem-define or 'dictem-match
(defun dictem-run (search-fun &optional database query strategy)
  "Creates new *dictem* buffer and run search-fun"
  (interactive)

  (let ((coding-system nil))
    (if (and (functionp 'coding-system-list)
	     (member 'utf-8 (coding-system-list)))
 	(setq coding-system 'utf-8))
    (let (
	  (selected-window (frame-selected-window))
	  (coding-system-for-read coding-system)
	  (coding-system-for-write coding-system)
	  )
      (dictem)
      (funcall search-fun database query strategy)
      (beginning-of-buffer)
      )
    )
  )

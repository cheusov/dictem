(require 'cl)
(require 'dictem-vars)
(require 'dictem-prog)

(defun dictem-prepand-special-strats (l)
  (cons '("." nil) l))

(defun dictem-prepand-special-dbs (l)
  (cons '("*" nil) (cons '("!" nil) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;        Main Functions        ;;;;;


;;;;;; Functions for Initialising ;;;;;;

(defun dictem-set-strategies (&optional server port)
  "Obtain strategy ALIST from a DICT server
and sets dictem-strategy-list variable."
  (interactive)
  (setq dictem-strategy-list (dictem-get-strategies server port)))

(defun dictem-set-databases (&optional server port)
  "Obtain database ALIST from a DICT server
and sets dictem-database-list variable."
  (interactive)
  (setq dictem-database-list (dictem-get-databases server port)))

;;; Functions related to Minibuffer ;;;;

(defun dictem-select-strategy (&optional default-strat)
  "Switches to minibuffer and ask the user
to enter a search strategy."
  (interactive)
  (dictem-select
   "strategy"
   (dictem-prepand-special-strats dictem-strategy-list)
   (if default-strat
       default-strat
     (if dictem-strategy-history
	 (car dictem-strategy-history)
       dictem-default-strategy))
   'dictem-strategy-history))

(defun dictem-select-database (&optional default-db)
  "Switches to minibuffer and ask user
to enter a database name."
  (interactive)
  (dictem-select
   "db"
   (dictem-prepand-special-dbs dictem-database-list)
   (if default-db
       default-db
     (if dictem-database-history
	 (car dictem-database-history)
       "*"))
   'dictem-database-history))

(defun dictem-read-query (&optional default-query)
  "Switches to minibuffer and ask user to enter a query."
  (interactive)
  (read-string
   (concat "query:(" default-query ") ")
   nil
   'dictem-query-history
   default-query
   t))

;;;;;;;;    Search Functions     ;;;;;;;

(defcustom dictem-postprocess-define-hook
  nil
  "Hook run in dictem mode buffers containing DEFINE result.")

(defcustom dictem-postprocess-match-hook
  nil
  "Hook run in dictem mode buffers containing MATCH result.")

(defcustom dictem-postprocess-dbinfo-hook
  nil
  "Hook run in dictem mode buffers containing SHOW INFO result.")

(defcustom dictem-postprocess-showserver-hook
  nil
  "Hook run in dictem mode buffers containing SHOW SERVER result.")

(defun dictem-search-base (database query strategy)
  "dictem search: MATCH + DEFINE"
  (interactive)

  (let ((exit_status 
	 (call-process
	 dictem-client-prog nil (current-buffer) nil
	 "-P" "-" "-d" database "-s" strategy
	 "-h" dictem-server "-p" dictem-port
	 query)))

    (beginning-of-buffer)
    (cond ((= 0 exit_status)
	   (let ((dictem-current-dbname database))
	     (run-hooks 'dictem-postprocess-define-hook)))
	  ((= 21 exit_status)
	   (forward-line-nomark)
	   (run-hooks 'dictem-postprocess-match-hook))
	  )))

(defun dictem-define-base (database query strategy)
  "dictem search: DEFINE"
  (interactive)

  (let ((exit_status
	 (call-process
	  dictem-client-prog nil (current-buffer) nil
	  "-P" "-" "-d" database
	  "-h" dictem-server "-p" dictem-port
	  query)))

    (beginning-of-buffer)
    (cond ((= 0 exit_status)
	   (let ((dictem-current-dbname database))
	     (run-hooks 'dictem-postprocess-define-hook)))
	  ((= 21 exit_status)
	   (run-hooks 'dictem-postprocess-match-hook))
	  )))

(defun dictem-match-base (database query strategy)
  "dictem search: MATCH"
  (interactive)

  (let ((exit_status
	 (call-process
	  dictem-client-prog nil (current-buffer) nil
	  "-P" "-" "-d" database "-s" strategy
	  "-h" dictem-server "-p" dictem-port "-m"
	  query)))

    (beginning-of-buffer)
    (cond ((= 0 exit_status)
	   (run-hooks 'dictem-postprocess-match-hook))
	  )))

(defun dictem-dbinfo-base (database b c)
  "dictem: SHOW INFO command"
  (interactive)

  (let ((exit_status
	 (call-process
	  dictem-client-prog nil (current-buffer) nil
	  "-P" "-" "-i" database
	  "-h" dictem-server "-p" dictem-port
	  )))

    (beginning-of-buffer)
    (cond ((= 0 exit_status)
	   (let ((dictem-current-dbname database))
	     (run-hooks 'dictem-postprocess-dbinfo-hook))
	   ))))

(defun dictem-showserver-base (a b c)
  "dictem: SHOW SERVER command"
  (interactive)

  (let ((exit_status
	 (call-process
	  dictem-client-prog nil (current-buffer) nil
	  "-P" "-" "-I"
	  "-h" dictem-server "-p" dictem-port
	  )))

    (beginning-of-buffer)
    (cond ((= 0 exit_status)
	   (run-hooks 'dictem-postprocess-showserver-hook))
	  )))

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
      (make-local-variable 'dictem-last-strategy)
      (make-local-variable 'dictem-last-database)
      (make-local-variable 'case-replace)
      (make-local-variable 'case-fold-search)
      (setq dictem-last-strategy strategy)
      (setq dictem-last-database database)
      (setq case-replace nil)
      (setq case-fold-search nil)
      (funcall search-fun database query strategy)
      (beginning-of-buffer)
      (setq buffer-read-only t)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictem-help ()
  "Display a dictem help"
  (interactive)
  (describe-function 'dictem-mode))

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
	(selected-window (frame-selected-window)))
    (switch-to-buffer-other-window buffer)
    (dictem-mode)

    (make-local-variable 'dictem-window-configuration)
    (make-local-variable 'dictem-selected-window)
    (setq dictem-window-configuration window-configuration)
    (setq dictem-selected-window selected-window)
    ))

;(unless dictem-mode-map
(setq dictem-mode-map (make-sparse-keymap))
(suppress-keymap dictem-mode-map)

(define-key dictem-mode-map "q"
  'dictem-close)

(define-key dictem-mode-map "h"
  'dictem-help)

; SEARCH = MATCH + DEFINE
(define-key dictem-mode-map "s"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-search-base
     (dictem-select-database)
     (dictem-read-query)
     (dictem-select-strategy))))

; MATCH
(define-key dictem-mode-map "m"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-match-base
     (dictem-select-database)
     (dictem-read-query)
     (dictem-select-strategy))))

; DEFINE
(define-key dictem-mode-map "d"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     (dictem-select-database)
     (dictem-read-query)
     nil)))

; SHOW SERVER
(define-key dictem-mode-map "i"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-dbinfo-base
     (dictem-select-database)
     nil
     nil)))

; SHOW INFO
(define-key dictem-mode-map "r"
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-showserver-base
     nil
     nil
     nil)))

; DEFINE for the selected region
(define-key dictem-mode-map " "
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     "*"
     (thing-at-point 'word)
     nil)))

; DEFINE for the selected region
(define-key dictem-mode-map [C-SPC]
  '(lambda ()
    (interactive)
    (dictem-run
     'dictem-define-base
     (dictem-select-database dictem-last-database)
     (thing-at-point 'word)
     nil)))

;  (link-initialize-keymap dictem-mode-map)

(defun dictem-mode-p ()
  "Return non-nil if current buffer has dictem-mode"
  (eq major-mode 'dictem-mode))

(defun dictem-ensure-buffer ()
  "If current buffer is not a dictem buffer, create a new one."
  (unless (dictem-mode-p)
    (dictem)))

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
		(set-window-configuration configuration)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     Top-level Functions     ;;;;;;

(defun dictem-run-match ()
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows matches in it."
  (interactive)
  (dictem-run
   'dictem-match-base
   (dictem-select-database)
   (dictem-read-query (thing-at-point 'word))
   (dictem-select-strategy)))

(defun dictem-run-define ()
  "Asks a user about database name and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive)
  (dictem-run
   'dictem-define-base
   (dictem-select-database)
   (dictem-read-query (thing-at-point 'word))
   nil))

(defun dictem-run-search ()
  "Asks a user about database name, search strategy and query,
creates new *dictem* buffer and
shows definitions in it."
  (interactive)
  (dictem-run
   'dictem-search-base
   (dictem-select-database)
   (dictem-read-query (thing-at-point 'word))
   (dictem-select-strategy)))

(defun dictem-run-dbinfo ()
  "Asks a user about database name
creates new *dictem* buffer and
shows information about it."
  (interactive)
  (dictem-run
   'dictem-dbinfo-base
   (dictem-select-database)))

(defun dictem-run-showserver ()
  "Creates new *dictem* buffer and
show information about DICT server in it."
  (interactive)
  (dictem-run
   'dictem-showserver-base))

(provide 'dictem)

(defvar edict-server "localhost")
(defvar edict-port "2628")
(defvar edict-client-prog "dict")

(defvar edict-strategy-list
  '(
    ("exact"     nil)
    ("prefix"    nil)
    ("substring" nil)
    ("suffix"  nil)
    ("re"      nil)
    ("word"    nil)
    ("lev"     nil)
    ("soundex" nil)
    ("regexp"  nil)
  )
)

(defvar edict-database-list
  '(
    ( "gcide" nil )
    ( "wn" nil )
    ( "foldoc" nil )
    ( "jargon" nil )
    ( "vera" nil )
    ( "devil" nil )
    ( "elements" nil )
    ( "easton" nil )
    ( "hitchcock" nil )
    ( "gaz" nil )
    ( "hi127" nil )
    ( "church" nil )
    ( "ahiezer" nil )
    ( "susv2" nil )
    ( "1000pbio" nil )
    ( "vdal" nil )
    ( "religion" nil )
    ( "biology" nil )
    ( "idioms" nil )
    ( "mech" nil )
    ( "ethnographic" nil )
    ( "ozhegov" nil )
    ( "teo" nil )
    ( "brok_and_efr" nil )
    ( "aviation" nil )
    ( "mueller24" nil )
    ( "sc-abbr" nil )
    ( "sinyagin_computer" nil )
    ( "sinyagin_general_re" nil )
    ( "sinyagin_general_er" nil )
    ( "sinyagin_alexeymavrin" nil )
    ( "sinyagin_business" nil )
    ( "sinyagin_abbrev" nil )
    ( "beslov" nil )
    ( "slovnyk_ru-en" nil )
    ( "sokrat_ru-en" nil )
    ( "sokrat_en-ru" nil )
    ( "korolew_ru-en" nil )
    ( "korolew_en-ru" nil )
    ( "geology_ru-en" nil )
    ( "geology_en-ru" nil )
    ( "ushakov" nil )
    ( "business" nil )
    ( "computer" nil )
    ( "eng-rus" nil )
  )
)

(defvar edict-default-strategy ".")
(defvar edict-default-database "*")
(defvar edict-strategy-history nil)
(defvar edict-database-history nil)
(defvar edict-query-history nil)

(defun edict-select (prompt alist default history)
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

(defun edict-select-strategy ()
  "Switches to minibuffer and ask user
to enter a search strategy"
  (interactive)

  (edict-select
   "strategy"
   edict-strategy-list
   (if edict-strategy-history
       (car edict-strategy-history)
     "exact"
     )
   'edict-strategy-history
   )
  )

(defun edict-select-database ()
  "Switches to minibuffer and ask user
to enter a database to be searched in"
  (interactive)

  (edict-select
   "db"
   edict-database-list
   (if edict-database-history
       (car edict-database-history)
     "*"
     )
   'edict-database-history
   )
  )

(defun edict-read-query ()
  "Switches to minibuffer and ask user
to enter a query be searched"
  (interactive)

  (read-string "query: " nil 'edict-query-history "" t)
)

(defcustom edict-mode-hook
  nil
  "Hook run in edict mode buffers.")

(defun edict-mode ()
  "This is a mode for dict client implementing
the protocol defined in RFC 2229.
"
  (interactive)

;  (unless (eq major-mode 'edict-mode)
;    (incf edict-instances))

  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map edict-mode-map)
  (setq major-mode 'edict-mode)
  (setq mode-name "EDict")

;  (make-local-variable 'edict-data-stack)
;  (setq edict-data-stack nil)
;  (make-local-variable 'edict-position-stack)
;  (setq edict-position-stack nil)

;  (make-local-variable 'edict-current-data)
;  (make-local-variable 'edict-positions)

;  (make-local-variable 'edict-default-edict)
;  (make-local-variable 'edict-default-strategy)
;  (toggle-read-only t)

  (add-hook 'kill-buffer-hook 'edict-close t t)
  (run-hooks 'edict-mode-hook)
  )

(defvar edict-window-configuration
  nil
  "The window configuration to be restored upon closing the buffer")

(defvar edict-selected-window
  nil
  "The currently selected window")

(defun edict ()
  "Create a new edict buffer and install edict-mode"
  (interactive)

  (let (
	(buffer (generate-new-buffer "*Edict buffer*"))
	(window-configuration (current-window-configuration))
	(selected-window (frame-selected-window))
	)
    (switch-to-buffer-other-window buffer)
    (edict-mode)

    (make-local-variable 'edict-window-configuration)
    (make-local-variable 'edict-selected-window)
    (setq edict-window-configuration window-configuration)
    (setq edict-selected-window selected-window)
    )
;    (setq edict-window-configuration
;	  (list window-configuration edict-window-configuration))
;    (setq edict-selected-window
;	  (list selected-window edict-selected-window))
  )

(defvar edict-mode-map
  nil
  "Keymap for edict mode")

(unless edict-mode-map
  (setq edict-mode-map (make-sparse-keymap))
  (suppress-keymap edict-mode-map)

  (define-key edict-mode-map "q"
    'edict-close)

  (define-key edict-mode-map "h"
    'edict-help)

  ; SEARCH = MATCH + DEFINE
  (define-key edict-mode-map "s"
    '(lambda ()
       (interactive)
       (edict-search
	(edict-select-database)
	(edict-select-strategy)
	(edict-read-query)
	'edict-search-base
	)
       )
    )

  ; MATCH
  (define-key edict-mode-map "m"
    '(lambda ()
       (interactive)
       (edict-search
	(edict-select-database)
	(edict-select-strategy)
	(edict-read-query)
	'edict-match-base
	)
       )
    )

  ; DEFINE
  (define-key edict-mode-map "d"
    '(lambda ()
       (interactive)
       (edict-search
	(edict-select-database)
	nil
	(edict-read-query)
	'edict-define-base
	)
       )
    )

  ; DEFINE for the selected region
  (define-key edict-mode-map " "
    '(lambda ()
       (interactive)
       (edict-search
	(edict-last-database)
	nil
	(thing-at-point 'word)
	'edict-define-base
	)
       )
    )
  )

;  (link-initialize-keymap edict-mode-map)

(defun edict-mode-p ()
  "Return non-nil if current buffer has edict-mode"
  (eq major-mode 'edict-mode))

(defun edict-ensure-buffer ()
  "If current buffer is not a edict buffer, create a new one."
  (unless (edict-mode-p)
    (edict)
  )
)

(defun edict-close ()
  "Close the current edict buffer"
  (interactive)
  (if (eq major-mode 'edict-mode)
      (progn
	(setq major-mode nil)
	(let ((configuration edict-window-configuration)
	      (selected-window edict-selected-window))
	  (kill-buffer (current-buffer))
	  (if (window-live-p selected-window)
	      (progn
		(select-window selected-window)
		(set-window-configuration configuration)))
;	  (setq edict-selected-window (cdr edict-selected-window))
;	  (setq edict-window-configuration (cdr edict-window-configration))
	  )
	)
    )
  )

(defun edict-search-base (database query strategy)
  "Edict search: MATCH + DEFINE"
  (interactive)

  (call-process
   edict-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database "-s" strategy
   "-h" edict-server "-p" edict-port
   query
   )
)

(defun edict-define-base (database query strategy)
  "Edict search: DEFINE"
  (interactive)

  (call-process
   edict-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database
   "-h" edict-server "-p" edict-port
   query
   )
)

(defun edict-match-base (database query strategy)
  "Edict search: MATCH"
  (interactive)

  (call-process
   edict-client-prog nil (current-buffer) nil
   "-P" "-" "-d" database "-s" strategy
   "-h" edict-server "-p" edict-port "-m"
   query
   )
)

; search type may be "", 'edict-define or 'edict-match
(defun edict-search (database strategy query search-fun)
  "Creates new *Edict* buffer and fun search-fun"
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
      (edict)
      (funcall search-fun database query strategy)
      (beginning-of-buffer)
      )
    )
  )

(defun edict-last-database ()
  "Returns last requested database name or nil"
  (interactive)
  (if
      edict-database-history
      (car edict-database-history)
    nil)
)

(defun edict-last-strategy ()
  "Returns last requested strategy name or nil"
  (interactive)
  (if
      edict-strategy-history
      (car edict-strategy-history)
    nil)
)

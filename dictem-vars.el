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

(defcustom dictem-default-strategy "."
  "The default search strategy."
  :group 'dictem
  :group 'string)

(defcustom dictem-default-database "*"
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
  "If t, only user's dictionaries from dictem-user-databases-alist
will be used by dictem-select-database"
  :group 'dictem
  :type 'boolean)

(defcustom dictem-mode-hook
  nil
  "Hook run in dictem mode buffers."
  :group 'dictem
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;           Variables          ;;;;;

(defvar dictem-version
  "0.1"
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

(provide 'dictem-vars)

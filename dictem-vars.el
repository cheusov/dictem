;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Custom Things        ;;;;;

(defgroup dictem nil
  "Client for accessing the DICT server."
  :group 'help
  :group 'hypermedia)

(defcustom dictem-server "dict.org"
  "The DICT server"
  :group 'dictem
  :type 'string)

(defcustom dictem-port "2628"
  "The port of the DICT server"
  :group 'dictem
  :type 'string)

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

(defcustom dictem-mode-hook
  nil
  "Hook run in dictem mode buffers."
  :group 'dictem
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;           Variables          ;;;;;

(defvar dictem-strategy-list
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

(defvar dictem-database-list
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
  "Check this variable int functions from dictem-color-define-hook.
You should not change this variable.")

(provide 'dictem-vars)

(require 'dictem-vars)
(require 'dictem-ll)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions Related to Error Object  ;;

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
;       (buffer-substring-no-properties
;	(progn (beginning-of-buffer) (point))
;	(progn (end-of-buffer) (point)))
       (beginning-of-buffer)
       (get-line)
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
  (defvar dictem-temp nil)
  (beginning-of-buffer)
  (loop
   (let ((line (get-line)))
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
   ))

;;;;;        GET Functions         ;;;;;

(defun dictem-get-matches (query &optional database strategy server port)
  "Returns ALIST of matches"
  (let ((exit_status
	 (call-process
	  dictem-client-prog nil
	  dictem-temp-buffer-name nil
	  "-P" "-" "-m"
	  "-d" (if database database "*")
	  "-s" (if strategy strategy dictem-default-strategy)
	  "-h" (if server server dictem-server)
	  "-p" (if port port dictem-port)
	  query)))
    (cond
     ((= exit_status 20) ;20 means "no matches found", See dict(1)
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
	     dictem-temp-buffer-name nil
	     "-P" "-" "-S"
	     "-h" (if server server dictem-server)
	     "-p" (if port port dictem-port))))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(beginning-of-buffer)
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
	  dictem-temp-buffer-name nil
	  "-P" "-" "-D"
	  "-h" (if server server dictem-server)
	  "-p" (if port port dictem-port))))
    (cond
     ((= exit_status 0)
      (save-excursion
	(set-buffer dictem-temp-buffer-name)
	(beginning-of-buffer)
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

(provide 'dictem-prog)

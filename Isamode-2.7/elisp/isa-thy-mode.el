;;; isa-thy-mode.el - Mode for Isabelle theory files.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-thy-mode.el,v 2.6 1997/05/27 22:27:00 da Exp
;;;


(require 'isa-load)
(require 'isa-menus)


;;; ========== Theory File Mode User Options ==========

(defvar isa-thy-heading-indent 0
  "*Indentation for section headings.")

(defvar isa-thy-indent-level 2
  "*Indentation level for Isabelle theory files.")

(defvar isa-thy-indent-strings t
  "*If non-nil, indent inside strings.
You may wish to disable indenting inside strings if your logic uses
any of the usual bracket characters in unusual ways.")

(defvar isa-thy-use-sml-mode isa-use-sml-mode
  "*If non-nil, invoke sml-mode inside \"ML\" section of theory files.")



;;; ========== Code ==========

(defvar isa-thy-mode-map nil)

(defvar isa-thy-mode-syntax-table nil)		; Shared below.

(if isa-thy-mode-syntax-table	
    nil		
  ;; This is like sml-mode, except:
  ;;   .   is a word constituent (not punctuation).  (bad for comments?)
  ;;   "   is a paired delimiter
  (setq isa-thy-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()1 " isa-thy-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4 " isa-thy-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\   " isa-thy-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" isa-thy-mode-syntax-table)
  (modify-syntax-entry ?_  "w   " isa-thy-mode-syntax-table)
  (modify-syntax-entry ?\' "w   " isa-thy-mode-syntax-table)
;  it's annoying to match with quotes from previous strings,
;  so this has been removed. 
;  (modify-syntax-entry ?\" "$   " isa-thy-mode-syntax-table)
  (modify-syntax-entry ?.  "w   " isa-thy-mode-syntax-table))

(or isa-thy-mode-map
    (let ((map (make-sparse-keymap)))
      (isa-define-key map '(control up) 'isa-thy-goto-previous-section)
      (isa-define-key map '(control down) 'isa-thy-goto-next-section)
      (define-key map "\C-c\C-n" 'isa-thy-goto-next-section)
      (define-key map "\C-c\C-p" 'isa-thy-goto-previous-section)
      (define-key map "\C-c\C-c" 'isa-thy-minor-sml-mode)
      (define-key map "\C-c\C-t" 'isa-thy-insert-template)
      (define-key map "\C-c\C-u" 'isa-thy-use-file)
      (define-key map "\C-c\C-l" 'isa-thy-raise-windows)
      (define-key map "\C-c\C-o" 'isa-thy-find-other-file)
      (define-key map "\C-M" 'newline-and-indent)
      (define-key map "\C-k" 'isa-thy-kill-line)
      (setq isa-thy-mode-map map)))

(defun isa-thy-mode (&optional nomessage)
  "Major mode for editing Isabelle theory files.
\\<isa-thy-mode-map>
\\[isa-thy-goto-next-section]\t Skips to the next section.
\\[isa-thy-goto-previous-section]\t Skips to the previous section.

\\[indent-for-tab-command]\t Indents the current line.

\\[isa-thy-insert-template]\t Inserts a template for the file or current section.

If isa-thy-use-sml-mode is non-nil, \\<isa-thy-mode-map>\\[isa-thy-minor-sml-mode] \
invokes sml-mode as a minor mode 
in the ML section.  This is done automatically by \
\\[indent-for-tab-command].

The style of indentation for theory files is controlled by these variables:
  isa-thy-heading-indent 
  isa-thy-indent-level
  isa-thy-indent-strings
- see individual variable documentation for details."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'isa-thy-mode)
  (setq mode-name "Theory")
  (use-local-map isa-thy-mode-map)
  (isa-menus)				                ; Add "isabelle" menu.
  (set-syntax-table isa-thy-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'isa-thy-indent-line)
  (make-local-variable 'comment-start)			; Following lines as in sml-mode
  (setq comment-start "(* ")				; .
  (make-local-variable 'comment-end)			; .
  (setq comment-end " *)")				; .
  (setq comment-start-skip "(\\*+[ \t]?")		; .
  (setq font-lock-keywords
	isa-thy-mode-font-lock-keywords)
  (run-hooks 'isa-thy-mode-hook)
  (force-mode-line-update)
  (if (null nomessage)
      (message
       (substitute-command-keys 
	"Isabelle theory-file mode.   Use \\[isa-thy-insert-template] to insert templates; \\[describe-mode] for help.")))
  )

(defun isa-thy-mode-quiet ()
  (interactive)
  (isa-thy-mode t))


;;; "use" and "use_thy" with theory files ========================

;;; NB: this is a mess at the moment because of the theory file
;;; naming conventions.  Really we need to parse the theory/ML
;;; file - yuk!!
;;; The next version of Isabelle will be more consistent.

(defun isa-thy-use-file (&optional force-use_thy)
  "Send the file of the current buffer to an Isabelle buffer with use_thy or use."
  (interactive "P")
  (let ((fname (buffer-file-name)))
    (if fname
	(isa-query-save (current-buffer))
      (setq fname
	    (or (buffer-file-name)
		(read-file-name "Use file: " nil nil t))))
    (let*
	((has-thy-extn   (string-match "\\.thy$" fname))   ; o/w assume ML.
	 (tname          (if has-thy-extn 
			     (substring fname 0 -4); cos use_thy is daft!
			   fname))
	 (use            (if (or has-thy-extn force-use_thy)
			     "use_thy"
			   "use"))
	 (use-thy-string (concat use " \"" tname "\";"))
	 (logic (isa-guess-root)))
      (isa-thy-send-string logic use-thy-string))))

(defun isa-thy-use-region (beg end)
  "Send the region to an Isabelle buffer, with use"
  (interactive "r")
  (write-region beg end isa-thy-use-tempname nil 'nomessage)
  (let* ((use-thy-string (concat "use \"" isa-thy-use-tempname "\";"))
	 (logic (isa-guess-root)))
    (isa-thy-send-string logic use-thy-string)))

(defun isa-thy-copy-region (beg end &optional isa-buffer)
  "Copy the region to an Isabelle buffer."
  (interactive "r")
  (let ((text (buffer-substring beg end))
	(logic (isa-guess-root)))
    (save-excursion
      (isa-thy-send-string logic text))))

(defun isa-thy-use-line (&optional isabuffer)
  "Send the current interactive ML line to an Isabelle buffer.
Advance to the next line."
  (interactive)
  (isa-apply-to-interactive-line 'isa-thy-copy-region))

(defun isa-thy-send-string (logic text &optional hide)
  "Send TEXT to a buffer running LOGIC.
If LOGIC is nil, pick the first Isabelle buffer."
  (require 'isa-mode)
  (setq logic nil) ;;; #### HACK! This all needs changing for single-session.
  (let ((cur-frm (selected-frame)))	; Preserve selected frame.
    (if logic				; switch to Isabelle buffer, without
	(isabelle-session logic)	; raising the frame.
					; (NB: this fails if was renamed).
      (set-buffer
       (or (car-safe (isa-find-buffers-in-mode 'isa-mode))
	   (error "Can't find an Isabelle buffer"))))
    (if hide
	(isa-send-string
	 (get-buffer-process (current-buffer))
	 text)
      (isa-insert-ret text))     ; send use command
    (select-frame cur-frm)))

(defun isa-thy-raise-windows ()
  "Raise windows/frames associated with Isabelle session."
  (interactive)
  (isa-select-buffer isa-session-buffer t)
  (let ((raise t))
    (mapcar 'isa-display-if-active   
	    isa-associated-buffers)))


(defun isa-thy-guess-logic-in-use ()
  (if (featurep 'isa-mode)
      (let* ((buf (car-safe (isa-find-buffers-in-mode 'isa-mode)))
	     (log (and buf
		       (save-excursion
			 (set-buffer buf)
			 isa-logic-name))))
	log)
    nil))


(defvar isa-thy-use-tempname ".region.ML"
  "*Name of temporary file to hold region dring isa-thy-use-region.")


(defconst isa-thy-logic-image-regexp
  "[lL][oO][gG][iI][cC] [iI][mM][aA][gG][eE]:[ \t]*\"?\\([^ \t\n\"]+\\)\"?[ \t]*$"
  "Regexp for locating name of logic image file in a .thy or .ML file.")

(defvar isa-logic-parents
  ;; I can't be bothered to write all of them in here,
  ;; and anyway they're ambiguous.  Use "Logic image:"
  ;; instead.  (Or find a way of getting emacs to track
  ;; theory structure...)
  '(("List" . "HOL") ("Prod" . "HOL") ("Nat" . "HOL")
    ("Ord" . "HOL") ("Set" ."HOL") ("Sexp" . "HOL")
    ("Univ" . "HOL") ("WF" . "HOL") ("Sum" . "HOL")
    ("IFOL" . "FOL"))
  "*An alist of parents of theories that live in logic files.")

(defun isa-guess-root ()
  "Guess the root logic of the .thy or .ML file in current buffer.
Choice based on first name found by:
  (i) special text: \"Logic Image: <name>\" toward start of file
 (ii) guess work based on parent in THY = <parent> if a .thy file."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward isa-thy-logic-image-regexp 500 t)
      (buffer-substring (match-beginning 1) (match-end 1)))
     ((and (string-match "\\.thy$" (or (buffer-file-name) ""))
	   (re-search-forward 
	    "\\w+[ \t\n]*=[ \t\n]*\\(\\w+\\)[ \t\n]*\+" 500 t))
      ;; Something looks like a parent theory:
      ;; 		MyThy = HOL + ...
      (let ((child
	     (buffer-substring (match-beginning 1) (match-end 1))))
	    (or (cdr-safe (assoc child isa-logic-parents))
		child))))))

(defun isa-query-save (buffer)
  (and (buffer-modified-p buffer)
       (y-or-n-p (concat "Save file "
			 (buffer-file-name buffer)
			 "? "))
       (save-excursion (set-buffer buffer) (save-buffer))))




;;; Interfacing with sml-mode ========================

;; extending sml-mode. This only works if you visit the theory file 
;; (or start Isabelle mode) first.
;; This is probably fairly close to The Right Thing...

(defun isa-sml-hook ()
  "Hook to customize sml-mode for use with Isabelle."
  (isa-menus)				; Add Isabelle main menu
  ;; NB: these keydefs will affect other sml-mode buffers too!
  (define-key sml-mode-map "\C-c\C-o" 'isa-thy-find-other-file)
  (define-key sml-mode-map "\C-c\C-u" 'isa-thy-use-file)
  (define-key sml-mode-map "\C-c\C-r" 'isa-thy-use-region)
  (define-key sml-mode-map "\C-c\C-l" 'isa-thy-use-line)
  (define-key sml-mode-map "\C-c\C-c" 'listener-minor-mode)
  (define-key sml-mode-map "\C-c\C-t" 'isa-thy-insert-id-header))

(add-hook 'sml-mode-hook 'isa-sml-hook)

(defun isa-sml-mode ()
  "Invoke sml-mode after installing Isabelle hook."
  (interactive)
  (sml-mode))

(defvar isa-ml-file-extension ".ML"
  "*File name extension to use for ML files.")

(defun isa-thy-find-other-file ()
  "Find associated .ML or .thy file."
  (interactive)
  (and 
   (buffer-file-name)
   (let ((fname (buffer-file-name)))
     (cond ((string-match "\\.thy$" fname)
	    (find-file-other-window
	     (concat 
	      (substring fname 0 -4) 
	      isa-ml-file-extension)))
	   ((string-match (concat (regexp-quote isa-ml-file-extension) "$")
			  fname)
	    (find-file (concat
			(substring fname 0 (- (length isa-ml-file-extension)))
			".thy")))))))

  

;;; "minor" sml-mode inside theory files ==========

(defvar isa-thy-minor-sml-mode-map nil)

(defun isa-thy-install-sml-mode ()
  (progn
    (require 'sml-mode)
    (setq isa-thy-minor-sml-mode-map (copy-keymap sml-mode-map))
    ;; Bind TAB to what it should be in sml-mode.
    (define-key isa-thy-minor-sml-mode-map "\t" 'indent-for-tab-command)
    (define-key isa-thy-minor-sml-mode-map "\C-c\C-c" 'isa-thy-mode-quiet)
    (define-key isa-thy-minor-sml-mode-map "\C-c\C-t" 'isa-thy-insert-template)))

(defun isa-thy-minor-sml-mode ()
  "Invoke sml-mode as if a minor mode inside a theory file.
This has no effect if isa-thy-use-sml-mode is nil."
  (interactive)
  (if isa-thy-use-sml-mode
      (progn
	(if (not (boundp 'sml-mode)) 
	    (isa-thy-install-sml-mode))
	(kill-all-local-variables)
	(sml-mode)					; Switch to sml-mode
	(setq major-mode 'isa-thy-mode)
	(setq mode-name "Theory Sml")			; looks like it's a minor-mode.
	(use-local-map isa-thy-minor-sml-mode-map)	; special map has \C-c\C-c binding.
	(make-local-variable 'indent-line-function)
	(setq indent-line-function 'isa-thy-do-sml-indent)
	(force-mode-line-update)
	(message "Use C-c C-c to exit SML mode."))))

(defun isa-thy-do-sml-indent ()
  "Run sml-indent-line in an Isabelle theory file, provided inside ML section.
If not, will turn off simulated minor mode and run isa-thy-indent-line."
  (interactive)
  (if (string= (isa-thy-current-section) "ML")		; NB: Assumes that TAB key was 
      (sml-indent-line)					; bound to sml-indent-line.
    (isa-thy-mode t)					; (at least, it is now!).
    (isa-thy-indent-line)))






;;; theory and ML file templates ===================================    

(defconst isa-thy-sections
  ;; NB: preceding white space in templates deleted by indentation alg.
  ;;     top must come first.
  '(("top" .     isa-thy-insert-header)
    ("classes" . isa-thy-insert-class)
    ("default" . isa-thy-insert-default-sort)
    ("types"   . isa-thy-insert-type) 
    ("arities" . isa-thy-insert-arity)
    ;; =================================
    ;; These only make sense for HOL.
    ;; Should be paramaterised on a theory or something?
    ("datatype") ("typedef")
    ("inductive") ("coninductive")
    ("intrs") ("monos")
    ("primrec") ("recdef")
    ("rep_datatype") ("distinct") ("induct")
    ;; ==============================
    ("consts"  . isa-thy-insert-const)
    ("translations" . "\"\"\t==\t\"\"")
    ("axclass")
    ("syntax")
    ("instance")
    ("rules"   . isa-thy-insert-rule)
    ("defs"    . isa-thy-insert-rule)
    ("axioms"    . thy-insert-rule)
    ("use")
    ("theory")
    ("files")
    ("constdefs")
    ("oracle")
    ("local")
    ("locale")
    ("nonterminals")
    ("setup")
    ("global")
    ("end")
    ("ML"))
  "Names of theory file sections and their templates")

(defun isa-thy-insert-name (name)
  "Insert NAME -- as a string if there are non-alphabetic characters in NAME."
  (if (string-match "[a-zA-Z]+" name)
      (insert name)
    (insert "\"" name "\"")))

(defun isa-thy-insert-class (name supers)
  (interactive 
   (list
    (isa-read-item "Class name: ")
    (isa-read-list "Super classes %s: ")))
  (insert name)
  (if supers (insert "\t< " (isa-splice-separator ", " supers)))
  (indent-according-to-mode)
  (forward-line 1))

(defun isa-thy-insert-default-sort (sort)
  (interactive
   (list
    (isa-read-item "Default sort: ")))
  (insert sort)
  (indent-according-to-mode)
  (isa-thy-goto-next-section))

(defun isa-thy-insert-type (name no-of-args)
  (interactive
   (list
    (isa-read-item "Type name: ")
    (isa-read-num  "Number of arguments: ")))
  ;; make type variables for arguments
  (cond 
   ((zerop no-of-args))
   ((= 1 no-of-args)  
    (insert "'a "))
   (t 
    (insert "(")
    (let ((i 0))
      (while (< i no-of-args)
	(if (> i 0) (insert ","))
	(insert "'" (+ ?a i))
	(setq i (1+ i))))
    (insert ") ")))
  (isa-thy-insert-name name)
  (indent-according-to-mode)
  ;; forward line, although use may wish to add infix.
  (forward-line 1))

(defun isa-thy-insert-arity (name param-sorts result-class)
  (interactive
   (list
    (isa-read-item "Type name: ")
    (isa-read-list "Parameter sorts %s: ")
    (isa-read-item "Result class: ")))
  (insert name " :: ")
  (if param-sorts 
      (insert "(" (isa-splice-separator ", " param-sorts) ") "))
  (insert result-class)
  (indent-according-to-mode)
  (forward-line 1))

(defun isa-thy-insert-const (name type)
  ;; only does a single constant, no lists.
  (interactive
   (let* ((thename  (isa-read-item "Constant name: "))
	  (thetype  (isa-read-item (format "Type of `%s' : " thename))))
     (list thename thetype)))
  (isa-thy-insert-name name)
  (insert " ::\t \"" type "\"\t\t")
  (indent-according-to-mode)
  ;; no forward line in case user adds mixfix
  )
  
(defun isa-thy-insert-rule (name)
  (interactive 
   (list
    (isa-read-item "Axiom name: ")))
  (end-of-line 1)
  (insert name "\t\"\"")
  (backward-char)
  (indent-according-to-mode))

(defun isa-thy-insert-template ()
  "Insert a syntax template according to the current section"
  (interactive)
  (isa-thy-check-mode)
  (let* ((sect (isa-thy-current-section))
	 (tmpl (cdr-safe (assoc sect isa-thy-sections))))
    ;; Ensure point is at start of an empty line.
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (if (looking-at sect) 
	(progn 
	  (forward-line 1) 
	  (skip-chars-forward "\t ")))
    (if (looking-at "$")
	nil
      (beginning-of-line)
      (newline)
      (forward-line -1))
    (cond ((stringp tmpl)
	   (insert tmpl)
	   (indent-according-to-mode))
	  ((null tmpl))		; nil is a symbol!
	  ((symbolp tmpl)
	   (call-interactively tmpl)))))


(defvar isa-thy-id-header
  "(* 
    File:	 %f
    Theory Name: %t
    Logic Image: %l
*)\n\n"
  "*Identification header for .thy and .ML files.")

(defconst isa-thy-template
"%t  =  %p +\n
classes\n
default\n
types\n
arities\n
consts\n
translations\n
rules\n
end\n
ML\n")


(defun isa-read-idlist (prompt &optional init)
  "Read a list of identifiers from the minibuffer."
  (let ((items init) item)
    (while (not (string= ""
			 (setq item (read-string 
				       (format prompt (or items ""))))))
      (setq items (nconc items (list item))))
    items))

(defun isa-read-item (prompt &optional init)
  ;; don't allow empty input
  (let ((result ""))
    (while (string= result "")
      (setq result (read-string prompt init)))
    result))

(defun isa-read-num (prompt &optional init)
  (let (result)
    (while (not (natnump result))
      (setq result (read-string prompt init))
      (setq result
	    (if result (string-to-number result) 0)))
    result))

(defun isa-thy-read-thy-name ()
  (let* ((default  (car 
		    (isa-file-name-cons-extension
		     (file-name-nondirectory 
		      (abbreviate-file-name (buffer-file-name)))))))
    default))

;(defun isa-thy-read-thy-name ()
;  (let* ((default  (car 
;		    (isa-file-name-cons-extension
;		     (file-name-nondirectory 
;		      (abbreviate-file-name (buffer-file-name))))))
;	 (name     (read-string 
;		    (format "Name of theory [default %s]: " default))))
;    (if (string= name "") default name)))

(defun isa-thy-read-logic-image ()
  (let*	((defimage (or (isa-thy-guess-logic-in-use)
		       "Pure"))
	 (logic    (read-string
		    (format "Name of logic image to use [default %s]: "
			    defimage))))
    (if (string= logic "") defimage logic)))

(defun isa-thy-insert-header (name logic parents)
  "Insert a theory file header, for LOGIC, theory NAME with PARENTS"
  (interactive 
   (list
    (isa-thy-read-thy-name)
    (isa-thy-read-logic-image)
    (isa-read-list "Parent theory %s: ")))
  (let* ((parentplus (isa-splice-separator 
		      " + " 
		      (or parents (list (or logic "Pure")))))
	 (formalist (list
		       (cons "%t" name)
		       (cons "%p" parentplus))))
    (isa-thy-insert-id-header name logic)
    (insert (isa-format formalist isa-thy-template)))
  (goto-char (point-min))
  (isa-thy-goto-next-section))

(defun isa-thy-insert-id-header (name logic)
  "Insert an identification header, for theory NAME logic image LOGIC."
  (interactive
   (list
    (isa-thy-read-thy-name)
    (isa-thy-read-logic-image)))
  (let* ((formalist (list
		     (cons "%f" (buffer-file-name))
		     (cons "%t" name)
		     (cons "%l" logic))))
    (insert (isa-format formalist isa-thy-id-header))))

(defun isa-thy-check-mode ()
  (if (not (eq major-mode 'isa-thy-mode))
      (error "Not in Theory mode.")))
  

(defconst isa-thy-headings-regexp
  (concat
   "^\\s-*\\("
   (substring (apply 'concat (mapcar 
			      '(lambda (pair)
				 (concat "\\|" (car pair)))
			      (cdr isa-thy-sections))) 2)
   "\\)[ \t]*")
  "Regular expression matching headings in theory files.")

(defvar isa-thy-mode-font-lock-keywords
  (list				
   (list isa-thy-headings-regexp 1
	 'font-lock-keyword-face))
  "Font lock keywords for isa-thy-mode.
Default set automatically from isa-thy-headings-regexp.")

;;; movement between sections ===================================    

(defun isa-thy-goto-next-section (&optional count noerror)
  "Goto the next (or COUNT'th next) section of a theory file.
Negative value for count means previous sections.
If NOERROR is non-nil, failed search will not be signalled."
  (interactive "p")
  (condition-case nil
      ;; string matching would probably be good enough
      (cond ((and count (< count 0))
	     (let ((oldp (point)))
	       (beginning-of-line)
	       (isa-thy-goto-top-of-section)
	       ;; not quite right here - should go to top
	       ;; of file, like top of section does.
	       (if (equal (point) oldp)
		   (progn
		     (re-search-backward isa-thy-headings-regexp 
					 nil nil (1+ (- count)))
		     (forward-line 1))))
	     t)
	    (t
	     (re-search-forward isa-thy-headings-regexp nil nil count)
	     (forward-line 1)
	     t))
    ;; could just move to top or bottom if this happens, instead
    ;; of giving this error.
    (search-failed (if noerror nil
		     (error "No more headings")))))

(defun isa-thy-goto-previous-section (&optional count noerror)
  "Goto the previous section (or COUNT'th previous) of a theory file.
Negative value for count means following sections.
If NOERROR is non-nil, failed search will not be signalled."
  (interactive)
  (isa-thy-goto-next-section (if count (- count) -1) noerror))

(defun isa-thy-goto-top-of-section ()
  "Goto the top of the current section"
  (interactive)
  (if (re-search-backward isa-thy-headings-regexp nil t)
      (forward-line 1)
    (goto-char (point-min))))

(defun isa-thy-current-section ()
  "Return the current section of the theory file, as a string.
\"top\" indicates no section."
  (save-excursion
    (let* ((gotsect (re-search-backward isa-thy-headings-regexp nil t))
	   (start   (if gotsect
			(progn
			  (skip-chars-forward " \t")
			  (point)))))
      (if (not start)
	  "top"
	(skip-chars-forward "a-zA-z")
	(buffer-substring start (point))))))



;;; kill line ==================================================

(defun isa-thy-kill-line (&optional arg)
  "As kill-line, except in a string will kill continuation backslashes also.
Coalesces multiple lined strings by leaving single spaces."
  (interactive "P")
  (let ((str           (isa-thy-string-start))
	(kill-start    (point))
	following-slash)
    (if (not str)
	;; Usual kill line if not inside a string.
	(kill-line arg)
      (if arg
	  (forward-line (prefix-numeric-value arg))
	(if (eobp)
	    (signal 'end-of-buffer nil)))
      (setq kill-start (point))
      (if (isa-thy-string-start str)		; if still inside a string
	  (cond 
	   ((looking-at "[ \t]*$")	; at end of line bar whitespace
	    (skip-chars-backward 
	     " \t"
	     (save-excursion (beginning-of-line) (1+ (point))))
	    (backward-char)
	    (if (looking-at "\\\\")	; preceding backslash
		(progn
		  (skip-chars-backward " \t")
		  (setq following-slash t)
		  (setq kill-start (min (point) kill-start)))
	      (goto-char kill-start))
	    (forward-line 1))
	   ((looking-at "[ \t]*\\\\[ \t]*$") ; before final backslash
	    (setq following-slash t)
	    (forward-line 1))
	   ((looking-at "\\\\[ \t]*\\\\[ \t]*$") ; an empty line!
	    (forward-line 1))
	   ((looking-at ".*\\(\\\\\\)[ \t]*$")	; want to leave backslash
	    (goto-char (match-beginning 1)))
	   ((and kill-whole-line (bolp))
	    (forward-line 1))
	   (t
	    (end-of-line))))
      (if (and following-slash 
	       (looking-at "[ \t]*\\\\"))	; delete following slash if
	  (goto-char (1+ (match-end 0)))) ; there's one
      (kill-region kill-start (point))	; do kill
      (if following-slash
	  ;; did do just-one-space, but it's not nice to delete backwards
	  ;; too
	  (delete-region (point)
			 (save-excursion
			   (skip-chars-forward " \t")
			   (point)))))))


;;; INDENTATION ==================================================
  
;;; Could do with isa-thy-correct-string function,
;;; which does roughly the same as indent-region.
;;; Then we could have an electric " that did this!

;;; Could perhaps have used fill-prefix to deal with backslash
;;; indenting, rather than lengthy code below?

(defun isa-thy-indent-line ()
  "Indent the current line in an Isabelle theory file.
If in the ML section, this switches into a simulated minor sml-mode."
  (let ((sect (isa-thy-current-section)))
    (cond 
     ((and isa-thy-use-sml-mode (string= sect "ML"))
      (progn				               ; In "ML" section,
	(isa-thy-minor-sml-mode)	               ; switch to sml mode.
	(sml-indent-line)))

     (t   (let ((indent   (isa-thy-calculate-indentation sect)))
	    (save-excursion
	      (beginning-of-line)
	      (let ((beg (point)))
		(skip-chars-forward "\t ")
		(delete-region beg (point)))
	      (indent-to indent))
	    (if (< (current-column) 
		   (current-indentation))	
		(skip-chars-forward "\t ")))))))

;; Better Emacs major modes achieve a kind of "transparency" to
;; the user, where special indentation,etc. happens under your feet, but
;; in a useful way that you soon get accustomed to.  Worse modes
;; cause frustration and repetitive re-editing of automatic indentation.
;; I hope I've achieved something in the first category...

(defun isa-thy-calculate-indentation (sect)
  "Calculate the indentation for the current line.
SECT should be the string name of the current section."
  (save-excursion
    (beginning-of-line)
    (or (isa-thy-long-comment-string-indentation)
	(if (looking-at "\\s-*$")
	    ;; Empty lines use indentation for section.
	    (isa-thy-indentation-for sect)
	  (if (looking-at isa-thy-headings-regexp)
	      isa-thy-heading-indent
	    (progn
	      (skip-chars-forward "\t ")
	      (cond
	       ;; A comment?
	       ((looking-at "(\\*")         
		(isa-thy-indentation-for sect))
	       ;; Anything else, use indentation for section
	       (t (isa-thy-indentation-for sect)))))))))

(defun isa-thy-long-comment-string-indentation ()
  "Calculate the indentation if inside multi-line comment or string.
Also indent string contents."
  (let* ((comstr (isa-thy-comment-or-string-start))
	 (bolpos (save-excursion
		   (beginning-of-line)
		   (point)))
	 (multi  (and comstr 
		      (< comstr bolpos))))
    (if (not multi)
	nil		
      (save-excursion
	(beginning-of-line)
	(cond

	 ;; Inside a comment?
	 ((char-equal (char-after comstr) ?\( )
	  (forward-line -1)
	  (if (looking-at "[\t ]*(\\*")
	      (+ 3 (current-indentation))
	    (current-indentation)))
	 
	 ;; Otherwise, a string.
	 ;; Enforce correct backslashing on continuing
	 ;; line and return cons of backslash indentation
	 ;; and string contents indentation for continued
	 ;; line.
	 (t
	  (let ((quote-col (save-excursion (goto-char comstr) 
					   (current-column))))
	     (if isa-thy-indent-strings
		(isa-thy-string-indentation comstr)
	      ;; just to right of matching " 
	      (+ quote-col 1)))))))))

(defun isa-thy-string-indentation (start)
  ;; Guess indentation for text inside a string
  (let* ((startcol  (save-excursion (goto-char start) (current-column)))
	 (pps-state (parse-partial-sexp (1+ start) (point)))
	 (par-depth (car pps-state)))
	 (cond (;; If not in nested expression, startcol+1
		(zerop par-depth)
		(1+ startcol))
	       (;; If in a nested expression, use position of opening bracket
		(natnump par-depth)
		(save-excursion
		  (goto-char (nth 1 pps-state))
		  (+ (current-column)
		     (cond ((looking-at "\\[|") 3)
			   (t 1)))))
	       (;; Give error for too many closing parens
		t
		(error "Mismatched parentheses")))))

(defun isa-thy-indentation-for (sect)
  "Return the indentation for section SECT"
  (if (string-equal sect "top")
      isa-thy-heading-indent
    isa-thy-indent-level))

(defun isa-thy-string-start (&optional min)
  "Return position of start of string if inside one, nil otherwise."
  (let ((comstr (isa-thy-comment-or-string-start)))
    (if (and comstr
	     (save-excursion
	       (goto-char comstr)
	       (looking-at "\"")))
	comstr)))

;;; Is this parsing still too slow?  (better way? e.g., try setting
;;; variable "char" and examining it, rather than finding current
;;; state first - fewer branches in non-interesting cases, perhaps.
;;; NB: it won't understand escape sequences in strings, such as \"

(defun isa-thy-comment-or-string-start (&optional min)
  "Find if point is in a comment or string, starting parse from MIN.
Returns the position of the comment or string start or nil.
If MIN is nil, starts from top of current section.

Doesn't understand nested comments."
  (or min
      (setq min
	    (save-excursion
	      (isa-thy-goto-top-of-section) (point))))
  (if (<= (point) min)
      nil
    (let ((pos   (point))
	  (incomdepth 0)
	  incom instring)  ; char
      (goto-char min)
      (while (< (point) pos)
	;; When inside a string, only look for its end
	(if instring
	    (if (eq (char-after (point)) ?\") ; looking-at "\""
		(setq instring nil))
	  ;; If inside a comment, look for a comment end
	  (if (> 0 incomdepth)	
	      (if (and			; looking-at "\\*)"
		   (eq (char-after (point)) ?\*)
		   (eq (char-after (1+ (point))) ?\)))
		  (setq incomdepth (1- incomdepth)))
	    ;; If inside neither comment nor string, look for 
	    ;; a string start.
	    (if (eq (char-after (point)) ?\") ; looking-at "\""
		(setq instring (point))))
	  ;; Look for a comment start (unless inside a string)
	  (if (and
	       (eq (char-after (point)) ?\()
	       (eq (char-after (1+ (point))) ?\*))
	      (progn
		(if (= 0 incomdepth)	; record start of main comment
		    (setq incom (point))) ; only
		(setq incomdepth (1+ incomdepth)))))
	(forward-char))
      (if (> 0 incomdepth) incom instring))))




(provide 'isa-thy-mode)

;;; end of isa-thy-mode.el

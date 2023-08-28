;;; isa-ruletable.el 
;;;       - Buffer holding table of rules and tactics for Isabelle mode.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-ruletable.el,v 2.6 1997/05/27 22:26:59 da Exp
;;;

;;; DESIRED CHANGES:
;;;   - improve pretty margin setting bits.
;;;   - improve table generation stuff to remove spurious blank lines.
;;;   - sort out display mechanism and moving point to correct place
;;;     on startup.
;;;   - improve cursor movement (or lose it altogether) so that
;;;     multiple word group names are possible.
;;;   - tidy

(require 'isa-mode)

;;; ============ Ruletable Mode ============

; This may be dropped in favour of font-lock-keywords, which
; is much less fuss, really.
(isa-make-face 'ruletableGroupname)

(defvar ruletable-mode-font-lock-keywords
  '("^\\S-+$"))

(defvar ruletable-theory-name nil
  "Name of theory displayed in ruletable")

(defvar ruletable-mode-map nil)

(or ruletable-mode-map
    (let ((map (make-keymap)))
      (suppress-keymap map)
      (isa-clear-mouse-bindings map)
      ;; This defines the action on a mouse button down event, if
      ;; such a distinction is made.  (Presently, yes in FSF, no in Lucid).
      (isa-define-popup-key map 'button1 
			    'ruletable-select-indicated-rule)
      (isa-define-popup-key map 'button2 
			    'ruletable-default-tactic-indicated-rule)
      (isa-define-popup-key map '(shift button2) 
			    'ruletable-rtac-indicated-rule)
      (isa-define-popup-key map '(control button2) 
			    'ruletable-etac-indicated-rule)
      (isa-define-popup-key map '(meta button2) 
			    'ruletable-dtac-indicated-rule)
      (isa-define-popup-key map 'button3 'ruletable-show-indicated-rule)
      (define-key map " " 'ruletable-select-pointed-rule)
      (define-key map "h" 'describe-mode)
      (define-key map "i" 'isa-select-isa-buffer)
      (define-key map "q" 'isa-remove-temp-buffer)
      (define-key map "\C-f"   'ruletable-forward-word)
      (define-key map "\C-b"   'ruletable-backward-word)
      (define-key map "\C-p"   'ruletable-up-word)
      (define-key map "\C-n"   'ruletable-down-word)
      (isa-define-key map '(right) 'ruletable-forward-word)
      (isa-define-key map '(left)  'ruletable-backward-word)
      (isa-define-key map '(up)    'ruletable-up-word)
      (isa-define-key map '(down)  'ruletable-down-word)
      (define-key map "\C-i" 'ruletable-toggle-long)
      (define-key map "\C-m" 'ruletable-show-pointed-rule)
      (setq ruletable-mode-map map)))

(defvar ruletable-rule-regexp "[ \t]+\\(\\S-+\\)")

(defun ruletable-forward-word ()
  "Move point to next identifier in table."
  (interactive)
  (if (re-search-forward ruletable-rule-regexp nil t)
      (goto-char (match-beginning 1))))

(defun ruletable-backward-word ()
  "Move point to previous identifier in table."
  (interactive)
  (if (re-search-backward ruletable-rule-regexp nil t)
      (goto-char (match-beginning 1))))

(defun ruletable-up-word ()
  "Move point to identifier at start of previous line in table."
  (interactive)
  (forward-line -1)
  (end-of-line)
  ;; without space below, regexp fails in Lucid.
  (if (re-search-backward (concat "^" ruletable-rule-regexp) nil t)
      (goto-char (match-beginning 1))
    (ruletable-forward-word)))

(defun ruletable-down-word ()
  "Move point to identifier at start of next line in table."
  (interactive)
  ;; without space below, regexp fails in Lucid.
  (if (re-search-forward (concat "\n" ruletable-rule-regexp) nil t)
      (goto-char (match-beginning 1))))

(defun ruletable-mode ()
  "Major mode for Isabelle rule-table buffers.
The cursor keys or mouse are used to move over the rule
names.
\\<ruletable-mode-map>
\\[ruletable-select-pointed-rule] or \\[ruletable-select-indicated-rule] copy the rule name under the 
cursor or mouse respectively into the related Isabelle buffer.
\\[ruletable-show-pointed-rule] or \\[ruletable-show-indicated-rule] display the rule using prth(rulename) 
in the related Isabelle buffer.

\\[ruletable-toggle-long] toggles the display format between short and
long forms: the long form includes section headings and subgroupings,
but takes up more space.

\\{ruletable-mode-map}
"
  ;; NB: this mode doesn't do kill-all-local-variables, since
  ;; we rely on isa-buffer variable being set!
  (setq buffer-read-only t)
  (setq major-mode 'ruletable-mode)
  (set-syntax-table isa-thy-mode-syntax-table)
  (setq mode-line-buffer-identification '("%17b h=help"))
  (make-local-variable 'ruletable-theory-name)
  (put 'ruletable-mode 'mode-class 'special)
  (use-local-map ruletable-mode-map)
  (isa-remove-menubar-if-multiple-frame-mode)
  (isa-set-mouse-follower 'ruletable-follow-mouse)
  (setq font-lock-keywords ruletable-mode-font-lock-keywords)
  (funcall 'isa-font-lock-set-defaults)	; run font-lock hook 
  (run-hooks 'ruletable-mode-hook))


(defun ruletable (&optional theory rehash)
  "Display a table of rules for THEORY, or current logic.
The table is constructed from the variable isa-theory-rules, or
loaded from a file THEORY.rules.   If prefix arg is given,
isa-theory-rules is reset."
  (interactive
   (list
    (if (eq major-mode 'isa-mode)
	(progn
	  ;; Must do rehash before reading theory name!
	  (if current-prefix-arg (isa-get-rules-files))
	  (completing-read "Name of theory: " isa-theory-rules nil t))
      (error "Must be in Isabelle buffer"))
    ;; Rehash always nil when calling interactively, then.
    nil))
  (if rehash (isa-get-rules-files))
  (ruletable-display-table (or theory isa-logic-name) t t))


(defvar ruletable-rules nil
  "Rule table displayed in this rule table buffer.")
(make-variable-buffer-local 'ruletable-rules)
(put 'ruletable-rules 'permanent-local t)

(defun ruletable-display-table (thy &optional noerror switchto)
  "Display a ruletable for theory THY.
If NOERROR is non-nil, no action if a ruletable cannot be found.
If SWITCHTO is non-nil, will switch to an existing buffer rather
than create a new one."
  (let* ((rules   (isa-theory-rules thy noerror))
	 (rbuf
	  (if rules
	      (save-excursion
		(set-buffer 
		 (isa-create-new-associated 'ruletable (not switchto)))
		(ruletable-mode)
		(setq ruletable-rules rules)
		(setq ruletable-theory-name thy)
		(setq mode-name (concat thy " rules"))
		(ruletable-make-table)
		(current-buffer)))))
    (if rules (isa-display-buffer rbuf t))))



;;; ========== Making rule tables ==========

(defun ruletable-make-table ()
  "Make a ruletable from ruletable-rules."
  (let* ((buffer-read-only nil)
	 (frm    (get-frame-for-buffer-noselect (current-buffer)))
	 (width (1- (frame-width frm))))
    (erase-buffer)
    ;; This is a nasty hack, which doesn't always work.
    ;; I think best is to forget special faces and rely on colour
    ;; fontification, these days.  Shame, because it's nice to
    ;; use larger fonts for headings, etc.
    ;; (or (face-differs-from-default-p 'ruletableGroupname)
    ;;    (make-face-bold 'ruletableGroupname))
    (mapcar 'ruletable-insert-group ruletable-rules))
  (goto-char (point-min))
  (ruletable-forward-word))

(defun ruletable-insert-group (rulegroup)
  (if isa-use-long-ruletables
      ;; First, the rulegroup name, if in long mode
      (progn
	;; Put it in special face
	(let ((pos (point)))
	  ;; Rely on no line breaks occuring here
	  (insert (car rulegroup))
	  (set-extent-face (make-extent pos (point))
			   'ruletableGroupname))
	(newline)))
  ;; Now the rule names
  (mapcar 'ruletable-insert-rule (cdr rulegroup))
  (newline))

(defun ruletable-insert-rule (rule)
  ;; NB: `width' passed in from ruletable-make-table
  ;; Do auto-fill by hand here, because it doesn't seem
  ;; to work well otherwise...
  (if (eq (current-column) width)
      (newline))
  (insert " ")
  (if rule 
      ;; The rule may be a name....
      (progn
	(if (< (+ (length rule) (current-column)) width)
	    (insert rule)
	  (newline)
	  (insert " ")
	  (insert rule))
	;; Insert space here and use (point - 1) below
	;; to prevent extent being extended as buffer grows.
	(insert " ")
	;; Highlight under mouse
	(set-extent-property
	 (make-extent (save-excursion
			(backward-char 1)
			(skip-chars-backward "^ \t\n") (point))
		      (1- (point)))
	 'mouse-face 'highlight))
    ;; or otherwise a dividing mark
    (if isa-use-long-ruletables (newline))))

(defun ruletable-toggle-long ()
  "Toggle isa-use-long-ruletables and update table accordingly"
  (interactive)
  (setq isa-use-long-ruletables (not isa-use-long-ruletables))
  (ruletable-make-table))




;;; ===== Finding the rule that mouse or point is on =====

(defun ruletable-indicated-rule (event &optional nomove)
  (let* ((win    (event-window event))
	 (buffer (window-buffer win))
	 (p      (and (save-excursion 
			(set-buffer buffer)
			(eq major-mode 'ruletable-mode))
		      (event-point event)))
	 (extent (and p (extent-at p buffer 'mouse-face))))
    (if extent
	 (let* ((curbuf (current-buffer))
		(exs    (extent-start-position extent))
		(exe    (extent-end-position extent))
		result)
	   (if (not nomove) (set-window-point win exs))
	   (set-buffer buffer)
	   (setq result (buffer-substring exs exe))
	   (set-buffer curbuf)
	   result))))

(defun ruletable-on-word-char ()
  (char-equal ?w (char-syntax (char-after (point)))))

(defun ruletable-pointed-rule ()
  "Return a rulename for point, or nil."
    (if (ruletable-on-word-char)
	(save-excursion
	  (let (start)
	    (forward-char)
	    (forward-word -1)		; Start of word
	    (if (bolp) nil		; mustn't be at start of line
	      (setq start (point))
	      (forward-word 1)
	      (buffer-substring start (point)))))))


;; Old versions allowed insertion into ML buffers as well -
;; no longer included because of trickiness of picking
;; and displaying correct buffer to insert into.

(defun ruletable-pick-isa-buffer (&optional event)
  "Pick the Isabelle buffer to do insertion in."
  (let* ((win    (and event (event-window event)))
	 (buffer (if win (window-buffer win) (current-buffer))))
    (save-excursion
      (set-buffer buffer)
      (if (isa-buffer-active isa-buffer)
	  isa-buffer
	(error "No associated Isabelle buffer")))))



;;; ========== Selecting (inserting) rules ==========

(defun ruletable-select-indicated-rule (event)
  "Select (insert into other buffer) the rule at the click-location.
If the point in the other buffer is on an identifier character,
then a comma is inserted before the rule name."
  (interactive "e")
  (let ((rule (ruletable-indicated-rule event)))
    (if rule
	(save-excursion 
	  (set-buffer (ruletable-pick-isa-buffer event))
	  (ruletable-comma-insert rule)
	  (isa-display-buffer (current-buffer))))))

(defun ruletable-select-pointed-rule ()
  "Select (insert into other buffer) the rule that point is on.
If the point in the other buffer is on an identifier character,
then a comma is inserted before the rule name."
  (interactive)
  (let* ((rule (ruletable-pointed-rule))
	 (buf  (ruletable-pick-isa-buffer)))
    (if rule
	(save-excursion
	  (isa-display-buffer buf)
	  (set-buffer buf)
	  (ruletable-comma-insert rule)))))

(defun ruletable-comma-insert (string)
  (isa-insert-as-if-selected
   (ruletable-add-comma string)))

(defun ruletable-add-comma (string)
  "Return STRING, possibly with a preceding comma."
  (if (> (point) (point-min))
	(progn
	  (backward-char)
	  (cond ((ruletable-on-word-char)
		 (forward-char)
		 (concat "," string))
		 (t (forward-char) string)))
    string))




;;; ========== Quick Tactics ==========

;; only with mouse at the moment.

(defun ruletable-tactic-indicated-rule (event tactic)
  "Use TACTIC on rule at click-location."
  (interactive "e")
  (let ((rule (ruletable-indicated-rule event)))
    (if rule
	(save-excursion 
	  (set-buffer (ruletable-pick-isa-buffer event))
	  (isa-insert-substitute tactic rule)
	  (isa-display-buffer (current-buffer))))))

(defun ruletable-default-tactic-indicated-rule (event)
  "Use the defaulting tactic on the rule at click-location."
  (interactive "e")
  (let* ((rule    (ruletable-indicated-rule event))
	 (cat     (if rule (isa-guess-rule-category rule)))
	 (tactic  (if rule (isa-tactic-for-category cat))))
    (if rule
	(save-excursion 
	  (set-buffer (ruletable-pick-isa-buffer event))
	  (isa-insert-substitute tactic rule)
	  (isa-display-buffer (current-buffer))))))

(defun ruletable-rtac-indicated-rule (event)
  "Use rtac on rule at click-location."
  (interactive "e")
  (ruletable-tactic-indicated-rule event "by (rtac %p %s);"))

(defun ruletable-etac-indicated-rule (event)
  "Use etac on rule at click-location."
  (interactive "e")
  (ruletable-tactic-indicated-rule event "by (etac %p %s);"))

(defun ruletable-dtac-indicated-rule (event)
  "Use dtac on rule at click-location."
  (interactive "e")
  (ruletable-tactic-indicated-rule event "by (dtac %p %s);"))

(defvar ruletable-last-mouse-track-event-rule nil)

(defun ruletable-follow-mouse (event)
  "Show the quick tactic invoked by middle mouse button on current rule."
  (interactive "e")
  (let ((rule    (ruletable-indicated-rule event t)))
    (if (string-equal ruletable-last-mouse-track-event-rule rule)
	;; Do nothing if same rule as was message'd before.  This hack
	;; is just to try and improve matters with FSF which generates
	;; far too many motion events, resulting in flickery display
	;; and multiple messaging.  With XEmacs there's no problem
	;; because it's more sensible about only generating motion
	;; events when they may matter, I think.
	nil
      (let* ((cat     (if rule (isa-guess-rule-category rule)))
	     (tactic  (if rule (isa-tactic-for-category cat))))
	(setq ruletable-last-mouse-track-event-rule rule)
	(if rule 
	    (save-excursion
	      ;; To get correct subgoal number
	      (set-buffer (ruletable-pick-isa-buffer event))
	      (message
	       (concat
		;; Nasty hardwiring of binding here!
		"Button 2 runs:  "
		(isa-insert-format tactic rule))))
	  (message ""))))))

;;; ========== Displaying rules ==========

(defun ruletable-show-indicated-rule (event)
  "Display the rule at the click-location, using prth."
  (interactive "e")
  (ruletable-show-rule (ruletable-indicated-rule event) event))

(defun ruletable-show-pointed-rule ()
  "Display the rule that the point is on."
  (interactive)
  (ruletable-show-rule (ruletable-pointed-rule)))

(defun ruletable-show-rule (rule &optional event)
  "Show RULE in a temporary buffer using prth."
  (interactive "sRule name: ")
  (let* ((isa-buffer (ruletable-pick-isa-buffer event))
	 (proc  (get-buffer-process isa-buffer))
	 (logic (save-excursion (set-buffer isa-buffer) isa-logic-name))
	 (caty  (isa-guess-rule-category rule))
	 (ruletype (isa-name-for-category caty))
	 (cmd   (concat (isa-display-cmd-for-category caty rule) ";"))
	 (text  (and rule proc (isa-send-string-catch-result proc cmd)))
	 (text2 (and text
		     (substring text 0 (string-match "^val" text)))))
    (cond (text2
	   (isa-set-pretty-margin proc (- (frame-width) 1))
	   (isa-show-output-in-temp-buffer "*Rule*" 
	     (concat ruletype " " rule " in " logic " :\n\n") text2)
	   (isa-set-default-pretty-margin proc))
	  ((null rule) nil) ; (error "No rule selected!"))
	  (t           (error "Can't find Isabelle process")))))


(defun isa-set-pretty-margin (proc marg)
  "Set Isabelle pretty printer margin"
  (isa-send-string-catch-result proc
   (concat "Pretty.setmargin " 
	   (int-to-string marg)
	   ";")))

(defun isa-default-pretty-margin ()
  "Default value for pretty printer margin"
  (-
   (let* ((pb (car-safe (isa-find-buffers-in-mode 'proofstate-mode)))
	  (wp (and pb (get-buffer-window pb)))
	  (ib (or wp (car-safe (isa-find-buffers-in-mode 'isa-mode))))
	  (wi (or wp (and ib (get-buffer-window ib)))))
     (if wi
	 (window-width wi)
       (window-width)))
   1))

(defun isa-set-default-pretty-margin (proc)
  "Set default value for Isabelle pretty printer margin"
  (interactive)
  (isa-set-pretty-margin proc (isa-default-pretty-margin)))


;;; ========= Parsing .rule files ==========

(defun ruletable-parse-table ()
  "Parse a ruletable contained in the current buffer."
  (ruletable-mode)
  (let (rules)
    (goto-char (point-min))
    (while (not (eobp))
      (let (rowname row)
	(if (looking-at "^\\S-[^ \n\t]*")
	    (progn
	      (setq rowname (buffer-substring (match-beginning 0)
					      (match-end 0)))
	      (forward-line 1))
	  (setq rowname " "))
	(while (and (not (eobp)) (looking-at "[ \t]"))
	  (skip-chars-forward " \t")
	  (let (subrow)
	    (while (looking-at "\\S-+")
	      (setq subrow (cons (buffer-substring (match-beginning 0)
						   (match-end 0))
				 subrow))
	      (skip-chars-forward "^ \t")
	      (skip-chars-forward " \t"))
	    (forward-line 1)
	    (if subrow 
		(setq row 
		      (append row 
			      (reverse (if (looking-at "[ \t]")
					   (cons nil subrow)
					 subrow)))))))
	(skip-chars-forward "\n")
	(setq rules (cons (cons rowname (vconcat row)) rules))))
    (reverse rules)))


				 
;;; ========== Finding ".rules" files ==========

(defconst isa-rules-extn ".rules")

(defvar isa-rules-file-regexp 
  (concat (regexp-quote isa-rules-extn) "$")
  "Regular expression recognizing rule files.")

(defvar isa-theory-rules nil
"Table of logics and rule names for rule-table buffers.

The format is:
  A list of lists, one per theory (or logic).
  Each theory list consists of:  (THYNAME  RULEGROUP1 RULEGROUP2 ...)
  Each rule group constists of:  (RULEGROUPNAME . [rulenames...])
  Each rule-name is either a string or nil - nil represents a further
    (un-named) subgrouping of the rules.

Unloaded rule tables are represented as (THYNAME . FILENAME).")

(defun isa-get-rules-files ()
  (interactive)
  "Generate isa-rule-files from isa-ruletable-paths."
  (message "Finding ruletable files...")
  (let ((rules-files 
	 (isa-get-file-alist isa-rules-file-regexp isa-ruletable-paths)))
    (setq isa-theory-rules (isa-alist-union rules-files isa-theory-rules )))
  (message "Finding ruletable files...done."))
  
(isa-get-rules-files)			; initialise.


(defun isa-theory-rules (name &optional noerror)
  "Find a rule table for the theory NAME.
This will use `isa-theory-rules' or look for a file NAME.rules to parse.
An error occurs if the file can't be found, unless NOERROR is non-nil,
in which case a message is given and nil is returned." 
  (let ((val  (cdr-safe (assoc name isa-theory-rules))))
    (if (and val (listp val)) 
	val
      (ruletable-load name noerror))))

(defun ruletable-load (name &optional noerror)
  (let
      ((filename (or (cdr-safe (assoc name isa-theory-rules))
		     (and (file-exists-p (concat name isa-rules-extn))
			  (concat name isa-rules-extn))
		     (and (file-exists-p name) name))))
    (if filename
	(let* 
	    ((shortname (car (isa-file-name-cons-extension 
			      (file-name-nondirectory filename))))
;	     (wasloaded (get-file-buffer filename))
; don't bother do this, because we destroy the mode, and stuff
; anyway.
	     (buf      (progn
			 (message "Reading ruletable file for %s..." shortname)
			 (find-file-noselect filename)))
	     (rules    (save-excursion
			 (set-buffer buf)
			 (ruletable-parse-table))))
;	  (if (not wasloaded) (kill-buffer buf))
	  (kill-buffer buf)
	  (message "Reading ruletable file for %s...done." shortname)
	  (isa-add-theory-rules (cons shortname rules))
	  rules)
      ;; Rules file not found
      (if noerror
	  (progn
	    (message "(No rules file for %s)" name)
	    nil)
	(error "Can't find rules file for %s" name)))))



;;; ======= User-level functions to alter ruletables ==========

(defun isa-add-theory-rules (rulelist)
  "Add or overwrite a rule table for a theory in isa-theory-rules.
See doc. of isa-theory-rules for format of argument."
  (let ((oldthy (assoc (car rulelist) isa-theory-rules)))
    (if oldthy
	(setcdr oldthy (cdr rulelist))
      (setq isa-theory-rules
	    (nconc isa-theory-rules (list rulelist))))))

(defun isa-add-theory-rulegroup (thy rulegroup)
  "Add or overwrite a rulegroup for a theory in isa-theory-rules."
  (let* ((oldthy  (assoc thy isa-theory-rules))
	 (oldgrps (and oldthy (assoc (car rulegroup) (cdr oldthy)))))
    (if oldthy
	(if oldgrps
	    (setcdr oldgrps (cdr rulegroup))
	  (nconc oldthy (list rulegroup)))
      (setq isa-theory-rules
	    (nconc isa-theory-rules 
		   (list (cons thy (list rulegroup))))))))



;;; ============ Rule Categories ============ 

(defconst isa-rule-categories
  '((intro  .  ("Introduction rule"  "by (rtac %p %s);"))
    (elim   .  ("Elimination rule"   "by (etac %p %s);"))
    (dest   .  ("Destruction rule"   "by (dtac %p %s);"))
    (def    .  ("Definition"         "by (rewrite_goals_tac [%p]);"))
    (rule   .  ("Rule"		     "by (rtac %p %s);"))
    (tactic .  ("Tactic"             "by %p;"))
    (itactic . ("Indexed Tactic"     "by (%p %s);"))
    (ruleset . ("Rule set"           "by (resolve_tac %p %s);"))
    (simpset . ("Simplifier set"     "by (asm_full_simp_tac %p %s);"))
    (claset .  ("Classical set"      "by (fast_tac %p %s);"))))

(defun isa-guess-rule-category (rule)
  "Guess the category of RULE from its name."
  ;; eventually replace this by extending the rule table above.
  ;; (or designing/generating lisp files to replace it!)
  (cond ((string-match "I$\\|I[1-9]$" rule)
	 'intro)
	((string-match "E$\\|E[1-9]$" rule)
	 'elim)
	((string-match "D$\\|D[1-9]$" rule)
	 'dest)
	((string-match "_def$" rule)
	 'def)
	((string-match "_rls$" rule)
	 'ruleset)
	((string-match "_tac$" rule)
	 'itactic)
	((string-match "(!simpset)$\\|_ss$" rule)
	 'simpset)
	((string-match "_cs$" rule)
	 'claset)
	(t
	 'rule)))

(defun isa-name-for-category (cat)
  "Return the name of category CAT."
  (or (nth 0 (cdr-safe (assoc cat isa-rule-categories)))
      (error "Unknown rulename category: %s" (symbol-name cat))))

(defun isa-tactic-for-category (cat)
  "Return a default tactic given a category."
  (or (nth 1 (cdr-safe (assoc cat isa-rule-categories)))
      (error "Unknown rulename category: %s" (symbol-name cat))))
    
(defun isa-display-cmd-for-category (cat item)
  (cond
   ((eq cat 'ruleset)  (format "(map prth %s; ())" item))
   ((eq cat 'itactic)  "writeln \"<tactic>\"")
   ((eq cat 'simpset)  
    (format "(fn {congs,simps,...}=>(writeln \"Congruence rules:\"; map prth congs; writeln \"\\nSimplification rules:\"; map prth simps; ())) (rep_ss %s)" item))
   ((eq cat 'claset)   (format "print_cs %s" item))
   (t                  (format "(prth %s; ())" item))))
   

(provide 'isa-ruletable)

;;; End of isa-ruletable.el

;;; isa-menus.el - Menus and commands for Isabelle mode.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-menus.el,v 2.6 1997/05/27 22:26:58 da Exp
;;;

(require 'isa-load)
(require 'easymenu)
(require 'comint)


;;; =============== Command Table ===============

;;; Key bindings:  (NB: try and avoid clashing with comint ones)
;;;
;;; C-c C-t <key>  tactics...
;;; C-c C-r <key>  rewriting tactics...
;;; C-c C-p <key>  prover tactics...
;;; C-c C-s <key>  goal tactics ("goal*S*tack" or "*S*tart" proof)
;;; C-c M-l        listener
;;; C-c M-p        proofstate
;;; C-c M-r        ruletable
;;; C-c C-q        quit Isabelle

(defconst isa-commands-table 
'(("Goal" .
   (("\C-c\C-s\C-u" isa-undo "undo" (isa-insert "undo();%n"))
    ("\C-c\C-s\C-b" isa-back "back" (isa-insert "back();%n"))
    ("\C-c\C-s\C-c" isa-chop "chop" (isa-insert "chop();%n"))
    (nil     isa-choplev "choplev" (isa-insert "choplev %p;"))
    ()
    ("\C-c\C-sg"    isa-goal-thy
     "goal thy" (isa-insert "val prems = goal thy \"%p\";"))
    ("\C-c\C-s\C-w"    isa-goalw-thy
     "goalw thy" (isa-insert "val prems = goalw thy %p \"\";"))
    ()
    ("\C-c\C-s\C-q"    isa-qed "qed"       (isa-insert "qed \"%p\";"))
; obsolete usage    
;    ("\C-c\C-sr"    isa-result "result" (isa-insert "val %p= result();"))
    ()
    ("\C-c\C-s\C-p"    isa-push-proof
     "push-proof" (isa-insert "push_proof();%n"))
    ("\C-c\C-s\C-o"    isa-pop-proof
     "pop proof"  (isa-insert "val prems = pop_proof();%n"))
    ("\C-c\C-s\C-n"    isa-rotate-proofs
     "rotate proofs" (isa-insert "val prems = rotate_proof();%n"))))

 ("Tactic" .
   (("\C-c\C-t\C-r" isa-resolve_tac
     "resolve"  
     (isa-insert "by (resolve_tac [%p] %s);"))
    ("\C-c\C-t\C-p" isa-resolve_tac-prems 
     "resolve prems" 
     (isa-insert  "by (resolve_tac prems %s);%n"))
    ("\C-c\C-t\C-a" isa-assume_tac
     "assume"   
     (isa-insert  "by (assume_tac %s);%n"))
    ("\C-c\C-t\C-e" isa-eresolve_tac      
     "eresolve" 
     (isa-insert "by (eresolve_tac [%p] %s);"))
    ("\C-c\C-t\C-d" isa-dresolve_tac
     "dresolve" 
     (isa-insert "by (dresolve_tac [%p] %s);"))
    ("\C-c\C-t\C-f" isa-forward_tac
     "forward"  
     (isa-insert "by (forward_tac [%p] %s);"))
    ()
    ("\C-c\C-tr"    isa-match_tac
     "match"    
     (isa-insert "by (match_tac [%p] %s);"))
    ("\C-c\C-tp"    isa-matc_tac-prems 
     "match prems" 
     (isa-insert  "by (match_tac prems %s);%n"))
    ("\C-c\C-ta"    isa-eq_assume_tac
     "eq_assume" 
     (isa-insert "by (eq_assume_tac %s);%n"))
    ("\C-c\C-te"    isa-ematch_tac
     "ematch"   
     (isa-insert "by (ematch_tac [%p] %s);"))
    ("\C-c\C-td"    isa-dmatch_tac
     "dmatch"
     (isa-insert "by (dmatch_tac [%p] %s);"))
    ()
    ("\C-c\C-rg" isa-rewrite_goals_tac
     "rewrite_goals" 
     (isa-insert "by (rewrite_goals_tac [%p]);"))
    ("\C-c\C-rw"    isa-rewrite_tac
     "rewrite"
     (isa-insert "by (rewrite_tac [%p]);"))
    ("\C-c\C-rf"    isa-fold_goals_tac
     "fold_goals" 
     (isa-insert "by (fold_goals_tac [%p]);"))
    ("\C-c\C-rc"    isa-cut_facts_tac
     "cut_facts" 
     (isa-insert "by (cut_facts_tac [%p]  %s);"))
    ("\C-c\C-rp"    isa-cut_facts_tac-prems
     "cut_facts prems" 
     (isa-insert "by (cut_facts_tac prems %s);%n"))))

  ("Prover" .
   (("\C-c\C-p\C-s"  isa-simp_tac
     "simp" 
     (isa-insert "by (simp_tac %l_ss %s);"))
    ("\C-c\C-p\C-a"  isa-asm_simp_tac 
     "asm_simp" 
     (isa-insert "by (asm_simp_tac %l_ss %s);"))
    ("\C-c\C-p\C-f"  isa-asm_full_simp_tac 
     "asm_full_simp" 
     (isa-insert "by (asm_full_simp_tac %l_ss %s);"))
    ()
    ("\C-c\C-pf"     isa-fast_tac 
     "fast"
     (isa-insert "by (fast_tac %l_cs %s);"))
    ("\C-c\C-pb"     isa-best_tac 
     "best" 
     (isa-insert "by (best_tac %l_cs %s);"))
    ("\C-c\C-ps"     isa-step_tac
     "step"
     (isa-insert "by (step_tac %l_cs %s);"))
    ()
    (nil         isa-contr_tac 
     "contr"
     (isa-insert "by (contr_tac %s);%n"))
    (nil         isa-mp_tac 
     "mp"
     (isa-insert "by (mp_tac %s);%n"))
    (nil        isa-eq_mp_tac 
     "eq_mp"
     (isa-insert "by (eq_mp_tac %s);%n"))))

  ("Options" .
   ;; It's handy to have keys for listener, etc, for non-window
   ;; usage.

   (("\C-c\el" nil "Listener"	   listener)
    ("\C-c\ep" nil "Proof State"   proofstate)
    ;; could activate here with logic-name.
    ("\C-c\er" nil  "Rules for logic"  (ruletable isa-logic-name))
    (nil     nil "Rules..."        (call-interactively 'ruletable))        
    (nil     nil "Rescan ruletables" isa-get-rules-files)
    ()
    (nil    isa-goals-limit
     "goals limit" (isa-set-setting "Goals limit="
				    "goals_limit:=%s;"))
    (nil    isa-print-depth
     "print depth" (isa-set-setting "Print depth="
				    "print_depth %s;"))
    (nil    isa-show-types
     "show types" (isa-select-option "Show types? " 
					 "show_types:=%s;"))
    (nil    isa-show-sorts
     "show sorts" (isa-select-option "Show sorts? " 
					 "show_sorts:=%s;"))
    (nil    isa-simp-tracing
     "simplifier tracing"
     (isa-select-option "Trace simplifier? "
			"trace_simp:=%s;"))
    (nil    isa-depth-tracing
     "DEPTH_FIRST tracing"
     (isa-select-option "Trace DEPTH_FIRST? "
			"trace_DEPTH_FIRST:=%s;"))
    (nil    isa-best-tracing
     "BEST_FIRST tracing"
     (isa-select-option "Trace BEST_FIRST? "
			"trace_BEST_FIRST:=%s;"))
    (nil    isa-repeat-tracing
     "REPEAT tracing"
     (isa-select-option "Trace REPEAT? "
			"trace_REPEAT:=%s;"))
    ()
    ("\C-c\C-q"  nil  "Exit Isabelle"     isa-quit-isabelle))))
"Table for building Isabelle commands and menus.
An alist: key is group name (menu heading), each entry is:

   (KEY SYMBOL TEXT FORM)  or  ()

where:
 
   KEY    - default key binding
   SYMBOL - function name (prefixed by \"isa-\")
   TEXT   - text form of function name (for menu entry)
   FORM   - body of command

KEY may be nil for no default binding.

SYMBOL may be nil, in which case FORM must be a symbol.

FORM may be a symbol, in which case it is invoked with call-interactively,
or else a list representing the body of a function to which SYMBOL 
will be bound.

() represents a grouping in the commands - separation bar in the menu.")



;;; =============== Command Functions ===============


(defun isa-quit-isabelle ()
  "Query and exit Isabelle using isa-quit-function"
  (interactive)
  (if (or isa-dont-query-quit
	  (save-excursion
	    (y-or-n-p "Quit Isabelle? ")))
      (funcall isa-quit-function (y-or-n-p "Save database? "))))

(defun isa-view-document (docname)
  "View Isabelle documentation, named DOCNAME."
  (interactive
   (list
    (completing-read "Which document? "
		    (funcall isa-list-docs-function) nil t)))
  (apply 'start-process
	 "isa-view-doc" nil (funcall isa-doc-command-function docname)))
  
;; these could do with being generalised with escape sequences, in a
;; way similar to isa-insert.

(defun isa-select-option (prompt cmdstring)
  "Set an ML boolean according to the users reply."
  (isa-send-string-catch-result
   (format cmdstring
	   (isa-bool-to-ml
	     (y-or-n-p prompt)))))

(defun isa-set-setting (prompt cmdstring)
  "Set an ML integer according to the users reply."
  (let ((interactive-code (concat "n" prompt)))
    (isa-send-string-catch-result
     (format cmdstring
	     (isa-int-to-ml
	      ;; this is rather convoluted...
	      ;; is there a better way?
	      (call-interactively
	       (`
		(lambda (arg)
		  (interactive (, interactive-code))
		  arg))))))))


;;; ========== Command Generation ==========

(defun isa-generate-commands ()
  "Generate commands from isa-commands-table"
  (mapcar '(lambda (pair)
	     (let* ((groupname (car pair))
		    (comtab    (cdr pair))
		    (docprefix (concat "Isabelle menu command.   Execute " groupname ": ")))
	       (mapcar 'isa-make-command comtab))) isa-commands-table))

(defun isa-make-command (table-entry)
  "Simulate a defun based on a command table entry.
Requires docprefix to be set to a prefix for the documentation string."
  (if (and table-entry (nth 1 table-entry))
      (let* ((sym  (nth 1 table-entry))
	     (doc  (nth 2 table-entry))
	     (form (nth 3 table-entry))
	     (cmd  (if (symbolp form)
		       (list 'call-interactively (list 'quote form))
		     form))
	     (f (list 'lambda () 
		      (concat docprefix doc)
		      '(interactive)
		      cmd)))
	(fmakunbound sym)
	(fset sym f))))

(isa-generate-commands)  ; Generate the commands now.


;;; ============ Isabelle mode map ============

;; appears here because of FSF menus-in-keymap system.

(defconst isa-mode-map
  (let
      ((map (copy-keymap comint-mode-map))
       (do-define-key 
	'(lambda (table-entry)
	   (if (or (null table-entry) 
		   (null (car table-entry)))
	       nil
	     (if (null (nth 1 table-entry))
		 (define-key map (car table-entry) (nth 3 table-entry))
	       (define-key map (car table-entry) (nth 1 table-entry)))))))
    ;; First some local prefix keys...
    (define-key map "\C-c\C-t" (make-sparse-keymap)) 
    (define-key map "\C-c\C-r" (make-sparse-keymap)) 
    (define-key map "\C-c\C-p" (make-sparse-keymap)) 
    ;; Now mouse/arrow keys
    (isa-define-popup-key map 'button3           'isa-popup-tactic-menu)
    (isa-define-popup-key map '(shift button3)   'isa-popup-goal-menu)
    (isa-define-popup-key map '(control button3) 'isa-popup-option-menu)
    (isa-define-popup-key map '(meta button3)    'isa-popup-prover-menu)
    (isa-define-key map '(control down) 'isa-inc-denoted-subgoal)
    (isa-define-key map '(control up) 'isa-dec-denoted-subgoal)
    ;; Normal key defs
    (define-key map "\M-\t"     'comint-dynamic-complete)
    (define-key map "\t"        'comint-dynamic-complete)
    (define-key map "\M-?"      'comint-dynamic-list-completions)
    (define-key map "\C-m"      'isa-send-input)
    (define-key map "\C-c\C-c"  'isa-interrupt)
    (mapcar '(lambda (pair)
	       (mapcar do-define-key (cdr pair))) isa-commands-table)
    map))

;;; ============ Interaction Mode Menus ============

(defun isa-generate-interaction-menus ()
  "Make menus from isa-commands-table."
  (mapcar '(lambda (pair)
	     (cons (car pair)
		   (mapcar 'isa-make-menu-entry (cdr pair))))
	  isa-commands-table))
 
(defun isa-make-menu-entry (table-entry)
  (if table-entry
      (let* ((command (or (nth 1 table-entry)
			  (nth 3 table-entry)))
	     (name    (nth 2 table-entry)))
	(vconcat (list name command t)))
    "----"))

(defun isa-apply-easy-menu-define (sym menu)
  (eval
   (` (easy-menu-define (, sym) 
			(list isa-mode-map)
			"Menu used in Isabelle mode."
			'(, menu))))
   sym)

(defconst isa-interaction-menus
  (mapcar
   '(lambda (m)
      (let ((menu-name (intern
			(concat "isa-" (car m) "-menu"))))
	(isa-apply-easy-menu-define menu-name m)
	menu-name))
   (isa-generate-interaction-menus)))

(defun isa-apply-easy-menu-add (menu)
  (easy-menu-add (symbol-value menu)))

(defun isa-add-interaction-menus () 
  "Add Isabelle interaction-mode menus to the menubar."
  (mapcar 'isa-apply-easy-menu-add isa-interaction-menus))


;;; ============== Main "Isabelle" Menu. ==============

(defvar isa-object-logic-names nil
  "Association list of object logic names and full path names.
Automatically generated from isa-logic-paths.")

(defun isa-cache-logics-list ()
  "Cache the list of available logics in isa-object-logic-names."
  (message "Finding object logics...")
  (setq isa-object-logic-names 
	(funcall isa-list-logics-function))
  (message "Finding object logics...done."))

(isa-cache-logics-list)			; belongs in a hook somewhere.


(defun isa-logics-menu-items ()
  (let ((vc '(lambda (nm)	
	       (vector nm (list 'isabelle-session nm) t))))
    (append
     '(["Logic File..."  (isabelle 16) t]
       "----")
     (mapcar vc isa-object-logic-names)
     '("----"
       ["Rebuild menu"    isa-rebuild-logics-menu t]
       "----"
       ["Exit saving database"    (funcall isa-quit-function t)  t]
       ["Exit *without* saving"   (funcall isa-quit-function nil)  t]
       "----"
       ["Kill session"		  (kill-buffer (current-buffer)) t]))))

;; I don't think it's worth bothering with a rebuild menu option here:
;; let's assume the list of documentation won't change during an Emacs
;; session!
(defconst isa-help-menu-items
  (let ((vc '(lambda (docdes)
	       (vector (car (cdr docdes))
		       (list 'isa-view-document (car docdes)) t))))
  (append
   '(["Isamode info"		 (progn (require 'info)
					(Info-goto-node "(Isamode)Top")) t])
    (mapcar vc (funcall isa-list-docs-function)))))

(defconst isa-edit-menu-items
  '(["Use file"               isa-thy-use-file t]
    ["Use_thy file"           (isa-thy-use-file 1) t]
    ["Use region"             isa-thy-use-region t]
    ["Switch thy/ML file"     isa-thy-find-other-file t]
    "----"
    ["Theory file template"   isa-thy-insert-template t]
    "----"
    ["Batchify proof"         isa-batchify t]
    ["Unbatchify proof"       isa-unbatchify t]
    "----"
    ["Listen"                 (if (eq major-mode 'isa-mode)
				  (listener)
				(listener-minor-mode)) t]))

(defun isa-main-menu-items ()
  (list
   (cons "Logic"  (isa-logics-menu-items))
   (cons "Edit"     isa-edit-menu-items)
   (cons "Manuals"  isa-help-menu-items)))

(defun isa-add-main-menu ()
  (easy-menu-define 
   isa-main-menu
   (list isa-mode-map)
   "Isabelle main menu"
   (cons "Isabelle" (isa-main-menu-items)))
  (easy-menu-add isa-main-menu))

(defun isa-add-logics-menu ()
  (easy-menu-define 
   isa-logics-menu
   (list isa-mode-map)
   "Isabelle logics menu"
   (cons "Logic" (isa-logics-menu-items)))
  (easy-menu-add isa-logics-menu))

(defun isa-add-edit-menu ()
 (easy-menu-define 
   isa-edit-menu
   (list isa-mode-map)
   "Isabelle edit menu"
   (cons "Edit" isa-edit-menu-items))
  (easy-menu-add isa-edit-menu))

(defun isa-add-man-menu ()
 (easy-menu-define 
   isa-man-menu
   (list isa-mode-map)
   "Isabelle man menu"
   (cons "Manuals" isa-help-menu-items))
  (easy-menu-add isa-man-menu))
  
(defun isa-add-session-menus ()
  ;; In FSF these appear in reverse order!  Why??
  (isa-add-logics-menu)
  (isa-add-edit-menu)
  (isa-add-man-menu))

(defun isa-rebuild-logics-menu ()
  (interactive)
  (isa-cache-logics-list)  ;  find object logics
  (easy-menu-change () "Logic" (isa-logics-menu-items)))


;;; ========== Menu Pop-up functions ==========

(defun isa-popup-tactic-menu (event)
  (interactive "@e")
  (popup-menu isa-Tactic-menu))

(defun isa-popup-goal-menu (event)
  (interactive "@e")
  (popup-menu isa-Goal-menu))

(defun isa-popup-option-menu (event)
  (interactive "@e")
  (popup-menu isa-Option-menu))

(defun isa-popup-prover-menu (event)
  (interactive "@e")
  (popup-menu isa-Prover-menu))



;;; ###autoload
(defun isa-menus ()	
  "Add Isabelle menu to current menubar."
  (interactive)
  (easy-menu-define isa-main-menu
		    (list (current-local-map))
		    "Isabelle main menu"
		    (cons "Isabelle" (isa-main-menu-items)))
  (easy-menu-add isa-main-menu))


(provide 'isa-menus)

;;; End of isa-menus.el

;;; isa-mode.el --- Isabelle interaction mode.

;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;; Created: December 1993.
;; Version: isa-mode.el,v 2.6 1997/05/27 22:26:58 da Exp
;; Keywords: languages,local

;;; Commentary:
;;; ----------------------------------------------------------------------
;;;          isa-mode -- GNU Emacs support for Isabelle
;;; ----------------------------------------------------------------------
;;;
;;; This file is part of a package which provides a facilities
;;; for use with the Isabelle theorem prover.
;;;
;;; In outline, the facilities are:
;;;
;;; * Theory mode    -  for editing theory files
;;; * Isabelle mode  -  for interacting with Isabelle
;;; * Menus          -  including tactics and common proof commands 
;;; * Listener       -  a buffer for recording interactive proofs
;;; * Proof-State    -  a buffer which displays the current proof state
;;; * Rule-table     -  rule names for the current logic
;;;
;;; Menus and mouse functions give Isabelle a primitive user-interface.
;;; They only function in the latest versions of Emacs - FSF Emacs 19
;;; or Lucid Emacs.
;;;
;;; For more details, please find the directory that comprises the
;;; distribution of this software, and read the files matching *.txt
;;;
;;; 				David Aspinall <da@dcs.ed.ac.uk>
;;;

;;; Code:


(require 'isa-load)
(require 'isa-proc)
(require 'isa-menus)
(require 'isa-display)			; 
(require 'isa-thy-mode)			; for isa-thy-mode-syntax-table
					; (should it be elsewhere?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; isa-mode  
;;
;;;
;;; local variables
;;;
;;; isa-input-sentinel          <see isa-proc>
;;; isa-weed-output-function	.
;;; isa-output-buf		.
;;; isa-last-trace-start	.
;;; isa-trace-count		.
;;; isa-denoted-subgoal		.
;;; isa-logic-name              <see below>
;;; isa-associated-buffers
;;; isa-instring
;;;
;;; <plus various comint variables, kill-buffer-hook>
;;;
;;; hooks
;;;
;;; isa-mode-hook
;;; isa-mode-startup-hook
;;;

(defvar isa-associated-buffers nil
  "List of buffers associated with current Isabelle buffer.")

(defvar isabelle-session-buffer nil
  "Buffer running Isabelle session.")

(defvar isa-logic-name nil
  "Name of logic for current Isabelle buffer.")

(defvar isa-output-buf nil
  "Hidden output buffer associated with current Isabelle buffer.")

(defvar isa-instring nil
  "Non-nil if last input string contained an unterminated string.")

(defvar isa-buffer nil
  "Isabelle buffer linked to this \"associated\" buffer")
(make-variable-buffer-local 'isa-buffer)



;;;
;;; Mode for Isabelle interaction buffers: isa-mode
;;;

;;; NB: isa-mode-map defined in isa-menus.el

(defun isa-mode ()
  "Major mode for interaction with Isabelle.

These commands activate/display further buffers:
\\<isa-mode-map>
 \\[listener]\t\t- Listener buffer 
 \\[proofstate]\t\t- Proof State buffer
 \\[ruletable]\t\t- a Rule Table for a given theory
 \\[isa-ruletable-for-logic]\t\t- a Rule Table for this logic

Listener and ProofState are activated automatically if
buffers or frames with the default names exist.

This mode is built on top of comint-mode, and most of the comint-mode
key bindings are available.  There are many additional functions for
inserting common Isabelle commands.

\\{isa-mode-map}
Entry to this mode runs isa-mode-hook."
  (interactive)

  ;; COMINT CUSTOMISATION ===================================
  (comint-mode)						; Based on comint
  (setq comint-prompt-regexp isa-prompt-pattern)	; Isabelle prompts
  (setq comint-input-autoexpand nil)	                ; no history expansion.
  (setq comint-scroll-to-bottom-on-output 'others)      ; 
  (make-variable-buffer-local 'comint-completion-addsuffix)
  (setq comint-completion-addsuffix "\"")               ; files are in strings.
  ;; (deal with different versions of comint)
  (cond
   ((boundp 'comint-dynamic-complete-functions)
    ;; New version of comint in FSF 19.25
    (setq comint-dynamic-complete-functions 
	  '(isa-complete-command comint-dynamic-complete-as-filename ))
    ;; (seems to work okay without isa-after-filename).
    )
   (t
    ;; Older versions of comint (for FSF <19.25, Lemacs until has new stuff).
    (setq comint-input-sentinel 'isa-call-comint-filter-functions)
    (setq comint-dynamic-complete-command-command 'isa-complete-command)
    (setq comint-after-partial-filename-command 'isa-after-filename)
    ))
  (setq comint-input-filter-functions '(isa-track-cd-and-use))
  ;; Future improvements:
  ;; ====================
  ;; ** Set comint-get-old-input to do something clever
  ;;    (split ML prompt into two parts for continuing lines)
  ;; END COMINT CUSTOMIZATION

  (setq major-mode 'isa-mode)				; 
  (setq mode-name "Isabelle")				; 
  (use-local-map isa-mode-map)
  (set-syntax-table isa-thy-mode-syntax-table)		; shares with isa-thy-mode
  (make-local-variable 'isa-weed-output-function)	; real output filter
  (make-local-variable 'isa-output-buf)			; output accumulator buffer
  (make-local-variable 'isa-input-sentinel)		
  (make-local-variable 'isa-denoted-subgoal)
  (setq isa-denoted-subgoal 1)
  (make-local-variable 'isa-associated-buffers)
  (setq isa-associated-buffers nil)	                ; no linked buffers yet.
  (make-local-variable 'isa-logic-name)			
  (make-local-variable 'kill-buffer-hook)
  (make-local-variable 'isa-instring)
  (setq isa-instring nil)
  (add-hook 'kill-buffer-hook 
	    'isa-kill-associated-buffers)		; Killing Isabelle buffer
							; kills associated ones too.
  (isa-mode-process-init)
  (set-buffer-menubar isa-default-menubar)		; Base menu for Isabelle Mode
  ;(isa-add-main-menu)					; .. with "Isabelle" menu
  (isa-add-session-menus)			        ; Replaces main main.
  (isa-add-interaction-menus)				;   .. and others.
  (run-hooks 'isa-mode-hook))				; User hooks.



(defun isa-kill-associated-buffers ()
  "Kill buffers associated with an Isabelle buffer.
Intended as a value for kill-buffer-hook"
  (let ((bufs-to-go (copy-sequence isa-associated-buffers)))
    (mapcar
     '(lambda (b)
	(if (isa-buffer-active b)
	    (kill-buffer b)))
     bufs-to-go))
  (if isa-multiple-frame-mode
      (isa-kill-associated-frames)
    (delete-other-windows)))

(defun isa-kill-associated-frames ()
  "If there are redundant Isabelle related frames, remove them."
  ;; bit daft at the moment: better to map over *frames*
  ;; and use real frame name symbol.  Change var. appropriately.
  (mapcar
   '(lambda (mode)
      (let* ((frmnm   (get mode 'frame-name))
	     (frms  (isa-find-frames frmnm))
	     (bfs  (if frms (isa-find-buffers-in-mode mode)))
	     (fbf (car-safe bfs))
	     (rbf (cdr-safe bfs)))
	(cond ((and bfs (not (and (null rbf) (eq fbf (current-buffer)))))
	       (isa-display-buffer-on-frame fbf (car frms))
	       (setq frms (cdr frms))))
	(if frms
	    (condition-case nil
		(mapcar 'delete-frame frms)
	      (error nil)))))
   isa-associated-frame-names))


;;;
;;; Start-up an Isabelle Interaction Buffer.
;;;

(defun isabelle (&optional rehash-readfile)
  "Start or switch to an Isabelle session.
Prompts for a logic name LOGIC, which should be a file found in a
directory on isa-logic-paths, or a filename.  
To override something on isa-logic-paths, use a full pathname.
To re-scan the directories on isa-logic-paths, give a prefix argument.
To read a filename with completion, give two prefix arguments.

There is no limit on the number of Isabelle sessions that you may
activate: to have another session with the same logic, simply rename
the Isabelle buffer.

See the mode documentation for isa-mode for the commands
available in Isabelle buffers."
  (interactive "p")
  (if (or ;; Single prefix arg was given
       (and rehash-readfile
	    (> rehash-readfile 1)
	    (< rehash-readfile 16))
       ;; Object logics not found yet
       (null isa-object-logic-names))
      (isa-cache-logics-list))
  (isabelle-session
   (if (and rehash-readfile
	    (> rehash-readfile 15))
       (expand-file-name
	(read-file-name  "File name of logic: "
			 (funcall isa-default-logic-dir-function) nil t))
     (completing-read "Name of logic: "
		      (mapcar 'list isa-object-logic-names)))))

(defun isabelle-session (logic &optional raise)
  (let* 
      ((buff-name         (isa-name logic))
       (coml              (funcall isa-run-command-function logic))
       (buffer            (get-buffer-create buff-name)))
    ;;; use new variable instead: isa-session-buffer.
    ;;; Check it: error if active, option to kill process.
    (setq isa-session-buffer buffer)
    (if (comint-check-proc buffer)	           ; if running, select
        (progn					   ; it, maybe raise
          (isa-select-buffer buffer raise)         ;  buffers then exit.
	  (isa-mode-process-init)		   ; (set process redirection)
	  (mapcar 'isa-display-if-active           ; NB: uses "raise" variable
		  isa-associated-buffers))
      (progn	    
	(message "Starting Isabelle...")		;
        (set-buffer buffer)
	(erase-buffer)					; Clear buffer
	(sit-for 0)					; Update display
	(apply 'make-comint logic (car coml)	; Start comint process
	                  (cons nil (cdr coml)))	; running Isabelle
	(isa-mode)					; Switch to Isabelle mode
	(isa-select-buffer buffer)			; Select buffer.
	(setq isa-logic-name logic)			; 

	(while (zerop (buffer-size))	; wait for process to get going.
	  (sit-for 2)					; 
	  (accept-process-output))			; 
  
        (if isa-session-prelude		                ; send standard prelude
          (isa-send-string-catch-result isa-session-prelude))
        (run-hooks 'isa-mode-startup-hook)
        (isa-synchronise-wd)
	(message "Starting Isabelle...done.")
	(sit-for 1)
	(message isa-mode-version)))))

(defun isa-startup-function-for (sym)
  "Used to make functions for isa-mode-startup-hook."
  (if (or (memq sym isa-startup-defaults)
	  (isa-find-frames sym)
	  (get-buffer (isa-buffer-name-for sym)))
      (funcall sym)))

(defun isa-find-associated (sym)
  (let* ((modesym (intern (concat (symbol-name sym) "-mode"))))
    (isa-find-buffers-in-mode 
     modesym 
     (cons (current-buffer) isa-associated-buffers) ; hack to make sure non-nil arg.
     )))
  
(defun isa-create-new-associated (sym &optional nocheck)
  "Find or create a new associated buffer for SYM."
  (if (or nocheck
	  (null (isa-find-associated sym)))
      (let ((newbuf (if nocheck
			(generate-new-buffer (isa-buffer-name-for sym))
		      (get-buffer-create (isa-buffer-name-for sym))))
	    (curbuf (current-buffer)))
	(setq isa-associated-buffers
	      (cons newbuf isa-associated-buffers))
	(set-buffer newbuf)
	(kill-all-local-variables)
	(make-local-variable 'isa-buffer) 
	(setq isa-buffer curbuf)			; linked isabelle buffer
	(make-local-variable 'kill-buffer-hook)
	(add-hook 'kill-buffer-hook 'isa-remove-associated-buffer)
	(set-buffer curbuf)
	newbuf)
    (car (isa-find-associated sym))))

(defun isa-remove-associated-buffer ()
  "Remove current buffer from isa-associated-buffers"
  (if (isa-buffer-active isa-buffer)
      (let ((rbuf (current-buffer)))
	(save-excursion
	  (set-buffer isa-buffer)
	  (setq isa-associated-buffers
		(delq rbuf isa-associated-buffers))))))


;;; ============ Completion ============

(defun isa-complete-command ()
  "Complete the ML value after point, use isa-completion-list."
  (interactive)
  (if (isa-inside-string)
      nil  ;; do nothing inside strings if filename expansion failed.
    (let ((stub (buffer-substring
		 (point)
		 (save-excursion
		   (skip-chars-backward "[a-zA-Z_.'0-9]")
		   (point)))))
      (if (string= "" stub)
	  t				; pretend it worked.
	(let ((comint-completion-addsuffix " "))
	  (comint-dynamic-simple-complete stub isa-completion-list)
	  t)))))

;;; this is for older comints: the hook no longer exists for new ones,
;;; so this function will become defunct shortly.  isa-complete-command
;;; always returns t unless in a string so that filename completion is
;;; not called, even if command completion fails.

(defun isa-after-filename ()
  "Non-nil if point is in a string that could be a partial filename."
  (if (isa-inside-string)
      (save-excursion
	(skip-chars-backward "~/A-Za-z0-9+@:_.$#,={}\\-")
	(backward-char)
	(looking-at "\"[~/A-Za-z0-9+@:_.$#,={}\\-]"))))


;;; ======== ML value completion table ========

;; Naive completion: don't distinguish between values used
;; as commands and others.  Smarter would be to split into
;; tables for commands, tactics and tacticals.

(defconst isa-completion-list
  ;; It'd be good to have Isabelle make this during build
  '("quit" 
    "cd" "use" "use_thy" "time_use" "time_use_thy"
    "Pretty.setdepth" "Pretty.setmargin" "print_depth"
    "show_hyps" "show_types" "show_sorts"
    "print_exn"
    "goal" "goalw" "goalw_cterm" "premises"
    "by" "byev" 
    "result" "uresult"
    "chop" "choplev" "back" "undo"
    "pr" "prlev" "goals_limit"
    "proof_timing"
    "prove_goal" "prove_goalw" "prove_goalw_cterm"
    "push_proof" "pop_proof" "rotate_proof"
    "save_proof" "restore_proof"
    "read" "prin" "printyp"
    "topthm" "getgoal" "gethyps"
    "filter_goal" "compat_goal"

    ;; short cuts - should these be included?
    "ba" "br" "be" "bd" "brs" "bes" "bds"
    "fs" "fr" "fe" "fd" "frs" "fes" "fds"
    "bw" "bws" "ren"

    "resolve_tac" "eresolve_tac" "dresolve_tac" "forward_tac"
    "assume_tac" "eq_assume_tac"
    "match_tac" "ematch_tac" "dmatch_tac"
    "res_inst_tac" "eres_inst_tac" "dres_inst_tac" "forw_inst_tac"
    "rewrite_goals_tac" "rewrite_tac" "fold_goals_tac"
    "fold_goals_tac" "fold_tac"
    "cut_facts_tac" "subgoal_tac"

    ;; short cuts - should these be included?
    "rtac" "etac" "dtac" "atac" "ares_tac" "rewtac"

    ;; In general, I think rules should appear in rule tables, not here.
    "asm_rl" "cut_rl"  

    "flexflex_tac" "rename_tac" "rename_last_tac"
    "Logic.set_rename_prefix" "Logic.auto_rename"

    "compose_tac"

    "biresolve_tac" "bimatch_tac" "subgoals_of_brl" "lessb"
    "head_string" "insert_thm" "delete_thm" "compat_resolve_tac"

    "could_unify" "filter_thms" "filt_resolve_tac"

    ;; probably shouldn't be included:
    "tapply" "Tactic" "PRIMITIVE" "STATE" "SUBGOAL"

    "pause_tac" "print_tac"

    "THEN" "ORELSE" "APPEND" "INTLEAVE"
    "EVERY" "FIRST" "TRY" "REPEAT_DETERM" "REPEAT" "REPEAT1"
    "trace_REPEAT"
    "all_tac" "no_tac"
    "FILTER" "CHANGED" "DEPTH_FIRST" "DEPTH_SOLVE"
    "DEPTH_SOLVE_1" "trace_DEPTH_FIRST"
    "BREADTH_FIRST" "BEST_FIRST" "THEN_BEST_FIRST"
    "trace_BEST_FIRST"
    "COND" "IF_UNSOLVED" "DETERM" 
    
    "SELECT_GOAL" "METAHYPS"

    "ALLGOALS" "TRYALL" "SOMEGOAL" "FIRSTGOAL"
    "REPEAT_SOME" "REPEAT_FIRST" "trace_goalno_tac"

    ;; include primed versions of tacticals?

    "EVERY1" "FIRST1"

    "prth" "prths" "prthq"
    "RSN" "RLN" "RL"

    ;; simplifier
    
    "addsimps" "addeqcongs" "delsimps"
    "setsolver" "setloop" "setmksimps" "setsubgoaler"
    "empty_ss" "merge_ss" "prems_of_ss" "rep_ss"
    "simp_tac" "asm_full_simp_tac" "asm_simp_tac"

    ;; classical prover

    "empty_cs" 
    "addDs" "addEs" "addIs" "addSDs" "addSEs" "addSIs" 
    "print_cs" 
    "rep_claset" "best_tac" "chain_tac" "contr_tac" "eq_mp_tac"
    "fast_tac" "joinrules" "mp_tac" "safe_tac" "safe_step_tac" 
    "slow_best_tac" "slow_tac" "step_tac" "swapify" 
    "swap_res_tac" "inst_step_tac" 
    
    ;; that's it for now!
    ))


;;; =========== string parsing =================

(defconst isa-ML-string-esc 
  "\\\\\\([nt\"\\\\]\\|^[@-_]\\|[0-9][0-9][0-9]\\|[ \t\n]+\\\\\\)"
  "Regular expression matching escape sequence in a string")

(defun isa-inside-string ()
  "Non-nil if point is in an ML string.
The test and relies on the variable isa-instring which is non-nil
between continued string lines."
  (let* ((proc (get-buffer-process (current-buffer)))
	 (boi (marker-position (process-mark proc)))
	 (p   (point)) 
	 (instring isa-instring))
    (save-excursion
      (goto-char boi)
      (if
	  (catch 'giveup
	    ;; If inside a string, we must see the rest of
	    ;; the escape sequence at the start of the line.
	    (if instring
		(if (looking-at "[ \t\n]*\\\\")
		    (goto-char (match-end 0))
		  (throw 'giveup t)))
	    (while (< (point) p)
	      (if instring
		  ;; if in a string, find first double quote
		  ;; or escape character.
		  (skip-chars-forward "^\"\\\\" p)
		;; otherwise, find first double quote
		(skip-chars-forward "^\"" p))
	      (cond (;; If it's a double quote...
		     (eq (char-after (point)) ?\")
		     ;;   toggle instring status and advance.
		     (setq instring (not instring))
		     (forward-char))
		    (;; If we're in a string and it's a complete escape
		     ;; sequence...
		     (and instring
			  (looking-at isa-ML-string-esc)
			  (<= (match-end 0) p))
		     ;;   skip it.
		     (goto-char (match-end 0)))
		    (;; If we're in a string and it's a
		     ;; backslash...
		     (and instring
			  (eq (char-after (point)) ?\\))
		     ;; skip forward over \ and whitespace
		     (forward-char)
		     (skip-chars-forward " \n\t" p)
		     (if (not (eq (point) p))
			 ;; should have reached end of input.
			 (throw 'giveup t)))))
	    nil)
	  (message "String syntax error?"))
      instring)))


;;; ========== Input ==========

(defun isa-send-input ()
  "Automatically add backslashes before sending input with comint-send-input.
If you don't like this behaviour, define RET as comint-send-input instead."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (boi  (marker-position (process-mark proc)))
	 (bol     (save-excursion (comint-bol nil) (point)))
	 (dotest  (<= boi (point)))	; don't bother for copy operations
					; (at the risk of getting confused).
	 (unterm  (and dotest 
		       (progn 
			 (end-of-line)	; comint-eol-on-send=t 
			 (isa-inside-string))))
	 (needsbk (and unterm
		       (not
			(string-match "\\\\[ \t\n]*$" 
				      (buffer-substring bol (point)))))))
    (if needsbk (insert "\\"))
    (comint-send-input)
    (if unterm  (insert "\\"))
    (setq isa-instring unterm)))



;;; ===== Make Startup Hooks =====

(mapcar 
 (function
  (lambda (buffer-name)
    (add-hook 'isa-mode-startup-hook 
	      (` (lambda () (isa-startup-function-for 
			     (quote (, buffer-name))))))))
 isa-possible-associated-buffer-names)



(provide 'isa-mode)

;;; End of isa-mode.el


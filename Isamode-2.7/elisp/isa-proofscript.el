;;; isa-proofscript.el - manipulating Isabelle proof scripts.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-proofscript.el,v 2.6 1997/05/27 22:26:59 da Exp
;;;

(require 'isa-thy-mode)


;;; ========== Regular Expressions ==========

(defconst isa-string-regexp
  "\\(\"[^\"]*\"\\)"
  ;; (doesn't understand escaped quotes inside string)
  "Pattern matching a string (as a group)")

(defconst isa-long-id-regexp
;  "\\(\\(\\w\\|\\_\\)+\\)" works for isa-thy-mode syntax table
  "\\(\\(\\w\\|\\.\\|\\_\\)+\\)"
  "Pattern matching a long identifier (as a group)")

(defconst isa-simple-pat-regexp
  "\\(\\(\\w\\|\\_\\)+\\|_\\)"
  "Pattern matching a simple pattern: id or _")

(defconst isa-semi-regexp
  "\\s-*;\\s-*"
  "Pattern matching optional w/s then semicolon, more w/s.")

;; NB: possibility of spurious w/s at end of group: 
;;    isa-to-semi-regexp, isa-val-pat-regexp

(defconst isa-to-semi-regexp
  ;; has preference to parse strings in text before semicolon,
  ;; thus ignoring semis in strings. Subsequent matches may 
  ;; pick them up, though.
  (concat
   "\\(\\("
         isa-string-regexp
         "\\|[^;]\\)*\\);[ \t]*")
  "Pattern matching text upto a semicolon (group), then semicolon, non-CR w/s")

(defconst isa-to-semi-eol-regexp
  "\\([^;]*\\)\\s-*;[ \t]*$"
  "Pattern matching text upto a semicolon (group), then semicolon, nl")

(defconst isa-val-pat-regexp
  ;; val PAT =
  (concat
   "\\s-*val\\s-+"
   "\\([^=]+\\)=\\s-*"))

(defconst isa-goal-thy-string-regexp
  ;; goal THY "STUFF";
  (concat 
   "\\s-*goal\\s-+"
   isa-long-id-regexp
   "\\s-*"
   isa-string-regexp
   isa-semi-regexp))

(defconst isa-goal-regexp
  ;; goalARGS;
  (concat "\\s-*goal" isa-to-semi-regexp))

(defconst isa-val-goal-regexp
  ;; val PAT = goalARGS;
  (concat isa-val-pat-regexp isa-goal-regexp))

(defconst isa-goal-val-regexp
  ;; goalARGS; val PAT = it;
  (concat		
   isa-goal-regexp
   "\\s-*"
   isa-val-pat-regexp "it"
   isa-semi-regexp))

(defconst isa-result-regexp
  ;; result();
  (concat
   "\\s-*result\\s-*\(\\s-*\)"
   isa-semi-regexp))

(defconst isa-qed-pat-regexp
  ;; qed "PAT";
  (concat
   "\\s-*qed\\s-*\"\\([^\"]+\\)\""
   isa-semi-regexp))

(defconst isa-val-result-regexp
  ;; val PAT = result(); or qed "PAT"
  (concat
   isa-val-pat-regexp
   isa-result-regexp
   "\\|"
   isa-qed-pat-regexp))

(defconst isa-by-tactic-regexp
  ;; by TACTIC;
  (concat "\\s-*by\\s-*" isa-to-semi-regexp))

(defconst isa-expandshorts-regexp-pairs
  (mapcar '(lambda (p)
	     (cons (concat (car p) isa-to-semi-regexp)
		   (cdr p)))

	  '(("ba"   . "by (assume_tac \\1);")
	    ("brs"  . "by (resolve_tac \\1);")
	    ("bes"  . "by (eresolve_tac \\1);")
	    ("bds"  . "by (dresolve_tac \\1);")
	    ("bws"  . "by (rewrite_goals_tac \\1);")
	    ("br"   . "by (rtac \\1);")
	    ("be"   . "by (etac \\1);")
	    ("bd"   . "by (dtac \\1);")
	    ("bw"   . "by (rewtac \\1);")
	    )))

(defconst isa-singleton-regexp
  ;; fooled by commas inside strings
  "\\s-*\\[\\([^],]+\\)\\]"
  "Pattern matching a list with a singleton element (grouped)")

(defconst isa-contractsingles-regexp-pairs
  (mapcar '(lambda (p)
	     (cons (concat (car p) isa-singleton-regexp)
		   (cdr p)))

	  '(("dresolve_tac"      . "dtac \\1")
	    ("eresolve_tac"      . "etac \\1")
	    ("resolve_tac"       . "rtac \\1")
	    ("rewrite_goals_tac" . "rewtac \\1")
	    )))

(defun isa-seqcar (f args)
  (while args
    (funcall f (car args))
    (setq args (cdr args))))

(defun isa-replace-regexp (regexp repl)
  (while (re-search-forward regexp nil t)
    (replace-match repl t nil)))

(defun isa-replace-regexp-pairs (re-pairs)
  (save-excursion
    (let ((start (point)))
      (isa-seqcar
       '(lambda (p)
	  (goto-char start)
	  (while (re-search-forward (car p) nil t)
	    (replace-match (cdr p) t nil)))
       re-pairs))))

(defun isa-expandshorts (beg end)
  "Normalize tactics and commands, like \"expandshorts\" shell-script.
That is, tactic command shorthands (ba,br,etc.) are expanded and long-forms
with singleton arguments (resolve_tac [a],etc.) are contracted."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (isa-replace-regexp-pairs isa-expandshorts-regexp-pairs)
      (isa-replace-regexp-pairs isa-contractsingles-regexp-pairs))))
  

;; not debugged yet:
;; spurious spaces after premise name, catching wrong semicolons
;; (don't use regexps for this), missing first line on [ ] for premises
;; name.
(defun isa-new-batchify (alpha omega)
 "Change common forms of goal...result proofs to use prove_goal.
Understands proofs starting with:
  val PAT = goalARGS;
  goalARGS; val PAT = it;
  goalARGS;
and ending with
  val ID = result();
  result();
or empty.

Constructs a prove_goal batch proof with the form

  val ID = prove_goalARGS
   (fn PAT=>
   [
    TACTICS
   ]);

the TACTICS are first normalized using isa-expandshorts."
  (interactive "*r")
  (save-restriction
    (unwind-protect
	(let (premsname thmname goalargs)
	  (goto-char alpha)
	  (skip-chars-forward " \n\t")
	  (narrow-to-region (point) omega)
	  (isa-find-set-goal-result)			; sets premsname,etc.
	  (isa-expandshorts (point-min) (point-max))
	  (isa-strip-by)
	  (goto-char (point-min))
	  (insert "val ")
	  (insert thmname)
	  (insert " = prove_goal")
	  (insert goalargs)
	  (insert "\n (fn ")
	  (insert premsname)
	  (insert " =>\n")
	  (goto-char (point-max))))))

	    
(defun isa-strip-by ()
  (goto-char (point-min))
; this version attempts to copy non-tactic lines to the end 
;  (while (re-search-forward isa-to-semi-regexp nil t)
;      (goto-char (match-beginning 1))
;      (let ((start (point))  (end   (match-end 1)))
;	(if (looking-at isa-by-tactic-regexp)
;	    (replace-match "\t \\1,\n" t nil)
;	(save-excursion
;	  (goto-char (point-max))
;	  (insert-buffer-substring (current-buffer) start end)
;	  (delete-region start end)))))
 (while (re-search-forward isa-by-tactic-regexp nil t)
   (replace-match "\t \\1,\n" t nil))
 (skip-chars-backward "^,")
 (backward-char)
 (if (looking-at ",")
     (delete-char 1)
   (forward-char))
 (insert "\n\t]);")
 ;; move anything outside proof 
 (goto-char (point-min))
 (insert "\t[\n"))


(defun isa-buffer-substring (start end)
  "Like buffer substring, except white space is stripped from the end."
  ;; ensure there is some non-ws between start and end!
  ;; skip-chars-backward "\\s-" - why not?    
  (while (progn
	   (setq end (1- end))
	   (eq ?- (char-syntax (char-after end)))))
  (buffer-substring start (1+ end)))
	     
(defun isa-find-set-goal-result ()
  ;; Find goal/val of suitable form and delete it
  (goto-char (point-min))
  (cond ((looking-at isa-val-goal-regexp)
	 (setq premsname (isa-buffer-substring (match-beginning 1)
					       (match-end 1)))
	 (setq goalargs  (isa-buffer-substring (match-beginning 2)
					       (match-end 2)))
	 (delete-region (match-beginning 0) (match-end 0)))

  ((looking-at isa-goal-val-regexp)
   (setq goalargs (isa-buffer-substring (match-beginning 1)
					(match-end 1)))
   (setq premsname (isa-buffer-substring (match-beginning 2)
					 (match-end 2)))
   (delete-region (match-beginning 0) (match-end 0)))

  ((looking-at isa-goal-regexp)
   (setq goalargs (isa-buffer-substring (match-beginning 1)
					(match-end 1)))
   (setq premsname "_")
   (delete-region (match-beginning 0) (match-end 0)))

  (t 
   (error "Can't find proof's goal")))

  ;; Look for first result() and delete it
  (goto-char (point-min))
  (cond ((re-search-forward isa-val-result-regexp nil t)
	 (setq thmname (isa-buffer-substring (match-beginning 1)
					     (match-end 1)))
	 (delete-region (match-beginning 0) (match-end 0))
	 ;; ignore anything after result
	 (narrow-to-region (point-min) (match-beginning 0)))

  ((re-search-forward isa-result-regexp nil t)
   (setq thmname "_")
   (delete-region (match-beginning 0) (match-end 0))
   ;; ignore anything after result
   (narrow-to-region (point-min) (match-beginning 0)))

  (t
   (setq thmname "_"))))
    
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:  
;;    ??              originated
;;;   F.Regensburger  improved, added ungoalify
;;;   D.Aspinall      expandshorts, save-restriction, isa-replace-regexp.
;;;		      changed name to "batchify", seems more logical.
;;;

;; some new hacks below to extend functionality
;; without completing implementation of new version.

(defun isa-batchify (alpha omega)
 "Change well-formed goal...result/qed proofs to use prove_goal"
 (interactive "*r")
 (save-restriction
   ;; 0: restrict editing to region and perform expandshorts
   (narrow-to-region alpha omega)
   (isa-expandshorts alpha omega)

   ;; 1: delimit the identifier in "val ID = result()" using ^Q
   (goto-char (point-min))
   (let
       ;; quick hack to get qed in (not clever).
       ((named-result-regexp 
	 "val[ \t\n]+\\([^ \t\n=]+\\)[ \t\n=]+result();[ \t]*$")
	(qed-regexp
	 "qed[ \t\n]*[ \t\n]*\"\\([^\"]+\\)\"[ \t\n]*;$"))
     (cond
      (;; val name=result();
       (save-excursion
	 (re-search-forward named-result-regexp omega t))
       (isa-replace-regexp named-result-regexp "\\1"))
      (;; qed "name";
       (save-excursion
	 (re-search-forward qed-regexp omega t))
       (isa-replace-regexp qed-regexp "\\1"))
      (t   ;; handle result() only case
       (isa-replace-regexp "result();[ \t]*$" "_"))))
   ;; handle no result() case.
   

  ;; 2: replace terminal \";  by  
  (goto-char (point-min))
  (isa-replace-regexp  "\";[ \t]*$" "")

  ;; 3: replace lines "by ...;" with "...,"
  (goto-char (point-min))
  (isa-replace-regexp  "by[ \n\t]*\\([^;]*\\)[ \t\n]*;"  "\t\\1,")

  ; 6: removing the extra commas, those followed by ^Q
  (goto-char (point-min))
  (isa-replace-regexp  ",[ \n\t]*"  "")

  ; 7: transforming goal... to prove_goal...
  (goto-char (point-min))
  (isa-replace-regexp
  "val[ \t\n]+\\([^ \n\t=]+\\)[ \t\n=]+goal\\([^]*\\)
\\([^]*\\)\\([^]*\\)"  
  "qed_goal \"\\4\" \\2\"\n (fn \\1 =>\n\t[\n\\3\n\t]);")

  ;; deal with no val case
  (goto-char (point-min))
  (isa-replace-regexp
   "[ \t]*goal\\([^]*\\)
\\([^]*\\)\\([^]*\\)"  
  "qed_goal \"\\3\" \\1\"\n (fn _ =>\n\t[\n\\2\n\t]);")
  ))


(defun isa-unbatchify (alpha omega)
 "Change well-formed qed_goal or prove_goal proofs to goal...qed"
  (interactive "*r")
  (undo-boundary)
  (save-restriction
    ;; 0: restrict editing to region
    (narrow-to-region alpha omega)

    ;; 1: insert delimiter ID 
    (goto-char (point-min))
    (isa-replace-regexp  
     "[ \t]*val[ \t]+\\([^ \t\n=]+\\)[ \t\n=]+prove_goal" "\\1")
    ;; Also handle qed_goal function
    (isa-replace-regexp  
     "[ \t]*qed_goal[ \t]*\"\\([^\"]+\\)\"" "\\1")

    ;; 2: insertt delimiter ARGS  PAT  and  before first command   
    (goto-char (point-min))
    (isa-replace-regexp  
     "[ \n\t]*(fn[ \t]+\\([^=]+\\)=>[^(]*" "\\1\n")

    ;; 3: shift  over all commands
    ;; Note: only one line per command
    (goto-char (point-max))
    (while (not (equal (point) (point-min)))
      (goto-char (point-min))
      (isa-replace-regexp  
       "[ \t]*\\(.*\\),[ \t]*\n" "by \\1;\n"))
    
    ;; 4: fix last 
    (goto-char (point-min))
    (isa-replace-regexp  
     "[ \t]*\\(.*\\)[ \t\n]*\][ \t\n]*)[ \t\n]*;" "by \\1;")

    ;; 5: arange new val Pat = goal .. 
    (goto-char (point-min))
    (isa-replace-regexp  
     "\\([^]*\\)\\([^]*\\)\\([^]*\\)\n\\([^]*\\)"
     "val \\3= goal\\2;\n\\4\nqed \"\\1\";")
    ))
		 

(defun isa-listener-batchify-proof (&optional arg)
  "Copy and batchify the last (or ARG'th last) proof from the listener."
  (interactive)
  (set-mark (point))
  (insert (listener-last-proof arg))
  (isa-batchify (region-beginning) (region-end)))






(provide 'isa-proofscript)

;;; End of isa-proofscript.el

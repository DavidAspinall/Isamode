;;; isa-proofstate.el - Buffer holding proof state in Isabelle mode
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-proofstate.el,v 2.6 1997/05/27 22:26:59 da Exp
;;;


;;; DESIRABLE CHANGES:
;;;  - tidy up!
;;;  - use a ring of previous proofstates for paging backwards,
;;;    to speed up by avoiding querying process.
;;;  - attempt to increase goals_limit on movement off the 
;;;    bottom of subgoal list?

;;; sort out updating/subgoal movement. Apt to jump to top
;;; of list rather than bottom - see proofstate-refresh.
;;; (something different done in proofstate-update...)

(require 'isa-mode)


;;; ========== Fonts for proofstate display ==========

(isa-make-face 'proofstateGoal
	   "Face used for goal in proofstate display")
(isa-make-face 'proofstateSubgoalNumber
	   "Face used for subgoal number in proofstate display.")
	  
(defvar proofstate-mode-font-lock-keywords
  '("^Level [0-9]+\\|^ [0-9]+\\."))


;;; ============ Proofstate Mode ============

(defvar proofstate-mode-map nil)


(or proofstate-mode-map
  (let ((map (make-keymap)))
      (suppress-keymap map)
      ;; Disable the mouse bindings because some folk
      ;; want to use mouse do cut and paste from proofstate
      ;; buffer.
      ;;(isa-clear-mouse-bindings map)
      ;;(isa-define-key map 'button1 'proofstate-motion)
      (define-key map "i" 'isa-select-isa-buffer)
      (define-key map "h" 'describe-mode)
      (define-key map "\C-f" 'proofstate-next-level)
      (define-key map "\C-b" 'proofstate-previous-level)
      (define-key map "\C-n" 'proofstate-next-subgoal)
      (define-key map "\C-p" 'proofstate-previous-subgoal)
      (isa-define-key map '(right) 'proofstate-next-level)
      (isa-define-key map '(left)  'proofstate-previous-level)
      (isa-define-key map '(down)  'proofstate-next-subgoal)
      (isa-define-key map '(up)    'proofstate-previous-subgoal)
      (define-key map " " 'proofstate-refresh)
      (define-key map "\C-m" 'proofstate-resize-and-refresh)
      (setq proofstate-mode-map map)))


; (defvar proofstate-level-string "Level 0")

(defun proofstate-mode ()
  "Major mode for Isabelle Proof-State buffers.

The cursor keys move up and down the subgoal list and left and right
through previous levels.  The subgoal that the cursor appears on will
be used in the menu tactic commands (i.e. isa-assume_tac, etc).

\\{proofstate-mode-map}"
  (setq buffer-read-only t)
  (setq major-mode 'proofstate-mode)
  (set-syntax-table isa-thy-mode-syntax-table)
; spoils C-xC-b and gives interleaving o/p problem
;  (setq mode-name 'proofstate-level-string)  
  (setq mode-name "Proof-State")
  (setq mode-line-buffer-identification '("%17b h=help"))
  (use-local-map proofstate-mode-map)
  (put 'proofstate-mode 'mode-class 'special)
  (isa-remove-menubar-if-multiple-frame-mode)
  ; Disable mouse follower in proofstate so that can use mouse
  ; for cut and paste.
  ;(isa-set-mouse-follower 'proofstate-motion)
  (setq font-lock-keywords proofstate-mode-font-lock-keywords)
  (funcall 'isa-font-lock-set-defaults)	; run font-lock hook 
  )


(defun proofstate ()
  "Activate a proofstate buffer for the Isabelle process in the current buffer."
  (interactive)
  (let*
      ((proofstate-buffer
	;; Make proofstate buffer
	(save-excursion
	  (set-buffer (isa-create-new-associated
		       'proofstate))
	  (proofstate-mode)
	  (proofstate-refresh)		        
	  (current-buffer)))
       (subgoal-tracker (proofstate-track-denoted-subgoal proofstate-buffer)))

    ;; Set weeder fn and subgoal change hook
    (setq isa-weed-output-function 'proofstate-weeder)
    (add-hook 'isa-set-denoted-subgoal-hook subgoal-tracker)

    ;; Highlight buffer 
    (let ((frm (get-frame-for-buffer-noselect proofstate-buffer)))
; See comment in isa-ruletable.el      
;      (or (face-differs-from-default-p 'proofstateSubgoalNumber frm)
;	  (copy-face proofstateSubgoalNumber-default
;		     'proofstateSubgoalNumber frm))
;      (or (face-differs-from-default-p 'proofstateGoal frm)
;	  (copy-face proofstateGoal-default
;		     'proofstateGoal frm))
      (proofstate-set-sizes (frame-root-window frm)))
    
    ;; 
    (isa-display-buffer proofstate-buffer t)))		; show it.

(defun proofstate-set-sizes (win)
  "Set Isabelle's printer margin and goals limit according to window size"
  (let* ((marg   (concat "Pretty.setmargin "
			 (isa-int-to-ml (- (window-width win) 2))
			 ";"))
	 (levl   (concat "goals_limit := "
			 (isa-int-to-ml (- (window-height win) 4))
			 ";")))
    (isa-send-string-catch-result marg)
    (isa-send-string-catch-result levl)))
	 
(defun proofstate-refresh (&optional level)
  "Refresh proofstate buffer - display current level.
With arg LEVEL, display that level of the proof."
  (interactive "P")
  ;; allow interactive use w/o arg.
  (setq level (and level (prefix-numeric-value level)))	
  (let* ((proc     (get-buffer-process isa-buffer))
	 (cmd      (if level 
		       (concat "prlev (" (isa-int-to-ml level) ");")
		     "pr();"))
	 (prf-text (isa-send-string-catch-result proc cmd))
	 (sg       (save-excursion (set-buffer isa-buffer) isa-denoted-subgoal)))
    (proofstate-strip-text prf-text)
    ;; Try to set marked subgoal to isa-denoted-subgoal, or else 
    ;; the last subgoal.
    (if (proofstate-goto-subgoal sg t)
	nil
      (if (proofstate-goto-subgoal 1 t)
	  (save-excursion
	    (set-buffer isa-buffer)
	    (setq isa-denoted-subgoal 1)))))
  (sit-for 0))

(defun proofstate-resize-and-refresh ()
"Refresh proofstate buffer and adjust Isabelle printer margin to window width."
  (interactive)
  (let ((win (get-buffer-window (current-buffer))))
    (save-excursion
      (set-buffer isa-buffer)
      (proofstate-set-sizes win)))
  (proofstate-refresh))



;;; ========== Updating the proofstate buffer ==========

(defun proofstate-weeder (outbuf)
  "Weeder for proofstates."				; Assume in isabelle buffer.
  (let ((proofstate-buffer
	 (car-safe (isa-find-associated 'proofstate))))
    (if (isa-buffer-active proofstate-buffer)	        ; if active, 
	(proofstate-weed-text outbuf proofstate-buffer)	; strip proof text.
							; If buffer killed, reset
      (setq isa-weed-output-function                    ; weeder and call it. 
	    'isa-weed-output-default)
      (isa-weed-output-default outbuf))
    t))							; Flag for showing prompt.

(defconst proofstate-proofstart-regexp "^Level [0-9]+"
  "Regular expression to match the start of proof state displays.")

;; NB:
;; Old value for proofstate-proofend-regexp was "^\\(val\\|$\\)"
;; Explanation: empty line catches "val _ =" in Poly/ML.
;; This is still fooled by "val _ =" in NJ/ML, which
;; just comes back with a prompt.
;; Removed empty line because it results in stripping of
;; type and sort information.
(defconst proofstate-proofend-regexp   "^\\(val\\)"
  "Regular expression to match the end of proof state displays.")


(defun proofstate-weed-text (outbuf proofstate-buffer)
  "Used during output filtering to remove proof state from output."
  (save-excursion
    (let ((data (match-data))
	  (ibuf (current-buffer)))
      (set-buffer outbuf)
      (goto-char (point-min))
      (unwind-protect					; strip proof text
	  (if (re-search-forward proofstate-proofstart-regexp nil t)
	      (let ((proof-start (match-beginning 0)))
		(if (re-search-forward proofstate-proofend-regexp nil t)
		    (let* ((proof-end (match-beginning 0))
			   (proof     (buffer-substring proof-start 
							proof-end)))
		      (set-buffer ibuf)
		      (proofstate-update-buffer proof proofstate-buffer)
		      (set-buffer outbuf)
		      (delete-region proof-start proof-end)))))
	(store-match-data data)))))

;;; This really needs a better way!  We've just stripped the text from
;;; the output so as avoid showing the result - now we stick it back
;;; in to cheat and use the weeder function above!  Better would be
;;; some way of disabling prompt outputs at some points (and
;;; synchonising input/output).

(defun proofstate-strip-text (text)
  "Strip proof state from TEXT and put it in current proofstate buffer."
  (save-excursion
    (let ((pbuf (current-buffer))
	  (opbuf (save-excursion
		   (set-buffer isa-buffer)
		   isa-output-buf)))
      (set-buffer opbuf)
      (goto-char (point-max))
      (let ((pm (point)))
	(insert text)
	(proofstate-weed-text opbuf pbuf)
	(delete-region pm (point-max))))))


(defun proofstate-update-buffer (newstate proofstate-buffer)
  "Reset buffer PROOFSTATE-BUFFER with NEWSTATE."
  (let (csb (bf (current-buffer)))
    (set-buffer proofstate-buffer)
    (let ((buffer-read-only nil))
      (setq csb (proofstate-current-subgoal))
      (erase-buffer)
      (insert newstate))
    (proofstate-make-extents)
;; removed because of possibility of output interleaving.
;; really need to trigger this at end of process o/p somehow.
;    ;; Set mode-line level indicator
;    (let ((lev (proofstate-current-level)))
;      (setq proofstate-level-string
;	    (concat "Level " 
;		   (if lev (int-to-string lev)
;		     "?"))))
    ;; Try and preserve point position in subgoal list
    ;; (It may fail if you apply a tactic to a subgoal above the cursor).
    (proofstate-goto-subgoal-near csb)
    (isa-update-window-point)
    (isa-display-buffer (current-buffer))
    (set-buffer bf)))

(defun proofstate-make-extents ()
  "Do highlighting in proofstate buffer.  Only for Emacs 19's in window sys."
  (goto-line 2)
  (let ((goalstart (point)) extent)
    (if (re-search-forward "^ 1\\.\\|^No subgoals" nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (setq extent (make-extent goalstart (1- (point))))
	  (set-extent-face extent 'proofstateGoal)
	  (while (re-search-forward "^ [0-9]+\\. " nil t)
	    (setq extent (make-extent (match-beginning 0) (match-end 0)))
	    (set-extent-face extent 'proofstateSubgoalNumber))))))



;;; ========== Browsing levels ==========

(defun proofstate-displayed-level ()
  (save-excursion
    (goto-char 0)
    (cond ((looking-at "Level [0-9]")
	   (goto-char 7)	
	   (skip-chars-forward "0-9")
	   (string-to-int (buffer-substring 6 (point))))
	  (t (proofstate-current-level)))))


(defun proofstate-current-level ()
  (let* ((proc (get-buffer-process isa-buffer))
	 (cmd  
"let val a = !goals_limit in (goals_limit:=0;pr();goals_limit:=a) end;")
	 ; my other way of doing this was "choplev ~1", but raising an
	 ; exception might not be so nice.

	 ;; expects "Level x"
	 (txt  (isa-send-string-catch-result proc cmd)))
    (string-to-int (substring txt 6))))

(defun proofstate-next-level (&optional arg)
  "Display next level of proof.
With arg, skip that many levels forwards."
  (interactive "p")
  (proofstate-refresh 
   (min (+ arg (proofstate-displayed-level))
	(proofstate-current-level))))

(defun proofstate-previous-level (&optional arg)
  "Display previous level of proof.
With ARG, skip that many levels backwards."
  (interactive "p")
  (proofstate-refresh
   (max (- (proofstate-displayed-level) arg) 0)))




;;; ====== Movement through subgoals ======


(defconst proofstate-subgoal-regexp "^ [0-9]+\\.")

(defun proofstate-previous-subgoal ()
  "Move point to previous subgoal in list.
This changes the denoted subgoal accordingly."
  (interactive)
  (re-search-backward proofstate-subgoal-regexp nil t)
  (proofstate-set-denoted-subgoal))

(defun proofstate-next-subgoal ()
  "Move point to next subgoal in list.
This changes the denoted subgoal accordingly."
  (interactive)
  (forward-char)
  (if (re-search-forward proofstate-subgoal-regexp nil t)
      (goto-char (match-beginning 0))
    (backward-char))
  (proofstate-set-denoted-subgoal))

(defun proofstate-goto-subgoal (subgoal &optional noerror)
  "Goto subgoal number SUBGOAL, error if impossible unless NOERROR non-nil.
Return t on success."
  (interactive "n")
  (cond
   ((save-excursion
      (goto-char 0)
      (re-search-forward (concat "^ " (int-to-string subgoal) "\\.") nil t))
    (goto-char (match-beginning 0))
    (proofstate-set-denoted-subgoal)
    t)
   (noerror nil)
   (t	    (error "Can't see subgoal %d" subgoal))))

(defun proofstate-goto-subgoal-near (subgoal)
  "Try to goto subgoal SUBGOAL, if can't choose nearest one."
  (cond ((and subgoal (proofstate-goto-subgoal subgoal t)) subgoal)
	(subgoal 
	 ;; can't find same number, so go to last
	 (goto-char (point-max))
	 (or (proofstate-previous-subgoal)
	     ;; or top if no subgoals
	     (goto-char 0)))
	(t
	 ;; If no previous position, go to first (if any).
	 (goto-char 0)
	 (proofstate-next-subgoal))))

(defun proofstate-current-subgoal ()
  (if (looking-at " [0-9]+")
      (save-excursion
	(forward-char)
	(string-to-int (buffer-substring 
			(point) 
			(save-excursion
			  (skip-chars-forward "0-9") 
			  (point)))))
    1))

(defun proofstate-set-denoted-subgoal ()
  (let ((sg (proofstate-current-subgoal))
	(bf (current-buffer)))
    (set-buffer isa-buffer)
    (setq isa-denoted-subgoal sg)
;    (message "Working subgoal: %d" isa-denoted-subgoal)
    (set-buffer bf)))

;; Really there should be just one variable controlling the
;; denoted subgoal: we should trash the functions that determine
;; it by looking at the proofstate buffer, and use 
;; isa-denoted-subgoal in the Isabelle buffer instead.
;; Current duplicity is a bit of a mess.

(defun proofstate-track-denoted-subgoal (pf-buf)
  "For making values for isa-set-denoted-subgoal-hook."
  (` (lambda ()
	(if (isa-buffer-active (, pf-buf))
	      (let ((sg isa-denoted-subgoal))
		(save-excursion
		  (set-buffer (, pf-buf))
		  (proofstate-goto-subgoal-near sg)
		  (isa-update-window-point)))))))


(defun proofstate-motion (event)
  "Follow the subgoal pointed to by the mouse."
  (interactive "@e")
  (if (and (eq major-mode 'proofstate-mode)
	   (setq point (event-point event)))
      (progn
	(setq oldpoint (point))
	(goto-char point)
	(beginning-of-line)
	(if (looking-at " [0-9]+.") 
	    (progn
	      (message "Working subgoal: %s"
		       (int-to-string (proofstate-current-subgoal)))
	      (proofstate-set-denoted-subgoal))
	  (goto-char oldpoint)
	  (message "")))))


(provide 'isa-proofstate)

;;; End of isa-proofstate.el

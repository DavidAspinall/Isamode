;;; isa-proc.el - Process handling for Isabelle Mode.
;;; 
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-proc.el,v 2.6 1997/05/27 22:26:59 da Exp
;;;

;;; DESIRABLE CHANGES:
;;;  - it would be nice to use the new output processing facilities
;;;    provided by comint, and avoid the hidden buffer.
;;;    Would this result in screen glitches?
;;;  - tracing parsing is a bit of a mess.
;;;  - probably merge this file with isa-mode.


(require 'isa-load)
(require 'isa-display)
(require 'comint)

;;; ========== Text insertion ==========

(defvar isa-denoted-subgoal 1
  "The subgoal to insert as part of tactic command insertion.")

(defun isa-inc-denoted-subgoal ()
  "Increment isa-denoted-subgoal."
  (interactive)
  (setq isa-denoted-subgoal (1+ isa-denoted-subgoal))
  (run-hooks 'isa-set-denoted-subgoal-hook)
  (message "Working subgoal: %d" isa-denoted-subgoal))

(defun isa-dec-denoted-subgoal ()
  "Decrement isa-denoted-subgoal."
  (interactive)
  (setq isa-denoted-subgoal (max 1 (1- isa-denoted-subgoal)))
  (run-hooks 'isa-set-denoted-subgoal-hook)
  (message "Working subgoal: %d" isa-denoted-subgoal))

(defun isa-insert-format (text &optional pointtext)
  "Format TEXT as if for isa-insert.
TEXT may include these special characters:
  %l  - insert the value of isa-logic-name
  %s  - insert the value returned by isa-current-subgoal
  %p  - substitute POINTTEXT here (at most one %p)."
  (let ((str text))
    (while (string-match "%l" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			isa-logic-name
			(substring str (match-end 0)))))
    (while (string-match "%s" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(int-to-string
			 (if (boundp 'isa-denoted-subgoal)
			     isa-denoted-subgoal
			   1))
			(substring str (match-end 0)))))
    (if (and pointtext (string-match "%p" str))
      (setq str (concat (substring str 0 (match-beginning 0))
			pointtext 
			(substring str (match-end 0)))))))

(defun isa-insert (text)
  "Insert TEXT into the Isabelle buffer, at the process mark.
TEXT may include these special characters:
  %l  - insert the value of isa-logic-name
  %s  - insert the value returned by isa-current-subgoal
  %n  - insert newline and send text so far
  %p  - place the point here after input
Any other %-prefixed character inserts itself."
  (isa-delete-pending-input)
  (let ((i 0) pos acc)
    (while (< i (length text))
      (let ((ch (elt text i)))
	(if (not (eq ch ?%))
	    (setq acc (concat acc (char-to-string ch)))
	  (setq i (1+ i))
	  (setq ch (elt text i))
	  (cond ((eq ch ?l)  
		 (setq acc (concat acc isa-logic-name)))
		((eq ch ?s)  
		 (setq acc 
		       (concat acc 
			       (int-to-string
				(if (boundp 'isa-denoted-subgoal)
				    isa-denoted-subgoal
				  1)))))
		((eq ch ?n)
		 (if acc (insert acc))
		 (setq acc nil)
		 (comint-send-input))
		((eq ch ?p)  
		 (if acc (insert acc))
		 (setq acc nil)
		 (setq pos (point)))
		(t (setq acc (concat acc (char-to-string ch)))))))
      (setq i (1+ i)))
    (if acc (insert acc))
    (if pos (goto-char pos))))

(defun isa-insert-substitute (text subst)
  "Insert TEXT into the Isabelle buffer, inserting SUBST at position of point
after insertion, and then send to process."
  (isa-insert text)
  (insert subst)
  (comint-send-input)
  (goto-char (point-max))
  (isa-update-window-point))

(defun isa-delete-pending-input ()
  "Delete any pending input at the process mark, and leave point there."
  (let ((proc (get-buffer-process (current-buffer)))
	(isam (eq major-mode 'isa-mode)))
    (if (not (and proc isam))
	(error "Must be in active Isabelle buffer")
      (let* ((pmark      (process-mark proc))
	     (pmark-pos  (marker-position pmark)))
	(delete-region pmark-pos (point-max))
	(goto-char pmark-pos)))))

(defun isa-insert-input (text) 
  "Insert TEXT at the process mark, deleting any text there already."
  (let ((proc (get-buffer-process (current-buffer)))
	(isam (eq major-mode 'isa-mode)))
    (if (not (and proc isam))
	(error "Must be in active Isabelle buffer")
      (let* ((pmark      (process-mark proc))
	     (pmark-pos  (marker-position pmark)))
	(delete-region pmark-pos (point-max))
	(goto-char pmark-pos)
	(insert text)))))

(defun isa-insert-ret (text)
  "Insert TEXT and send it to the process."
  (isa-insert-input text)
  (comint-send-input))


;;; =============== Input filtering ===============

(defun isa-sentinel (proc str)				; Could kill off corresponding
  "Process sentinel for Isabelle processes."		; proofstate, listener?
  (if (isa-buffer-active (process-buffer proc))		; (listener, at least might still
      (save-excursion					;  be wanted).
	(set-buffer (process-buffer proc))
	(insert (concat "\n" 
			(save-excursion
			  (set-buffer isa-output-buf)
			  (buffer-substring (point-min) (point-max)))
			"\nIsabelle process " 
			(buffer-name) " " str)))
    (message (concat "Isabelle process " (process-name proc) ": " 
		     (substring str 0 (- (length str) 1))))))


(defconst isa-cd-regexp "^cd *\"\\(.*\\)\""
  "Regexp to match cd commands in Isabelle.")

(defun isa-synchronise-wd ()
  "Synchronise directory of Emacs buffer with Isabelle process directory"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (cmd  "writeln (pwd());")
	 (op   (isa-send-string-catch-result proc cmd)))
    (if (string-match "\n" op)
	(isa-cd (substring op  0 (match-beginning 0))))))

(defun isa-cd (dir)
  (condition-case nil
      (cd (if (zerop (length dir)) "" ; not (getenv "HOME") - cd "" has no effect.
	    (substitute-in-file-name dir)))
    (error (message "Couldn't cd."))))

;; very simplistic directory tracking.
(defun isa-track-cd-and-use  (str)
  (if (string-match isa-cd-regexp str)
      (isa-cd (substring str (match-beginning 1) (match-end 1)))))
  ;; watching for "use"
;  (if (string-match isa-use-regexp str)
;      (or isa-in-use 
;	  (setq isa-in-use

(defun isa-send-string (proc str)                 
  (comint-simple-send proc str))


;;; =============== Output filtering ===============


(defvar isa-send-output-fn 'comint-output-filter
  "Function to send process output.")

(defvar isa-weed-output-function 'isa-weed-output-default
  "Output process filter for Isabelle buffer.")

(defun isa-weed-output-default (buf)
  "This default weeder function leaves output buffer untouched"
  t)

(defvar isa-text-spill-size 2000
  "Maximum size of between-prompt output to accummulate.")

(defvar isa-last-trace-start nil
  "Position of last trace output in Isabelle buffer.")

(defvar isa-trace-count nil
  "Level number of current trace.")

(defvar isa-big-output nil
  "Flag indicating whether we are in a period of large output, 
when output processing is by-passed and output is sent directly to
the Isabelle buffer.  This is so that long output from \"use\" and
similar is not hidden from the user until it terminates.")


(defun isa-filter (proc str)
  "Output filter for Isabelle processes."
;; This is supposed to be a superset of comint-output-filter.
  (let* ((stat   (process-status proc))
	 (isabuf (process-buffer proc))
	 (outbuf (save-excursion (set-buffer isabuf) isa-output-buf)))
  (cond 
   ;; A running process in an active buffer (normal case). 
   ((and (isa-buffer-active isabuf) (isa-buffer-active outbuf) (eq stat 'run))
    (save-excursion
      (set-buffer outbuf)
      (goto-char (point-max))
      (insert str)

      ;; Is the text accumulated so far in the output buffer bigger 
      ;; than the maximum allowed into the filters?
      (if (>= (- (point-max) (point-min)) isa-text-spill-size)
	  ;; If so, set the big output flag and output directly
	  ;; to the nearest previous newline.
	  (progn
	    (goto-char (point-max))
	    (skip-chars-backward "^\n")
	    (if (> (point) (point-min))
		(isa-dispatch-region proc (point-min) (point))
	      (isa-dispatch-region proc (point-min) (point-max)))
	    (setq isa-big-output t)))

      (let ((data (match-data))
	    (pat  (concat "\\(" isa-prompt-pattern "\\)")) ; normal prompts
	    (tpat "\\*\\* .*"))				; tracing prompt
	(unwind-protect
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward pat nil t)
		(let* ((prompt-start (match-beginning 1))
		       (prompt-end (match-end 1))
		       (prompt (buffer-substring prompt-start prompt-end)))

		  (if isa-big-output
		      ;; If we're in a phase of big output, it's terminated
		      ;; by this prompt (and just output the preceding text
		      ;; followed by the prompt).
		      (progn
			(isa-dispatch-region proc (point-min) prompt-end)
			(setq isa-big-output nil))
		    
		    ;; Otherwise, call the filtering functions...

		    (delete-region prompt-start prompt-end)     ; delete the prompt.
		    (narrow-to-region (point-min) prompt-start) 
		    (set-buffer isabuf)
		    
		    (cond 
		     ;; Is tracing in progress?
		     ((and isa-roll-tracing (string-match tpat prompt))
;		      (debug)
		      (isa-tracing-roller proc outbuf prompt)
		      (set-buffer outbuf)
		      (isa-dispatch-region proc (point-min) (point-max)))

		     ;; If not, call the weeder, conditionally displaying prompt
		     (t
		      (setq isa-last-trace-start nil)
		      (let ((showp (funcall isa-weed-output-function outbuf)))
			(set-buffer outbuf)
			(isa-dispatch-region proc (point-min) (point-max))
			(if showp
			    (funcall isa-send-output-fn proc prompt)))))
		    
		    (widen)))

		(sit-for 0)))				; update display
	  (store-match-data data)))

      ;; If we're in a phase of big output, we can flush
      ;; anything that's accumulated so far without a prompt.
      (if isa-big-output
	  (isa-dispatch-region proc (point-min) (point-max)))))

   ;; Process status - not runnning
   ((and (isa-buffer-active isabuf) (isa-buffer-active outbuf))
    (save-excursion
      (set-buffer outbuf)
      (funcall isa-send-output-fn proc (buffer-string))
      (delete-region (point-min) (point-max))
      (funcall isa-send-output-fn proc str)))
   
   ;; Somehow output buffer has been killed - re-initialise
   ((isa-buffer-active isabuf)
    (save-excursion
      (set-buffer isabuf)
      (isa-mode-process-init)))

   ;; Isabelle buffer killed
   (t (message str)))))

(defun isa-dispatch-region (proc b e)
  (cond ((> e b)
	 (funcall isa-send-output-fn proc 
		  (buffer-substring b e))
	 (delete-region b e))))


(defun isa-tracing-roller (proc outbuf prompt)
  "Roll tracing output - replace previous output by new."
  (save-excursion
    (if isa-last-trace-start
	(let ((pmark (process-mark proc)))
	  ;; increment level counter if input was RET
	  (goto-char isa-last-trace-start)
	  (if (looking-at "** Trace")
	      (setq isa-trace-count
		    (1+ isa-trace-count)))
	  ;; delete previous output
	  (delete-region 
	   isa-last-trace-start
	   (marker-position pmark))
	  (goto-char pmark)
	  (set-marker pmark (point))
	  (set-marker comint-last-input-start (point))

	  ;; Now work in output buffer.
	  (let ((count isa-trace-count))
	    (set-buffer outbuf)
	    (goto-char (point-min))
	    (isa-munge-tracing-output prompt count (not (looking-at "Type")))))

      ;; A new tracing run, initialise buffer position record
      ;; and level count.
      (setq isa-last-trace-start 
	    (marker-position comint-last-input-end))
      (setq isa-trace-count 1)
      (set-buffer outbuf)
      (isa-munge-tracing-output prompt 1 t))))

(defun isa-munge-tracing-output (prompt count prefix-flag)
  (cond (prefix-flag 
	 (goto-char (point-min))
	 (insert (concat "** Trace, level "
			 (int-to-string count)
			 " **\n"))))
  (goto-char (point-max))
  (insert prompt)
  (narrow-to-region (point-min) (point)))

;;; needs some work!
(defun isa-do-and-catch-result (proc f &rest args)
  ;; any pending output first which had better go through old weeder
  ;; vague attempt to flush
  (if (accept-process-output)
      (while (accept-process-output proc 0 1000)))
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((old-weed-output isa-weed-output-function)
	  catch)
      (setq isa-weed-output-function 
	    '(lambda (buf) 
	       (set-buffer buf)
	       (setq catch (buffer-string))
	       (delete-region (point-min) (point-max))
	       nil))
      (apply f args)
      (while (not catch)
	(accept-process-output proc)) ;	(sit-for 0))
      (setq isa-weed-output-function old-weed-output)
      catch)))
  
(defun isa-send-string-catch-result (proc-or-str &optional optstr)
  "Send STR to Isabelle process PROC and return output (less prompts)."
  (let
      ((proc   (if optstr proc-or-str (get-buffer-process (current-buffer))))
       (str    (or optstr proc-or-str)))
    (isa-do-and-catch-result proc 'isa-send-string proc str)))



;;; ============ Setting up Processes ============

(defun isa-mode-process-init ()
  "Initialise hidden output buffer for current Isabelle buffer."
  (setq isa-output-buf (get-buffer-create	        ; Allocate output buffer
		    (concat " " (buffer-name)
			    "-*Output*")))		; - name starts with space.
  (save-excursion
    (set-buffer isa-output-buf)
    (erase-buffer)					; Clear output buffer.
    (make-local-variable 'isa-big-output)
    (setq isa-big-output nil)) 				; not in big output yet.

  (make-local-variable 'isa-last-trace-start)
  (make-local-variable 'isa-trace-count)
  (setq isa-last-trace-start nil)			; not in tracing.
  (let ((proc  
	 (get-buffer-process (current-buffer)))) 
    (set-process-filter proc 'isa-filter)		; Set process filter
    (set-process-sentinel proc 'isa-sentinel))		; and sentinel.
  )


;;; =============== Interruption ===============

(defun isa-interrupt ()
  "Interrupt the process of the current buffer"
  (interactive)
  (interrupt-process)
  (message "Interrupt"))

(defun isa-hidden-interrupt ()
  "Interrupt the process without outputting to the buffer"
  (interactive)
  (isa-do-and-catch-result (get-buffer-process (current-buffer))
			   'interrupt-process))


;;; ========== Miscellany ==========

(defun isa-int-to-ml (n)
  (cond ((natnump n)  (int-to-string n))
	(t            (concat "~" (int-to-string (- n))))))
	
(defun isa-bool-to-ml (b)
  (cond (b    "true")
	(t    "false")))


(provide 'isa-proc)

;;; End of isa-proc.el


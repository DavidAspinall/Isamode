;;; isa-listener.el - Listener minor mode for ML buffers.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-listener.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;

(require 'isa-mode)
(require 'isa-thy-mode)

;;; TO DO:
;;;  More key bindings, better undo, menus, etc.

;;; ===== Listener dedicated buffer =====

(defvar listener-base-mode 
  (if isa-use-sml-mode 'sml-mode 'fundamental-mode)
  "*Base mode for dedicated listener buffers.")

(defun listener ()
  "Activate a dedicated listener buffer for this buffer's Isabelle process."
  (interactive)
  (let ((isabuf (current-buffer)))
    (isa-display-buffer					; create and display.
     (save-excursion
       (set-buffer (isa-create-new-associated 
		    'listener))
       (erase-buffer)
       (listener-mode)
       (listener-minor-mode t isabuf)
       (current-buffer)) t)
    ))

(defun listener-mode ()
  "Major mode for dedicated listener buffers.
This is merely listener-base-mode renamed."
  (interactive)
  (funcall listener-base-mode)
  (setq major-mode 'listener-mode))
  

;;; ========== Listener minor mode ==========

(defvar listener-minor-mode nil
  "Non-nil if listening from an Isabelle buffer is activated for this buffer.")
(make-variable-buffer-local 'listener-minor-mode)
(put 'listener-minor-mode 'permanent-local t)
(or (assq 'listener-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(listener-minor-mode " Listener")))))

(defvar listener-minor-mode-map nil)
(if listener-minor-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" 'listener-use-line)
    (define-key map "\C-c\C-n" 'listener-use-line)
    (define-key map "\C-c\C-u" 'listener-undo)
    (define-key map "\C-c\C-p" 'listener-undo)
    (define-key map "\C-c\C-f" 'isa-forward-interactive-line)
    (define-key map "\C-c\C-b" 'isa-backward-interactive-line)
    (define-key map "\C-c\C-c" 'listener-minor-mode)
    (define-key map "\C-c\C-y" 'listener-yank-and-use)
    (setq listener-minor-mode-map map))
)

(or (assq 'listener-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'listener-minor-mode listener-minor-mode-map)
		minor-mode-map-alist))
)



(defvar listener-buffers nil
  "A list of buffers which are listeners for a particular Isabelle buffer.")
(make-variable-buffer-local 'listener-buffers)
(put 'listener-buffers 'permanent-local t)

(defun listener-minor-mode (&optional arg isabuf)
  "Toggle Listener minor mode.
With arg, turn Listener minor mode on if arg is positive, off otherwise.
Listener minor mode allows convenient communication with an Isabelle process.
Lines typed into the Isabelle session will be copied into this buffer
at the current point.
\\<listener-minor-mode-map>
You can also use \\[listener-use-line] to send the current line to Isabelle,
\\[listener-undo] to send an undo command to Isabelle and move backwards.
The movement commands alone are \\[isa-forward-interactive-line] and \\[isa-backward-interactive-line].

If you have killed some text, \\[listener-yank-and-use] will copy it
into this buffer and also send it to the Isabelle process.

Finally, you can use \\[listener-minor-mode] to turn off listening
(and listener-minor-mode)."
  (interactive "P")
  (setq listener-minor-mode
	(if (null arg) (not listener-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq isabuf
	(or isabuf
	    (car-safe (isa-find-buffers-in-mode 'isa-mode))))
  (let* ((lisbuf (current-buffer))
	 (killfn (` (lambda ()
		      (if (isa-buffer-active (, isabuf))
		       (save-excursion
			(set-buffer (, isabuf))
			(setq listener-buffers 
			      (delq (, lisbuf) listener-buffers))
			(if (null listener-buffers)
			    (remove-hook 'comint-input-filter-functions 
					 'listener-copy))))))))
    (cond
      ;; Turn on listening...
     ((and listener-minor-mode (isa-buffer-active isabuf))
      (setq isa-buffer isabuf)
      (make-local-variable 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook killfn)
      (save-excursion
	(set-buffer isabuf)
	(or (memq lisbuf listener-buffers) 
	    (setq listener-buffers (cons lisbuf listener-buffers)))
	(add-hook 'comint-input-filter-functions
		  'listener-copy)
	(run-hooks 'listener-minor-mode-hook)))
      ;; Turn off listening...
     (isabuf 
      (funcall killfn)))))

 
	  
    
;;; ========== Copying lines to listeners ==========

(defvar listener-copy-predicate 'listener-copy-unless-sent-from-here)

(defun listener-copy-unless-sent-from-here (str) 
  (let ((copy (not listener-just-sent-line)))
    (setq listener-just-sent-line nil)
    (if copy
	(null (string-match "^[ \t\n]+$" str)))))

(defun listener-copy (str)
  "Copy the input string to current listener buffers."
  (let ((lisbufs listener-buffers))
    (while lisbufs
      (save-excursion
	(set-buffer (car lisbufs))		    ; copy lines satisfying 
	(if (funcall listener-copy-predicate str)   ; predicate.
	      (isa-insert-as-if-selected str)))
      (setq lisbufs (cdr lisbufs)))))


;;; ========== Listener mode functions ==========

(defvar listener-just-sent-line nil
  "Indicates line has just been sent from this buffer, so not to re-copy it.")
(make-variable-buffer-local 'listener-just-sent-line)
(put 'listener-just-sent-line 'permanent-local t)

(defun listener-use-line ()
  "Send the current interactive ML line to the associated Isabelle buffer.
Advance to the next line."
  (interactive)
  (setq listener-just-sent-line t)
  (isa-apply-to-interactive-line 'listener-insert-region))

(defun listener-insert-region (start end)
  (let ((text (buffer-substring start end))
	(b (current-buffer)))
    (set-buffer isa-buffer)
    (isa-insert-ret text)
    (set-buffer b)))

(defun listener-undo ()
  "Send an `undo' command to the associated Isabelle buffer, move back."
  (interactive)
  (setq listener-just-sent-line t)
  (listener-send-string "undo();")
  (isa-backward-interactive-line)
  )

(defun listener-yank-and-use ()
  "Yank the last killed text, and execute listener-use-line on it."
  (interactive)
  (let ((p (point)))
    (yank)
    (goto-char p)
    (listener-use-line)))

(defun listener-send-string (str)
  "Send STR to the Isabelle buffer in isa-buffer."
  (let ((cur-scr (selected-screen)))
    (save-excursion
      (set-buffer isa-buffer)
      (isa-insert-ret str))
    (select-screen cur-scr)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Following is dead.
;;;



;; NB:- This doesn't work with listener as input-sentinel
;;      it needs listener as filter again.

(defun listener-copy-by-undo (proc str)
  "Only accept lines that look like tactics, and that don't cause ERROR.
undo() and choplev(n) are interpreted simplistically."
  (let ((accept nil))
    (accept-process-output proc)                           ; wait for output
    (save-excursion
      (goto-char comint-last-input-start)
      ;; NB:- should save match data here.
      (cond ((looking-at "[ \t]*undo();")                 ; in case of "undo"
	     (listener-delete-last-lines 1))
	    ((looking-at "[ \t]*choplev.*\\([0-9]+\\)")   ; in case of "choplev"
	     (listener-delete-last-lines
	      (string-to-int (buffer-substring
			      (match-beginning 1)
			      (match-end 1)))))
	    ((looking-at "[ \t]*b;")
	     (goto-char comint-last-input-end)
	     (if (not (looking-at "\\(.\\|\n\\)*ERROR"))
		 (setq accept t)))))
    accept))

(defun listener-delete-last-lines (num)
  (set-buffer listener-buffer)
  (goto-char (point-max))
  (forward-line (- num))
  (delete-region (point) (point-max)))


(defconst listener-proof-start-regexp
  "^val.*goal\\|^goal"  "Start of proof in listener pattern")

(defconst listener-proof-end-regexp
  "^val.*result\\|^result\\|^uresult" "End of proof in listener pattern")

(defun listener-last-proof (&optional num)
  "Return the last (or NUMth last) proof from a listener buffer as a string."
  (let ((listener-buffer (car-safe (isa-find-buffers-in-mode 'listener-mode))))
    (if listener-buffer
	(save-excursion
	  (set-buffer listener-buffer)
	  (goto-char (point-max))
	  (let* ((start (and (re-search-backward 
			      listener-proof-start-regexp nil t num)
			     (point)))
		 ;; NB:- separating start and end means 
		 ;; confusion with incomplete proofs.
		 (end   (if start
			    (progn
			      (re-search-forward 
			       listener-proof-end-regexp nil t)
			      (end-of-line) 
			      (point)))))
	    (if end 
		(buffer-substring start end)
	      (error "Can't find listener proof"))))
      (error "Can't find listener buffer"))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'isa-listener)

;;; End of isa-listener.el

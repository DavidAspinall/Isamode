;;; isa-display.el - Display manipulation for Isabelle mode.
;;; 
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-display.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;

;;; THINGS TO DO:- Is it possible to easily make sure that
;;; minibuffer-less frames use the minibuffer of the Isabelle
;;; interaction frame?  --- Yes, in XEmacs, we set frame property
;;; (minibuffer . window)

(require 'isa-load)
(require 'isa-frame)			; it won't die!

;;; Dependency note:
;;;  Some functions here make use of per-buffer Isabelle variables:
;;;     isa-associated-buffers, isa-logic-name, isa-buffer.
;;; These are not declared here, nor is 'isa-mode required since
;;; that would create a loop.  Expect byte compiler warnings.

;;; ============ Setup special font ============

(and isa-use-special-font
     isa-setup-font-function
     (funcall isa-setup-font-function))

;; This is nasty hack to get over differences between
;; XEmacs and FSF Emacs.
(cond
 ((eq isa-emacs-version 'isa-xemacs)
  (isa-make-face 'isa-output-face
		 "Face used for displaying miscellaneous output from Isabelle")
  (if isa-use-special-font
      (set-face-font 'isa-output-face isa-use-special-font))))


;;; ========== Buffer names ==========

(defvar isa-buffer-names
  '((listener   . "listener")
    (proofstate . "proofstate")
    (ruletable  . "ruletable")))

(defun isa-buffer-name-for (sym)
  (if (and (boundp 'isa-logic-name) isa-logic-name)
      (concat "*" isa-logic-name "-" (cdr (assq sym isa-buffer-names))  "*")
    (error "Not in Isabelle buffer")))

(defun isa-name (logic)
  (concat "*" logic "*"))



;;; ========== Setting display properties ==========

(defun isa-toggle-multi-frame (&optional set)
  "Toggle use of multiple frames, or switch on if arg non-nil."
  (interactive)
  (setq isa-multiple-frame-mode 
	(and window-system
	     (or set
		 (not isa-multiple-frame-mode))))
  (cond (isa-multiple-frame-mode
	 (isa-remove-display-props isa-single-frame-display-props)
	 (isa-set-display-props isa-multi-frame-display-props))
	(t
	 (isa-remove-display-props isa-multi-frame-display-props)
	 (isa-set-display-props isa-single-frame-display-props)))
  ;; Add font setting to isabelle and proofstate frames if
  ;; configuration variable requires it.
  (if isa-use-special-font
      (progn
	(put 'isabelle 'frame-defaults
	     (cons (cons 'font isa-use-special-font) ; FSF
		   (cons
		    (cons '[default font] isa-use-special-font) ; XEmacs
		    (get 'isabelle 'frame-defaults))))
	(put 'proofstate 'frame-defaults
	     (cons (cons 'font isa-use-special-font) 
		   (cons
		    (cons '[default font] isa-use-special-font) ; XEmacs
		    (get 'proofstate 'frame-defaults))))
      )))

(defun isa-set-display-props (props)
  (let ((setprops
	 '(lambda (symprops)
	    (mapcar '(lambda (p) (put (car symprops) 
				      (car p) 
				      (cdr p))) 
		    (cdr symprops)))))
    (mapcar setprops props)))

(defun isa-remove-display-props (props)
  (let ((remprops
	 '(lambda (symprops)
	    (mapcar '(lambda (p) (remprop (car symprops) (car p))) 
		    (cdr symprops)))))
    (mapcar remprops props)))
	 


;;; ===== Locating frames and buffers ===== 

(defun isa-find-frames (frame-name-sym)
  (if isa-multiple-frame-mode
    (let ((scs-left (frame-list)) scs-got)
      (while scs-left
	(if (eq frame-name-sym (intern (frame-name (car scs-left))))
	    (setq scs-got (nconc scs-got (list (car scs-left)))))
	(setq scs-left (cdr scs-left)))
      scs-got)))

(defun isa-find-buffers-in-mode (mode &optional buflist)
  "Return a list of the buffers in the buffer list in major-mode MODE."
  (save-excursion
    (let ((bufs-left (or buflist (buffer-list))) bufs-got)
      (while bufs-left
	(if (isa-buffer-active (car bufs-left))
	    (progn
	      (set-buffer (car bufs-left))
	      (if (eq mode major-mode)
		  (setq bufs-got (nconc bufs-got (list (car bufs-left)))))))
	(setq bufs-left (cdr bufs-left)))
      bufs-got)))

(defun isa-buffer-active (buf)
  "Test to see if a buffer is active"
  (and (bufferp buf) (buffer-name buf)))



;;; ========== Temporary buffer management ==========

;;; <should really change this to use regular method, because
;;;  that can be customised via hook>
;;; <another possibility: use comint's "space to flush" routines>

;;  YUK!!!
;;; This is a nasty hack so that *Rule* display gets Shown in correct
;;  font in FSF Emacs 19.  Only works in multiple screen mode.
;;  We're relying on fact that isa-ruletable uses the name "*Rule*" here.
;;  I wish there were a better way of unifying the behaviour.
;;  (The easiest thing  *is* to have the ugly Isabelle font
;;  used everywhere, but...)
;;
(and (eq isa-emacs-version 'isa-19)
     isa-multiple-frame-mode
     isa-use-special-font
     (setq special-display-buffer-names
	   (cons (cons "*Rule*"
		       (list (cons 'font isa-use-special-font)
			     '(width . 60) '(height . 10)))
	   special-display-buffer-names)))

(make-variable-buffer-local 'isa-temp-window)
(defvar isa-temp-window nil
  "Flag indicating status of temporary window associated with current buffer")

(defun isa-show-output-in-temp-buffer (bufname heading output)
  "Show OUTPUT in temporary buffer BUFNAME and Isabelle font, preceded by HEADING."
  (setq isa-temp-window (cons bufname
			      (or isa-temp-window
				  (if (one-window-p t) 'one-window t))))
  (with-output-to-temp-buffer bufname
    (princ heading)
    (princ output)
    (princ "\n(Hit q to remove)"))
  ;; FIXME: UGLY HACK
  (if (eq isa-emacs-version 'isa-xemacs)
      ;; Actually we make an extent for the whole buffer.
      ;; Never mind.
      (save-excursion
	(set-buffer bufname)
	(let ((ext (make-extent (point-min) (point-max))))
	  (set-extent-face ext 'isa-output-face)))))

(defun isa-remove-temp-buffer ()
  (interactive)
  (if isa-temp-window
      (let ((bf  (car isa-temp-window))
	    (itw (cdr isa-temp-window))
	    (sw  (selected-window)))
	;; FIXME: UGLY HACK
	(if (eq isa-emacs-version 'isa-19)
	    ;; Rely on this destroying temporary frame.
	    (kill-buffer bf)
	  (setq isa-temp-window nil)
	  (if (eq itw 'one-window)
	      (if pop-up-windows
		  (delete-other-windows)
		(switch-to-buffer (other-buffer)))
	    (progn
	      (switch-to-buffer-other-window (other-buffer))
	      (select-window sw)))))))


;;; ========== Single frame mode  ==========

(defun isa-one-frame-set-format ()   
  "Set single frame format for the current Isabelle buffer."
  (interactive)
  (delete-other-windows)
  (let ((bufs isa-associated-buffers))
    ;; Squeeze as many buffers as will fit according to their
    ;; window-height property, which is treated as a minimal height.
    (condition-case ()
	(while bufs
	  (if (and (isa-buffer-active (car bufs))
		   (isa-one-frame-height (car bufs)))
	      (save-excursion
		(split-window-vertically (isa-one-frame-height (car bufs)))
		(set-window-buffer (selected-window) (car bufs))
		(other-window 1)))
	  (setq bufs (cdr bufs)))
      (args-out-of-range nil)))
  (set-window-buffer (selected-window) (current-buffer))
  ;; After squeezing on windows, we shrink to fit those that allow it,
  ;; but make no attempt to squeeze more on.
  (walk-windows 'isa-one-frame-shrinker))

(defun isa-one-frame-height (buf)
  "Return the height BUF should have in single-frame format."
  (get (save-excursion (set-buffer buf) major-mode)
       'window-height))

(defun isa-one-frame-shrinker (win)
  "Shrink window if buffer it displays has 'shrink-to-fit of t."
  (if (get (save-excursion (set-buffer (window-buffer win)) major-mode)
	   'shrink-to-fit)
      (shrink-window-if-larger-than-buffer win)))


;;; ========== Multiple frame mode ============

(defun isa-remove-menubar-if-multiple-frame-mode ()
  (if isa-multiple-frame-mode
      (set-buffer-menubar nil)))

(defun isa-select-isa-buffer ()
  "Select the isabelle buffer associated with the current buffer."
  (interactive)
  (if (isa-buffer-active isa-buffer)
      (if isa-multiple-frame-mode
	  (isa-select-buffer isa-buffer) ; or perhaps nothing?
	(switch-to-buffer-other-window isa-buffer))))


;;; ===== Displaying and selecting buffers =====

(defun isa-display-if-active (buffer)
  "Show BUFFER if it is an active buffer, in the right place.  Note free parameter RAISE."
  (if (isa-buffer-active buffer)
      (isa-display-buffer buffer raise)))

(defun isa-display-buffer (buffer &optional raise)
  "Ensure that BUFFER is displayed.
If RAISE is non-nil, make sure it's in the right place too."
  (if (and (not raise) (get-buffer-window buffer t))
      nil
    (if isa-multiple-frame-mode
	;; Emacs 19 version...
	(let ((sc (selected-frame)))
	  (save-excursion
	    (isa-select-buffer buffer raise)
	    (select-frame sc)))
	  
      ;; Emacs 18 version
      (let ((curbuf  (current-buffer))
	    ;; find an Isabelle buffer
	    (isabuf  (or (if (eq major-mode 'isa-mode) (current-buffer))
			 (if (boundp 'isa-buffer) isa-buffer)
			 (isa-find-buffers-in-mode 'isa-mode))))
	;; format the frame using it
	(set-buffer isabuf)
	(isa-one-frame-set-format)
	;; if not displayed now, use display-buffer.
	(if (not (get-buffer-window buffer))
	    (display-buffer buffer))
	;; if possible, select window on original buffer
	(if (get-buffer-window curbuf)
	    (select-window (get-buffer-window curbuf))
	  (set-buffer curbuf))))))

(defun isa-select-buffer (buffer &optional raise)
  "Display and select buffer on a suitable frame, without splitting windows."
  (interactive "B")
  (if isa-multiple-frame-mode
      (let ((frm (get-frame-for-buffer-noselect buffer)))
	(select-frame frm)
	(if raise (raise-frame frm))
	(isa-set-buffer-dedicated-frame buffer frm)
	(delete-other-windows)))
  (let ((pop-up-windows nil))
    (switch-to-buffer buffer))
  (sit-for 0)
  ;; Next is hack for FSF 19?
  (set-buffer buffer))


;;; ========== Updating window point ==========

(defun isa-insert-as-if-selected (str)
  "Insert STR into the current buffer, as if the buffer's window was selected."
  (insert str)
  (isa-update-window-point))

(defun isa-update-window-point ()
  "Update a window point, if any, for current buffer, to match point."
  (let ((win (get-buffer-window				; Look everywhere for a window
	      (current-buffer) t)))
    (if win
	(set-window-point win (point)))			; update window point
    (sit-for 0)))					; update display
							; (doesn't work)



;;; ======== Updating frame height ========

; to be written - automagically make ruletable shrink and grow.
; (defun isa-resize-frame 
;     (ruletable           (max-frame-height . 25))






;;; Initialisation

(isa-toggle-multi-frame isa-multiple-frame-mode)

(provide 'isa-display)

;;; End of isa-display.el

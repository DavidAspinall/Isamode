;;; isa-frame.el - Screen/frame manipulation functions.
;;;      
;;; Versions of functions provided in frame.el in XEmacs
;;; distribution for application specific frame management.
;;;
;;; Eventually this file should die, and contents moved to
;;; isa-19.el.
;;;
;;; Unfortunately, as of XEmacs 19.15, the functions in prim/frame.el
;;; have been broken!!   So we keep this file.
;;;
;;; isa-frame.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;

;;; Frame manipulation functions from Xemacs.
;;; This is taken from prim/frame.el  in Xemacs 19.15.
;;;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.
;;;; Copyright (C) 1995, 1996 Ben Wing.

;;; Modified in a couple of places to accord better with documentation.
;;;  (specifically: obeys 'frame-defaults specification).


;;; Application-specific frame-management

(defvar get-frame-for-buffer-default-frame-name nil
  "The default frame to select; see doc of `get-frame-for-buffer'.")

(defvar get-frame-for-buffer-default-instance-limit nil)

(defun get-frame-name-for-buffer (buffer)
  (let ((mode (and (get-buffer buffer)
		   (save-excursion (set-buffer buffer)
				   major-mode))))
    (or (get mode 'frame-name)
	get-frame-for-buffer-default-frame-name)))


(defun get-frame-for-buffer-make-new-frame (buffer &optional frame-name defaults)
  (let* ((prps (if frame-name
		   (cons (cons 'name frame-name) defaults)
		 defaults))
	  (fr (make-frame prps))
	  (w (frame-root-window fr)))
    ;;
    ;; Make the one buffer being displayed in this newly created
    ;; frame be the buffer of interest, instead of something
    ;; random, so that it won't be shown in two-window mode.
    ;; Avoid calling switch-to-buffer here, since that's something
    ;; people might want to call this routine from.
    ;;
    ;; (If the root window doesn't have a buffer, then that means
    ;; there is more than one window on the frame, which can only
    ;; happen if the user has done something funny on the frame-
    ;; creation-hook.  If that's the case, leave it alone.)
    ;;
    (if (window-buffer w)
	(set-window-buffer w buffer))
    fr))

(defun get-frame-for-buffer-noselect (buffer
				      &optional not-this-window-p on-frame)
  "Return a frame in which to display BUFFER.
This is a subroutine of `get-frame-for-buffer' (which see)."
  (let (name limit)
    (cond
     ((or on-frame (eq (selected-window) (minibuffer-window)))
      ;; don't switch frames if a frame was specified, or to list
      ;; completions from the minibuffer, etc.
      nil)

     ((setq name (get-frame-name-for-buffer buffer))
      ;;
      ;; This buffer's mode expressed a preference for a frame of a particular
      ;; name.  That always takes priority.
      ;;
      (let ((limit (get name 'instance-limit))
	    (defaults (get name 'frame-defaults))
	    (matching-frames '())
	    frames frame already-visible)
	;; Sort the list so that iconic frames will be found last.  They
	;; will be used too, but mapped frames take precedence.  And
	;; fully visible frames come before occluded frames.
        ;; Hidden frames come after really visible ones
	(setq frames
	      (sort (frame-list)
		    #'(lambda (s1 s2)
			(cond ((frame-totally-visible-p s2)
			       nil)
			      ((not (frame-visible-p s2))
			       (frame-visible-p s1))
			      ((eq (frame-visible-p s2) 'hidden)
			       (eq (frame-visible-p s1) t ))
			      ((not (frame-totally-visible-p s2))
			       (and (frame-visible-p s1)
				    (frame-totally-visible-p s1)))))))
	;; but the selected frame should come first, even if it's occluded,
	;; to minimize thrashing.
	(setq frames (cons (selected-frame)
			   (delq (selected-frame) frames)))
	
	(setq name (symbol-name name))
	(while frames
	  (setq frame (car frames))
	  (if (equal name (frame-name frame))
	      (if (get-buffer-window buffer frame)
		  (setq already-visible frame
			frames nil)
		(setq matching-frames (cons frame matching-frames))))
	  (setq frames (cdr frames)))
	(cond (already-visible
	       already-visible)
	      ((or (null matching-frames)
		   (eq limit 0) ; means create with reckless abandon
		   (and limit (< (length matching-frames) limit)))
	       (get-frame-for-buffer-make-new-frame buffer name defaults))
	      (t
	       ;; do not switch any of the window/buffer associations in an
	       ;; existing frame; this function only picks a frame; the
	       ;; determination of which windows on it get reused is up to
	       ;; display-buffer itself.
;;	       (or (window-dedicated-p (selected-window))
;;		   (switch-to-buffer buffer))
	       (car matching-frames)))))

     ((setq limit get-frame-for-buffer-default-instance-limit)
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name, but the user wants a new frame rather than
      ;; reusing the existing one.
      (let* ((defname
	       (or (plist-get default-frame-plist 'name)
		   default-frame-name))
	     (frames
	      (sort (filtered-frame-list #'(lambda (x)
					     (or (frame-visible-p x)
						 (frame-iconified-p x))))
		    #'(lambda (s1 s2)
			(cond ((and (frame-visible-p s1)
				    (not (frame-visible-p s2))))
			      ((and (eq (frame-visible-p s1) t)
				    (eq (frame-visible-p s2) 'hidden)))
			      ((and (frame-visible-p s2)
				    (not (frame-visible-p s1)))
			       nil)
			      ((and (equal (frame-name s1) defname)
				    (not (equal (frame-name s2) defname))))
			      ((and (equal (frame-name s2) defname)
				    (not (equal (frame-name s1) defname)))
			       nil)
			      ((frame-totally-visible-p s2)
			       nil)
			      (t))))))
	;; put the selected frame last.  The user wants a new frame,
	;; so don't reuse the existing one unless forced to.
	(setq frames (append (delq (selected-frame) frames) (list frames)))
	(if (or (eq limit 0) ; means create with reckless abandon
		(< (length frames) limit))
	    (get-frame-for-buffer-make-new-frame buffer)
	  (car frames))))

     (t
      ;;
      ;; This buffer's mode did not express a preference for a frame of a
      ;; particular name.  So try to find a frame already displaying this
      ;; buffer.  
      ;;
      (let ((w (or (get-buffer-window buffer 'visible)	; check visible first
		   (get-buffer-window buffer 0))))	; then iconic
	(cond ((null w)
	       ;; It's not in any window - return nil, meaning no frame has
	       ;; preference.
	       nil)
	      ((and not-this-window-p
		    (eq (selected-frame) (window-frame w)))
	       ;; It's in a window, but on this frame, and we have been
	       ;; asked to pick another window.  Return nil, meaning no
	       ;; frame has preference.
	       nil)
	      (t
	       ;; Otherwise, return the frame of the buffer's window.
	       (window-frame w))))))))


;; The pre-display-buffer-function is called for effect, so this needs to
;; actually select the frame it wants.  Fdisplay_buffer() takes notice of
;; changes to the selected frame.
(defun get-frame-for-buffer (buffer &optional not-this-window-p on-frame)
  "Select and return a frame in which to display BUFFER.
Normally, the buffer will simply be displayed in the current frame.
But if the symbol naming the major-mode of the buffer has a 'frame-name
property (which should be a symbol), then the buffer will be displayed in
a frame of that name.  If there is no frame of that name, then one is
created.  

If the major-mode doesn't have a 'frame-name property, then the frame
named by `get-frame-for-buffer-default-frame-name' will be used.  If
that is nil (the default) then the currently selected frame will used.

If the frame-name symbol has an 'instance-limit property (an integer)
then each time a buffer of the mode in question is displayed, a new frame
with that name will be created, until there are `instance-limit' of them.
If instance-limit is 0, then a new frame will be created each time.

If a buffer is already displayed in a frame, then `instance-limit' is 
ignored, and that frame is used.

If the frame-name symbol has a 'frame-defaults property, then that is
prepended to the `default-frame-plist' when creating a frame for the
first time.

This function may be used as the value of `pre-display-buffer-function', 
to cause the display-buffer function and its callers to exhibit the above
behavior."
  (let ((frame (get-frame-for-buffer-noselect
		buffer not-this-window-p on-frame)))
    (if (not (eq frame (selected-frame)))
	frame
      (select-frame frame)
      (or (frame-visible-p frame)
	  ;; If the frame was already visible, just focus on it.
	  ;; If it wasn't visible (it was just created, or it used
	  ;; to be iconified) then uniconify, raise, etc.
	  (make-frame-visible frame))
      frame)))

(defun frames-of-buffer (&optional buffer visible-only)
  "Return list of frames that BUFFER is currently being displayed on.
If the buffer is being displayed on the currently selected frame, that frame
is first in the list.  VISIBLE-ONLY will only list non-iconified frames."
  (let ((list (windows-of-buffer buffer))
	(cur-frame (selected-frame))
	next-frame frames save-frame)

    (while list
      (if (memq (setq next-frame (window-frame (car list)))
		frames)
	  nil
	(if (eq cur-frame next-frame)
	    (setq save-frame next-frame)
	  (and 
	   (or (not visible-only)
	       (frame-visible-p next-frame))
	   (setq frames (append frames (list next-frame))))))
	(setq list (cdr list)))

    (if save-frame
	(append (list save-frame) frames)
      frames)))






(provide 'isa-frame)

;;; End of isa-frame.el



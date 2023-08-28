;;; isa-19.el - Isabelle mode support for FSF Emacs 19.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-19.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;

(require 'lucid)  ; I don't like using this, but...

;;; Version-specific functions.

;; Clearout default mouse bindings from a keymap.

(defun isa-ignore (&rest args)
  "Ignore all args"
  (interactive))
  
(defun isa-clear-mouse-bindings (keymap)
  ;; 'undefined gives beep, nil doesn't seem to change
  ;; binding.  Yuk!
  (define-key keymap [mouse-1] 'isa-ignore)	 
  (define-key keymap [mouse-2] 'isa-ignore)	 
  (define-key keymap [mouse-3] 'isa-ignore)     
  (define-key keymap [C-mouse-1] 'isa-ignore)	 
  (define-key keymap [C-mouse-2] 'isa-ignore)	 
  (define-key keymap [C-mouse-3] 'isa-ignore)     
  (define-key keymap [S-mouse-1] 'isa-ignore)	 
  (define-key keymap [S-mouse-2] 'isa-ignore)	 
  (define-key keymap [S-mouse-3] 'isa-ignore)     
  (define-key keymap [M-mouse-1] 'isa-ignore)	 
  (define-key keymap [M-mouse-2] 'isa-ignore)	 
  (define-key keymap [M-mouse-3] 'isa-ignore)     
  (define-key keymap [down-mouse-1] 'isa-ignore)
  (define-key keymap [down-mouse-2] 'isa-ignore)
  (define-key keymap [down-mouse-3] 'isa-ignore)
  (define-key keymap [drag-mouse-1] 'isa-ignore)
  (define-key keymap [drag-mouse-2] 'isa-ignore)
  (define-key keymap [drag-mouse-3] 'isa-ignore)
  (define-key keymap [double-mouse-1] 'isa-ignore)
  (define-key keymap [triple-mouse-1] 'isa-ignore)
  (define-key keymap [down-mouse-3] 'isa-ignore))


;; use isa-define-key when lucid-style bindings are given, including
;; mouse bindings.
(defun isa-define-popup-key (keymap key def)
  (define-key-convert 'lucid-symbol-convert-popup keymap key def))

;; use isa-define-key when lucid-style bindings are given.
(defun isa-define-key (keymap key def)
  (define-key-convert 'lucid-symbol-convert keymap key def))

(defvar isa-track-mouse nil
  "*Whether to generate mouse motion events in FSF Emacs 19.
This allows Isabelle to display tactics according to ruletable entries,
for example.  Unfortunately FSF Emacs makes a hash of this, resulting
in flickery display.")
  
(setq track-mouse isa-track-mouse) 

(defun isa-set-mouse-follower (function)
  "Set mouse following function for this mode."
  (and window-system (lookup-key global-map [mouse-movement])
       (define-key (current-local-map) [mouse-movement] function)))

(defun isa-display-buffer-on-frame (buf sc)
  (let ((f (selected-frame)))
    (select-frame sc t)
    (display-buffer buf)
    (select-frame f)))

(defun isa-make-face (name &optional doc-string temporary)
  "Call (make-face NAME)."
  (make-face name))

(defun isa-set-buffer-dedicated-frame (buffer frame)
  ;; Best to do nothing, I think!
  ;(isa-display-buffer-on-frame buffer frame)
  ;(set-window-dedicated-p (frame-first-window frame) t))
  )
    
;; Sigh...
(defun popup-menu (menu-desc)
  (let (cmd answer (menu menu-desc))
    (while menu
      (setq answer (x-popup-menu t menu))
      (setq cmd (lookup-key menu (vector answer)))
      (setq menu nil)
      (and cmd
	   (if (keymapp cmd)
	       (setq menu cmd)
	     (call-interactively cmd))))))


(defun set-buffer-menubar (menu)	
  (cond 
   ((eq menu t)   ; leave menu bar alone
    )
   ((or (equal menu '(nil)) (eq menu nil)) ; empty menu bar
    (define-key (current-local-map) [menu-bar buffer] 'undefined)
    (define-key (current-local-map) [menu-bar file] 'undefined)
    (define-key (current-local-map) [menu-bar edit] 'undefined)
    (define-key (current-local-map) [menu-bar help] 'undefined))
   (t
    ;; not quite the right thing, but...
    (define-key (current-local-map)
      [menu-bar]
      (easy-menu-create-keymaps (car menu) (cdr menu))))))


;;; More define key hacks to convert Xemacs symbols.

;;; hacks to define key...

(defun lucid-symbol-convert (sym)
  (cond ((eq sym 'button1) 'mouse-1)
	((eq sym 'button2) 'mouse-2)
	((eq sym 'button3) 'mouse-3)
	((eq sym 'shift) 'S)
	((eq sym 'control) 'C)
	((eq sym 'meta) 'M)
	(t sym)))

(defun lucid-symbol-convert-popup (sym)
  (cond ((eq sym 'button1) 'down-mouse-1) 
	((eq sym 'button2) 'down-mouse-2) 
	((eq sym 'button3) 'down-mouse-3) 
	((eq sym 'shift) 'S)
	((eq sym 'control) 'C)
	((eq sym 'meta) 'M)
	(t sym)))

(defun lucid-symbol-concat (converter syms)
  (and syms
       (concat (symbol-name (funcall converter (car syms)))
	       (if (cdr syms) "-") 
	       (lucid-symbol-concat converter (cdr syms)))))

(defun define-key-convert (converter keymap key def)
  ;; A vague attempt...
  (if keymap
      (cond ((symbolp key)
	     (define-key keymap 
			     (vector (funcall converter key))
			     def))
	    ((listp key)
	     (define-key keymap
			     (vector (intern (lucid-symbol-concat converter key)))
			     def))
	    (t
	     (define-key keymap key def)))))


;;; Some primitives in XEmacs.

(defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))


;;; More stuff that isn't in lucid.el  distributed with FSF Emacs19.

;;; Functions used above.

(defun frame-totally-visible-p (frame)
  "Return T if frame is not obscured by any other X windows, NIL otherwise.
Always returns t for tty frames.
NB: Hacked to: (eq (frame-visible-p FRAME) t)"
  (eq (frame-visible-p frame) t))

(defun frame-name (frame)
  "Return the name of FRAME (defaulting to the selected frame).
This is not the same as the `title' of the frame.
NB: Hacked to: (cdr-safe (assoc 'name (frame-parameters frame)))"
  (cdr-safe (assoc 'name (frame-parameters frame))))


;;; Events

(defun event-window (event)
  (let ((win (car-safe (car-safe (cdr-safe event)))))
    (and (window-live-p win) win)))

(defun event-point (event)
  ;; could check event-basic-type
  (let ((p (nth 1 (car-safe (cdr-safe event)))))
    (and (integerp p) p)))

;;; Simulating extents as overlays and properties.

(defalias 'extent-start-position 'overlay-start)
(defalias 'extent-end-position 'overlay-end)

(defun extent-at (pos &optional buffer flag)
  (if (integer-or-marker-p pos)		; can get called with 'mode-line
      (progn
	(setq buffer (or buffer (current-buffer)))
	(save-excursion
	  (set-buffer buffer)
	  (let ((overs (overlays-at pos)) result)
	    (if flag
		(while overs
		  (if (overlay-get (car overs) flag)
		      (setq result (cons (car overs) result)))
		  (setq overs (cdr overs)))
	      (setq result overs))
	    (and (overlayp (car-safe result))
		 (car-safe result)))))))

;;; cover font-lock differences

(defalias 'isa-font-lock-set-defaults 'turn-on-font-lock)

(provide 'isa-19)

;;; End of isa-19.el

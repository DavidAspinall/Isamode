;;; isa-load.el - Load definitions for Isabelle mode.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-load.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;
;;; User options, configuration settings and utilities live here.
;;;  

(require 'isa-site)
(require isa-emacs-version)

;;; Developers' note: This file is the initial startup that all other
;;; files should require.  Non-initial load definitions for the whole
;;; package appear here, as well as general configuration variables
;;; (user options).  The separation between this file and isa-site
;;; keeps isa-site as small as possible - desirable since isa-site may
;;; be loaded for every Emacs session.
;;;
;;; This file also contains some general utility functions.
;;;

;;; ========== Version ==========

;;; DO NOT EDIT THIS VARIABLE!  
(defconst isa-mode-version "** Isamode, by David Aspinall <da@dcs.ed.ac.uk>  Version 2.7. March 14th 2000. **"
      "Version stamp for Isamode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ======= USER CONFIGURATION SETTINGS ==========


;;; ========== Ruletable paths =========

(defvar isa-ruletable-paths
  (if (string-equal (isa-getenv "ISAMODE_RULETABLE_PATH") "")
      (list (concat (isa-getenv "ISAMODE_HOME") "/ruletables"))
    (split-string (isa-getenv "ISAMODE_RULETABLE_PATH") ":"))
  "*List of directories to search for ruletables.
Defaults to setting of ISAMODE_RULETABLE_PATH, or if that is unset,
ISAMODE_HOME/ruletables.")


;;; ========== Session settings ==========

(defconst isa-possible-associated-buffer-names
  '(proofstate listener ruletable)
  "Names of types of buffers which may be associated to an Isabelle session.")

(defvar isa-startup-defaults
  (let
      ((wins (isa-getenv "ISAMODE_WINDOWS")))
    (if (string-equal wins "") 
      '(proofstate ruletable)
      ;; FIXME: remove nil's from list resulting below.
      (mapcar 'intern-soft (split-string wins "[ \t]"))))
  "*List of symbol names of associated buffers to initialise by default
when starting an Isabelle session. 
Pick from isa-possible-associated-buffer-names.")

(defvar isa-session-prelude
  (isa-getenv "ISAMODE_PRELUDE")
  "*If non-nil, an ML string to issue at the start of every Isabelle session.
Handy for setting options, or loading personal startup files.
NOTE: make sure the strings are properly terminated and don't
lead to errors - you don't see the results of executing it!")

(defvar isa-dont-query-quit nil
  "*If non-nil, don't query the user before quitting Isabelle session.")

(defvar isa-roll-tracing t
  "*If non-nil, Isabelle's tracing output is \"rolled\".
In other words, previous levels are replaced by new ones in the
interaction buffer.")


;;; ========== Display options ==========

(defvar isa-multiple-frame-mode
  window-system
  "*If non-nil, use multiple Emacs 19 frames.
Setting this to nil in Emacs 19 varieties restricts Isabelle-interaction 
mode to use a single frame.")

(defvar isa-use-special-font
  "isabelle14"
  "*Name of special font to use for Isabelle buffers (interaction and proofstate).
If nil, use default Emacs font.")

(defvar isa-output-face-properties
  (if isa-use-special-font
      '(font . isa-use-special-font)
    '())
  "*List of face properties to set for faces used for Isabelle output.")

(defvar isa-use-long-ruletables 
  isa-multiple-frame-mode
  "*If non-nil, use long-form of ruletables, which include section headings.
Probably best set to nil for single-frame working.")

(defvar isa-default-menubar 
  '(nil)
  "*Base menubar for Isabelle mode.")



;;; ============ Display properties ============

(defconst isa-associated-frame-names
   '(isa-mode listener-mode proofstate-mode ruletable-mode)
   "Names of modes of buffers which may have own frames for an Isabelle session.")

;; Probably this variable should have separate defaults
;; for XEmacs and FSF Emacs.

(defvar isa-multi-frame-display-props
  '((proofstate-mode     (frame-name . proofstate))
    (listener-mode       (frame-name . listener))
    (ruletable-mode      (frame-name . ruletable))
    (isa-mode            (frame-name . isabelle))
    (ruletable           (instance-limit . 3))
    (ruletable           (max-frame-height . 25))
    (ruletable 		 (frame-defaults .
			   ((top . 300)   (left . 5)
			    (height . 22) (width . 60)
			    (menu-bar-lines . 0)
			    ;; Leave minibuffer there for display of tactics.
			    ;; Would be better to select isabelle frame
			    ;; as minibuffer for this frame, perhaps,
			    ;; but that would have to be set dynamically.
			    (default-toolbar . nil)
			   ; unsplittable gives bad behaviour in Xemacs
			   ;(unsplittable . t)
			    )))
    (isabelle            (frame-defaults .             
			  ((top .  0)    (left .  0)        
			   (height . 35) (width . 80)   
			   (default-toolbar . nil)
			   )))
    (listener            (frame-defaults .
			  ((top . 620)   (left . 530)
			   (height . 7)  (width . 65)
			   (menu-bar-lines . 0)
			   (default-toolbar . nil)
			   )))
    (proofstate          (frame-defaults .
			  ((top .  30)    (left . 700)
			   (height . 30)  (width . 50)
			   (menu-bar-lines . 0)
			   (minibuffer . nil)
			   (default-toolbar . nil)
			   (scrollbar-width . 0)
			   (auto-raise . t)
			   ; unsplittable gives bad behaviour in Xemacs
			   ;(unsplittable . t)
			   ))))
  "*Display properties for Isabelle buffers in multiple frame mode.")

(defvar isa-single-frame-display-props
  '((proofstate-mode     (window-height . 8))    ; height for single mode
    (listener-mode       (window-height . 5))
    (ruletable-mode      (window-height . 15))
    (ruletable-mode      (shrink-to-fit . t)))
  "*Display properties for Isabelle buffers in single frame mode.")


;;; ========== Prompt pattern ==========

(defvar isa-prompt-pattern  "^2?\\(ML\\(Works\\)?\\)?[---=#>]>? *\\|^\\*\\* .*"    
  "*Regexp to recognise prompts from the Isabelle interpreter.
Must include all forms of prompt; the default:
 ^2?[---=#>]>? *\\|^\\*\\* .*
splits into a two parts:
 ^2?[---=#>] *
matches all forms of prompt from Poly/ML and New Jersey ML, and:
 ^\\*\\* .*
matches Isabelle tracing prompts.")

;;; ========== SML mode =========

(defvar isa-use-sml-mode
   (if (fboundp 'sml-mode) 'sml-mode)
  "*If non-nil, use sml-mode within theory files and as basis for listener.")
   
;;; ========== Command hooks ==========

(defvar isa-run-command-function
  'isa-tool-run-command
  "Name of function that makes command for running Isabelle with built-in logics.
The function is called with the name of the logic as an argument,
and it must build a list of the form:
     (PROGRAM ARG ... ARG)
consisting of the program name and arguments to run Isabelle.")

(defvar isa-quit-function 
  'isa-quit
  "Name of function to quit Isabelle session.
Called with one argument: t to save database, nil otherwise.")

(defvar isa-list-logics-function
  'isa-tool-list-logics
  "Name of function that returns a list of available object logics")

(defvar isa-doc-command-function
  'isa-tool-doc-command
  "Name of function which constructs command to view document, when passed
name document as its argument.  The result should be the form:
     (PROGRAM ARG ... ARG)
consisting of the program name and arguments to view DOCNAME.")

(defvar isa-list-docs-function
  'isa-tool-list-docs
  "Name of function which returns a list of lists of the form
 ((DOCNAME DESCRIPTION) ....)
of Isabelle document names and descriptions.  When DOCNAME is
passed to isa-view-doc-function, DOCNAME will be viewed.")

(defvar isa-setup-font-function
  'isa-tool-setup-font
  "Name of function called to setup special Isabelle fonts,
for example, to install fonts on the X font server.
If nil, do nothing.")

(defvar isa-default-logic-dir-function
  'isa-tool-default-logic-dir
  "Name of function to call to return a default directory containing logics.")


;;; ==== End of USER CONFIGURATION VARIABLES section ====
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ========== Defaults for Command Hooks using Isabelle tools ==========
;;;
;;; No defaults are supplied for functions not using Isabelle tools ----
;;; Isamode 2.4 had suitable functions built in which you could adapt
;;; if need be. 
;;;

(defun isa-tool-run-command (logic-name)
  "Make a command for running Isabelle using Isabelle tools."
  (if isa-use-special-font
      (list (isa-getenv "ISABELLE") "-misabelle_font" "-msymbols" logic-name)
    (list (isa-getenv "ISABELLE") logic-name)))

(defun isa-tool-list-logics ()
  "Generate a list of available object logics using Isabelle tools."
  (split-string (isa-evaluate-shell-command
		 (concat isa-isatool-command " findlogics")) "[ \t]"))

(defun isa-tool-doc-command (docname)
  "Return command for viewing document DOCNAME using Isabelle tools."
  (list isa-isatool-command "doc" docname))

(defun isa-tool-list-docs ()
  "Generate a list of documentation files available, with descriptions."
  (mapcar
    (function (lambda (docdes)
		(list
		 (substring docdes (string-match "\\(\\S-+\\)[ \t]+" docdes)
				   (match-end 1))
		 (substring docdes (match-end 0)))))
	      (split-string
	       (isa-evaluate-shell-command
		(concat isa-isatool-command " doc")) "\n")))

(defun isa-tool-setup-font ()
  "Setup special font for isabelle, using Isabelle tools."
  (call-process isa-isatool-command nil nil nil "installfonts"))

(defun isa-tool-default-logic-dir ()
  "Return a directory containing logic images."
   (car (split-string (isa-getenv "ISABELLE_PATH") ":")))

(defun isa-quit (save)
  "Quit / save the Isabelle session."
  (if (not save)
      (isa-insert-ret "quit();"))
  (comint-send-eof))



;;; ========== Further Autoloads ==========

(autoload 'isa-thy-insert-template "isa-thy-mode"
	  "Insert template in theory file." t)
(autoload 'isa-batchify "isa-proofscript"
	  "Convert an interactive proofscript to a batch one." t)
(autoload 'isa-unbatchify "isa-proofscript"
	  "Convert a batch proofscript into an interactive one." t)
(autoload 'isa-thy-use-line "isa-thy-mode"
          "Send the current interactive ML line to an Isabelle buffer." t)

(autoload 'isabelle-session "isa-mode")
(autoload 'proofstate "isa-proofstate"
	  "Activate a proof state buffer." t nil)
(autoload 'listener   "isa-listener"
	  "Activate a listener buffer." t nil)
(autoload 'listener-minor-mode   "isa-listener"
	  "Switch on listening in this buffer." t nil)
(autoload 'ruletable  "isa-ruletable"
	  "Activate a ruletable buffer." t nil)
(autoload 'isa-get-rules-files  "isa-ruletable"
	  "Generate isa-theory-rules from isa-ruletable-paths." t nil)
  
;;; ==========  Utility functions ==========

(defun isa-splice-separator (sep strings)
  (let (stringsep)
    (while strings
      (setq stringsep (concat stringsep (car strings)))
      (setq strings (cdr strings))
      (if strings (setq stringsep 
			(concat stringsep sep))))
    stringsep))

(defun isa-format (alist string)
  "Format a string by matching regexps in ALIST against STRING"
  (while alist
    (while (string-match (car (car alist)) string)
      (setq string
	    (concat (substring string 0 (match-beginning 0))
		    (cdr (car alist))
		    (substring string (match-end 0)))))
    (setq alist (cdr alist)))
  string)

(defun isa-forward-interactive-command () 
  "Move forward to after the next semi-colon.  
Semi-colons inside strings or comments are ignored: assumption is that point
is not inside a comment or string when this function is called."
  (interactive)
  ;; find semi-colon
  (while (and (not (eobp))
	      (not (eq (char-after (point)) ?\;)))
    (skip-chars-forward "^\;\"(")
    (cond ((eq (char-after (point)) ?\")
	   ;; skip strings
	   (forward-char)
	   (if (null (re-search-forward "[^\\\\]\"" nil t))
	       (error "Unclosed string?")))
	  ((eq (char-after (point)) ?\()
	   (if (eq (char-after (1+ (point))) ?*)
	       ;; skip comments (but not nested ones will go wrong).
	       (re-search-forward "\\*\)")
	     (forward-char)))))
  (forward-char))

(defun isa-forward-interactive-line ()
  "Move forward to after what seems like an interactive line."
  (interactive)
  (isa-forward-interactive-command)
  (skip-chars-forward " \t\n"))

(defun isa-backward-interactive-command () 
  "Move backward to what looks like the start of the previous interactive command.
Exactly one semi-colon is skipped.
Semi-colons inside strings or comments are ignored: assumption is that point
is not inside a comment or string when this function is called."
  (interactive)
  ;; find skipped semi-colon
  (skip-chars-backward "^\;")
  (backward-char)
  (while (and (not (bobp))
	      (not (eq (char-after (1- (point))) ?\;)))
    (skip-chars-backward "^\;\"(")
    (cond ((eq (char-after (1- (point))) ?\")
	   ;; skip strings
	   (backward-char)
	   (if (null (re-search-backward "[^\\\\]\"" nil t))
	       (error "Unclosed string?")))
	  ((eq (char-after (1- (point))) ?\()
	   (if (eq (char-after (point)) ?*)
	       ;; skip comments (but not nested ones will go wrong).
	       (re-search-backward "\\*\)")
	     (backward-char))))))

(defun isa-backward-interactive-line ()
 "Move backward to what looks like the start of the previous interactive line."
 (interactive)
 (isa-backward-interactive-command)
 (skip-chars-forward " \t\n"))

(defun isa-apply-to-interactive-line (f)
  "Apply F to the text of the current interactive line, and advance.
If at the end of the buffer, go forward one line too, unless read only."
  ;; If looks to be at the end of an interactive line, use that one,
  ;; otherwise advance past any spaces.
  (if (eq ?\; (char-after (1- (point))))
      (backward-char 1)
    (skip-chars-forward " \t\n"))
  (beginning-of-line)
  (let ((start   (point)))
    (isa-forward-interactive-command)
    (funcall f start (point))
    (if (eq (char-after (1- (point))) ?\;)
	(progn
	  (if (and (eobp) (not buffer-read-only))
	      (insert "\n")
	    (forward-char))))))
  
(defun isa-file-name-cons-extension (name)
  "Return cons cell of NAME without final extension and extension"
  (if (string-match "\\.[^\\.]+$" name)
      (cons (substring name 0 (match-beginning 0))
	    (substring name (match-beginning 0)))
    (cons name "")))

(defun isa-alist-union (l m)
  (let (ls)
    (while m
      (if (not (assoc (car (car m)) l))
	  (setq ls (cons (car m) ls)))
      (setq m (cdr m)))
    (nconc l (reverse ls))))

(defun isa-zip (l m)
  (let (lm)
    (while l
      (setq lm (cons (cons (car l) (car m)) lm))
      (setq l (cdr l))
      (setq m (cdr m)))
    (reverse lm)))

(defun isa-get-file-alist (regexp dirs)
  "Generate an alist of file names minus extensions and full paths."
  (let (files atts)
    (while dirs
      (setq atts (file-attributes (car dirs)))
      (if (and atts (eq t (car atts)))
	  (setq files 
		(isa-alist-union
		 files
		 (isa-zip
		  (mapcar 'car (mapcar
				'isa-file-name-cons-extension
				(directory-files (car dirs) nil regexp)))
		  (directory-files (car dirs) t regexp))))
	(sit-for 1)
	(message "Warning: can't find directory %s" (car dirs))
	(sit-for 1))
	(setq dirs (cdr dirs)))
    files))



(provide 'isa-load)

;;; isa-load.el ends here

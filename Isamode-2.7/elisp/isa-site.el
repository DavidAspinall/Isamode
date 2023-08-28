;;; isa-site.el - Site specifics for Isabelle mode, initial autoloads.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-site.el,v 2.6 1997/05/27 22:26:59 da Exp
;;;

;;; Installation instructions:
;;;
;;; If $ISAMODE_HOME is correctly configured in the Isabelle settings
;;; file, which it will be if Isamode is installed in
;;; $ISABELLE_HOME/contrib/Isamode, then you won't need to edit this
;;; file.
;;;
;;; It may be possible to activate Isamode a fresh Emacs session using
;;; one of the Isabelle tool scripts.
;;; Otherwise, you may like to make Isamode available to any Emacs
;;; session.  To do this you must add this line to your .emacs or
;;; site-wide initialisation:
;;;
;;;       (load-file "<path>/isa-site")
;;;
;;; where <path> is replaced by path to this file.  You must also
;;; have ISABELLE_HOME/bin on your PATH, or edit this file to set
;;; isa-isatool-command to the value of $ISABELE_HOME/bin/isatool
;;;
;;; See file  isa-load.el  for other customization settings
;;; to set in your .emacs.
;;;

;;; ================ Extract Isabelle settings ================

;; If isa-isatool-command is nil, then Isabelle tools will not be used.
;; This means that some command hooks in isa-load must be set to
;; alternative functions, which aren't supplied in this version of
;; Isamode.   But I can see no reason *not* to use the new tools...
;;
(defvar isa-isatool-command
  (or (getenv "ISATOOL") "isatool")
  "Command to invoke Isabelle tool 'isatool'.
Use a full path name here if  isatool is not on PATH when Emacs is started.")



(defun isa-getenv (envvar)
  "Extract an environment variable setting, using 'isatool getenv' if available."
  (if isa-isatool-command
      (isa-evaluate-shell-command
       (concat isa-isatool-command " getenv -b " envvar))
    (getenv envvar)))

(defun isa-evaluate-shell-command (command)
  "Evaluate COMMAND and return result as a string"
  ;; Might be better to use call-process instead, actually:
  ;; shell commands are handled slightly differently.
    (interactive (list
		  (read-from-minibuffer "Shell command: "
				       nil nil nil
				       'shell-command-history)))
  (let ((buffer (get-buffer-create " *Evaluate shell command*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (shell-command-on-region (point-min) (point-min) command t)
      (goto-char (point-max))
      (delete-char -1)			; final char always CR
      (buffer-string))))


;;;
;;; Site-specific variable ========================
;;;

(defvar isa-elisp-dir 
  (concat (isa-getenv "ISAMODE_HOME") "/elisp/")
  "Directory where the lisp files for Isabelle mode are located.
Default is value $ISAMODE_HOME/elisp/.")


;;;
;;; Load path and initial autoloads ========================
;;;

(setq load-path (cons isa-elisp-dir load-path))

(setq auto-mode-alist 
      (cons '("\\.thy$" . isa-thy-mode) auto-mode-alist))

(autoload 'isa-thy-mode "isa-thy-mode" 
	  "Major mode for Isabelle theory files" t nil)

(autoload 'isa-sml-mode   "isa-thy-mode" 
          "Invoke sml-mode after installing Isabelle hook." t nil)

(autoload 'isabelle   "isa-mode" 
	  "Start an Isabelle session" t nil)

(autoload 'isa-menus  "isa-menus"
          "Show Isabelle menus." t nil)

;;;
;;; Isamode 2.6 is only available for:
;;;
;;;    XEmacs 19.14 or later
;;;    

(defvar isa-emacs-version nil
  "Name of Emacs-version configuration file to use.")

(cond
 ((and
   (string-match "XEmacs" emacs-version)
   (boundp 'emacs-major-version)
   (or (and
	(= emacs-major-version 19)
	(>= emacs-minor-version 14))
       (>= emacs-major-version 20)))
  ;; XEmacs 19.14 or later
  (setq isa-emacs-version 'isa-xemacs)
  (provide 'isa-site))
 ((and
   (string-match "^19" emacs-version)
   (boundp 'emacs-major-version)
   (or (and
	(= emacs-major-version 19)
	(>= emacs-minor-version 34))
       (= emacs-major-version 20)))
  ;; FSF Emacs 19.34 or later
   (setq isa-emacs-version 'isa-19)
   (provide 'isa-site))
 (t
  (error "Sorry, Isamode is not compatible with your version of Emacs.")))


;;; end of isa-site.el

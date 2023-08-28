;;; isa-compdefs.el - Dummy compatibility file to allow compile/run
;;;		      across different versions of Emacs.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-compdefs.el,v 2.6 1997/05/27 22:26:57 da Exp
;;;


;;; Notes:
;;; =====
;;;  1. This file is for use during compilation only
;;;  2. To compile isa-mode for use only under the same version 
;;;     of Emacs, change byte-compile-compatibility to nil.
;;;  3. Developers: turn the warnings back on! - they're useful.
;;;	(see bytecomp.el for details) 
;;;  4. How this works:
;;;       Hope is that .elc's can be built during one bath compilation
;;;       of emacs.

;;; ===== Set byte compiler options =====

(setq byte-compile-warning-types nil)   ; Suppress warnings. Careful!


;;; ===== Configure the compilation environment =====

(setq isa-emacs-version 'isa-compdefs)	; avoid isa-19, etc.
(setq load-path (cons "." load-path))	; pick up right files
(setq isa-install-dir "<unset>")	; dummy installation directory
(defvar frame-initial-frame-alist nil)  ; prevents "definition is void"


;;; ==== Dummy definitions ====

(defun isa-define-popup-key (&rest args))
(defun isa-define-key (&rest args))


(provide 'isa-compdefs)

;;; End of isa-compdefs.el



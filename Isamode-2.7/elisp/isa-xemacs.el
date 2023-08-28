;;; isa-xemacs.el - Isabelle mode support for Lucid Emacs.
;;;
;;; Author:  David Aspinall <da@dcs.ed.ac.uk>
;;;
;;; isa-xemacs.el,v 2.6 1997/05/27 22:27:00 da Exp
;;;

;;; Define functions for xemacs.


(defun isa-ignore (&rest args)
  "Do nothing - interactively!"
  (interactive))
  
(defun isa-clear-mouse-bindings (keymap)
  ;; nil gives beep 
  (define-key keymap 'button1 'isa-ignore)
  (define-key keymap 'button2 'isa-ignore)
  (define-key keymap 'button3 'isa-ignore))

(fset 'isa-define-popup-key 'define-key)
(fset 'isa-define-key       'define-key)

(defun isa-set-mouse-follower (function)
  "Set mouse following function for current mode."
  (setq mode-motion-hook function))

(defun isa-display-buffer-on-frame (buf sc)
  (display-buffer buf nil sc))

  (defalias 'isa-make-face 'make-face)
(defun isa-set-buffer-dedicated-frame (buffer frame)
  ;; Best to do nothing, I think!
  ;;(defalias 'isa-set-buffer-dedicated-frame 'set-buffer-dedicated-frame)
  )

;;; Versions of font-lock.

(if (fboundp 'font-lock-set-defaults)
    (defalias 'isa-font-lock-set-defaults 'font-lock-set-defaults)
  ;; Not very satisfactory, but hopefully above works in 19.14 too.
  (defalias 'isa-font-lock-set-defaults 'turn-on-font-lock))

;;;
;;; FSF compatibility
;;;

(if (fboundp 'read-no-blanks-input)
    nil
  (defun read-no-blanks-input (prompt)
    ;; could do better than this, but never mind.
    (read-string prompt)))

(provide 'isa-xemacs)

;;; End of isa-xemacs.el

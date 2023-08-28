----------------------------------------------------------------------
             Isamode 2.7  --   GNU Emacs support for Isabelle
----------------------------------------------------------------------

Isamode provides facilities for interacting with Isabelle and editing
theory files, inside GNU Emacs.

In outline, the facilities are:

 * Theory mode    -  for editing theory files
 * Isabelle mode  -  for interacting with Isabelle
 * Menus          -  including tactics and common proof commands 
 * Listener       -  a buffer for recording interactive proofs
 * Proof-State    -  a buffer which displays the current proof state
 * Rule-table     -  rule names for the current logic

Menus and mouse functions give Isabelle a primitive user-interface.

To activate Isamode once installed, visit a theory file or type:
 
     M-x isabelle            to start an Isabelle session, or
     M-x isa-menus           to show the Isabelle main menu.

The main documentation is written in Texinfo in doc/Isamode.texi.  You
can TeX this to produce a dvi file, or run "makeinfo" to get an info
file.  The documentation may be out of date in some places.  Please
let me know where it is inaccurate.

Isamode 2.7 has been updated for use with Isabelle99.  Do not
use it with earlier versions. 

It has been tested with XEmacs 21.1, but it should work with recent
earlier versions and recent versions of FSF Emacs.

I recommend that you use XEmacs.  It provides a much nicer environment
than FSF Emacs and Isamode works better with it.  Future versions of
Isamode may not be compatible with FSF Emacs at all; the effort of
supporting both Emacs versions is too great.

For installation instructions, read INSTALL.txt.

Have fun!

				 	  David Aspinall, March 2000.
					  (da@dcs.ed.ac.uk)
  






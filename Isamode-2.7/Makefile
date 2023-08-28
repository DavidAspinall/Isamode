## -*- makefile -*-
##
## Makefile for Isamode 
##
## Author:  David Aspinall <da@dcs.ed.ac.uk>
##
## Makefile,v 2.6.2.1 1997/08/21 10:44:02 da Exp
## 
###########################################################################
##
## Use:
##
##    make compile        - cleanout all .elc's and recompile
##    make <file.elc>     - byte compile file if .el file is newer
##                          (do all if <file.elc> is omitted)
##    make docs           - build info and .dvi files from isamode.texi
##    make pdf / html     - make other documentation files
##    make install        - install .el and .elc's into INSTALLDIR
##                          .rules files into RULESINSTALLDIR
##    make public         - install doc files and .el's into PUBLICDIR
##    make dist           - make a distribution (cleaned dir + tarfile)
## 
###########################################################################
##
## Things to edit: INSTALLDIR, PUBLICDIR, EMACS, MAKEINFO, TEXI2DVI
##
## See-also: isa-site.el, isa-compdefs.el
## 
###########################################################################

# Directory to install .elc and .el files in.
INSTALLDIR = elisp

# Directory to install .rules files in.
RULESINSTALLDIR = ruletables

# Emacs to use as byte compiler  (needs batch-byte-compile function).
EMACS = xemacs

# Directory to make distribution in
FTPDIR = /home/da/pub/ftp

# program to make info files from .texi
MAKEINFO = makeinfo
# program to make dvi files from .texi
TEXI2DVI = texi2dvi
# program to make pdf files from .texi
TEXI2PDF = texi2pdf
# program to make html files from .texi
TEXI2HTML = texi2html -split_chapter


############################################################################

# Byte compilation command
BYTECOMP = $(EMACS) -batch -q -l ./isa-compdefs.el -f batch-byte-compile 

# Name of ftp directory and tar file
NAME = Isamode-2.7

# Working directory
WRKDIR=`pwd`

SHELL = /bin/sh

EL = *.el

ELC = $(EL:.el=.elc)

DISTFILES=*.txt Xdefaults-* Makefile elisp ruletables doc

.SUFFIXES:	.el .elc .texi .info .dvi .html

.el.elc:
	$(BYTECOMP) $< 

.texi.info:
	$(MAKEINFO) $< 

.texi.dvi:
	$(TEXI2DVI) $< 

.texi.html:	
	$(TEXI2HTML) $< 

all:    $(ELC)


## 
## compile : byte compile files in working directory:
##           Clearout old .elc's and re-compile in a
##           single Emacs process (faster than make all).
##
compile:
	@echo "*************************************************"
	@echo " Byte compiling..."
	@echo "*************************************************"
	(cd elisp; rm -f $(ELC); $(BYTECOMP) $(EL))
	@echo "*************************************************"
	@echo " Finished."
	@echo "*************************************************"

## 
## doc : build all in doc directory.
##
docs:
	(cd doc; $(MAKE))
##
## Other targets in doc directory: pdf, html
##
pdf:
	(cd doc; $(MAKE) pdf TEXI2PDF="$(TEXI2PDF)")	

html:
	(cd doc; $(MAKE) html TEXI2HTML="$(TEXI2HTML)")	

##
## install :  copy lisp files into INSTALLDIR
##                 ruletables into RULESINSTALLDIR
##	      (watch that install dirs are set correctly)
##
## NB: do `make isa-site' if you want to allow for Emacs 18 users
## accidently loading it.
##
install: 
	@echo "*************************************************"
	@echo " Cleaning target directory and copying files..."
	@echo "*************************************************"
	if test $(INSTALLDIR) != $(WRKDIR)/elisp ;\
        then mkdirhier $(INSTALLDIR)       ;\
	     rm -f $(INSTALLDIR)/*         ;\
	     for lispfile in $(EL)         ;\
	     do if test -r $${lispfile}c       ;\
	        then cp $${lispfile}c $(INSTALLDIR); \
		fi; \
		cp $${lispfile}  $(INSTALLDIR); \
	     done					 ;\
	fi ;\
	if test $(RULESINSTALLDIR) != $(WRKDIR)/ruletables ;\
        then mkdirhier $(RULESINSTALLDIR)       ;\
	     rm -f $(RULESINSTALLDIR)/*         ;\
	     cp $(WRKDIR)/ruletables/* $(RULESINSTALLDIR) ;\
	fi
	@echo "*************************************************"
	@echo " Finished."
	@echo "*************************************************"


##
## public : copy lisp, ruletable & doc files into PUBLICDIR
##
public:
	@echo "*************************************************"
	@echo " Cleaning target directory and copying files..."
	@echo "*************************************************"
	if test $(PUBLICDIR) != $(WRKDIR) ;\
        then mkdirhier $(PUBLICDIR)       ;\
	     rm -rf $(PUBLICDIR)/*        ;\
	     for file in $(EL) $(DOC) ruletables ;\
		do cp -r $${file} $(PUBLICDIR)  ;\
	     done			  ;\
	fi
	@echo "*************************************************"
	@echo " Finished."
	@echo "*************************************************"


##
## clean: Remove .elc's and subsidiary documentation files
##
clean:
	@echo "*************************************************"
	@echo " Removing .elc's and subsidiary documentation files..."
	@echo "*************************************************"
	(cd doc; make clean)
	rm -f elisp/*.elc *~

distclean: 
	(cd doc; make distclean)
	rm -f elisp/*.elc *~

##
## dist: create a distribution: make directory and 
##       packaged contents in FTPDIR.  
##
dist:   distclean
	@echo "*************************************************"
	@echo " Cleaning ftp directory..."
	@echo "*************************************************"
	rm -rf $(FTPDIR)/$(NAME)/*
	rm -f $(FTPDIR)/$(NAME).tar*
	@echo "*************************************************"
	@echo " Copying files from working directory..."
	@echo "*************************************************"
	mkdir -p $(FTPDIR)/$(NAME)
	for distfile in $(DISTFILES) ;\
	  do cp -r $${distfile} $(FTPDIR)/$(NAME) ;\
	done
	@echo "*************************************************"
	@echo " Making compressed tar file..."
	@echo "*************************************************"
	tar -cvhf $(FTPDIR)/$(NAME).tar -C $(FTPDIR) $(NAME)
	gzip -9 $(FTPDIR)/$(NAME).tar
	@echo "*************************************************"
	@echo " Finished."
	@echo "*************************************************"

##
## export: make a distribution in FTPDIR from CVS sources, Isamode module.
##	   (Author only)
##	   TODO:  increment version, change stamp in isa-load.el.
##
export:
	@echo "*************************************************"
	@echo " Cleaning ftp directory..."
	@echo "*************************************************"
	rm -rf $(FTPDIR)/$(NAME)/*
	rm -f $(FTPDIR)/$(NAME).tar*
	@echo "*************************************************"
	@echo " Running cvs export .."
	@echo "*************************************************"
	cvs export -kv -Dtoday -d$(FTPDIR)/$(NAME) Isamode
	@echo "*************************************************"
	@echo " Making compressed tar file..."
	@echo "*************************************************"
	tar -cvhf $(FTPDIR)/$(NAME).tar -C $(FTPDIR) $(NAME)
	gzip -9 $(FTPDIR)/$(NAME).tar
	@echo "*************************************************"
	@echo " Finished."
	@echo "*************************************************"

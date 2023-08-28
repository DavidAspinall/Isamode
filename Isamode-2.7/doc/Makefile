## -*- makefile -*-
##
## Makefile for Isamode documentation.
##
## Author:  David Aspinall <da@dcs.ed.ac.uk>
##
## Makefile,v 2.6.2.1 1997/08/21 10:44:06 da Exp
##
## 
###########################################################################
##
## use targets for make in main directory rather than here: docs, pdf, html
##
###########################################################################


# Working directory
WRKDIR=`pwd`

SHELL = /bin/sh

DOC = *.txt Xdef* *.dvi *.pdf

.SUFFIXES:  .texi .info .dvi .html .pdf

.el.elc:
	$(BYTECOMP) $< 

.texi.info:
	$(MAKEINFO) $< 

.texi.dvi:
	$(TEXI2DVI) $< 

.texi.pdf:
	$(TEXI2PDF) $< 

.texi.html:	
	$(TEXI2HTML) $< 

all:    doc
dvi:    Isamode.dvi
pdf:    Isamode.pdf
html:   Isamode.html
info:   Isamode.info

# NB: for info, could make localdir automatically from
# START-INFO-DIR-ENTRY / END-INFO-DIR-ENTRY.
# Does some utility do this?

## 
## doc : build info and dvi files from Isamode.texi
##
doc:	dvi info
	@echo "*************************************************"
	@echo " Documentation built in Isamode.info* and Isamode.dvi"
	@echo " - Now copy Isamode.info* to your info directory,"
	@echo " and add an entry in the \`dir' file."
	@echo "*************************************************"

##
## dvisel : rearrange dvi file to put contents in the right place.
##          (isn't it easier than this!?)
##          (this is still wrong for double sided - probably best
##           to do it yourself)
dvisel: Isamode.dvi
	@echo " Rearranging pages in Isamode.dvi"
	dviselect =1 =2 Isamode.dvi > Isamode-title.dvi
	dviselect =3: Isamode.dvi > Isamode-tmp.dvi
	dviselect :_1 Isamode-tmp.dvi > Isamode-contents.dvi
	dviselect 1: Isamode-tmp.dvi > Isamode-main.dvi
	dviconcat Isamode-title.dvi Isamode-contents.dvi \
                  Isamode-main.dvi > Isamode.dvi

##
## clean: Remove subsidiary documentation files
##
clean:
	rm -f Isamode.?? Isamode.cps Isamode.cp0
	rm -f Isamode.aux Isamode.log Isamode.toc
	rm -f *~

##
## distclean: Remove documentation targets
##
distclean:   clean
	rm -f Isamode.info* Isamode.dvi Isamode.pdf Isamode*.html

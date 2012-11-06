## This Makefile was inspired from the RcppGSL package
## Copyright (C) 2011 Romain François and Edd Eddelbuettel
## It was modifed by Renaud Gaujoux to make it more generic and to generate the 
## fake vignettes on the fly.
## Copyright (C) 2011 Renaud Gaujoux

## There is an old bug in texidvi that makes it not swallow the ~
## marker used to denote whitespace. This is actually due to fixing
## another bug whereby you could not run texidvi on directory names
## containing a tilde (as we happen to do for Debian builds of R
## alpha/beta/rc releases). The 'tilde' bug will go away as it
## reportedly has been squashed upstream but I am still bitten by it
## on Ubuntu so for now Dirk will insist on pdflatex and this helps.

#%AUTHOR_USER%#
#%MAKE_R_PACKAGE%#

ifndef MAKE_R_PACKAGE
$(error Required make variable 'MAKE_R_PACKAGE' is not defined.)
endif
#ifndef AUTHOR_USER
#$(error Required make variable 'AUTHOR_USER' is not defined.)
#endif
ifndef MAKEPDF
MAKEPDF=1
endif

##---------------------------------------------------------------------
## Everything below this should be generic and work for any package provided that
## they have the following directory inst/doc setting:
## - inst/vignettes/src: contains the Rnw files for normal vignettes
## - tests: contains R code to run unit tests that are run if not checking and
## produce the file <package>-unitTests.pdf  
##---------------------------------------------------------------------

SRC_DIR=src
RNW_SRCS = #%RNW_SRCS%#
PDF_OBJS=$(RNW_SRCS:.Rnw=.pdf)

TEX_OBJS=$(PDF_OBJS:.pdf=.tex)

ifneq (${R_HOME},)
R_CHECK=1
else
R_CHECK=0

# Enabling local mode?
#%LOCAL_MODE%#

# in local mode: use pdflatex
ifdef LOCAL_MODE
USE_PDFLATEX=1
endif

export R_LIBS=$(shell pwd)/../../lib
export MAKE_R_PACKAGE

ifdef LOCAL_MODE
MAKEPDF=1
endif

# Type of pre-install:
# for back-compatibility
ifdef QUICK
quick=1
endif

ifdef quick
install=quick
endif
ifndef install
install=yes
endif

ifeq ('$(install)','yes')
# install in temporary directory at each run
TMP_INSTALL_DIR:=$(shell mktemp -d)
else
ifeq ('$(install)','quick')
QUICK=1
$(shell mkdir -p tmplib)
TMP_INSTALL_DIR:=tmplib
endif
endif

endif #end not R_CHECK


# Define command for temporary installation (used when make is directly called,
# i.e. when not in check/build/INSTALL)
ifdef TMP_INSTALL_DIR
define do_install
  @if [ ! -d "$(TMP_INSTALL_DIR)" ]; then \
  	echo "ERROR: installation directory '$(TMP_INSTALL_DIR)' does not exist."; \
  	exit 1; \
  fi
  
  @if [ ! -d "$(TMP_INSTALL_DIR)/$(MAKE_R_PACKAGE)" ]; then \
		echo "# Installing the package in tempdir '$(TMP_INSTALL_DIR)'"; \
  	$(eval R_LIBS := $(TMP_INSTALL_DIR):$(R_LIBS)) \
  	echo "# Using R_LIBS: $(R_LIBS)"; \
  	$(RSCRIPT) --vanilla --quiet -e "pkgmaker::quickinstall('..', lib='$(TMP_INSTALL_DIR)')" > Rinstall.log 2> Rinstall.err; \
  	if [ ! -d "$(TMP_INSTALL_DIR)/$(MAKE_R_PACKAGE)" ]; then \
  		echo "ERROR: Temporary installation failed: see files Rinstall.log and Rinstall.err"; \
  		echo "# Removing temporary library directory $(TMP_INSTALL_DIR)"; \
  		exit 1; \
  	else \
  		echo "# Package successfully installed"; \
  	fi \
  fi
endef
else
define do_install
endef	
endif

#ifdef LOCAL_MODE
#define update_inst_doc
#	# Updating PDF files in inst/doc
#	mv -f *.pdf ../inst/doc
#endef
#else
#define update_inst_doc
#endef	
#endif

all: init $(PDF_OBJS) do_clean
	@echo "# All vignettes in 'vignettes' are up to date"

init:
	# Generating vignettes for package $(MAKE_R_PACKAGE)
	# Maintainer(s): #%USER%#
ifdef LOCAL_MODE
	# Mode: Local Development [$(LOCAL_MODE)]
else
	# Mode: Production
endif
ifneq ($(R_CHECK),0)
	# R CMD check: TRUE
else
	# R CMD check: FALSE
endif

	# Detected vignettes: $(RNW_OBJS)

clean:
	rm -fr *.bbl *.run.xml *.blg *.aux *.out *-blx.bib \
	*.log *.err Rplots.pdf tests-results tmplib vignette_*.mk vignette.mk \
	cleveref.sty 
ifndef LOCAL_MODE
	rm -f $(TEX_OBJS)
endif

clean-all: clean
	rm -fr $(TEX_OBJS) $(PDF_OBJS) $(RNW_SRCS)

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript

.SECONDARY: %.tex

do_clean:
ifndef QUICK
	# Removing temporary install directory '$(TMP_INSTALL_DIR)'
	@-rm -rf $(TMP_INSTALL_DIR);
endif

# Generate .pdf from .Rnw
%.pdf: ${SRC_DIR}/%.Rnw
	$(do_install)
	# Generating vignette $@ from ${SRC_DIR}/$*.Rnw
	# Using R_LIBS: $(R_LIBS)
	# Compiling ${SRC_DIR}/$*.Rnw into $*.tex
	$(RSCRIPT) --vanilla -e "pkgmaker::rnw('${SRC_DIR}/$*.Rnw', '$*.tex');"
		
	# Generating pdf $@ from $*.tex
ifdef MAKEPDF
ifdef USE_PDFLATEX
	$(eval VIGNETTE_BASENAME := $*)
	# Using pdflatex
	# LaTeX compilation 1/3
	@pdflatex $(VIGNETTE_BASENAME) >> $(VIGNETTE_BASENAME)-pdflatex.log
	# Compiling bibliography with bibtex
	-bibtex $(VIGNETTE_BASENAME)
	# LaTeX compilation 2/3
	@pdflatex $(VIGNETTE_BASENAME) >> $(VIGNETTE_BASENAME)-pdflatex.log
	# LaTeX compilation 3/3
	@pdflatex $(VIGNETTE_BASENAME) >> $(VIGNETTE_BASENAME)-pdflatex.log
ifndef QUICK
	# Compact vignettes
	$(RSCRIPT) --vanilla -e "tools::compactPDF('$(VIGNETTE_BASENAME).pdf')"
endif
	# Remove temporary LaTeX files (but keep the .tex)
	rm -fr $(VIGNETTE_BASENAME).toc $(VIGNETTE_BASENAME).log \
	$(VIGNETTE_BASENAME).bbl $(VIGNETTE_BASENAME).blg \
	$(VIGNETTE_BASENAME).aux $(VIGNETTE_BASENAME).out $(VIGNETTE_BASENAME)-blx.bib	
	
else
	# Using tools::texi2dvi
	# LaTeX compilation 1/2
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( '$*.tex', pdf = TRUE, clean = FALSE )"
	# Compiling bibliography with bibtex
	-bibtex $*
	# LaTeX compilation 2/2
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( '$*.tex', pdf = TRUE, clean = TRUE )"
endif
endif	
	# Update fake vignette file ./$*.Rnw
	$(RSCRIPT) --vanilla -e "pkgmaker::makeFakeVignette('${SRC_DIR}/$*.Rnw', '$*.Rnw')"
	$(update_inst_doc)

# only run tests if not checking: CRAN check run the tests separately
%-unitTests.pdf:
	$(do_install)
	# Generating vignette for unit tests: $@
	# Using R_LIBS: $(R_LIBS)
	$(RSCRIPT) --vanilla -e "pkgmaker::makeUnitVignette('package:$(MAKE_R_PACKAGE)', check=$(R_CHECK))" >> unitTests.log
ifdef LOCAL_MODE
	$(eval VIGNETTE_BASENAME := $(shell basename $@ .pdf))
	# Compact vignette file
	$(RSCRIPT) --vanilla -e "tools::compactPDF('$(VIGNETTE_BASENAME).pdf')"
endif
	$(update_inst_doc)

#-o vim: set noexpandtab:

# Useful commands
# - make         compiles your project 
# - make tidy    removes all intermediary tex files (like *.aux)
# - make clean   removes all compiled files (like *.pdf, *.aux)
# - make release creates a tarball that you can give to others

#--------------------------------------------------------------------
# Makefile for the LEX2ALL Converter
# 
# Written by Y. Parmentier (LORIA)
# Based on Geni's Makefile by E. Kow (LORIA)
# 
# date: 14 Nov 2005
#--------------------------------------------------------------------

# --------------------------------------------------------------------
# configuration 
# --------------------------------------------------------------------

MYDIR    = LEX2ALL
SRC 	 = .
VERSION  = 1.1
GHCFLAGS = $(LDFLAGS) -cpp -fglasgow-exts -threaded -O
PREFIX   = /usr/local
BINDIR   = $(PREFIX)/bin
INSTALL  = install


GHCINCLUDE      = -i$(SRC)
GHC             = ghc $(GHCFLAGS) $(GHCINCLUDE)
#GHC_PROF	= $(GHC) -prof -auto-all -hisuf p_hi -osuf p_o 


SOFTWARE        = lexConverter
SOFTVERS        = $(SOFTWARE)-$(VERSION)

TO_INSTALL      = $(SOFTWARE)

# --------------------------------------------------------------------
# source stuff 
# --------------------------------------------------------------------

SCRIPT_FILES = 

IFILE = $(SRC)/Main
OFILE = ./$(SOFTWARE)

SOURCE_FILES := $(wildcard $(SRC)/*.hs)
SOURCE_HSPP  := $(SOURCE_FILES)

# Phony targets do not keep track of file modification times
.PHONY: all clean release

# --------------------------------------------------------------------
# main targets 
# --------------------------------------------------------------------

normal: compile
all: compile tidy
release: compile tidy tarball

tarball:
	rm -f $(MYDIR)*.tar.gz;\
	cd .. ;\
	tar czvf $(MYDIR)_$(DATE).tar.gz $(MYDIR);\
	mv $(MYDIR)_$(DATE).tar.gz $(MYDIR)

clean: tidy
	rm -f *~
	rm $(OFILE)

tidy:
	rm -f $(SRC)/*.o $(SRC)/*.hi

test: test/test.tagml test/test.tag.xml test/test.geni

test/%.tagml: test/%.lex $(SOFTWARE)
	./$(SOFTWARE) -L -i $< -o $@ -l

test/%.tag.xml: test/%.lex $(SOFTWARE)
	./$(SOFTWARE) -L -i $< -o $@ -d

test/%.geni: test/%.lex $(SOFTWARE)
	./$(SOFTWARE) -L -i $< -o $@ -g


# --------------------------------------------------------------------
# compilation 
# --------------------------------------------------------------------

compile: $(OFILE) 

$(OFILE) : $(SOURCE_FILES)
	$(GHC) -W --make $(IFILE).hs -o $(OFILE)

# --------------------------------------------------------------------
# installing 
# --------------------------------------------------------------------

install:
	mkdir -p $(BINDIR)
	$(foreach file,$(TO_INSTALL), $(INSTALL) $(file) $(BINDIR) &&) :

uninstall:
	$(foreach file,$(TO_INSTALL), $(RM) $(BINDIR)/$(file) &&) :


# --------------------------------------------------------------------
# documentation 
# --------------------------------------------------------------------

# haddock: $(SOURCE_HSPP)
# 	mkdir -p $(HADDOCK_OUT)
# 	haddock -h $(SOURCE_HSPP) -o $(HADDOCK_OUT)

# $(SOURCE_HSPP_1): %.hspp: %.lhs
# 	etc/lhs2haddock < $< > $@

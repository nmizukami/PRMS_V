#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: prmsv

prmsv:
# Create lib directory, if necessary
	@if [ ! -d $(MMFDIR) ]   ; then        \
	  mkdir $(MMFDIR) ;                   \
	  echo  Created directory $(MMFDIR) ; \
	fi
# Create bin directory, if necessary
	@if [ ! -d $(BINDIR) ]   ; then        \
	  mkdir $(BINDIR) ;                   \
	  echo  Created directory $(BINDIR) ; \
	fi
	cd $(MMFDIR); $(MAKE);
	cd $(PRMSDIR); $(MAKE);

clean:
	cd $(MMFDIR); $(MAKE) clean;
	cd $(PRMSDIR); $(MAKE) clean;
	$(RM) $(BINDIR)/prmsv*~


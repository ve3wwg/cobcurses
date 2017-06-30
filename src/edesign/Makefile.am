# edesign/Makefile.am for CobCurses

#############################################################
# 	COBOL RELATED OPTIONS
#############################################################

COBWARN=-Wall -Wno-call-params
COBSTD=-std=cobol85
COBOPTS=$(COBWARN) $(COBDBG) $(COBSTD) -I../copybk -free
COBOL=cobc

.cob.$(OBJEXT):
	$(COBOL) -c $(COBOPTS) $< -o $*.$(OBJEXT)

.PHONY: all lcl_clean lcl_distclean

#############################################################

BUILT_EXES = edesign$(EXEEXT) 

BUILT_SRCS = ED000010-PD.cbl ED000010-WS.cbl

all: edesign$(EXEEXT) EDPR010$(COBCURSES_MODULE_EXT)

OBJS=

edesign$(EXEEXT): ed000010.cob $(OBJS) ED000010-WS.cbl ED000010-PD.cbl
	$(COBOL) -x $(COBOPTS) ed000010.cob $(OBJS) -o edesign$(EXEEXT)

EDPR010$(COBCURSES_MODULE_EXT): EDPR010.cob EDPR010-WS.cbl EDPR010-PD.cbl
	$(COBOL) -m $(COBOPTS) EDPR010.cob -o EDPR010.$(COBCURSES_MODULE_EXT)

ED000010-PD.cbl ED000010-WS.cbl:
	sdgenerate -s ED000010

EDPR010-PD.cbl EDPR010-WS.cbl:
	sdgenerate -s EDPR010

clean-local:
	rm -f *.$(OBJEXT) core *.stackdump
	rm -f edesign$(EXEEXT)
	rm -f $(BUILT_SRCS) errs.t
	rm -f *.$(COBCURSES_MODULE_EXT)
	rm -f EDPR*-??.cbl ED000*-??.cbl

distclean-local: lcl_clean
	rm -f Makefile
	rm -fr .deps

# install-exec-local: $(BUILT_EXES)
# 	@$(NORMAL_INSTALL)
# 	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
# 	@list='$(BUILT_EXES)'; for p in $$list; do \
# 	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
# 	  if test -f $$d$$p; then \
# 	    echo " $(binSCRIPT_INSTALL) '$$d$$p' '$(DESTDIR)$(bindir)/$$p'"; \
# 	    $(install_sh_PROGRAM) "$$d$$p" "$(DESTDIR)$(bindir)/$$p"; \
# 	  else :; fi; \
# 	done
# 
# uninstall-local:
# 	@$(NORMAL_UNINSTALL)
# 	@list='$(BUILT_EXES)'; for p in $$list; do \
# 	  echo " rm -f '$(DESTDIR)$(bindir)/$$p'"; \
# 	  rm -f "$(DESTDIR)$(bindir)/$$p"; \
# 	done

# End sdesign/Makefile.am

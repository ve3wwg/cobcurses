# utils/Makefile.am for CobCurses

#############################################################
# 	COBOL RELATED OPTIONS
#############################################################

COBWARN=-Wall -Wno-call-params
COBSTD=-std=cobol85
COBOPTS=$(COBWARN) $(COBDBG) $(COBSTD) -I../copybk -free
COBOL=cobc

.cob.$(OBJEXT):
	$(COBOL) -c $(COBOPTS) $< -o $*.$(OBJEXT)

#############################################################

.PHONY: initdata

bin_SCRIPTS=$(BIN_EXES)

UTILS_BINDIR=@COBCURSES_INST_BINDIR@

BUILT_EXES=

BIN_EXES=sdbackup$(EXEEXT) sdload$(EXEEXT) init-cobc$(EXEEXT) \
  sdgenerate$(EXEEXT) loadtm$(EXEEXT) unloadtm$(EXEEXT) sdmenu$(EXEEXT)

all-local: $(BUILT_EXES) initdata

initdata: ../data/SCREENS.X

../data/SCREENS.X: init-cobc$(EXEEXT)
	./lclinit -f -d ../data ../bak/backup.bak

sdgenerate$(EXEEXT): sdgenerate.cob
	$(COBOL) -x $(COBOPTS) sdgenerate.cob -o sdgenerate$(EXEEXT)

sdbackup$(EXEEXT): sdbackup.cob ../copybk/BKU-RECD.cbl ../lib/codegen.$(OBJEXT)
	$(COBOL) -x $(COBOPTS) sdbackup.cob ../lib/codegen.$(OBJEXT) -o sdbackup$(EXEEXT)

init-cobc$(EXEEXT): init-cobc.cob
	$(COBOL) -x $(COBOPTS) init-cobc.cob -o init-cobc$(EXEEXT)

sdload$(EXEEXT): sdload.cob ../copybk/BKU-RECD.cbl ../lib/codegen.$(OBJEXT)
	$(COBOL) -x $(COBOPTS) sdload.cob ../lib/codegen.$(OBJEXT) -o sdload$(EXEEXT)

loadtm$(EXEEXT): loadtm.cob
	$(COBOL) -x $(COBOPTS) loadtm.cob -o loadtm$(EXEEXT)

unloadtm$(EXEEXT): unloadtm.cob
	$(COBOL) -x $(COBOPTS) unloadtm.cob -o unloadtm$(EXEEXT)

sdmenu.$(OBJEXT): sdmenu.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -c sdmenu.c -o sdmenu.$(OBJEXT)

sdmenu$(EXEEXT): sdmenu.$(OBJEXT)
	$(COBOL) -x sdmenu.$(OBJEXT) -o sdmenu$(EXEEXT)

clean-local:
	rm -f *.$(OBJEXT) core 
	rm -f $(BUILT_EXES) $(BIN_EXES)
	rm -fr ../data

distclean-local:
	rm -f Makefile sdgenerate
	rm -fr .deps

install-exec-local: $(BUILT_EXES) install-binSCRIPTS
	@$(NORMAL_INSTALL)
	test -z "$(libexecdir)" || $(MKDIR_P) "$(DESTDIR)$(libexecdir)"
	@list='$(BUILT_EXES)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  if test -f $$d$$p; then \
	    echo " $(binSCRIPT_INSTALL) '$$d$$p' '$(DESTDIR)$(libexecdir)/$$p'"; \
	    $(install_sh_PROGRAM) "$$d$$p" "$(DESTDIR)$(libexecdir)/$$p"; \
	  else :; fi; \
	done

uninstall-local: uninstall-binSCRIPTS
	@$(NORMAL_UNINSTALL)
	@list='$(BUILT_EXES)'; for p in $$list; do \
	  echo " rm -f '$(DESTDIR)$(libexecdir)/$$p'"; \
	  rm -f "$(DESTDIR)$(libexecdir)/$$p"; \
	done

# End utils/Makefile.am

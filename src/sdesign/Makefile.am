# sdesign/Makefile.am for CobCurses

#############################################################
# 	COBOL RELATED OPTIONS
#############################################################

COBWARN=-Wall -Wno-call-params
COBSTD=-std=cobol2002
COBOPTS=$(COBWARN) $(COBDBG) $(COBSTD) -I../copybk -free
COBOL=cobc

.cob.$(OBJEXT):
	$(COBOL) -c $(COBOPTS) $< -o $*.$(OBJEXT)

.PHONY: all lcl_clean lcl_distclean

#############################################################

OBJS=sd002010.$(OBJEXT) sd002020.$(OBJEXT) sd002030.$(OBJEXT) \
	sd002033.$(OBJEXT) sd002035.$(OBJEXT) sd002040.$(OBJEXT) sd002050.$(OBJEXT) \
	sd002060.$(OBJEXT) sd002070.$(OBJEXT) sd002080.$(OBJEXT) sd002090.$(OBJEXT) \
	sd002085.$(OBJEXT)

BUILT_EXES = sdesign$(EXEEXT)

BUILT_SRCS = SD002010-PD.cbl SD002010-WS.cbl SD002020-PD.cbl SD002020-WS.cbl \
		SD002040-PD.cbl SD002040-WS.cbl SD002050-PD.cbl SD002050-WS.cbl \
		SD002060-PD.cbl SD002060-WS.cbl SD002070-WS.cbl SD002070-PD.cbl \
		SD002080-WS.cbl SD002080-PD.cbl SD002090-WS.cbl SD002090-PD.cbl \
		SD002085-WS.cbl SD002085-PD.cbl

all: sdesign$(EXEEXT)

sdesign$(EXEEXT): $(OBJS)
	$(COBOL) -x $(COBOPTS) sd002000.cob $(OBJS) -o sdesign$(EXEEXT)

SD002010-PD.cbl SD002010-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002010

SD002020-PD.cbl SD002020-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002020

SD002040-PD.cbl SD002040-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002040

SD002050-PD.cbl SD002050-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002050

SD002060-PD.cbl SD002060-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002060

SD002070-PD.cbl SD002070-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002070

SD002080-PD.cbl SD002080-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002080

SD002090-PD.cbl SD002090-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002090

SD002085-PD.cbl SD002085-WS.cbl:
	../utils/genscr -D $(PWD)/../data -s SD002085

sd002010.o: SD002010-PD.cbl SD002010-WS.cbl sd002010.cob
sd002020.o: SD002020-PD.cbl SD002020-WS.cbl sd002020.cob
sd002030.o: ../copybk/GRPHMENU.cbl
sd002040.o: SD002040-PD.cbl SD002040-WS.cbl sd002040.cob
sd002050.o: SD002050-PD.cbl SD002050-WS.cbl sd002050.cob
sd002060.o: SD002060-PD.cbl SD002060-WS.cbl sd002060.cob
sd002070.o: SD002070-PD.cbl SD002070-WS.cbl sd002070.cob
sd002080.o: SD002080-PD.cbl SD002080-WS.cbl sd002080.cob
sd002090.o: SD002090-PD.cbl SD002090-WS.cbl sd002090.cob
sd002085.o: SD002085-PD.cbl SD002085-WS.cbl sd002085.cob

clean-local:
	rm -f *.$(OBJEXT) core *.stackdump
	rm -f sdesign$(EXEEXT)
	rm -f $(BUILT_SRCS) errs.t

distclean-local: lcl_clean
	rm -f Makefile
	rm -fr .deps

install-exec-local: $(BUILT_EXES)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	@list='$(BUILT_EXES)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  if test -f $$d$$p; then \
	    echo " $(binSCRIPT_INSTALL) '$$d$$p' '$(DESTDIR)$(bindir)/$$p'"; \
	    $(install_sh_PROGRAM) "$$d$$p" "$(DESTDIR)$(bindir)/$$p"; \
	  else :; fi; \
	done

uninstall-local:
	@$(NORMAL_UNINSTALL)
	@list='$(BUILT_EXES)'; for p in $$list; do \
	  echo " rm -f '$(DESTDIR)$(bindir)/$$p'"; \
	  rm -f "$(DESTDIR)$(bindir)/$$p"; \
	done

# End sdesign/Makefile.am

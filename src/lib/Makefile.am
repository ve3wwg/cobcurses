# lib/Makefile.am for cobcurses

#############################################################
# 	COBOL RELATED OPTIONS
#############################################################

AM_CFLAGS=$(SHARED_PIC_FLAGS)

COBWARN=-Wall -Wno-call-params
COBSTD=-std=cobol2002
COBOPTS=$(COBWARN) $(COBDBG) $(COBSTD) -I../copybk -free
COBOL=cobc

.cob.$(OBJEXT):
	$(COBOL) -c $(COBOPTS) $< -o $*.$(OBJEXT)

#############################################################

noinst_PROGRAMS = genkeys
genkeys_SOURCES = genkeys.c

SHARED_LIBS = libcobcurses.$(COBCURSES_MODULE_EXT) libcobcursex.$(COBCURSES_MODULE_EXT) \
    libcobcurses_codegen.$(COBCURSES_MODULE_EXT) \
    COBCURSES-INIT-PATHNAME.$(COBCURSES_MODULE_EXT) \
    COBCURSES-SCI-NOTATION-COMP-2.$(COBCURSES_MODULE_EXT) \
    COBCURSES-SCI-NOTATION-COMP-1.$(COBCURSES_MODULE_EXT) \
    COBCURSES-ECVT-COMP-2.$(COBCURSES_MODULE_EXT) \
    COBCURSES-ECVT-COMP-1.$(COBCURSES_MODULE_EXT) \
    COBCURSES-EDIT-COMP-2.$(COBCURSES_MODULE_EXT) \
    COBCURSES-EDIT-COMP-1.$(COBCURSES_MODULE_EXT) \
    COBCURSES-FORMAT-COMP-2.$(COBCURSES_MODULE_EXT) \
    COBCURSES-FORMAT-COMP-1.$(COBCURSES_MODULE_EXT) \
    COBCURSES-TEST-UNITS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-SCREENS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-FIELDS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-CHARSETS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-MENUS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-STATES.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-MENU-ITEMS.$(COBCURSES_MODULE_EXT) \
    COBCURSES-MENU-SD-MENU-MREFS.$(COBCURSES_MODULE_EXT)

LIBOBJS=cobcurses.$(OBJEXT) terminal.$(OBJEXT) term_curses.$(OBJEXT) term_curses_conv.$(OBJEXT) \
 	term_curses_menu.$(OBJEXT) cc_menu.$(OBJEXT) \
	environ.$(OBJEXT) misc.$(OBJEXT) dynstr.$(OBJEXT) pathname.$(OBJEXT) \
	fcomp2.$(OBJEXT) ecomp1.$(OBJEXT) ecomp2.$(OBJEXT) expon.$(OBJEXT) \
	ecvt.$(OBJEXT) units.$(OBJEXT) eunits.$(OBJEXT) enumber.$(OBJEXT) cobmenu.$(OBJEXT) \
	cobtrace.$(OBJEXT)

LIBXOBJS=csvsupport.$(OBJEXT) assoc.$(OBJEXT) assoc_cob.$(OBJEXT) misc.$(OBJEXT) dynstr.$(OBJEXT)
LIBPOBJS=pathname.$(OBJEXT) dynstr.$(OBJEXT) misc.$(OBJEXT) environ.$(OBJEXT) initcobc.$(OBJEXT)

LIBDEPS=../copybk/COBCATTR.cbl ../copybk/COBCCOLOUR.cbl ../copybk/COBCPARMS.cbl \
	../copybk/COBCURSG.cbl ../copybk/COBCURSL.cbl ../copybk/COBCEXTRA.cbl ../cobcurses.h

libcobcurses.$(COBCURSES_MODULE_EXT): libcobcurses.cob $(LIBOBJS)
	$(COBOL) -b $(COBOPTS) libcobcurses.cob $(LIBOBJS) $(COBCURSES_LIBS)

libcobcurses_codegen.$(COBCURSES_MODULE_EXT): libcobcurses_codegen.cob codegen.$(OBJEXT)
	$(COBOL) -b $(COBOPTS) libcobcurses_codegen.cob codegen.$(OBJEXT)

libcobcursex.$(COBCURSES_MODULE_EXT): $(LIBXOBJS) $(LIBDEPS) assoc_api.cob libcobcursex.cob
	$(COBOL) -b $(COBOPTS) libcobcursex.cob assoc_api.cob $(LIBXOBJS)

COBCURSES-INIT-PATHNAME.$(COBCURSES_MODULE_EXT): $(LIBPOBJS) cobcurses_init_pathname.cob
	$(COBOL) -b $(COBOPTS) cobcurses_init_pathname.cob $(LIBPOBJS) -o COBCURSES-INIT-PATHNAME.$(COBCURSES_MODULE_EXT)

COBCURSES-SCI-NOTATION-COMP-2.$(COBCURSES_MODULE_EXT): expon.$(OBJEXT) ecvt.$(OBJEXT) cobcurses_sci_notation_comp_2.cob
	$(COBOL) -b $(COBOPTS) cobcurses_sci_notation_comp_2.cob expon.$(OBJEXT) ecvt.$(OBJEXT) -o COBCURSES-SCI-NOTATION-COMP-2.$(COBCURSES_MODULE_EXT)

COBCURSES-SCI-NOTATION-COMP-1.$(COBCURSES_MODULE_EXT): expon.$(OBJEXT) ecvt.$(OBJEXT) cobcurses_sci_notation_comp_1.cob
	$(COBOL) -b $(COBOPTS) cobcurses_sci_notation_comp_1.cob expon.$(OBJEXT) ecvt.$(OBJEXT) -o COBCURSES-SCI-NOTATION-COMP-1.$(COBCURSES_MODULE_EXT)

COBCURSES-ECVT-COMP-2.$(COBCURSES_MODULE_EXT): expon.$(OBJEXT) ecvt.$(OBJEXT) cobcurses_ecvt_comp_2.cob
	$(COBOL) -b $(COBOPTS) cobcurses_ecvt_comp_2.cob expon.$(OBJEXT) ecvt.$(OBJEXT) -o COBCURSES-ECVT-COMP-2.$(COBCURSES_MODULE_EXT)

COBCURSES-ECVT-COMP-1.$(COBCURSES_MODULE_EXT): expon.$(OBJEXT) ecvt.$(OBJEXT) cobcurses_ecvt_comp_1.cob
	$(COBOL) -b $(COBOPTS) cobcurses_ecvt_comp_1.cob expon.$(OBJEXT) ecvt.$(OBJEXT) -o COBCURSES-ECVT-COMP-1.$(COBCURSES_MODULE_EXT)

OBJSEDITCOMP2=ecomp2.$(OBJEXT) dynstr.$(OBJEXT) eunits.$(OBJEXT) misc.$(OBJEXT) units.$(OBJEXT) enumber.$(OBJEXT)
COBCURSES-EDIT-COMP-2.$(COBCURSES_MODULE_EXT): cobcurses_edit_comp2.cob $(OBJSEDITCOMP2)
	$(COBOL) -b $(COBOPTS) cobcurses_edit_comp2.cob $(OBJSEDITCOMP2) -o COBCURSES-EDIT-COMP-2.$(COBCURSES_MODULE_EXT)

OBJSEDITCOMP1=ecomp1.$(OBJEXT) dynstr.$(OBJEXT) eunits.$(OBJEXT) misc.$(OBJEXT) units.$(OBJEXT) enumber.$(OBJEXT)
COBCURSES-EDIT-COMP-1.$(COBCURSES_MODULE_EXT): cobcurses_edit_comp1.cob $(OBJSEDITCOMP1)
	$(COBOL) -b $(COBOPTS) cobcurses_edit_comp1.cob $(OBJSEDITCOMP1) -o COBCURSES-EDIT-COMP-1.$(COBCURSES_MODULE_EXT)

OBJSFMTCOMP2=fcomp2.$(OBJEXT) dynstr.$(OBJEXT) units.$(OBJEXT) misc.$(OBJEXT) expon.$(OBJEXT) ecvt.$(OBJEXT)
COBCURSES-FORMAT-COMP-2.$(COBCURSES_MODULE_EXT): cobcurses_format_comp2.cob $(OBJSFMTCOMP2)
	$(COBOL) -b $(COBOPTS) cobcurses_format_comp2.cob $(OBJSFMTCOMP2) -o COBCURSES-FORMAT-COMP-2.$(COBCURSES_MODULE_EXT)

COBCURSES-FORMAT-COMP-1.$(COBCURSES_MODULE_EXT): cobcurses_format_comp1.cob
	$(COBOL) -b $(COBOPTS) cobcurses_format_comp1.cob -o COBCURSES-FORMAT-COMP-1.$(COBCURSES_MODULE_EXT)

OBJSTESTUNITS=cobcurses_test_units.cob testunits.$(OBJEXT) misc.$(OBJEXT) units.$(OBJEXT) dynstr.$(OBJEXT)
COBCURSES-TEST-UNITS.$(COBCURSES_MODULE_EXT): $(OBJSTESTUNITS)
	$(COBOL) -b $(COBOPTS) $(OBJSTESTUNITS) -o COBCURSES-TEST-UNITS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-SCREENS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_screens.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_screens.cob -o COBCURSES-MENU-SD-SCREENS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-FIELDS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_fields.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_fields.cob -o COBCURSES-MENU-SD-FIELDS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-CHARSETS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_charsets.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_charsets.cob -o COBCURSES-MENU-SD-CHARSETS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-MENUS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_menus.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_menus.cob -o COBCURSES-MENU-SD-MENUS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-STATES.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_states.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_states.cob -o COBCURSES-MENU-SD-STATES.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-MENU-ITEMS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_menu_items.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_menu_items.cob -o COBCURSES-MENU-SD-MENU-ITEMS.$(COBCURSES_MODULE_EXT)

COBCURSES-MENU-SD-MENU-MREFS.$(COBCURSES_MODULE_EXT): cobcurses_menu_sd_mrefs.cob
	$(COBOL) -m $(COBOPTS) cobcurses_menu_sd_mrefs.cob -o COBCURSES-MENU-SD-MENU-MREFS.$(COBCURSES_MODULE_EXT)

all:	all-local

all-local: $(SHARED_LIBS)

distclean-local: clean
	rm -f Makefile
	rm -fr .deps

clean-local:
	rm -f *.$(OBJEXT) core errs.t
	rm -f $(SHARED_LIBS)

install-exec-local: $(SHARED_LIBS)
	@$(NORMAL_INSTALL)
	test -z "$(libdir)" || $(MKDIR_P) "$(DESTDIR)$(libdir)"
	@list='$(SHARED_LIBS)'; for p in $$list; do \
	  if test -f $$p ; then \
	   echo " $(INSTALL_PROGRAM_ENV) $(install_sh_PROGRAM) '$$p' '$(DESTDIR)$(libdir)/$$p'"; \
	   $(INSTALL_PROGRAM_ENV) $(install_sh_PROGRAM) "$$p" "$(DESTDIR)$(libdir)/$$p" || exit 1; \
	  else :; fi; \
	done

uninstall-local:
	@$(NORMAL_UNINSTALL)
	@list='$(SHARED_LIBS)'; for p in $$list; do \
	  echo " rm -f '$(DESTDIR)$(libdir)/$$p'"; \
	  rm -f "$(DESTDIR)$(libdir)/$$p"; \
	done

cobcurses.$(OBJEXT): ../cobcurses.h environ.h misc.h terminal.h
csvsupport.$(OBJEXT): ../cobcurses.h misc.h assoc.h dynstr.h
assoc.$(OBJEXT): ../cobcurses.h assoc.h
assoc_cob.$(OBJEXT): ../cobcurses.h assoc.h
dynstr.$(OBJEXT): ../cobcurses.h dynstr.h misc.h
environ.$(OBJEXT): ../cobcurses.h dynstr.h misc.h environ.h
pathname.$(OBJEXT): ../cobcurses.h dynstr.h misc.h environ.h
enumber.$(OBJEXT): ../cobcurses.h enumber.h misc.h units.h
eunits.$(OBJEXT): ../cobcurses.h enumber.h eunits.h misc.h units.h
ecomp1.$(OBJEXT): ../cobcurses.h eunits.h misc.h units.h
ecomp2.$(OBJEXT): ../cobcurses.h eunits.h misc.h units.h
fcomp2.$(OBJEXT): ../cobcurses.h misc.h units.h
initcobc.$(OBJEXT): ../cobcurses.h misc.h environ.h
misc.$(OBJEXT): ../cobcurses.h misc.h
testunits.$(OBJEXT): ../cobcurses.h misc.h units.h
units.$(OBJEXT): ../cobcurses.h misc.h units.h
terminal.$(OBJEXT): ../cobcurses.h terminal.h term_curses.h
term_curses.$(OBJEXT): ../cobcurses.h terminal.h term_curses.h term_curses_priv.h term_curses_conv.h term_curses_menu.h
term_curses_conv.$(OBJEXT): ../cobcurses.h terminal.h term_curses.h term_curses_priv.h term_curses_conv.h
term_curses_menu.$(OBJEXT): ../cobcurses.h terminal.h term_curses.h term_curses_priv.h term_curses_menu.h
ecvt.$(OBJEXT): ../cobcurses.h 
expon.$(OBJEXT): ../cobcurses.h 
genkeys.$(OBJEXT): ../cobcurses.h terminal.h

# End lib/Makefile.am

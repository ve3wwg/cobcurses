# copybk/Makefile.am for cobcurses

PACKAGE_COPYBOOKS = COBC3270.cbl COBCATTR.cbl COBCCOLOUR.cbl COBCKEYS.cbl COBCPARMS.cbl \
	COBCURSG.cbl COBCURSL.cbl COBCURSP.cbl COBCURSQ.cbl COBCURSX.cbl COBCEXTRA.cbl \
        NULLEVENTS.cbl BKU-RECD.cbl

distclean-local :
	rm -f Makefile

install-data-local : $(PACKAGE_COPYBOOKS)
	@$(NORMAL_INSTALL)
	test -z "$(pkgincludedir)" || $(MKDIR_P) "$(DESTDIR)$(pkgincludedir)"
	@list='$(PACKAGE_COPYBOOKS)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  echo " $(includeHEADERS_INSTALL) '$$d$$p' '$(DESTDIR)$(pkgincludedir)/$$p'"; \
	  $(install_sh_DATA) "$$d$$p" "$(DESTDIR)$(pkgincludedir)/$$p"; \
	done

uninstall-local :
	@$(NORMAL_UNINSTALL)
	@list='$(PACKAGE_COPYBOOKS)'; for f in $$list; do \
	  echo " rm -f '$(DESTDIR)$(pkgincludedir)/$$f'"; \
	  rm -f "$(DESTDIR)$(pkgincludedir)/$$f"; \
	done

# End copybk/Makefile.am

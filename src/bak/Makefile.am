# bak/Makefile.am

BAKDIR="@COBCURSES_INST_DATADIR@/bak"
LDFILES=backup.bak data_tm.bak

install-data-am:   lcl_install
uninstall-am: lcl_uninstall

.PHONY: lcl_install lcl_uninstall

lcl_install: $(LDFILES)
	@$(NORMAL_INSTALL)
	test -z "$(BAKDIR)" || $(MKDIR_P) "$(DESTDIR)$(BAKDIR)"
	@list='$(LDFILES)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  if test -f $$d$$p; then \
	    echo " $(binSCRIPT_INSTALL) '$$d$$p' '$(DESTDIR)$(BAKDIR)/$$p'"; \
	    $(install_sh_DATA) "$$d$$p" "$(DESTDIR)$(BAKDIR)/$$p"; \
	  else :; fi; \
	done

lcl_uninstall:
	@$(NORMAL_UNINSTALL)
	@list='$(LDFILES)'; for p in $$list; do \
	  echo " rm -f '$(DESTDIR)$(BAKDIR)/$$p'"; \
	  rm -f "$(DESTDIR)$(BAKDIR)/$$p"; \
	done

# End bak/Makefile.am

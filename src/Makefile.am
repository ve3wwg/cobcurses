# Top-level/Makefile.am

PACKAGE_INCLUDES = cobcurses.h config.h
bin_SCRIPTS = cobcurses-config
SUBDIRS = bak copybk lib utils sdesign

check-local:
	@(cd ./test; $(MAKE) $(AM_MAKEFLAGS) check)

install-data-local: $(PACKAGE_INCLUDES)
	@$(NORMAL_INSTALL)
	test -z "$(pkgincludedir)" || $(MKDIR_P) "$(DESTDIR)$(pkgincludedir)"
	@list='$(PACKAGE_INCLUDES)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " $(includeHEADERS_INSTALL) '$$d$$p' '$(DESTDIR)$(pkgincludedir)/$$f'"; \
	  $(install_sh_DATA) "$$d$$p" "$(DESTDIR)$(pkgincludedir)/$$f"; \
	done

uninstall-local:
	@$(NORMAL_UNINSTALL)
	@list='$(PACKAGE_INCLUDES)'; for p in $$list; do \
	  f=$(am__strip_dir) \
	  echo " rm -f '$(DESTDIR)$(pkgincludedir)/$$p'"; \
	  rm -f "$(DESTDIR)$(pkgincludedir)/$$p"; \
	done
	rm -r "$(DESTDIR)$(pkgincludedir)"

install-exec-local:
	@{ \
		echo ; \
		echo "************************************************************" ; \
		echo "Note: You'll need to establish or modify the" ; \
		echo "environment variable COB_LIBRARY_PATH, in order for" ; \
		echo "the executable components to find the CobCurses" ; \
		echo "shared library.  Add the following pathname :" ; \
		echo ; \
		echo "  @COBCURSES_INST_LIBDIR@ " ; \
		echo ; \
		echo "to the existing COB_LIBRARY_PATH, or establish" ; \
		echo "a new one. The following is recommended: " ; \
		echo ; \
		echo "COB_LIBRARY_PATH=\"@COBCURSES_INST_LIBDIR@:\$$COB_LIBRARY_PATH\"" ; \
		echo "export COB_LIBRARY_PATH" ; \
		echo ; \
		echo "Add this to your /etc/profile, \$$HOME/.profile or other" ; \
		echo "suitable environment setup script." ; \
		echo ; \
		echo "Following that you need to initialize your personal" ; \
		echo "CobCurses database by invoking the command : " ; \
		echo ; \
		echo "  init-cobc [-t -]" ; \
		echo ; \
		echo "This initializes files in @COBCURSES_DATADIR@" ; \
		echo "for you. Specifying the option 't -' will cause" ; \
		echo "the optional Template indexed file to be created" ; \
		echo "for the Template demo program." ; \
		echo "************************************************************" ; \
		echo ; \
	}

clean-local:
	@(cd ./test; $(MAKE) $(AM_MAKEFLAGS) clean)
	@(cd ./edesign; $(MAKE) $(AM_MAKEFLAGS) clean)

distclean-local:
	rm -f config.h.in~

$(top_builddir)/config.status: $(top_srcdir)/configure $(CONFIG_STATUS_DEPENDENCIES)
	@{ \
		if test -f $(top_builddir)/config.status ; then \
			touch $(top_builddir)/config.status ; \
		else \
			echo "You need to ./configure" ; \
			exit 1; \
		fi ; \
	}

# End top-level/Makefile.am

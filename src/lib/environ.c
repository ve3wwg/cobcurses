/* CobCurses : environment routines for OpenCobol
 * Warren W. Gay
 * Wed Jul 25 10:05:09 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <errno.h>

#include <misc.h>
#include <dynstr.h>
#include <environ.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

static const char *cobcurses_topdir = 0;	/* getenv(COBCURSES_TOPDIR) */
static const char *cobcurses_datadir = 0;	/* getenv(COBCURSES_DATADIR) */
static const char *cobcurses_sharedir = 0;	/* getenv(COBCURSES_SHAREDIR) */

/*
 * As part of initialization, mkdir(TOPDIR) and
 * mkdir(DATADIR) as required.
 */
static void
make_directory(const char *dirname) {
	char buf[1025];
	cct_ushort plen;

	if ( !dirname )
		return;		/* No pathname */

	/*
	 * Perform variable subsitutions :
	 */
	strncpy(buf,dirname,sizeof buf-1)[sizeof buf - 1] = 0;
	plen = strlen(buf);
	if ( plen < sizeof buf ) {
		memset(buf+plen,' ',sizeof buf - plen - 1);
	}
	cobcurses_substitute(buf,sizeof buf - 1);
	plen = (cct_ushort)cobcurses_trim_trailing(buf,sizeof buf - 1,' ');
	buf[plen] = 0;

	/*
	 * Best effort attempt to make the necessary
	 * directory :
	 */
	MKDIR(buf,0755);
}

/*
 * Perform intialization of default environment values :
 */
static void
do_initialization(void) {
	const char *cp;

	/*
	 * Define library default values :
	 */
	cp = cobcurses_getenv(COBCURSES_TOPDIR_ENVNAME);
	cobcurses_getenv(COBCURSES_DATADIR_ENVNAME);
	cobcurses_getenv(COBCURSES_SHAREDIR_ENVNAME);

	/*
	 * Define a default for USER_SHELL if necesary :
	 */
	if ( !getenv(USER_SHELL_ENVNAME) )
		cobcurses_setenv(USER_SHELL_ENVNAME,USER_SHELL);

	/*
	 * Make missing directory levels, if necessary :
	 */
	if ( !strcmp(cp,COBCURSES_TOPDIR) ) {
		/*
		 * These directories are made only if the default
		 * settings are in place. The defaults are established
		 * as $HOME/cobcurses for the TOPDIR, and ./data
		 * underneath that. If the environment has moved
		 * these to any other location, we don't want to
		 * willy-nilly create them, because there might be
		 * security implications.
		 */
		make_directory(cobcurses_getenv(COBCURSES_TOPDIR_ENVNAME));
		make_directory(cobcurses_getenv(COBCURSES_DATADIR_ENVNAME));
	}
}

/*
 * Get the value named. For certain variables, if the
 * value is unknown (like COBCURSES_DATADIR), define
 * a default and return its value.
 */
const char *
cobcurses_getenv(const char *env_name) {
	const char *cp = 0;
	static cct_bool needs_init = 1;

	if ( !strcmp(env_name,COBCURSES_TOPDIR_ENVNAME) ) {
		if ( !cobcurses_topdir && !(cobcurses_topdir = getenv(COBCURSES_TOPDIR_ENVNAME)) )
			cobcurses_topdir = COBCURSES_TOPDIR;
		cp = cobcurses_topdir;
	} else if ( !strcmp(env_name,COBCURSES_DATADIR_ENVNAME) ) {
		if ( !cobcurses_datadir && !(cobcurses_datadir = getenv(COBCURSES_DATADIR_ENVNAME)) )
			cobcurses_datadir = COBCURSES_DATADIR;
		cp = cobcurses_datadir;
	} else if ( !strcmp(env_name,COBCURSES_SHAREDIR_ENVNAME) ) {
		if ( !cobcurses_sharedir && !(cobcurses_sharedir = getenv(COBCURSES_SHAREDIR_ENVNAME)) )
			cobcurses_sharedir = COBCURSES_SHAREDIR;
		cp = cobcurses_sharedir;
	} else	cp = getenv(env_name);

	if ( needs_init ) {
		needs_init = 0;
		do_initialization();
	}

	return cp;
}

/*
 * Create/Modify an environment variable :
 */
int
cobcurses_setenv(const char *varname,const char *value) {
	int rc = 0;
	
#ifdef HAVE_SETENV
	if ( setenv(varname,value,1) != 0 )
		rc = errno;
#else
	/*
	 * The string here must be malloc()ed, since it
	 * remains as part of the environment :
	 */
	cct_ushort vnlen = strlen(varname);
	cct_ushort valen = strlen(value);
	char *a = malloc(vnlen + 1 + valen + 1);

	strcpy(a,varname);
	a[vnlen] = '=';
	strcpy(a+vnlen+1,value);
	if ( putenv(a) != 0 )
		rc = errno;
#endif
	if ( !strcmp(varname,COBCURSES_TOPDIR_ENVNAME) )
		cobcurses_topdir = getenv(varname);
	if ( !strcmp(varname,COBCURSES_DATADIR_ENVNAME) )
		cobcurses_datadir = getenv(varname);
	if ( !strcmp(varname,COBCURSES_SHAREDIR_ENVNAME) )
		cobcurses_sharedir = getenv(varname);
	return rc;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/environ.c,v $ */

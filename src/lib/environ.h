/* CobCurses : environment routines for OpenCobol
 * Warren W. Gay
 * Wed Jul 25 10:23:47 2007
 */
#ifndef _environ_h_
#define _enviorn_h_

extern const char *cobcurses_getenv(const char *env_name);
extern int cobcurses_setenv(const char *varname,const char *value);

#endif

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/environ.h,v $ */

/* CobCurses : Misc utility routines for CobCurses
 * Warren W. Gay
 * Wed Jul 18 09:59:41 2007
 *
 * $Id: misc.h,v 1.12 2007/10/18 21:12:54 ve3wwg Exp $
 */
#ifndef _misc_h_
#define _misc_h_

#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef BUILD_MINGW
#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif
#endif

#ifdef BUILD_MINGW
#define MKDIR(path,mode) mkdir(path)
#else
#define MKDIR(path,mode) mkdir(path,mode)
#endif

extern char cobcurses_switchchar(const char *pathname,cct_ushort slen,char swdef);
extern size_t cobcurses_dirname(const char *pathname);
extern int cobcurses_mkdir_p(const char *pathname,mode_t mode);
extern int cobcurses_mkdir_p(const char *pathname,mode_t mode);

extern char *cobcurses_strn(const char *sbuf,size_t slen);
extern char *cobcurses_strp(const char *sbuf,const char *endp);

extern char *cobcurses_ucase(char *sbuf,size_t slen);
extern char *cobcurses_lcase(char *sbuf,size_t slen);

extern size_t cobcurses_trim_trailing(const char *buffer,size_t buflen,char blank);
extern size_t cobcurses_strlen(const char *buffer,char delim);
extern size_t cobcurses_nstrlen(const char *buffer,size_t buflen,char delim);
extern char *cobcurses_strnchr(const char *buffer,size_t buflen,char ch);
extern char *cobcurses_strnchrp(const char *buffer,const char *endp,char ch);

extern size_t cobcurses_eat_tail(char *buffer,size_t start_offset,char eat);
extern size_t cobcurses_append_trunc(char *buf,size_t buflen,size_t start,const char *appstr);

extern cct_bool cobcurses_is_blank(char *s,cct_ushort len);
extern cct_ushort cobcurses_is_charset(char *buf,cct_ushort len,char *chrset);

extern char *cobcurses_center(char *buf,cct_ushort buflen,const char *text);
extern unsigned cobcurses_2blen(const char *cobol_text);

#endif /* _misc_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/misc.h,v $ */

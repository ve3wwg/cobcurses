/* debug.h : Debug header file for temporary debugging
 * Warren W. Gay VE3WWG
 * Tue Oct 16 09:06:31 2007
 * $Id: debug.h,v 1.1 2007/10/16 17:15:24 ve3wwg Exp $
 */
#ifndef _debug_h_
#define _debug_h_

#include <stdio.h>

#define DBG_DEF	FILE *dbg = 0

extern FILE *dbg;

#define DBG	{ if ( !dbg ) dbg = fopen("dbg.t","w"); fprintf(dbg,"%s:%04u:  ",__FILE__,(unsigned)__LINE__); fprintf(dbg,
#define END	); fflush(dbg); }

#endif /* _debug_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/debug.h,v $ */

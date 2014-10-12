/* cobtrace.h : Trace facilities for CobCurses programs
 * Warren W. Gay VE3WWG
 * Thu Oct 18 14:17:46 2007
 * $Id: cobtrace.h,v 1.1 2007/10/23 16:11:45 ve3wwg Exp $
 */
#ifndef _cobtrace_h_
#define _cobtrace_h_

#include <stdio.h>

extern FILE *cobcurses_trace_file;

extern FILE *cobcurses_trace_open(void);
extern void  cobcurses_trace_close(void);

extern FILE *cobcurses_trace_printf(unsigned level,const char *format,...);

#endif /* _cobtrace_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/cobtrace.h,v $ */

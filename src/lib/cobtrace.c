/* cobtrace.c : Trace facilities for CobCurses programs
 * Warren W. Gay VE3WWG
 * Thu Oct 18 14:17:46 2007
 * $Id: cobtrace.c,v 1.1 2007/10/23 16:11:45 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <cobtrace.h>
#include <dynstr.h>
#include <stdlib.h>
#include <stdarg.h>

FILE *cobcurses_trace_file = 0;					/* Opened trace file */
static const char *trace_file_name = 0;				/* Trace file pathname */
static unsigned trace_level = 0;				/* Requested trace level */

/*
 * Open a trace file :
 */
FILE *
cobcurses_trace_open(void) {
	char *cp;
	cct_strvar s;
	cct_bool okflag;

	if ( cobcurses_trace_file )
		return cobcurses_trace_file;					/* Trace is already open */

	dyn_str_init(&s);

	if ( !trace_file_name ) {
		if ( (trace_file_name = getenv(COBCURSES_TRACE)) != 0 ) {
			cobcurses_trace_file = fopen(trace_file_name,"w");	/* New trace file */
			if ( !cobcurses_trace_file )
				trace_file_name = "cobcurses_trace.txt";
			cobcurses_trace_file = fopen(trace_file_name,"w");
		}
		if ( (cp = getenv(COBCURSES_TRACE_LEVEL)) != 0 ) {
			dyn_str_assign(&s,cp);
			dyn_str_trim(&s);
			trace_level = (unsigned) dyn_str_to_ulong(&s,&okflag);
			if ( !okflag )
				trace_level = 5;				/* Default level */
		} else	trace_level = 5;					/* Default trace level */
		dyn_str_clear(&s);
	} else
		cobcurses_trace_file = fopen(trace_file_name,"a");		/* Append */

	if ( cobcurses_trace_file ) {
		fprintf(cobcurses_trace_file,"Opened CobCurses Trace File.\n");
		fprintf(cobcurses_trace_file,"COBCURSES_TRACE_LEVEL=%u\n\n",trace_level);
		fflush(cobcurses_trace_file);
	}
	return cobcurses_trace_file;
}

/*
 * Close a trace file, if open :
 */
void
cobcurses_trace_close(void) {

	fputc('\n',cobcurses_trace_file);
	if ( cobcurses_trace_file )
		fclose(cobcurses_trace_file);
	cobcurses_trace_file = 0;
}

static FILE *
trace_vprintf(const char *format,va_list ap) {

	if ( cobcurses_trace_file ) {
		vfprintf(cobcurses_trace_file,format,ap);
		fflush(cobcurses_trace_file);
	}

	return cobcurses_trace_file;
}

FILE *
cobcurses_trace_printf(unsigned level,const char *format,...) {
	va_list ap;

	if ( cobcurses_trace_file && level >= trace_level ) {
		va_start(ap,format);
		trace_vprintf(format,ap);
		va_end(ap);
	}
	return cobcurses_trace_file;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/cobtrace.c,v $ */

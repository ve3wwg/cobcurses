/* CobCurses : ncurses(3) interface module for OpenCobol
 * Warren W. Gay
 * Wed Dec 20, 2006
 */
#include <cobcurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>

#include <cobtrace.h>
#include <environ.h>
#include <misc.h>
#include <cobmenu.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

static cct_bool     title_init = F;
static cct_termattr title_attr = CCA_NORMAL;

/*
 * Forward declarations :
 */
static cct_bool Is_Numeric(
    char        *buf,
    cct_ushort  len,
    cct_bool    signch,
    cct_ushort  maxdigits,
    cct_ushort  maxdecplaces
);

/*
 * Exit codes for NC-GET-TEXT (NC_GETTEXT)
 */
#define NC_FIELD_EXIT_CR        '0'
#define NC_FIELD_EXIT_CU        '1'
#define NC_FIELD_EXIT_CD        '2'
#define NC_FIELD_EXIT_TAB       '3'
#define NC_FIELD_EXIT_BTAB      '4'
#define NC_FIELD_EXIT_ESC       '5'
#define NC_FIELD_EXIT_DOT	'.'
#define NC_FIELD_EXIT_SLASH	'/'
#define NC_FIELD_EXIT_FKEY	'F'

/*
 * Simplified Exit codes for NC-GET-TEXT (NC_GETTEXT)
 */
#define NC_FIELD_FORWARD        'F'
#define NC_FIELD_BACKWARD       'B'

/*
 * Curses state information :
 */
static cct_terminal *term = 0;			/* Our virtualized terminal object */
static cct_bool is_initialized = 0;		/* T if initialized */

/*
 * Undo buffer size for NC-GET-TEXT (NC_GETTEXT)
 */
#define UNDO_BUFFER		32

/*
 * Change types :
 */
#define DEF_CHG(ch)		((cct_uchar)(ch))
#define CHG_EMPTY		DEF_CHG(0x00)
#define CHG_DELETE		DEF_CHG('D')
#define CHG_INSERT		DEF_CHG('I')
#define CHG_REPLCH		DEF_CHG('C')
#define CHG_STRING		DEF_CHG('S')

/********************************************************************************
 * Undo buffer support :
 ********************************************************************************/
typedef struct {
	cct_uchar	type;
	cct_ushort	csr;
	union {
		cct_uchar	ch;
		cct_uchar	*text;
	} change;
} cct_change;

static cct_change changes[UNDO_BUFFER];
static cct_ushort chg_count = 0;
static cct_ushort chgx = 0;

static cct_uchar *yanked = 0;
static cct_ushort yanklen = 0;

/*
 * Yank rest of buffer, excluding blanks on tail end.
 */
static void
put_yank(cct_ushort csr,char *buffer,cct_ushort buflen) {
	unsigned ylen;

	/* Eliminate blank tail */
	while ( buflen > csr && buffer[buflen-1] == ' ' )
		--buflen;
	ylen = buflen - csr;	/* yank length */
	yanked = realloc(yanked,yanklen=ylen);
	memcpy(yanked,buffer+csr,ylen);
}

/*
 * Put yanked text back into buffer at csr :
 */
static cct_ushort
yank(cct_ushort csr,char *buffer,cct_ushort buflen) {
	cct_ushort ylen = buflen - csr;

	if ( !yanked )
		return 0;	/* Nothing to paste */
	if ( ylen > yanklen )
		ylen = yanklen;	/* Limit the yank */
	memcpy(buffer+csr,yanked,ylen);
	return ylen;
}

/*
 * Free the change at index chgx :
 */
static cct_change *
free_change(cct_ushort chgx) {
	cct_change *chgp = &changes[chgx];

	switch ( chgp->type ) {
	case CHG_EMPTY :
		break;			/* Empty entry */
	case CHG_DELETE :
	case CHG_INSERT :
	case CHG_REPLCH :
		break;
	case CHG_STRING :
		free(chgp->change.text);
		break;
	default :
		abort();
	}

	memset(chgp,0,sizeof *chgp);
	return chgp;
}

/*
 * Free all saved changes :
 */
static void
free_changes(void) {
	cct_ushort x;

	for ( x=0; x<UNDO_BUFFER; ++x )
		free_change(x);
	chg_count = chgx = 0;
}

/*
 * Allocate a new change entry :
 */
static cct_change *
alloc_change(void) {
	if ( chg_count < UNDO_BUFFER )
		return &changes[chgx = chg_count++];
	chgx = ( chgx + 1 ) % UNDO_BUFFER;
	return free_change(chgx);
}

/*
 * Record a single character change :
 */
static void
record_ch(unsigned csr,cct_uchar ch) {
	cct_change *chgp = alloc_change();

	chgp->type = CHG_REPLCH;
	chgp->csr  = csr;		/* Cursor */
	chgp->change.ch = ch;		/* The character that was there */
}

/*
 * Record an insert character event :
 */
static void
record_insch(cct_ushort csr,cct_uchar lastch) {
	cct_change *chgp = alloc_change();

	chgp->type = CHG_INSERT;	/* Insert character change */
	chgp->csr = csr;		/* Where the insert happened */
	chgp->change.ch = lastch;	/* The last character shifted out of the buffer */
}

/*
 * Record a delete character event :
 */
static void
record_delch(cct_ushort csr,cct_uchar delch) {
	cct_change *chgp = alloc_change();

	chgp->type = CHG_DELETE;
	chgp->csr  = csr;
	chgp->change.ch = delch;	/* The character that was deleted */
}

/*
 * Record a kill line :
 */
static void
record_kill(cct_ushort csr,char *field_text,cct_ushort fldlen) {
	cct_change *chgp = alloc_change();

	if ( csr + 1 == fldlen ) {
		record_delch(csr,field_text[csr]);
	} else	{
		chgp->type = 'S';
		chgp->csr  = csr;
		chgp->change.text = malloc(fldlen - csr + 1);
		memcpy(chgp->change.text,field_text+csr,fldlen-csr+1);
	}
}

/*
 * Undo the last change :
 */
static cct_ushort
undo(cct_ushort *csr,char *field_text,cct_ushort fldlen) {
	cct_ushort x;
	cct_change *chgp;

	if ( chg_count < 1 )
		return 0;
	chgp = &changes[chgx];

	switch ( chgp->type ) {
	case CHG_STRING :
		*csr = chgp->csr;
		memcpy(field_text+*csr,chgp->change.text,fldlen-*csr+1);
		break;
	case CHG_DELETE :
		*csr = chgp->csr;
		for ( x=fldlen-1; x>*csr; --x )
			field_text[x] = field_text[x-1];
		field_text[*csr] = chgp->change.ch;
		break;
	case CHG_INSERT :
		*csr = chgp->csr;
		for ( x=*csr; x + 1 < fldlen; ++x )
			field_text[x] = field_text[x+1];
		field_text[fldlen-1] = chgp->change.ch;
		break;
	case CHG_REPLCH :		/* Restore single character */
		*csr = chgp->csr;
		field_text[*csr] = chgp->change.ch;
		break;
	case CHG_EMPTY :		/* Empty entry */
	default :
		return 0;
	}

	free_change(chgx);
	chgx = ( chgx + UNDO_BUFFER - 1 ) % UNDO_BUFFER;
	return 1;
}

/********************************************************************************
 * Remainder of the cobcurses.c support module :
 ********************************************************************************/

/*
 * This procedure is registered with atexit(3) to be called
 * upon program termination. Its purpose is simply to call
 * endwin() if that remains outstanding, to end the reign
 * of curses and restore the tty parameters to sane ones
 * for the user.
 */
#ifdef COBCURSES_ATEXIT_SUPPORT
static void
cobcurses_finalization(void) {

	if ( is_initialized != 0 && curses_window != 0 ) {
		if ( term ) {
			term->api->close(term);
			term->api->_delete(term);
		}
	}

	cobcurses_trace_printf(0,"CobCurses Finalized.\n");
	cobcurses_trace_close();	/* Close if open */
}

/*
 * Check environment variable COBCURSES_NORECOVERY to
 * see if recovery is to be suppressed.
 */
static int
disabled_finalization(void) {
	const char *cp = getenv(COBCURSES_NORECOVERY_ENVNAME);
	int nr = 0;

	if ( cp && *cp ) {
		if ( *cp == 'Y' || *cp == 'y' || *cp == '1' )
			nr = 1;
		else	nr = 0;
	}
	return nr;			/* True if recovery is to be suppressed */
}
#endif

/*
 * Test the environment variable COBCURSES_NORECOVERY
 * to see if it is defined and set to 'Y' or '1'.
 * If undefined or any other value, is treated as 'N'.
 */
static cct_uchar
cobcurses_norecovery(void) {
	const char *cp = getenv(COBCURSES_NORECOVERY_ENVNAME);
	char flag = 'N';

	if ( cp != 0 )
		switch ( *cp ) {
		case 'Y' :
		case 'y' :
		case '1' :
			flag = 'Y';
			break;
		default :
			flag = 'N';
		}
	return (cct_uchar) flag;
}

/*
 * Cobol interface to test the environment variable
 * COBCURSES_NORECOVERY for Y or N.
 */
int
NC_NORECOVERY(char *pflag) {
	cct_uchar flag = cobcurses_norecovery();

	cobcurses_trace_printf(2,"\nNC_NORECOVERY('%c') called.\n",*pflag);
	*pflag = flag;
	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}

/*
 * Start up ncurses, and return useful info :
 */
int
NC_OPEN(
    cct_ushort  *pcols,			/* Pass back # columns in display */
    cct_ushort  *plines, 		/* Pass back # lines in display */
    char        *phas_colour,		/* Pass back 'Y' if colour is supported */
    char        *pchg_colour,		/* Pass back 'Y' if colour can be changed */
    cct_ushort  *pcolour_pairs,    	/* Pass back # of colour pairs supported */
    char        *phave_mouse,		/* Pass back 'Y' if we have mouse support */
    char        *pmouse_click_ms,	/* Pass back mouse click ms */
    char        *phas_underline		/* Pass back 'Y' if we can support underline */
) {
	cct_ushort no_lines, no_cols, no_pairs;
	static cct_mmask init_mmask = BSTATE_CLICKED(1);
#ifndef BUILD_MINGW
	char *TERM = getenv("TERM");
#endif

	if ( !is_initialized ) {
		cobcurses_trace_open();		/* Depends on environment variable */
		cobcurses_trace_printf(2,"\nNC_OPEN() called (with trace enabled)\n");
#ifdef COBCURSES_ATEXIT_SUPPORT
		if ( !disabled_finalization() ) {
			atexit(cobcurses_finalization);	/* Do any necessary cleanup */
			cobcurses_trace_printf(0,"Finalization enabled.\n");
		} else
			cobcurses_trace_printf(0,"Finalization disabled.\n");
#else
		cobcurses_trace_printf(0,"Finalization is not compiled in.\n");
#endif
		is_initialized = 1;		/* Initialization done */
	} else
		cobcurses_trace_printf(2,"\nNC_OPEN() called.\n");

	/*
	 * Initialize return values in case we fail :
	 */
	*plines = *pcols = no_cols = no_lines = no_pairs = 0;
	*phas_colour = *pchg_colour = 'N';
	*pcolour_pairs = 0;

	/*
	 * Create terminal interface object :
	 */
	if ( !term)
		term = terminal_new();		/* Gain access to terminal window */
	else	{
		cobcurses_trace_printf(2,"NC_OPEN() returns NC-OPEN (already open)\n");
		return RET_OPEN;		/* Curses already open! */
	}

#ifndef BUILD_MINGW
	cobcurses_trace_printf(5,"TERM='%s'\n",TERM ? TERM : "(null)");
#else
	cobcurses_trace_printf(5,"This is a MinGW session.\n");
#endif

	/*
	 * See if we have curses(3X) support :
	 */
	if ( !term->api->curses_init ) {	/* Check for curses(3X) support */
		term->api->_delete(term);
		term = 0;
		cobcurses_trace_printf(5,"No curses(3X) support!\n");
		cobcurses_trace_printf(2,"NC_OPEN() returns NC-FAILED.\n");
		return RET_FAILED;		/* No curses(3X) support */
	} else	{
		term->api->curses_init(term);	/* Select and initialize curses(3X) support */
		cobcurses_trace_printf(0,"Curses(3X) support has been initialized.\n");
	}

	/*
	 * Open the selected terminal :
	 */
	if ( !term->api->open(term) ) {
		/* Curses failed to initialize */
		cobcurses_trace_printf(99,"Unable to start curses(3X) terminal support (open failed).\n");
		term->api->_delete(term);
		term = 0;
		cobcurses_trace_printf(2,"NC_OPEN() returns NC-FAILED.\n");
		return RET_FAILED;		/* Failed */
	}

	/*
	 * Return terminal information back to the caller :
	 */
	*pcols = no_cols = term->api->columns(term);
	*plines = no_lines = term->api->lines(term);
	*pcolour_pairs = no_pairs = term->api->colour_pairs(term);

	*phas_colour = term->api->has_underline_cap(term) ? 'Y' : 'N';
        *pchg_colour = term->api->has_change_colour_cap(term) ? 'Y' : 'N';
        *phave_mouse = term->api->have_mouse(term) ? 'Y' : 'N';

	*phas_underline = term->api->has_underline_cap(term) ? 'Y' : 'N';

	cobcurses_trace_printf(5,"      # of columns: %u\n",no_cols);
	cobcurses_trace_printf(5,"        # of lines: %u\n",no_lines);
	cobcurses_trace_printf(5," # of colour pairs: %u\n",no_pairs);
	cobcurses_trace_printf(5,"        Has colour: %c\n",*phas_colour);
	cobcurses_trace_printf(5," Can change colour: %c\n",*pchg_colour);
	cobcurses_trace_printf(5,"Have mouse support: %c\n",*phave_mouse);
	cobcurses_trace_printf(5," Can use underline: %c\n",*phas_underline);

	/*
	 * Initialize mouse support :
	 */
	if ( term->api->have_mouse(term) ) {
		term->api->set_mouse_mask(term,init_mmask);
		cobcurses_trace_printf(0,"\nInitial mouse mask: 0x%04X\n",init_mmask);
	}

	cobcurses_trace_printf(2,"NC_OPEN() returns NC-OK\n");
	return RET_OK;				/* Successful */
}

#define open_check() if ( !term ) { cobcurses_trace_printf(9,"Terminal is not open yet. Returns NC-OPEN.\n"); return RET_OPEN; }

/*
 * Refresh the screen :
 */
int
NC_REFRESH(void) {

	cobcurses_trace_printf(2,"\nNC_REFRESH() called.\n");
	open_check();
	term->api->flush(term);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Get any key :
 */
int
NC_PAUSE(void) {

	cobcurses_trace_printf(2,"\nNC_PAUSE() called.\n");
	open_check();
	term->api->getkey(term);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Position cursor :
 */
int
NC_MOVE(cct_ushort *py,cct_ushort *px) {
	cct_ushort x, y;

	cobcurses_trace_printf(2,"\nNC_MOVE(y=%u,x=%u) called.\n",*py,*px);
	open_check();
	y = *py - 1;
	x = *px - 1;
	term->api->move(term,y,x);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Write 1 character :
 */
int
NC_ADDCH(char *pch) {
	char ch = *pch;

	if ( !is_acs(ch) ) 
		cobcurses_trace_printf(2,"\nNC_ADDCH('%c') called.\n",ch);
	else	cobcurses_trace_printf(2,"\nNC_ADDCH(ACS 0x02X) called.\n",(unsigned)ch);
	open_check();
	term->api->putch(term,*pch);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Write n characters :
 */
int
NC_ADDSTR(char **ppstring,cct_ushort *pslen) {

	cobcurses_trace_printf(2,"\nNC_ADDSTR('%.*s',len=%u) called.\n",(unsigned)*pslen,*ppstring,(unsigned)*pslen);
	open_check();
	term->api->putsn(term,*ppstring,*pslen);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Shutdown ncurses :
 */
int
NC_CLOSE(void) {
	
	cobcurses_trace_printf(5,"\nNC_CLOSE() called.\n");
	open_check();
	term->api->flush(term);
	term->api->close(term);
	term->api->_delete(term);
	term = 0;
	cobcurses_trace_printf(5,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Clear screen :
 */
int
NC_CLEAR(void) {

	cobcurses_trace_printf(2,"\nNC_CLEAR() called.\n");
	open_check();
	term->api->clear(term);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Clear to bottom of screen :
 */
int
NC_CLRTOBOT(void) {

	cobcurses_trace_printf(2,"\nNC_CLRTOBOT() called.\n");
	open_check();
	term->api->clrtobot(term);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Clear to end of line :
 */
int
NC_CLRTOEOL(void) {

	cobcurses_trace_printf(2,"\nNC_CLRTOEOL() called.\n");
	open_check();
	term->api->clrtoeol(term);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Get a character / key code :
 */
int
NC_GETCH(
  cct_ushort 	*pkey_code,	/* RETURNED: Key code */
  char		*pasc_char,	/* RETURNED: Character */
  char		*pis_key_code	/* RETURNED: 'Y' if a key code, else 'N' */
) {	
        int key;

	cobcurses_trace_printf(2,"\nNC_GETCH() called.\n");
	open_check();

	key = term->api->getkey(term);

	if ( key >= cck_MIN && key <= cck_MAX ) {
		*pis_key_code = 'Y';
		*pkey_code = (cct_ushort) key & 0x00FFFF;
		*pasc_char = ' ';
		cobcurses_trace_printf(2,"got key code 0x%04X\n",*pkey_code);
	} else	{
		*pis_key_code = 'N';
		*pkey_code = 0;
		*pasc_char = (char) (key & 0xFF);
		if ( isprint(*pasc_char) )
			cobcurses_trace_printf(2,"Got ascii char '%c'\n",*pasc_char);
		else	cobcurses_trace_printf(2,"Got ascii char 0x%02X\n",(unsigned)*pasc_char);
	}
	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}

/*
 * Get keystroke, ignoring disallowed characters :
 */
static int
internal_getch(
    cct_ushort 		uppers,
    char 		*restrict,
    cct_unsigned 	*c2,
    cct_ushort 		csr
) {
	int rc;
	cct_ushort rlen = 0;

	if ( restrict ) {
		/* Minimum length is 1 (put blank in first) */
		for ( rlen=1; restrict[rlen] != ' '; ++rlen );
	}

	*c2 = rc = term->api->getkey(term);

	if ( rc < cck_MIN ) {
		/* Ascii character */
		if ( uppers )
			rc = toupper(rc);
		if ( !csr && restrict && ( rc == '.' || rc == '/' ) )
			; /* Allow this special case -- it will be caught later */
		else if ( rc >= ' ' && restrict && rlen > 0 ) {
			int x;

			for ( x = 0; x < rlen; ++x )
				if ( rc == restrict[x] )
					return rc;
                        return 0;       /* This char not allowed */
		}
	} /* else if ( rc == cck_MOUSE ) */
		/* we have mouse data */

	return rc;
}

/*
 * Start an input field attribute :
 */
static int
start_field(cct_pair pair,cct_termattr fb_attr) {

	if ( !term )
		return RET_OPEN;

	if ( !pair )
		term->api->set_attr(term,fb_attr);	/* Monichrome */
	else	term->api->set_attr_and_pair(term,fb_attr,pair);
	return RET_OK;
}

#define TOX(x,woff,csr)		(x + (unsigned)(csr - woff))

/*
 * Scroll/Redisplay a window if necessary :
 */
static void
fix_window(
    cct_ushort y,		/* Window start y */
    cct_ushort x,		/* Window start x */
    cct_ushort field_length,	/* Field's internal length */
    cct_ushort window_length,	/* Window's length */
    char       *field_text,	/* Ptr to receiving buffer */
    cct_ushort *pcsr,		/* Field cursor */
    cct_ushort *pwoff,		/* Window offset */
    cct_ushort mask,            /* Mask for a password */
    cct_ushort redisplay,       /* Forces a redisplay */
    cct_termattr  fb_attr,	/* Fallback attribute */
    cct_pair pair) {		/* Field colours else 0 */

	int csr = (int)*pcsr;
	int woff = (int)*pwoff;
        cct_ushort cx, alen;

	if ( field_length < window_length )
		window_length = field_length;

	if ( (csr - woff) >= window_length ) {
		/* Some sort of a field scroll is required */
		if ( window_length > 3 )
			woff = csr - 2;
		else	woff = csr;
		if ( woff < 0 )
			woff = 0;
		if ( woff + window_length > field_length )
			woff = field_length - window_length;
	} else if ( csr < woff ) {
		/* Some sort of a reverse field scroll is required */
		if ( window_length > 3 )
			woff = csr - 3;
		else	woff = csr - 1;
		if ( woff < 0 )
			woff = 0;
		if ( csr - woff > window_length )
			woff = csr - window_length + 1;
	}

	if ( redisplay || csr != *pcsr || woff != *pwoff ) {
		term->api->move(term,y,x);
		start_field(pair,fb_attr);
		if ( !mask )
			term->api->putsn(term,(char *)field_text+woff,window_length);
		else	{
			alen = (cct_ushort) cobcurses_trim_trailing((char *)field_text,field_length,' ');
			for ( cx=0; cx<alen; ++cx )
				term->api->putch(term,'*');
			for ( ; cx < field_length; ++cx )
				term->api->putch(term,' ');
                }
	}

	term->api->move(term,y,TOX(x,woff,csr));
	*pcsr = (cct_ushort)csr;
	*pwoff = (cct_ushort)woff;
}

/*
 * Return 'Y' if the contents differ, else 'N' :
 */
static cct_uchar
has_changed(char *f1,char *f2,cct_ushort length) {

	return memcmp(f1,f2,length) ? 'Y' : 'N';
}

/*
 * Edit a numeric field for easier editing by
 * removing redundant zeros etc. This routine
 * also removes anything unacceptable, like the
 * $. A "CR" will be interpreted as a - sign
 * if no other sign indication has been previously
 * found.
 */
static void
edit_numfield(char *buffer,cct_ushort length,cct_bool strip_comma) {
	char thesign = '+';
	cct_bool sgn = 0;
	cct_bool decpt = 0;
	cct_bool suppress = 0;
	cct_ushort digits = 0;
	cct_ushort decplaces = 0;
	cct_ushort zeros = 0;
	cct_ushort x;
	char ch, c2, *src, *dst;
	char *buf2 = ALLOCA(length);

	memcpy(buf2,buffer,length);

	/*
	 * First scan for a sign :
	 */
	for ( x=0; x<length; ++x ) {
		ch = toupper(buf2[x]);
		if ( !sgn && ( ch == '-' || ch == '+' ) ) {
			thesign = ch;
			sgn = 2;
		} else if ( isdigit(ch) ) {
			++digits;
			sgn = 1;
		} else if ( sgn <= 1 && ch == 'D' && x+1 < length ) {
			c2 = toupper(buf2[x+1]);
			if ( c2 == 'B' ) {
				thesign = '-';
				sgn = 2;
			}
		}
	}

	/*
	 * Count decimal places :
	 */
	decpt = 0;
	decplaces = 0;
	zeros = 0;
	for ( x=0; x<length; ++x ) {
		ch = buf2[x];
		if ( ch == '.' )
			decpt = 1;
		else if ( decpt && isdigit(ch) ) {
			if ( ch == '0' )
				++zeros;
			else	{
				decplaces += 1 + zeros;
				zeros = 0;
			}
		}
	}

	if ( decpt && decplaces == 0 )
		suppress = 1;

	/*
	 * Now edit the field :
	 */
	src = buf2;
	dst = buffer;
	memset(buffer,' ',length);
	digits = 0;
	decpt = 0;
	zeros = 0;

	if ( thesign != '+' )
		*dst++ = thesign;

	while ( dst - buffer < length && src - buf2 < length ) {
		ch = *src++;
		if ( !digits && !decpt && ch == '0' )
			++zeros;		/* Ignore leading zeros */
		else if ( isdigit(ch) ) {
			if ( !decpt || !suppress )
				*dst++ = ch;	/* Copy over digit */
			++digits;
		} else if ( ch == '.' ) {
			if ( !digits ) {
				if ( suppress || 1 + decplaces < length ) {
					*dst++ = '0';	/* Insert leading zero */
					++digits;
					--zeros;
				}
			}
			if ( !suppress )
				*dst++ = ch;	/* Copy over decimal point */
			decpt = 1;
		} else if ( !strip_comma && ch == ',' )
			*dst++ = ch;	/* Copy over commas */
	}

	if ( !decpt || suppress ) {
		if ( !digits && zeros && dst - buffer < length )
			*dst++ = '0';
	} else	{
		/*
		 * Now remove redundant trailing zeros after decimal point :
		 */
		while ( dst > buffer ) {
			ch = *dst;
			if ( ch == '0' )
				*dst-- = ' ';	/* Blank it out */
			else	break;
		}
	}
	FREEA(buf2);
}

/*
 * Present the menu and get a selection :
 */
static cct_bool
menu_selection(
    char 	*field_text,		/* Field buffer */
    cct_ushort 	fldlen,			/* Field buffer length */
    cct_menu 	**pmenu,		/* Ptr to cct_menu * var */
    const char	*cobol_menu,		/* Ptr to COBOL defn of menu */
    cct_pair 	menu_pair,		/* 0 or colour pair to use for menu title and frame */
    cct_termattr menu_tattr		/* attribute for the menu title to use */
) {
	cct_menu *menu = *pmenu;
	char *sel;
	cct_ushort slen;

	if ( !menu && !cobol_menu )
		return F;			/* No menu is possible */

	if ( !menu )
		*pmenu = menu = cobcurses_COBOL_menu(term,cobol_menu,menu_pair,menu_tattr);
	if ( !menu ) {
		cobcurses_trace_printf(5,"COBOL menu error(s) were encountered\n");
		return F;			/* Menu error(s) -> no menu */
	}

	term->api->show_menu(menu);
	sel = term->api->get_menu_selection(menu);

	if ( sel ) {
		cobcurses_trace_printf(5,"Menu selection was '%s'\n",sel);
		slen = strlen(sel);
		if ( fldlen < slen )
			slen = fldlen;
	} else  {
		cobcurses_trace_printf(5,"No menu selection was made (escaped menu)\n");
		slen = 0;
	}

	memset(field_text,' ',fldlen);
	if ( sel )
		memcpy(field_text,sel,slen);	/* Copy menu selection */

	cobcurses_trace_printf(5,"Returning buffer = '%.*s'\n",fldlen,field_text);

	/*
	 * Edit special case of '#    ', which is converted to
	 * all blanks for the action field :
	 */
	if ( slen > 0 ) {
		slen = cobcurses_trim_trailing(field_text,slen,' ');
		if ( slen == 1 && *field_text == '#' )
			*field_text = ' ';
	}

	term->api->hide_menu(menu);		/* Pop down the menu */
	return T;
}

/*
 * Get text from a windowed terminal field :
 *
 * INPUTS :
 *	py, px			Terminal position of start of field
 *	field_length		The receiving buffer's length (total field length)
 *	window_length		The terminal field length (when 0 assumes field_length)
 *	field_text		Pointer to the receiving buffer
 *	uppercase		When 'Y', force input to uppercase
 *	restrict		List of characters to restrict input to (blank delimited)
 *	
 * RETURNED :
 *	xpos			The 1-based relative cursor position upon exit
 *	field_exit		Field exit codes
 *	fb			Forward/Back simplified code
 *
 * NOTES :
 *	1.	field_exit returns '.' if a '.' in column 1 is immediately
 *		followed by a CR (shorthand to go back to prior field).
 *		The field's content is restored to the original content when
 *		this exit is taken.
 *	2.	field_exit returns '/' if a '/' in column 1 is immediately
 *		followed by a CR (shorthand to skip this field, or enter a
 *		null value).
 *		The field's content is restored to the original content when
 *		this exit is taken.
 *	3.	If blank is to be included in the restricted character set,
 *		then include the blank in the first position.
 */
int
NC_GETTEXT(
    cct_ushort		*py,		/* Field's Y position */
    cct_ushort		*px,		/* Field's X position */
    cct_termattr	*pfb_attr,	/* Field's fallback attribute if no colour */
    cct_pair		*ppair,		/* Field colour pair to use else 0 */
    cct_ushort		*pfield_length,	/* Field's length */
    cct_ushort		*pwindow_length, /* Field's window length */
    char 		**pfield_text,	/* Pointer to Field's buffer address */
    char		*pclear,	/* Clear field upon entry? */
    char		*puppercase,	/* Uppercase field if 'Y' */
    char		*pwmask,	/* Mask as a password field when 'Y' */
    char		*pnotblank,	/* When 'Y' do not allow FWD movement with blank field */
    char		**prestrict,	/* Restrict to these characters */
    char		*psignyn,	/* When 'Y', allow a sign character in numeric field */
    cct_ushort		*pnumdigits,	/* Max Number of digits for numeric field */
    cct_ushort		*pnumdecplaces,	/* Max number of decimal places in numeric field */
    char		*pmode,		/* 'A' = action mode, 'F' = 3270 FKEY mode */
    cct_ushort		*pxpos,		/* RETURNED: Relative position within the field */
    char		*pfield_exit,	/* RETURNED: How the field was exited */
    char		*pfb,		/* RETURNED: general exit field code */
    char		*pchgflag,	/* RETURNED: 'Y' if field content has changed */
    char		*pmouseflag,	/* RETURNED: 'Y' if exit due to a mouse event */
    char		*paction,	/* RETURNED: action character, if any */
    cct_ushort		*psearch,	/* RETURNED: Search field # in action fields */
    cct_ushort		*pcomp_type,	/* Input: COMP-2/COMP-1 type (CBL_COMP_2 etc.) */
    void      		**ppcomp_x,	/* RETURNED: Ptr to COMP-1 or COMP-2 data item */
    char		**pmenu,	/* Pointer to an optional menu (may be NULL) */
    cct_pair		*pmenu_pair,	/* The colour pair to use for menu frame */
    cct_termattr	*pmenu_attr	/* The menu title attributes to use */
) {
	cct_ushort y;			/* Terminal line number */
	cct_ushort x;			/* Terminal column number */
	cct_pair pair;
	void *pcomp_x = *ppcomp_x; 	/* Ptr to a pointer to COMP-X */
        char *restrict;
        char *field_text;
	cct_ushort comp_type = *pcomp_type; /* Comp type if any */
	char *units_config = 0;		/* Units config if any */
	cct_ushort xpos;
	cct_ushort search;		/* In action fields, a field # may also be given */
	cct_ushort csr = 0;		/* Field cursor */
	cct_ushort woff = 0;		/* Window offset */
	cct_ushort fldlen;		/* Buffer length */
	cct_ushort winlen;			/* Field's length on screen */
        cct_bool fclear = TOBOOL(*pclear);    	/* Clear field upon entry? */
        cct_bool uppers = TOBOOL(*puppercase);	/* Uppercase input? */
        cct_bool mask   = TOBOOL(*pwmask);   	/* Mask password characters? */
	cct_bool notblk = TOBOOL(*pnotblank); 	/* Don't allow blank field going forward */
	cct_bool isYN;				/* Is this a Y/N field? */
	cct_bool dot_slash;			/* Dot/Slash exits in Action mode? */
	cct_ushort maxdigits;			/* Max digits before decimal place if numeric */
	cct_ushort maxdecs;			/* Max digits after decimal place if numeric */
	cct_bool isnum; 			/* Is this a numeric only field? */
        cct_bool signch = TOBOOL(*psignyn);  	/* Is this a signed numeric? */
	cct_unsigned ch, c2;			/* Current input character */
	cct_ushort force_redisp = 0;		/* Force a redisplay when true */
	cct_ushort insmod = 0;			/* In insert mode when true */
	cct_ushort dot_flag = 0;		/* '.' entered in first column */
	cct_ushort slash_flag = 0;		/* '/' entered in first column */
	cct_ushort end_flag = 0;		/* True when last key was ^E */
	cct_ushort cx;				/* Work index */
	char *full_undo = 0; 			/* Storage for full undo */
	cct_termattr fb_attr;			/* Cobol vers of attribute */
	cct_termattr at;
	cct_pair pr;
	void *cobol_menu = *pmenu;		/* Pointer to menu structure (if any) */
	enum { Undefined_Mode, Action_Mode, Mode_3270 } mode = Undefined_Mode;
	int xc = RET_OK;			/* Exit code */
	cct_menu *menu = 0;			/* cobol_menu => menu */
	cct_pair menu_pair = pmenu_pair ? *pmenu_pair : 0;
	cct_termattr menu_attr = pmenu_attr ? *pmenu_attr : CCA_NORMAL;

	menu_attr &= ~(CCA_UNDERLINE);		/* Don't use underline in the menu */

	cobcurses_trace_printf(2,"\nNC_GETTEXT() called (mode = '%c').\n",*pmode);
	cobcurses_trace_printf(2,"  y,x : %u, %u, menu=%p\n",*py,*px,cobol_menu);
	open_check();

	y 	  = *py - 1;
	x 	  = *px - 1;
	fb_attr   = *pfb_attr;
	pair      = *ppair;
	fldlen    = *pfield_length;
	winlen    = *pwindow_length;
	field_text = *pfield_text;
	restrict  = *prestrict;
	maxdigits = *pnumdigits;
	maxdecs   = *pnumdecplaces;
	xpos      = *pxpos;

	cobcurses_trace_printf(2,"  field = '%.*s' (length=%u, winlen=%u, maxdigits=%u, maxdecs=%u, xpos=%u)\n",
		fldlen,field_text,fldlen,winlen,maxdigits,maxdecs,xpos);

	switch ( *pmode ) {
	case 'A' :
		mode = Action_Mode;
		dot_slash = 1;
		break;
	case 'F' :
		mode = Mode_3270;
		dot_slash = 0;
		break;
	default :
		printf("*** BAD MODE '%c' ***\n",*pmode); fflush(stdout);
		abort();
	}

	if ( !winlen )
		winlen = fldlen;

	/*
	 * Special setup for COMP-X types :
	 */
	switch ( comp_type ) {
	case CBL_COMP_1 :
	case CBL_COMP_2 :
		isnum = 0;	/* It is numeric, but allow units input */
		units_config = (char *) restrict;
		restrict = (char *) " 0123456789+-.,abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ";
		cobcurses_trace_printf(2,"For a COMP-%u data type.\n",comp_type == CBL_COMP_1 ? 1 : 2);
		break;
	default :
		isnum = ( maxdigits > 0 || maxdecs > 0 ) ? 1 : 0;
		cobcurses_trace_printf(2,"For a numeric, but not a COMP-X type.\n");
	}

	/*
	 * Keep the maxdigits sane for COMP-X types :
	 */
	switch ( comp_type ) {
	case CBL_COMP_1 :
		if ( maxdigits > 6 ) {
			maxdigits = 6;
			cobcurses_trace_printf(2,"Restricting to maxdigits = %u\n",maxdigits);
		}
		break;
	case CBL_COMP_2 :
		if ( maxdigits > 16 ) {
			maxdigits = 16;
			cobcurses_trace_printf(2,"Restricting to maxdigits = %u\n",maxdigits);
		}
		break;
	default :
		;
	}

	full_undo = ALLOCA(fldlen);
	isYN = restrict && !strncmp((char *)restrict,"YN ",3) ? 1 : 0;

	*pfield_exit = ' ';		/* Clear field exit code */
	*pfb = ' ';
	*paction = ' ';			/* Clear returned action character */
	*pmouseflag = 'N';		/* Clear mouse exit flag */

        search = 0;                     /* Reset search field # */
	*psearch = search;

	/* Save attributes & colour */
	term->api->get_attr_and_pair(term,&at,&pr);

	if ( xpos >= 1 ) {
		csr = xpos - 1;
		if ( csr >= fldlen )
			csr = fldlen - 1;
	}

	/*
	 * If numeric, then use a default restricted charset, and
	 * edit the buffer for editing convenience :
	 */
	if ( isnum ) {
		if ( !restrict )
			restrict = (char *) (signch ? " 0123456789+-., " : " 0123456789., ");
		if ( !fclear )
			edit_numfield(field_text,fldlen,0);
	}

	/*
	 * Blank out the buffer is clear option is set :
	 */
        if ( fclear ) /* Clear input field? */
		memset(field_text,' ',fldlen);

	/* Save original content */
	memcpy(full_undo,field_text,fldlen);

	term->api->move(term,y,x);	/* Move to field's coordinates */
	force_redisp = 1;		/* Force a redisplay upon entry */

	/*
	 * Edit loop :
	 */
loop:	while ( 1 ) {
		fix_window(y,x,fldlen,winlen,field_text,&csr,&woff,mask,force_redisp,fb_attr,pair);
		force_redisp = 0;		/* Reset force redisplay flag */

		/*
		 * Get next input key event :
		 */
		ch = internal_getch(uppers,restrict,&c2,csr);

		/*
		 * Dispatch on entered key :
		 */
		switch ( ch ) {
		/*
		 * ^O opens the menu manually :
		 */
		case cck_F1 :
			if ( mode == Mode_3270 || !cobol_menu )
				goto fkey_case;
		case CONTROL('O') :
			if ( mode == Action_Mode ) {
				force_redisp = 1;
				if ( menu_selection(field_text,fldlen,&menu,cobol_menu,menu_pair,menu_attr) ) {
					/* Fake a CR exit */
			                *pfield_exit = NC_FIELD_EXIT_CR;
			                *pfb = NC_FIELD_FORWARD;
					goto xit;
				}
			}
			break;
		/*
		 * ^G or Escape key :
		 */
		case CONTROL('G') :
		case 033 :
			if ( mode == Action_Mode ) {
		                *pfield_exit = NC_FIELD_EXIT_ESC;
		                *pfb = NC_FIELD_FORWARD;
				end_flag = dot_flag = slash_flag = 0;
				goto xit;
			}
			break;
		/*
		 * Mouse event :
		 */
		case cck_MOUSE :
			if ( term->api->get_mouse_event(term) != 0 ) {
				cct_mevent *mevt = term->api->get_mouse_event(term);

				if ( y + 1 == mevt->y
				&&   mevt->x >= x + 1
				&&   mevt->x <= x + winlen ) {
					/* This event is within the field - move cursor */
					csr = mevt->x - 1 - x;
				} else	{
					/* This event exits the field */
					*pfield_exit = NC_FIELD_EXIT_ESC;
			                *pfb = NC_FIELD_FORWARD;
					*pmouseflag = 'Y';	/* Mark as a mouse event */
					end_flag = dot_flag = slash_flag = 0;
					term->api->clear_mouse_event(term);
					goto xit;
				}
				term->api->clear_mouse_event(term);
			}
			break;
		/*
		 * Enter :
		 */
		case cck_ENTER :
			ch = cck_ENTER;
			if ( mode == Action_Mode ) {
				if ( !dot_flag && !slash_flag ) {
					/*
					 * Normal Enter "exit" :
					 */
			                *pfield_exit = NC_FIELD_EXIT_CR;
			                *pfb = NC_FIELD_FORWARD;
					if ( cobol_menu && cobcurses_is_blank(field_text,fldlen) )
						menu_selection(field_text,fldlen,&menu,cobol_menu,menu_pair,menu_attr);
				} else if ( dot_flag != 0 ) {
					/*
					 * Dot (in col 1) + Enter :
					 */
					*pfield_exit = NC_FIELD_EXIT_DOT;
					*pfb = NC_FIELD_BACKWARD;
					memcpy(field_text,full_undo,fldlen);	/* Restore to original content */
				} else if ( slash_flag != 0 ) {
					/*
					 * Slash (in col 1) + Enter :
					 */
					*pfield_exit = NC_FIELD_EXIT_SLASH;
					*pfb = NC_FIELD_FORWARD;
					memcpy(field_text,full_undo,fldlen);	/* Restore to original content */
				}
			} else if ( mode == Mode_3270 ) {
		                *pfield_exit = NC_FIELD_EXIT_CR;
		                *pfb = NC_FIELD_FORWARD;
			}
			goto xit;
		/*
		 * Tab key :
		 */
		case '\t' :
			*pfield_exit = NC_FIELD_EXIT_TAB;
			*pfb = NC_FIELD_FORWARD;
			end_flag = dot_flag = slash_flag = 0;
			goto xit;
		/*
		 * Back tab key :
		 */
		case cck_BTAB :
			*pfield_exit = NC_FIELD_EXIT_BTAB;
			*pfb = NC_FIELD_BACKWARD;
			end_flag = dot_flag = slash_flag = 0;
			goto xit;
		/*
		 * ^N or Arrow Down key :
		 */
	        case CONTROL('N') :
		case cck_DOWN :
			*pfield_exit = NC_FIELD_EXIT_CD;
			*pfb = NC_FIELD_FORWARD;
			end_flag = dot_flag = slash_flag = 0;
			goto xit;
		/*
		 * ^P or Arrow Up Key :
		 */
	        case CONTROL('P') :
		case cck_UP :
			*pfield_exit = NC_FIELD_EXIT_CU;
			*pfb = NC_FIELD_BACKWARD;
			end_flag = dot_flag = slash_flag = 0;
			goto xit;
		/*
		 * ^B or Left arrow key :
		 */
	        case CONTROL('B') :
		case cck_LEFT :
			if ( csr > 0 )
				--csr;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^F or Arrow right key :
		 */
	        case CONTROL('F') :
		case cck_RIGHT :
			if ( csr + 1 < fldlen )
				++csr;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^A or Home key :
		 */
	        case CONTROL('A') :
		case cck_HOME :
			csr = 0;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^E or End key :
		 */
	        case CONTROL('E') :
		case cck_END :
			csr = fldlen - 1;
			dot_flag = slash_flag = 0;
			if ( end_flag ) {	/* Was the prior key a ^E also? */
				/* Double ^E means the end excluding blanks */
				for ( csr = fldlen - 1; csr > 0; --csr )
					if ( field_text[csr] != ' ' ) {
						++csr;
						break;
					}
				if ( csr >= fldlen )
					csr = fldlen - 1;
				end_flag = 0;
			} else
				end_flag = 1;	/* This is the first consecutive ^E */
			break;
		/*
		 * ^K key :  Delete to end of line.
		 */
		case CONTROL('K') :
			end_flag = dot_flag = slash_flag = 0;
	                put_yank(csr,field_text,fldlen);
			record_kill(csr,field_text,fldlen);
			for ( cx=csr; cx < fldlen; ++cx )
				field_text[cx] = ' ';
			force_redisp = 1;
			break;
		/*
		 * ^R key :  Move "Word left"
		 */
		case CONTROL('R') :
			if ( csr > 0 )
				--csr;
			while ( csr > 0 && field_text[csr] == ' ' )
				--csr;
			while ( csr > 0 && field_text[csr-1] != ' ' )
				--csr;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^W key :  Move "Word right"
		 */
		case CONTROL('W') :
			while ( csr < fldlen && field_text[csr] != ' ' )
				++csr;
			cx = csr;		/* Save for later */
			while ( csr < fldlen && field_text[csr] == ' ' )
				++csr;
			if ( csr >= fldlen )
				csr = fldlen - 1;
			if ( field_text[csr] == ' ' && cx < fldlen )
				csr = cx;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * Insert key :  Toggle insert mode
		 */
		case cck_SIC :
		case cck_IC :
			insmod ^= 1;		/* Toggle insert mode */
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * Backspace key :
		 */
		case cck_BACKSPACE :
			if ( csr > 0 )
				--csr;
			else	break;
			/* Fall Thru */
		/*
		 * ^D or Delete or Rubout key :
		 */
		case CONTROL('D') :
		case 0x7F :
		case cck_DC :
			record_delch(csr,field_text[csr]);
			dot_flag = slash_flag = 0;
			for ( cx = csr; cx+1 < fldlen; ++cx )
				field_text[cx] = field_text[cx+1];
			field_text[fldlen-1] = ' ';
			force_redisp = 1;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^U for Full Undo of field :
		 */
		case CONTROL('U') :
			memcpy(field_text,full_undo,fldlen);	/* Restore to original content */
			force_redisp = 1;			/* Force redisplay of field */
			csr = 0;				/* Reset cursor to zero */
			woff = 0;				/* Start window at zero */
			free_changes();				/* Destroy saved changes */
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * Yank from kill buffer :
		 */
	        case CONTROL('Y') :
			csr += yank(csr,field_text,fldlen);
			force_redisp = 1;
			break;
		/*
		 * Undo the last change :
		 */
		case CONTROL('Z') :
			force_redisp = undo(&csr,field_text,fldlen);
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * ^L for force redisplay of field :
		 */
		case CONTROL('L') :
			force_redisp = 1;
			end_flag = dot_flag = slash_flag = 0;
			break;
		/*
		 * Function Keys :
		 */
		case cck_F2 :
		case cck_F3 :
		case cck_F4 :
		case cck_F5 :
		case cck_F6 :
		case cck_F7 :
		case cck_F8 :
		case cck_F9 :
		case cck_F10 :
		case cck_F11 :
		case cck_F12 :
fkey_case:		*pfield_exit = NC_FIELD_EXIT_FKEY;
			if ( mode == Mode_3270 )
				*pfb = NC_FIELD_BACKWARD;
			else	*pfb = NC_FIELD_FORWARD;
			switch ( ch ) {
			case cck_F1 :
		                search = 1;
				break;
			case cck_F2 :
		                search = 2;
				break;
			case cck_F3 :
		                search = 3;
				break;
			case cck_F4 :
		                search = 4;
				break;
			case cck_F5 :
		                search = 5;
				break;
			case cck_F6 :
		                search = 6;
				break;
			case cck_F7 :
		                search = 7;
				break;
			case cck_F8 :
		                search = 8;
				break;
			case cck_F9 :
		                search = 9;
				break;
			case cck_F10 :
		                search = 10;
				break;
			case cck_F11 :
		                search = 11;
				break;
			case cck_F12 :
		                search = 12;
			}
	                goto xit;
		/*
		 * All other key codes here :
		 */
	        case 0 :
			term->api->beep(term);
			break;
		/*
		 * Text input :
		 */
	        default :
			end_flag = 0;
			if ( c2 < cck_MIN && c2 >= ' ' ) {
				/*
				 * Dot & Slash Return processing :
				 */
				if ( mode == Action_Mode && !csr ) {
					switch ( c2 ) {
					case '.' :
						dot_flag = 1;
						slash_flag = 0;
						break;
					case '/' :
						dot_flag = 0;
						slash_flag = 1;
						break;
					default :
						dot_flag = slash_flag = 0;
					}
				} else
					dot_flag = slash_flag = 0;
	                }
	                if ( ch < cck_MIN && ch >= ' ' ) {
				/*
				 * Process a text character :
				 */
				if ( csr < fldlen ) {
					if ( !insmod ) {
						record_ch(csr,field_text[csr]);
						field_text[csr] = ch;
						term->api->putch(term,!mask ? ch : '*');
					} else	{
						record_insch(csr,field_text[fldlen-1]);
						for ( cx = fldlen-1; cx > csr; --cx )
							field_text[cx] = field_text[cx-1];
						field_text[csr] = ch;
						force_redisp = 1;
				}
				if ( csr + 1 < fldlen )
					++csr;
			}
			} else	{
				term->api->beep(term);
			}
		}
	}

	/*
	 * The field is being exited :
	 *
	 * NB: In 3270 mode, there is no verification done
	 *     upon the field's contents.
	 */
xit:	if ( mode == Mode_3270 || (mode == Action_Mode && *pfield_exit == NC_FIELD_EXIT_FKEY) ) {
		if ( *pfield_exit == NC_FIELD_EXIT_FKEY ) {
			*paction = ' ';
		} else	search = 0;
		*psearch = search;

		/*
		 * Redisplay input field :
		 */
		term->api->move(term,y,x);	/* Reposition to start of field */
	        start_field(pair,fb_attr);
		if ( !mask )
			/* Refresh the field's display */
			term->api->putsn(term,(char *)field_text,winlen);
		else
			for ( csr=0; csr<winlen; ++csr )
				term->api->putch(term,'*');

		*pxpos = xpos = csr + 1;
		free_changes();			/* Release all undo buffers */

	        *pchgflag = has_changed(field_text,full_undo,fldlen);
		FREEA(full_undo);		/* Release full undo buffer */
		full_undo = 0;
		term->api->set_attr_and_pair(term,at,pr);/* Restore attributes & colour */
		goto xitrc;
	}

	/*
	 * Code only executes here in Action Mode exits :
	 */
	if ( *pfield_exit != NC_FIELD_EXIT_ESC
          && *pfield_exit != NC_FIELD_EXIT_DOT
          && *pfield_exit != NC_FIELD_EXIT_SLASH ) {

		if ( isYN && toupper(*field_text) != 'Y' && toupper(*field_text) != 'N' )
			goto loop;

		if ( notblk && *pfb == 'F' && cobcurses_is_blank(field_text,fldlen) )
			goto loop;	/* Don't exit until something entered! */

		if ( restrict != 0 && (csr = cobcurses_is_charset(field_text,fldlen,restrict)) > 0 ) {
			--csr;		/* Cursor is zero based */
			goto loop;	/* Invalid characters! (due to paste) */
		}

		if ( isnum ) {
			edit_numfield(field_text,fldlen,1);	/* Strip out blanks + commas */
			term->api->move(term,y,x);
		        start_field(pair,fb_attr);
			if ( !mask )
				/* Refresh the field's display */
				term->api->putsn(term,(char *)field_text,winlen);
			else
				for ( csr=0; csr<winlen; ++csr )
					term->api->putch(term,'*');

			if ( !Is_Numeric(field_text,fldlen,signch,maxdigits,maxdecs) ) {
				if ( *field_text == ' ' && !notblk )
					*field_text = '0'; /* Just assume zero for blanks */
				else	{
					csr = 0;
					goto loop; /* Loop until we got something numeric */
				}
			}
		} else if ( comp_type >= CBL_COMP_1 && comp_type <= CBL_COMP_2 ) {
			char *buf = (char *)field_text;
                        const char *cbuf = (const char *)field_text;
			const char *ucfg = (const char *)units_config;

			switch ( comp_type ) {
			case CBL_COMP_1 : {
				float fv;
                                double dv;
				char y = 'Y';

				xc = NC_EDIT_COMP1(&fv,&cbuf,&fldlen,&ucfg);
				if ( xc != RET_OK )
					goto loop;
				dv = (double) fv;
				memset(buf,' ',fldlen);
				xc = NC_FORMAT_COMP2(&dv,&y,&maxdigits,&ucfg,&buf,&fldlen);
				if ( xc != RET_OK )
					goto loop;

				if ( pcomp_x != 0 )
					*(cct_float *)pcomp_x = fv;
				}
				break;
			case CBL_COMP_2 : {
				double dv;
				char y = 'Y';

				xc = NC_EDIT_COMP2(&dv,&cbuf,&fldlen,&ucfg);
				if ( xc != RET_OK )
					goto loop;
				memset(buf,' ',fldlen);
				xc = NC_FORMAT_COMP2(&dv,&y,&maxdigits,&ucfg,&buf,&fldlen);
				if ( xc != RET_OK )
					goto loop;
				if ( pcomp_x != 0 )
					*(cct_double *)pcomp_x = dv;
				}
				break;
			default :
				abort();
			}
		}
	}

	/*
	 * Are we "escaping" out of this field? If so,
	 * we must restore the original field's content.
	 */
	if ( *pfield_exit == NC_FIELD_EXIT_ESC
          || *pfield_exit == NC_FIELD_EXIT_DOT
          || *pfield_exit == NC_FIELD_EXIT_SLASH )
		/* Back out any changes for this "escape exit" */
		memcpy(field_text,full_undo,fldlen);

	/*
	 * Redisplay input field :
	 */
	term->api->move(term,y,x);	/* Reposition to start of field */
        start_field(pair,fb_attr);
	if ( !mask )
		/* Refresh the field's display */
		term->api->putsn(term,(char *)field_text,winlen);
	else
		for ( csr=0; csr<winlen; ++csr )
			term->api->putch(term,'*');

	*pxpos = xpos = csr + 1;
	free_changes();			/* Release all undo buffers */
        *pchgflag = has_changed(field_text,full_undo,fldlen);
	FREEA(full_undo);		/* Release full undo buffer */
	full_undo = 0;

	term->api->set_attr_and_pair(term,at,pr);/* Restore attributes & colour */

	/*
	 * Does this have "action field" format?
	 */
	if ( *pfield_exit == NC_FIELD_EXIT_CR ) {
		unsigned short x2;
		unsigned short nlen;
		char a;
		short nnnn = 0;

		/* Skip leading blanks */
		for ( csr=0; csr<fldlen && field_text[csr] == ' '; ++csr );
		a = csr < fldlen ? field_text[csr] : 0;
		if ( isalpha(a) ) {
			for ( cx=++csr; cx<fldlen && field_text[cx] == ' '; ++cx);
			if ( cx < fldlen && !isdigit(field_text[cx]) )
				a = 0;		/* Not a single alpha action character */
		} else if ( isdigit(a) )
			a = ' ';		/* Action field # (only) */

		/* Skip blanks in front of number */
		for ( ; csr<fldlen && field_text[csr] == ' '; ++csr );
		if ( csr < fldlen ) {
			/* Find the end of the numeric field */
			for ( cx=csr; cx<fldlen && isdigit(field_text[cx]); ++cx );
			nlen = cx - csr;
			if ( nlen > 3 )
				a = 0;		/* Too long */
			else if ( nlen > 0 ) {
				/* Is the rest of the field blank? */
				for ( x2=cx; x2<fldlen && field_text[x2] == ' '; ++x2 );
				if ( x2 < fldlen )
					a = 0;	/* Other junk present */
				if ( a != 0 ) {
					char *nbuf = ALLOCA(nlen+1);

					strncpy(nbuf,(char *)&field_text[csr],nlen)[nlen] = 0;
					nnnn = atoi(nbuf);
					FREEA(nbuf);
				}
			}
		}
		if ( !a ) {
                        a = ' ';        /* Return blank to Cobol program */
			nnnn = 0;
		}
		*paction = a;			/* Return action character (if any) */
		*psearch = search = nnnn;	/* Return field number (if any) */
	}

xitrc:	if ( menu ) {
		term->api->delete_menu(menu);
		menu = 0;
	}
	cobcurses_trace_printf(2,"final field = '%.*s'\n",fldlen,field_text);
	cobcurses_trace_printf(2,"field exit = '%c'\n",*pfield_exit);
	cobcurses_trace_printf(2,"change flag = '%c'\n",*pchgflag);
	cobcurses_trace_printf(2,"action = '%c', search = %u\n",*paction,*psearch);
	cobcurses_trace_printf(2,"returning rc = %u\n",xc);
	return xc;
}

/*
 * NC_VERIFY is called by 3270 code when validation on the form's
 * input fields are required. This routine returns the following:
 *
 * RETURN-CODE :
 *	0 (RET_OK)	This field verifies OK.
 *	1 (RET_FAILED)	This field does not verify.
 */
int
NC_VERIFY(
    cct_ushort	*pfield_length,	/* Field's length */
    char 	**pfield_text,	/* Pointer to Field's buffer address */
    char 	*pnotblank,	/* When 'Y' do not allow FWD movement with blank field */
    char	**prestrict,	/* Restrict to these characters */
    char	*psignyn,	/* When 'Y', allow a sign character in numeric field */
    cct_ushort	*pnumdigits,	/* Max Number of digits for numeric field */
    cct_ushort	*pnumdecplaces	/* Max number of decimal places in numeric field */
) {
        char *restrict;				/* Restricted character set to use */
        char *field_text;			/* Pointer to the field buffer */
	cct_ushort fldlen;			/* Buffer length */
	cct_bool notblk = TOBOOL(*pnotblank); 	/* Don't allow blank field going forward */
	cct_bool isYN;				/* Is this a Y/N field? */
	cct_ushort maxdigits;			/* Max digits before decimal place if numeric */
	cct_ushort maxdecs;			/* Max digits after decimal place if numeric */
	cct_bool isnum; 			/* Is this a numeric only field? */
        cct_bool signch = TOBOOL(*psignyn);  	/* Is this a signed numeric? */

	fldlen 		= *pfield_length;
	field_text 	= *pfield_text;
	restrict	= *prestrict;
	maxdigits	= *pnumdigits;
	maxdecs		= *pnumdecplaces;

	isnum = maxdigits > 0 || maxdecs > 0 ? 1 : 0;
	isYN = restrict && !strncmp((char *)restrict,"YN ",3) ? 1 : 0;

	cobcurses_trace_printf(2,"\nNC_VERIFY('%.*s') called.\n",fldlen,field_text);

	if ( isYN && toupper(*field_text) != 'Y' && toupper(*field_text) != 'N' ) {
		cobcurses_trace_printf(2,"invalid Y/N character\n");
		goto failed;		/* Invalid char for Y/N field */
	}

	if ( notblk && cobcurses_is_blank(field_text,fldlen) ) {
		cobcurses_trace_printf(2,"field cannot be blank\n");
		goto failed;		/* Field cannot be blank */
	}

	if ( restrict != 0 && cobcurses_is_charset(field_text,fldlen,restrict) > 0 ) {
		cobcurses_trace_printf(2,"invalid character in field.\n");
		goto failed;		/* Bad character in restricted charset field */
	}

	if ( isnum && !Is_Numeric(field_text,fldlen,signch,maxdigits,maxdecs) ) {
		cobcurses_trace_printf(2,"invalid numeric or exceeds digit ranges.\n");
		goto failed;		/* Bad character/range for numeric field */
	}

	cobcurses_trace_printf(2,"returning NC-OK.\n");
	return RET_OK;

failed:	cobcurses_trace_printf(2,"returning NC-FAILED.\n");
	return RET_FAILED;
}

/*
 * Display the buffer in the input field in the
 * same attributes/colour as NC_GETTEXT :
 */
static int
puttext0(
    cct_ushort		*py,			/* Field's Y position */
    cct_ushort		*px,			/* Field's X position */
    cct_termattr	fb_attr,		/* Fallback attribute */ 
    cct_pair		*ppair,			/* Colour pair if non-zero */
    cct_ushort		*pfield_length,		/* Field's length */
    cct_ushort		*pwindow_length,	/* Field's window length */
    char 		**pfield_text		/* Pointer to Field's buffer address */
) {
        char *field_text;
	cct_ushort y;
	cct_ushort x;
	cct_ushort fldlen;
	cct_ushort winlen;
	cct_termattr at;
	cct_pair pair;
	cct_pair pr;

	if ( !term )
		return RET_OPEN;		/* Terminal not open! */

	y = *py - 1;
	x = *px - 1;
	field_text	= *pfield_text;
	fldlen		= *pfield_length;
	winlen		= *pwindow_length;
	pair		= *ppair;

	if ( !winlen )
		winlen = fldlen;
	if ( winlen > fldlen )
		winlen = fldlen;	/* Default to buffer's length */
	
	/* Get attributes to restore later */
	term->api->get_attr_and_pair(term,&at,&pr);

	/*
	 * Display the buffer :
	 */
	term->api->move(term,y,x);
	start_field(pair,fb_attr);
	term->api->putsn(term,(char *)field_text,winlen);
	term->api->set_attr_and_pair(term,at,pr);

	return RET_OK;
}

/*
 * Display the buffer in the input field in the
 * same attributes/colour as NC_GETTEXT :
 */
int
NC_PUTTEXT(
    cct_ushort	*py,		/* Field's Y position */
    cct_ushort	*px,		/* Field's X position */
    cct_pair	*ppair,		/* Colour pair if non-zero */
    cct_ushort	*pfield_length,	/* Field's length */
    cct_ushort	*pwindow_length, /* Field's window length */
    char 	**pfield_text	/* Pointer to Field's buffer address */
) { 
	int rc;

	cobcurses_trace_printf(2,"\nNC_PUTTEXT('%.*s') for pos %u,%u called.\n",
		*pfield_length,*pfield_text,*py,*px);
	open_check();

	rc = puttext0(py,px,CCA_REVERSE,ppair,pfield_length,pwindow_length,pfield_text);
	cobcurses_trace_printf(2,"returns %u\n",rc);
	return rc;
}

/*
 * Display the buffer in the input field in the
 * same attributes/colour in A_NORMAL attribute.
 */
int
NC_PUTTEXTNORM(
    cct_ushort	*py,		/* Field's Y position */
    cct_ushort	*px,		/* Field's X position */
    cct_pair	*ppair,		/* Colour pair if non-zero */
    cct_ushort	*pfield_length,	/* Field's length */
    cct_ushort	*pwindow_length, /* Field's window length */
    char 	**pfield_text	/* Pointer to Field's buffer address */
) { 
	int rc;

	cobcurses_trace_printf(2,"\nNC_PUTTEXTNORM('%.*s') for pos %u,%u called.\n",
		*pfield_length,*pfield_text,*py,*px);
	open_check();

	rc = puttext0(py,px,CCA_NORMAL,ppair,pfield_length,pwindow_length,pfield_text);
	cobcurses_trace_printf(2,"returns %u\n",rc);
	return rc;
}

/*
 * Set terminal attributes, without affecting the colour :
 */
int
NC_SETATTR(cct_termattr *pattr) {

	cobcurses_trace_printf(2,"\nNC_SETATTR(0x%04X) called.\n",*pattr);
	open_check();
	term->api->set_attr(term,*pattr);
	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Configure a colour pair :
 */
int
NC_INITCOLOUR(
    cct_pair	*ppair_no,		/* Pair number to assign colour to */
    cct_ushort	*pforeground,		/* Foreground colour for pair */
    cct_ushort	*pbackground		/* Background colour for pair */
) {
	cct_colour foreground, background;

	cobcurses_trace_printf(2,"\nNC_INITCOLOUR(pair=%u,fg=%u,bg=%u) called.\n",
		*ppair_no,*pforeground,*pbackground);
	open_check();

	foreground = (cct_colour) *pforeground;
	background = (cct_colour) *pbackground;
	term->api->init_pair(term,foreground,background,*ppair_no);

	cobcurses_trace_printf(2,"returns NC-OK.\n");
	return RET_OK;
}

/*
 * Change the current colour pair :
 */
int
NC_SETCOLOUR(cct_pair *ppair_no) {

	cobcurses_trace_printf(2,"\nNC_SETCOLOUR(pair=%u) called.\n",*ppair_no);
	open_check();
	term->api->set_pair(term,*ppair_no);
	cobcurses_trace_printf(2,"return NC-OK.\n");
	return RET_OK;
}

/*
 * Draw a box :
 */
int
NC_DRAW_BOX(
    cct_ushort	*ptop_line,		/* Top line # */
    cct_ushort	*pleft_col,		/* Top left col # */
    cct_ushort	*pbot_line,		/* Bottom line # */
    cct_ushort	*pright_col		/* Bottom right col # */
) {
        cct_ushort top_line = *ptop_line - 1;
        cct_ushort left_col = *pleft_col - 1;
        cct_ushort bot_line = *pbot_line - 1;
        cct_ushort right_col = *pright_col - 1;
        cct_ushort cols = right_col - left_col + 1;
        cct_ushort lcllines = bot_line - top_line + 1;

	cobcurses_trace_printf(2,"\nNC_DRAW_BOX(%u,%u,%u,%u) called.\n",
		top_line + 1, left_col + 1, bot_line + 1, right_col + 1);
	open_check();

	term->api->move(term,top_line,left_col);
	term->api->hline(term,acs_HLINE,cols);
	term->api->vline(term,acs_VLINE,lcllines);
	term->api->putch(term,acs_ULCORNER);

	term->api->move(term,bot_line,left_col);
	term->api->hline(term,acs_HLINE,cols);
	term->api->move(term,top_line,right_col);
	term->api->vline(term,acs_VLINE,lcllines);
	term->api->putch(term,acs_URCORNER);
	
	term->api->move(term,bot_line,left_col);
	term->api->putch(term,acs_LLCORNER);
	term->api->move(term,bot_line,right_col);
	term->api->putch(term,acs_LRCORNER);

	term->api->move(term,top_line+1,left_col+1);
	term->api->flush(term);

	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}

/*
 * Draw the title line at the top of the screen :
 */
int
NC_TITLE(
    cct_ushort	*py,		/* Line number for title */
    char	**ptext,	/* Title text */
    cct_ushort	*plength,	/* Length of title text */
    cct_pair	*ppair,		/* Colour pair or zero if none */
    char 	*pbold,		/* Y for bold */
    char 	*punderline,	/* Y for underline */
    char	*preverse,	/* Y for reverse */
    cct_date	*pdate,		/* Ptr to date for title only */
    cct_time	*ptime		/* Ptr to time for title only */
) {
	cct_ushort y = *py - 1;		/* Line number for title */
	char *title = *ptext;		/* Pointer to title text */
	cct_ushort tlen = *plength;	/* Title's length */
	cct_pair pair = *ppair;		/* Colour pair in ncurses type */
	cct_bool bold = TOBOOL(*pbold);	/* Bold */
#ifndef BUILD_MINGW
	cct_bool underline = TOBOOL(*punderline);
#endif
	cct_bool reverse = TOBOOL(*preverse);
	cct_unsigned date = pdate ? *pdate : 0;
	cct_unsigned time = ptime ? *ptime : 0;
        cct_ushort x;
	cct_termattr svattr = 0;
	cct_pair     svpair = 0;
	cct_termattr attrs = 0;

	cobcurses_trace_printf(2,"\nNC_TITLE(y=%u,title='%.*s',pair=%u,bold=%c,underline=%c,reverse=%c,date=%u,time=%u) called.\n",
		*py,*plength,*ptext,*ppair,*pbold,*punderline,*preverse,*pdate,*ptime);
	open_check();

	term->api->get_attr_and_pair(term,&svattr,&svpair);	/* Save current attribute & pair */
	term->api->cap_attr(term,&attrs);			/* Get terminal attr capabilities */

	/* Ignore title's trailing blanks */
	while ( tlen > 0 && title[tlen-1] == ' ' )
		--tlen;
	x = term->api->columns(term) / 2 - tlen / 2;

	if ( bold )
		title_attr |= CCA_BOLD;
#ifndef BUILD_MINGW
	if ( underline )
		title_attr |= CCA_UNDERLINE;
#else
	title_attr &= ~(CCA_UNDERLINE);		/* PDCurses can't support via DOS Window */
#endif
	if ( reverse )
		title_attr |= CCA_REVERSE;

	title_init = T;

        if ( !pair )
		term->api->set_attr(term,title_attr);
	else	term->api->set_attr_and_pair(term,title_attr,pair);

	term->api->move(term,y,0);
	term->api->clrtoeol(term);

	{
		char *buf = ALLOCA(term->api->columns(term));

		memset(buf,' ',term->api->columns(term));
		term->api->putsn(term,buf,term->api->columns(term));
		FREEA(buf);
	}

        {
		unsigned yy = date % 100;
		unsigned mm = ( date / 100 ) % 100;
		unsigned dd = date / 10000;
		char buf[9];

		sprintf(buf,"%02u/%02u/%02u",yy,mm,dd);
		term->api->move(term,y,0);
		term->api->putsn(term,buf,8);
	}

	{
		unsigned hh = time / 1000000;
		unsigned mm = ( time / 10000 ) % 100;
		char buf[19];

		sprintf(buf,"%02u:%02u",hh,mm);
		term->api->move(term,y,term->api->columns(term)-5);
		term->api->putsn(term,buf,5);
	}

	term->api->move(term,y,x);
	term->api->putsn(term,(char *)title,tlen);
	term->api->set_attr_and_pair(term,svattr,svpair);

	cobcurses_trace_printf(2,"return NC-OK.\n");
	return RET_OK;
}
    
/*
 * Put a status line message at the bottom of the screen :
 */
int
NC_MSG(
    char	**ptext,			/* Pointer to message */
    cct_ushort	*tlen,				/* Text length */
    cct_pair	*ppair,				/* Colour pair to use if non-zero */
    char	*reqpause			/* 'Y' if pause required */
) {
	char *text = *ptext;			/* Message text through a pointer */
	cct_ushort length = *tlen;		/* Message length */
	cct_pair pair = *ppair;			/* Colour pair to use */
        cct_bool pause = TOBOOL(*reqpause); 	/* Pause required? */
	cct_termattr at;			/* Saved attributes */
	cct_pair pr;				/* Saved colour pair */
	cct_ushort y, x;			/* Saved y, x */

	cobcurses_trace_printf(2,"\nNC_MSG('%.*s',pair=%u,pause=%c) called.\n",
		*tlen,*ptext,*ppair,*reqpause);
	open_check();

	x = term->api->getyx(term,&y);				/* Get y & x position */
	term->api->get_attr_and_pair(term,&at,&pr);		/* Save current settings */

	term->api->move(term,term->api->lines(term)-1,0);	/* Move to bottom of screen */
	term->api->set_attr_and_pair(term,CCA_NORMAL,pair);	/* Choose our attributes and colour */

        if ( text != 0 && length > 0 )
		term->api->putsn(term,(char *)text,length);	/* Put message out */
        else	length = 0;					/* We put 0 bytes out */
	term->api->clrtoeol(term);				/* Clear to end of line/screen */

	/* Fill out rest of line (for attribute) */
	for ( ; length < term->api->columns(term); ++length )
		term->api->putch(term,' ');

	/* Put curser in right hand corner */
	term->api->move(term,term->api->lines(term)-1,term->api->columns(term)-1);

        if ( pause ) {
		int ch;
    
		do	{
			ch = term->api->getkey(term);	/* Wait for CR */
			switch ( ch ) {
			case '\r' :
			case '\n' :
			case cck_ENTER :
				ch = 0;
				break;
			default :
				term->api->beep(term);
			}
		} while ( ch != 0 );
        }

	term->api->set_attr_and_pair(term,at,pr);	/* Restore attr & colour */
	term->api->move(term,y,x);			/* Restore cursor position */

	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}

/*
 * Blank out a field's buffer contents :
 */
int
NC_BLANK(
    char	**ppfield,
    cct_ushort	*plength
) {
	char *pfield = *ppfield;
	cct_ushort length = *plength;

	if ( !pfield )
		return RET_FAILED;

	memset(pfield,' ',length);	/* Set field to blanks */
	return RET_OK;
}

/*
 * INTERNAL - Numeric Test :
 */
static cct_bool
Is_Numeric(
    char        *buf,
    cct_ushort  len,
    cct_bool    signch,
    cct_ushort  maxdigits,
    cct_ushort  maxdecplaces
) {
	cct_ushort cx;
	cct_uchar ch;
	cct_ushort digits = 0;
	cct_ushort decplaces = 0;
	cct_ushort zeros = 0;
	cct_bool gotdigit = 0;
	cct_bool started = 0;
	cct_bool gotdec = 0;

	/*
	 * First make sure that no invalid characters are present :
	 */
	for ( cx=0; cx<len; ++cx ) {
		ch = buf[cx];
		if ( ch == ' ' ) {
			if ( started )
				break;
		} else if ( started && gotdigit && !gotdec && ch == ',' ) {
			;		/* Ignore comma before decimal place */
		} else if ( !started && (ch == '-' || ch == '+' ) ) {
                        if ( ch == '-' && !signch )
				return 0;	/* Sign not allowed ('+' can be ignored) */
			started = 1;
		} else if ( !gotdec && ch == '.' ) {
			started = 1;
			gotdec = 1;
		} else if ( isdigit(ch) ) {
			started = 1;
			gotdigit = 1;
			if ( !gotdec ) {
				if ( digits || ch != '0' )
					++digits;
			} else	{
				if ( ch == '0' ) {
					++zeros;	/* Defer counting of trailing zeros */
				} else	{
					decplaces += 1 + zeros;
					zeros = 0;
				}
			}
		} else
			return 0;	/* Inappropriate character */
	}

	if ( gotdigit )
		digits = 1;		/* We got a leading zero at least */
	if ( !gotdigit )
		return 0;		/* No numeric digits present */
	if ( digits > maxdigits || decplaces > maxdecplaces )
		return 0;		/* Exceeds capacity */
	for ( ; cx<len && buf[cx] == ' '; ++cx );
	return cx < len ? 0 : 1;
}

/*
 * Extract a text segment out of the input buffer (inbuf)
 * and place it into the output buffer (otbuf) of *otlen
 * bytes. The starting offset within inbuf is returned in
 * *column. The segment's length is returned in *seglen
 * (which excludes trailing blanks).
 */
int
NC_EXTRACT_SEGMENT(
    char        *pinbuf,    /* Input buffer */
    cct_ushort  *pinlen,    /* Input buffer width */
    char        *potbuf,    /* Output buffer */
    cct_ushort  *potlen,    /* Output buffer's max length */
    cct_ushort  *pcolumn,   /* Returned offset from inbuf */
    cct_ushort  *pseglen    /* Segment's length in otbuf */
) {
	cct_ushort inlen	= *pinlen;
	cct_ushort otlen	= *potlen;
	cct_ushort x;
	cct_ushort slen;
	cct_ushort column = 0;
        cct_ushort seglen = 0;
	int rc = RET_OK;

	for ( x=0; x<inlen && pinbuf[x] == ' '; ++x );	/* Skip leading blanks */
	if ( x < inlen ) {
	    	/*
		 * Copy over largest segment possible :
		 */
		slen = otlen;				/* Assume full segment size */
		if ( x + slen > inlen )
			slen = inlen - x;		/* The segment will be shorter */
		slen = (cct_ushort) cobcurses_trim_trailing((char *)pinbuf+x,slen,' ');
	
		memset(potbuf,' ',otlen);		/* Blank out receiving buffer */
		memcpy(potbuf,pinbuf+x,slen);		/* Copy over segment */
		memset(pinbuf+x,' ',slen);		/* Blank out segment we copied */
	
		column = x + 1;				/* Return 1-based index */
		seglen = slen;				/* Return the segment's length */
		rc = RET_OK;
	} else
		rc = RET_FAILED;

	*pcolumn = column;
	*pseglen = seglen;
	return rc;
}

/*
 * Upon entry, buflen contains the full buffer length.
 * Upon return buflen is set to the length with the
 * trailing blanks excluded.
 */
int
NC_STRIP(
    char	*buf,
    cct_ushort	*pbuflen
) {
	cct_ushort buflen = *pbuflen;

	while ( buflen > 0 && buf[buflen-1] == ' ' )
		--buflen;
	*pbuflen = buflen;
	return RET_OK;
}

/*********************************************************************************
 *      MOUSE SUPPORT (OPTIONAL)
 ********************************************************************************/

/*
 * Set the time for a mouse "click"
 */
int
NC_MOUSE_INTERVAL(
    cct_int *pmillisecs
) {
	cct_mouse_ival ms;

	cobcurses_trace_printf(2,"\nNC_MOUSE_INTERVAL(%u ms) called.\n",*pmillisecs);
	open_check();

	if ( !term->api->have_mouse(term) ) {
		cobcurses_trace_printf(2,"No mouse support compiled in. returns NC-NSUPPORT.\n");
		return RET_NSUPPORT;    	/* No mouse support */
	}

	ms = term->api->get_mouse_ival(term);
	*pmillisecs = (int) ms;

	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}	

int
NC_MOUSE_MASK(
    char *pb1_pressed,		/* 'Y' / 'N' : Mouse button 1, pressed  */
    char *pb1_released,		/* 'Y' / 'N' : Mouse button 1, released */
    char *pb1_clicked,		/* 'Y' / 'N' : Mouse button 1, clicked  */
    char *pb1_d_clicked,	/* 'Y' / 'N' : Mouse button 1, double-clicked */
    char *pb1_t_clicked,	/* 'Y' / 'N' : Mouse button 1, triple-clicked */
    char *pb2_pressed,		/* 'Y' / 'N' : Mouse button 2, etc. */
    char *pb2_released,		/* 'Y' / 'N' */
    char *pb2_clicked,		/* 'Y' / 'N' */
    char *pb2_d_clicked,	/* 'Y' / 'N' */
    char *pb2_t_clicked,	/* 'Y' / 'N' */
    char *pb3_pressed,		/* 'Y' / 'N' : Mouse button 3, etc. */
    char *pb3_released,		/* 'Y' / 'N' */
    char *pb3_clicked,		/* 'Y' / 'N' */
    char *pb3_d_clicked,	/* 'Y' / 'N' */
    char *pb3_t_clicked		/* 'Y' / 'N' */
) {
	cct_bool b1_pressed	= TOBOOL(*pb1_pressed);
	cct_bool b1_released	= TOBOOL(*pb1_released);
	cct_bool b1_clicked	= TOBOOL(*pb1_clicked);
	cct_bool b1_d_clicked	= TOBOOL(*pb1_d_clicked);
	cct_bool b1_t_clicked	= TOBOOL(*pb1_t_clicked);
	cct_bool b2_pressed	= TOBOOL(*pb2_pressed);
	cct_bool b2_released	= TOBOOL(*pb2_released);
	cct_bool b2_clicked	= TOBOOL(*pb2_clicked);
	cct_bool b2_d_clicked	= TOBOOL(*pb2_d_clicked);
	cct_bool b2_t_clicked	= TOBOOL(*pb2_t_clicked);
	cct_bool b3_pressed	= TOBOOL(*pb3_pressed);
	cct_bool b3_released	= TOBOOL(*pb3_released);
	cct_bool b3_clicked	= TOBOOL(*pb3_clicked);
	cct_bool b3_d_clicked	= TOBOOL(*pb3_d_clicked);
	cct_bool b3_t_clicked	= TOBOOL(*pb3_t_clicked);
	cct_mmask nmask = 0;

	cobcurses_trace_printf(2,"\nNC_MOUSE_MASK() called.\n");
	open_check();

	if ( !term->api->have_mouse(term) ) {
		cobcurses_trace_printf(2,"returned NC-NSUPPORT. (no mouse support)\n");
		return RET_NSUPPORT;	/* No mouse support */
	}

	if ( b1_pressed )
		nmask |= BSTATE_PRESSED(1);
	if ( b1_released )
		nmask |= BSTATE_RELEASED(1);
	if ( b1_clicked )
		nmask |= BSTATE_CLICKED(1);
	if ( b1_d_clicked )
		nmask |= BSTATE_DOUBLE_CLICKED(1);
	if ( b1_t_clicked )
		nmask |= BSTATE_TRIPLE_CLICKED(1);

	if ( b2_pressed )
		nmask |= BSTATE_PRESSED(2);
	if ( b2_released )
		nmask |= BSTATE_RELEASED(2);
	if ( b2_clicked )
		nmask |= BSTATE_CLICKED(2);
	if ( b2_d_clicked )
		nmask |= BSTATE_DOUBLE_CLICKED(2);
	if ( b2_t_clicked )
		nmask |= BSTATE_TRIPLE_CLICKED(2);

	if ( b3_pressed )
		nmask |= BSTATE_PRESSED(3);
	if ( b3_released )
		nmask |= BSTATE_RELEASED(3);
	if ( b3_clicked )
		nmask |= BSTATE_CLICKED(3);
	if ( b3_d_clicked )
		nmask |= BSTATE_DOUBLE_CLICKED(3);
	if ( b3_t_clicked )
		nmask |= BSTATE_TRIPLE_CLICKED(3);

	term->api->set_mouse_mask(term,nmask);

	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;
}

int
NC_MOUSE_EVENT(
    cct_ushort 	*pid,		/* RETURNED : Mouse ID */
    cct_ushort 	*px,		/* RETURNED : X (1-based) */
    cct_ushort 	*py,		/* RETURNED : Y (1-based) */
    cct_ushort 	*pz,		/* RETURNED : Z (1-based) */
    char 	*pb1_pressed,	/* RETURNED : Button 1 Pressed Y/N */
    char 	*pb1_released,	/* RETURNED : Button 1 Released Y/N */
    char 	*pb1_clicked,	/* RETURNED : Button 1 Clicked Y/N */
    char 	*pb1_d_clicked,	/* RETURNED : Button 1 Double clicked Y/N */
    char 	*pb1_t_clicked,	/* RETURNED : Button 1 Triple clicked Y/N */
    char 	*pb2_pressed,	/* RETURNED : Button 2 Pressed Y/N */
    char 	*pb2_released,	/* RETURNED : Button 2 Released Y/N */
    char 	*pb2_clicked,	/* RETURNED : Button 2 Clicked Y/N */
    char 	*pb2_d_clicked,	/* RETURNED : Button 2 Double clicked Y/N */
    char 	*pb2_t_clicked,	/* RETURNED : Button 2 Triple clicked Y/N */
    char 	*pb3_pressed,	/* RETURNED : Button 3 Pressed Y/N */
    char 	*pb3_released,	/* RETURNED : Button 3 Released Y/N */
    char 	*pb3_clicked,	/* RETURNED : Button 3 Clicked Y/N */
    char 	*pb3_d_clicked,	/* RETURNED : Button 3 Double clicked Y/N */
    char 	*pb3_t_clicked 	/* RETURNED : Button 3 Triple clicked Y/N */
) {
	cct_mevent *mevt = 0;

	cobcurses_trace_printf(2,"\nNC_MOUSE_EVENT() called.\n");
	open_check();

	if ( !term->api->have_mouse(term) ) {
		cobcurses_trace_printf(2,"returned NC-NSUPPORT. (no mouse support)\n");
		return RET_NSUPPORT;		/* No mouse support */
	}

	mevt = term->api->get_mouse_event(term);
	if ( !mevt ) {
		cobcurses_trace_printf(2,"returned NC-FAILED. (no mouse data)\n");
		return RET_FAILED;		/* No mouse data */
	}

	*pid = (cct_ushort) mevt->id;		/* Mouse ID */
	*px = (cct_ushort) mevt->x + 1;		/* X coordinate */
	*py = (cct_ushort) mevt->y + 1;		/* Y coordinate */
	*pz = (cct_ushort) mevt->z + 1;		/* Z coordinate */

	cobcurses_trace_printf(2,"Mouse ID %u, x=%u, y=%u, z=%u\n",*pid,*px,*py,*pz);

	/* Return Y/N flags for each button state */

	*pb1_pressed 	= BOOLCH(mevt->bstate & BSTATE_PRESSED(1));
	*pb1_released	= BOOLCH(mevt->bstate & BSTATE_RELEASED(1));
	*pb1_clicked	= BOOLCH(mevt->bstate & BSTATE_CLICKED(1));
	*pb1_d_clicked	= BOOLCH(mevt->bstate & BSTATE_DOUBLE_CLICKED(1));
	*pb1_t_clicked	= BOOLCH(mevt->bstate & BSTATE_TRIPLE_CLICKED(1));

	*pb2_pressed 	= BOOLCH(mevt->bstate & BSTATE_PRESSED(2));
	*pb2_released	= BOOLCH(mevt->bstate & BSTATE_RELEASED(2));
	*pb2_clicked	= BOOLCH(mevt->bstate & BSTATE_CLICKED(2));
	*pb2_d_clicked	= BOOLCH(mevt->bstate & BSTATE_DOUBLE_CLICKED(2));
	*pb2_t_clicked	= BOOLCH(mevt->bstate & BSTATE_TRIPLE_CLICKED(2));

	*pb3_pressed 	= BOOLCH(mevt->bstate & BSTATE_PRESSED(3));
	*pb3_released	= BOOLCH(mevt->bstate & BSTATE_RELEASED(3));
	*pb3_clicked	= BOOLCH(mevt->bstate & BSTATE_CLICKED(3));
	*pb3_d_clicked	= BOOLCH(mevt->bstate & BSTATE_DOUBLE_CLICKED(3));
	*pb3_t_clicked	= BOOLCH(mevt->bstate & BSTATE_TRIPLE_CLICKED(3));

	term->api->clear_mouse_event(term);

	cobcurses_trace_printf(2,"returned NC-OK.\n");
	return RET_OK;			/* A mouse event was returned */
}

/*
 * Return an initialized title attribute :
 */
int
NC_TITLE_ATTRS(cct_termattr *pattr) {

	if ( !title_init ) {
		title_attr = CCA_BOLD | CCA_REVERSE | CCA_UNDERLINE;
#ifdef BUILD_MINGW
		title_attr &= ~(CCA_UNDERLINE);		/* PDCurses can't support via DOS Window */
#endif
		title_init = T;
	}
	*pattr = title_attr;
	return RET_OK;
}

/*
 * Initialize when libcobcurses.sl gets instantiated :
 */
int
NC_LIBCOBCURSES(void) {

	cobcurses_getenv(COBCURSES_TOPDIR_ENVNAME);
	return 0;
}

/*
 * Provide state trace info :
 */
int
NC_TRACE_STATE(cct_ushort *pfseq,cct_ushort *pfseq_next,cct_ushort *pfno,char *pverify_flag) {

	cobcurses_trace_printf(5,"\nState Change:\n");
	cobcurses_trace_printf(5,"  Current State: %04d\n",*pfseq);
	cobcurses_trace_printf(5,"  Next State:    %04d\n",*pfseq_next);
	cobcurses_trace_printf(5,"  Field No.:     %04d\n",*pfno);
	cobcurses_trace_printf(5,"  Verify Flag:   '%c'\n",*pverify_flag);
	return RET_OK;
}

int
NC_TRACE_MSG(const char *pmsg) {

	cobcurses_trace_printf(5,"Trace Msg: %.*s\n",cobcurses_2blen(pmsg),pmsg);
	return RET_OK;
}

/*
 * Show the menu and return the selection(s) :
 */
int
NC_SHOW_MENU(
    const char 		*pmenu_defn,	/* Ptr to COBOL menu definition */
    char        	*psel_buffer,	/* Ptr to receiving buffer PIC X(*pbuflen) */
    cct_ushort  	*pbuflen,	/* Ptr to PIC 9999 COMP-5 SYNCHRONIZED length of buffer */
    cct_pair		*pmenu_pair,	/* The colour pair to use for menu frame */
    cct_termattr	*pmenu_attr	/* The menu title attributes to use */
) {
	cct_menu *menu = 0;

	cobcurses_trace_printf(2,"NC_SHOW_MENU(mdefn=%p,buf=%p,buflen=%u,pair=%u,attr=%u) called..\n",
		pmenu_defn, psel_buffer, *pbuflen, *pmenu_pair, *pmenu_attr);

	if ( !menu_selection(psel_buffer,*pbuflen,&menu,pmenu_defn,*pmenu_pair,*pmenu_attr) ) {
		cobcurses_trace_printf(2,"  returned NC-RET-FAILED (menu error)\n");
		return RET_FAILED;
	}

	if ( menu != 0 )
		term->api->delete_menu(menu);

	cobcurses_trace_printf(2,"  returned NC-RET-OK (selection '%.*s')\n",*pbuflen,psel_buffer);
	return RET_OK;
}

/*
 * Suspend terminal support to allow system() call :
 */
int
NC_SUSPEND(void) {

	cobcurses_trace_printf(2,"\nNC_SUSPEND() called.\n");
	open_check();

	term->api->suspend(term);
	return RET_OK;
}

/*
 * Resume terminal support to allow system call :
 */
int
NC_RESUME(void) {

	cobcurses_trace_printf(2,"\nNC_RESUME() called.\n");
	open_check();

	term->api->resume(term);
	return RET_OK;
}

/* End ncurses.c */

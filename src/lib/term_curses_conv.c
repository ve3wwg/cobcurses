/* term_curses_conv.c : Conversion routines for terminal.h to Curses(3X) values
 * Warren W. Gay VE3WWG
 * Thu Sep 27 13:01:17 2007
 * $Id: term_curses_conv.c,v 1.1 2007/09/27 17:17:23 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <term_curses.h>
#include <memory.h>

/*
 * Convert Curses(3X) mouse mask to cct_mmask :
 */
cct_mmask
termcurs_conv_to_term_mmask(mmask_t mmask) {
	cct_mmask m = 0;

#ifdef COBCURSES_MOUSE_SUPPORT
	if ( mmask & BUTTON1_PRESSED )
		m |= BSTATE_PRESSED(1);
	if ( mmask & BUTTON1_RELEASED )
		m |= BSTATE_RELEASED(1);
	if ( mmask & BUTTON1_CLICKED )
		m |= BSTATE_CLICKED(1);
	if ( mmask & BUTTON1_DOUBLE_CLICKED )
		m |= BSTATE_DOUBLE_CLICKED(1);
	if ( mmask & BUTTON1_TRIPLE_CLICKED )
		m |= BSTATE_TRIPLE_CLICKED(1);

	if ( mmask & BUTTON2_PRESSED )
		m |= BSTATE_PRESSED(2);
	if ( mmask & BUTTON2_RELEASED )
		m |= BSTATE_RELEASED(2);
	if ( mmask & BUTTON2_CLICKED )
		m |= BSTATE_CLICKED(2);
	if ( mmask & BUTTON2_DOUBLE_CLICKED )
		m |= BSTATE_DOUBLE_CLICKED(2);
	if ( mmask & BUTTON2_TRIPLE_CLICKED )
		m |= BSTATE_TRIPLE_CLICKED(2);

	if ( mmask & BUTTON3_PRESSED )
		m |= BSTATE_PRESSED(3);
	if ( mmask & BUTTON3_RELEASED )
		m |= BSTATE_RELEASED(3);
	if ( mmask & BUTTON3_CLICKED )
		m |= BSTATE_CLICKED(3);
	if ( mmask & BUTTON3_DOUBLE_CLICKED )
		m |= BSTATE_DOUBLE_CLICKED(3);
	if ( mmask & BUTTON3_TRIPLE_CLICKED )
		m |= BSTATE_TRIPLE_CLICKED(3);
#endif
	return m;
}

/*
 * Convert cct_mmask to Curses(3X) mouse mask value :
 */
mmask_t
termcurs_conv_to_curses_mmask(cct_mmask mmask) {
	mmask_t m = 0;

#ifdef COBCURSES_MOUSE_SUPPORT
	if ( mmask & BSTATE_PRESSED(1) )
		m |= BUTTON1_PRESSED;
	if ( mmask & BSTATE_RELEASED(1) )
		m |= BUTTON1_RELEASED;
	if ( mmask & BSTATE_CLICKED(1) )
		m |= BUTTON1_CLICKED;
	if ( mmask & BSTATE_DOUBLE_CLICKED(1) )
		m |= BUTTON1_DOUBLE_CLICKED;
	if ( mmask & BSTATE_TRIPLE_CLICKED(1) )
		m |= BUTTON1_TRIPLE_CLICKED;

	if ( mmask & BSTATE_PRESSED(2) )
		m |= BUTTON2_PRESSED;
	if ( mmask & BSTATE_RELEASED(2) )
		m |= BUTTON2_RELEASED;
	if ( mmask & BSTATE_CLICKED(2) )
		m |= BUTTON2_CLICKED;
	if ( mmask & BSTATE_DOUBLE_CLICKED(2) )
		m |= BUTTON2_DOUBLE_CLICKED;
	if ( mmask & BSTATE_TRIPLE_CLICKED(2) )
		m |= BUTTON2_TRIPLE_CLICKED;

	if ( mmask & BSTATE_PRESSED(3) )
		m |= BUTTON3_PRESSED;
	if ( mmask & BSTATE_RELEASED(3) )
		m |= BUTTON3_RELEASED;
	if ( mmask & BSTATE_CLICKED(3) )
		m |= BUTTON3_CLICKED;
	if ( mmask & BSTATE_DOUBLE_CLICKED(3) )
		m |= BUTTON3_DOUBLE_CLICKED;
	if ( mmask & BSTATE_TRIPLE_CLICKED(3) )
		m |= BUTTON3_TRIPLE_CLICKED;
#endif
	return m;
}

/*
 * Convert Curses(3X) Mouse event data to cct_mevent :
 */
void
termcurs_conv_to_term_mevent(cct_mevent *term_mevent,MEVENT *curs_mevent) {
	
	memset(term_mevent,0,sizeof *term_mevent);
#ifdef COBCURSES_MOUSE_SUPPORT
	term_mevent->id 	= (cct_mouseid) curs_mevent->id;
	term_mevent->x		= (cct_ushort) curs_mevent->x;
	term_mevent->y		= (cct_ushort) curs_mevent->y;
	term_mevent->z		= (cct_ushort) curs_mevent->z;
	term_mevent->bstate	= termcurs_conv_to_term_mmask(curs_mevent->bstate);
#endif
}

/*
 * Convert curses keycode to terminal.h keycode :
 */
int
termcurs_conv_to_term_key(int keycode) {

	switch ( keycode ) {
	case KEY_BREAK :
		return cck_BREAK;
	case KEY_SRESET :
		return cck_SRESET;
	case KEY_RESET :
		return cck_RESET;
	case KEY_DOWN :
		return cck_DOWN;
	case KEY_UP :
		return cck_UP;
	case KEY_LEFT :
		return cck_LEFT;
	case KEY_SLEFT :
		return cck_SLEFT;
	case KEY_RIGHT :
		return cck_RIGHT;
	case KEY_SRIGHT :
		return cck_SRIGHT;
	case KEY_HOME :
		return cck_HOME;
	case KEY_SHOME :
		return cck_SHOME;
	case KEY_BACKSPACE :
		return cck_BACKSPACE;
	case KEY_F0 :
		return cck_F0;
	case KEY_F(1) :
		return cck_F1;
	case KEY_F(2) :
		return cck_F2;
	case KEY_F(3) :
		return cck_F3;
	case KEY_F(4) :
		return cck_F4;
	case KEY_F(5) :
		return cck_F5;
	case KEY_F(6) :
		return cck_F6;
	case KEY_F(7) :
		return cck_F7;
	case KEY_F(8) :
		return cck_F8;
	case KEY_F(9) :
		return cck_F9;
	case KEY_F(10) :
		return cck_F10;
	case KEY_F(11) :
		return cck_F11;
	case KEY_F(12) :
		return cck_F12;
	case KEY_DL :
		return cck_DL;
	case KEY_SDL :
		return cck_SDL;
	case KEY_IL :
		return cck_IL;
	case KEY_DC :
		return cck_DC;
	case KEY_SDC :
		return cck_SDC;
	case KEY_IC :
		return cck_IC;
	case KEY_SIC :
		return cck_SIC;
	case KEY_EIC :
		return cck_EIC;
	case KEY_CLEAR :
		return cck_CLEAR;
	case KEY_EOS :
		return cck_EOS;
	case KEY_EOL :
		return cck_EOL;
	case KEY_SEOL :
		return cck_SEOL;
	case KEY_SF :
		return cck_SF;
	case KEY_SR :
		return cck_SR;
	case KEY_NPAGE :
		return cck_NPAGE;
	case KEY_PPAGE :
		return cck_PPAGE;
	case KEY_STAB :
		return cck_STAB;
	case KEY_CTAB :
		return cck_CTAB;
	case KEY_CATAB :
		return cck_CATAB;
	case KEY_ENTER :
		return cck_ENTER;
	case KEY_PRINT :
		return cck_PRINT;
	case KEY_SPRINT :
		return cck_SPRINT;
	case KEY_LL :
		return cck_LL;
	case KEY_A1 :
		return cck_A1;
	case KEY_A3 :
		return cck_A3;
	case KEY_B2 :
		return cck_B2;
	case KEY_C1 :
		return cck_C1;
	case KEY_C3 :
		return cck_C3;
	case KEY_BTAB :
		return cck_BTAB;
	case KEY_BEG :
		return cck_BEG;
	case KEY_SBEG :
		return cck_SBEG;
	case KEY_CANCEL :
		return cck_CANCEL;
	case KEY_SCANCEL :
		return cck_SCANCEL;
	case KEY_CLOSE :
		return cck_CLOSE;
	case KEY_COMMAND :
		return cck_COMMAND;
	case KEY_SCOMMAND :
		return cck_SCOMMAND;
	case KEY_COPY :
		return cck_COPY;
	case KEY_SCOPY :
		return cck_SCOPY;
	case KEY_CREATE :
		return cck_CREATE;
	case KEY_SCREATE :
		return cck_SCREATE;
	case KEY_END :
		return cck_END;
	case KEY_EXIT :
		return cck_EXIT;
	case KEY_SEXIT :
		return cck_SEXIT;
	case KEY_FIND :
		return cck_FIND;
	case KEY_SFIND :
		return cck_SFIND;
	case KEY_HELP :
		return cck_HELP;
	case KEY_SHELP :
		return cck_SHELP;
	case KEY_MARK :
		return cck_MARK;
	case KEY_MESSAGE :
		return cck_MESSAGE;
	case KEY_SMESSAGE :
		return cck_SMESSAGE;
	case KEY_MOVE :
		return cck_MOVE;
	case KEY_SMOVE :
		return cck_SMOVE;
	case KEY_NEXT :
		return cck_NEXT;
	case KEY_SNEXT :
		return cck_SNEXT;
	case KEY_OPEN :
		return cck_OPEN;
	case KEY_OPTIONS :
		return cck_OPTIONS;
	case KEY_SOPTIONS :
		return cck_SOPTIONS;
	case KEY_PREVIOUS :
		return cck_PREVIOUS;
	case KEY_SPREVIOUS :
		return cck_SPREVIOUS;
	case KEY_REDO :
		return cck_REDO;
	case KEY_SREDO :
		return cck_SREDO;
	case KEY_REFERENCE :
		return cck_REFERENCE;
	case KEY_REFRESH :
		return cck_REFRESH;
	case KEY_REPLACE :
		return cck_REPLACE;
	case KEY_SREPLACE :
		return cck_SREPLACE;
	case KEY_RESTART :
		return cck_RESTART;
	case KEY_RESUME :
		return cck_RESUME;
	case KEY_SRSUME :
		return cck_SRSUME;
	case KEY_SAVE :
		return cck_SAVE;
	case KEY_SSAVE :
		return cck_SSAVE;
	case KEY_SELECT :
		return cck_SELECT;
	case KEY_SEND :
		return cck_SEND;
	case KEY_SUNDO :
		return cck_SUNDO;
	case KEY_SUSPEND :
		return cck_SUSPEND;
	case KEY_SSUSPEND :
		return cck_SSUSPEND;
	case KEY_UNDO :
		return cck_UNDO;
	case KEY_MOUSE :
		return cck_MOUSE;
	case KEY_RESIZE :
		return cck_RESIZE;
	case KEY_EVENT :
		return cck_EVENT;
	case ERR :
		return cck_IDLE;
	}
	return (0x00FF & keycode);
}

/*
 * Convert curses character to terminal character :
 */
#if 0	/* Not Required at this time */
char
termcurs_conv_to_term_char(chtype ch) {

	if ( ch == ACS_ULCORNER )
		return acs_ULCORNER;	/* upper left corner */
	if ( ch == ACS_LLCORNER )
		return acs_LLCORNER;	/* lower left corner */
	if ( ch == ACS_URCORNER )
		return acs_URCORNER;	/* upper right corner */
	if ( ch == ACS_LRCORNER )
		return acs_LRCORNER;	/* lower right corner */
	if ( ch == ACS_LTEE )
		return acs_LTEE;	/* tee pointing right */
	if ( ch == ACS_RTEE )
		return acs_RTEE;	/* tee pointing left */
	if ( ch == ACS_BTEE )
		return acs_BTEE;	/* tee pointing up */
	if ( ch == ACS_TTEE )
		return acs_TTEE;	/* tee pointing down */
	if ( ch == ACS_HLINE )
		return acs_HLINE;	/* horizontal line */
	if ( ch == ACS_VLINE )
		return acs_VLINE;	/* vertical line */
	if ( ch == ACS_PLUS )
		return acs_PLUS;	/* large plus or crossover */
	if ( ch == ACS_S1 )
		return acs_S1;		/* scan line 1 */
	if ( ch == ACS_S9 )
		return acs_S9;		/* scan line 9 */
	if ( ch == ACS_DIAMOND )
		return acs_DIAMOND;	/* diamond */
	if ( ch == ACS_CKBOARD )
		return acs_CKBOARD;	/* checker board (stipple) */
	if ( ch == ACS_DEGREE )
		return acs_DEGREE;	/* degree symbol */
	if ( ch == ACS_PLMINUS )
		return acs_PLMINUS;	/* plus/minus */
	if ( ch == ACS_BULLET )
		return acs_BULLET;	/* bullet */
	if ( ch == ACS_LARROW )
		return acs_LARROW;	/* arrow pointing left */
	if ( ch == ACS_RARROW )
		return acs_RARROW;	/* arrow pointing right */
	if ( ch == ACS_DARROW )
		return acs_DARROW;	/* arrow pointing down */
	if ( ch == ACS_UARROW )
		return acs_UARROW;	/* arrow pointing up */
	if ( ch == ACS_BOARD )
		return acs_BOARD;	/* board of squares */
	if ( ch == ACS_LANTERN )
		return acs_LANTERN;	/* lantern symbol */
	if ( ch == ACS_BLOCK )
		return acs_BLOCK;	/* solid square block */
#if 0
	if ( ch == ACS_S3 )
		return acs_S3;		/* scan line 3 */
	if ( ch == ACS_S7 )
		return acs_S7;		/* scan line 7 */
	if ( ch == ACS_LEQUAL )
		return acs_LEQUAL;	/* less/equal */
	if ( ch == ACS_GEQUAL )
		return acs_GEQUAL;	/* greater/equal */
	if ( ch == ACS_PI )
		return acs_PI;		/* Pi */
	if ( ch == ACS_NEQUAL )
		return acs_NEQUAL;	/* not equal */
#endif
	if ( ch == ACS_STERLING )
		return acs_STERLING;	/* UK pound sign */

	return (char) (ch & 0xFF);	/* Non-special */
}
#endif

/*
 * Convert terminal character to curses character :
 */
chtype
termcurs_conv_to_curses_char(char ch) {

	switch ( ch ) {
	case acs_ULCORNER :
		return ACS_ULCORNER;	/* upper left corner */
	case acs_LLCORNER :
		return ACS_LLCORNER;	/* lower left corner */
	case acs_URCORNER :
		return ACS_URCORNER;	/* upper right corner */
	case acs_LRCORNER :
		return ACS_LRCORNER;	/* lower right corner */
	case acs_LTEE :
		return ACS_LTEE;	/* tee pointing right */
	case acs_RTEE :
		return ACS_RTEE;	/* tee pointing left */
	case acs_BTEE :
		return ACS_BTEE;	/* tee pointing up */
	case acs_TTEE :
		return ACS_TTEE;	/* tee pointing down */
	case acs_HLINE :
		return ACS_HLINE;	/* horizontal line */
	case acs_VLINE :
		return ACS_VLINE;	/* vertical line */
	case acs_PLUS :
		return ACS_PLUS;	/* large plus or crossover */
	case acs_S1 :
		return ACS_S1;		/* scan line 1 */
	case acs_S9 :
		return ACS_S9;		/* scan line 9 */
	case acs_DIAMOND :
		return ACS_DIAMOND;	/* diamond */
	case acs_CKBOARD :
		return ACS_CKBOARD;	/* checker board (stipple) */
	case acs_DEGREE :
		return ACS_DEGREE;	/* degree symbol */
	case acs_PLMINUS :
		return ACS_PLMINUS;	/* plus/minus */
	case acs_BULLET :
		return ACS_BULLET;	/* bullet */
	case acs_LARROW :
		return ACS_LARROW;	/* arrow pointing left */
	case acs_RARROW :
		return ACS_RARROW;	/* arrow pointing right */
	case acs_DARROW :
		return ACS_DARROW;	/* arrow pointing down */
	case acs_UARROW :
		return ACS_UARROW;	/* arrow pointing up */
	case acs_BOARD :
		return ACS_BOARD;	/* board of squares */
	case acs_LANTERN :
		return ACS_LANTERN;	/* lantern symbol */
	case acs_BLOCK :
		return ACS_BLOCK;	/* solid square block */
#if 0
	case acs_S3 :
		return ACS_S3;		/* scan line 3 */
	case acs_S7 :
		return ACS_S7;		/* scan line 7 */
#endif
	case acs_LEQUAL :
		return ACS_LEQUAL;	/* less/equal */
	case acs_GEQUAL :
		return ACS_GEQUAL;	/* greater/equal */
	case acs_PI :
		return ACS_PI;		/* Pi */
	case acs_NEQUAL :
		return ACS_NEQUAL;	/* not equal */
	case acs_STERLING :
		return ACS_STERLING;	/* UK pound sign */
	}

	return (chtype)ch;		/* Not special */
}

/*
 * Map terminal's colour code to curses(3X) code :
 */
cst_colour
termcurs_conv_to_curses_colour(cct_colour colour) {

	switch ( colour ) {
	case COLOR_BLACK :
		return clr_BLACK;
	case COLOR_RED :
		return clr_RED; 
	case COLOR_GREEN :
		return clr_GREEN;
	case COLOR_YELLOW :
		return clr_YELLOW;
	case COLOR_BLUE :
		return clr_BLUE;
	case COLOR_MAGENTA :
		return clr_MAGENTA;
	case COLOR_CYAN :
		return clr_CYAN;
	case COLOR_WHITE :
		return clr_WHITE;
	}
	return clr_BLACK;
}

#if 0	/* NOT REQUIRED AT THIS TIME */
/*
 * Map curses(3X) colour code to terminal's code :
 */
cct_colour
termcurs_conv_to_term_colour(cst_colour colour) {

	switch ( colour ) {
	case clr_BLACK :
		return COLOR_BLACK;
	case clr_RED :
		return COLOR_RED; 
	case clr_GREEN :
		return COLOR_GREEN;
	case clr_YELLOW :
		return COLOR_YELLOW;
	case clr_BLUE :
		return COLOR_BLUE;
	case clr_MAGENTA :
		return COLOR_MAGENTA;
	case clr_CYAN :
		return COLOR_CYAN;
	case clr_WHITE :
		return COLOR_WHITE;
	}
	return COLOR_BLACK;
}
#endif

/*
 * Convert curses(3X) attributes to terminal.h attributes :
 */
cct_termattr
termcurs_conv_to_term_attrs(attr_t from) {
	cct_termattr a = 0;

	if ( from & A_NORMAL )
		a |= CCA_NORMAL;
	if ( from & A_STANDOUT )
		a |= CCA_STANDOUT;
	if ( from & A_UNDERLINE )
		a |= CCA_UNDERLINE;
	if ( from & A_REVERSE )
		a |= CCA_REVERSE;
	if ( from & A_BLINK )
		a |= CCA_BLINK;
	if ( from & A_DIM )
		a |= CCA_DIM;
	if ( from & A_BOLD )
		a |= CCA_BOLD;
#ifdef A_ALTCHARSET
	if ( from & A_ALTCHARSET )
		a |= CCA_ALTCHARSET;
#endif
#ifdef A_INVIS
	if ( from & A_INVIS )
		a |= CCA_INVIS;
#endif
#ifdef A_PROTECT
	if ( from & A_PROTECT )
		a |= CCA_PROTECT;
#endif
#ifdef A_HORIZONTAL
	if ( from & A_HORIZONTAL )
		a |= CCA_HORIZONTAL;
#endif
#ifdef A_LEFT
	if ( from & A_LEFT )
		a |= CCA_LEFT;
#endif
#ifdef A_LOW
	if ( from & A_LOW )
		a |= CCA_LOW;
#endif
#ifdef A_RIGHT
	if ( from & A_RIGHT )
		a |= CCA_RIGHT;
#endif
#ifdef A_TOP
	if ( from & A_TOP )
		a |= CCA_TOP;
#endif
#ifdef A_VERTICAL
	if ( from & A_VERTICAL )
		a |= CCA_VERTICAL;
#endif
	return a;
}

/*
 * Convert from terminal.h attributes to curses(3X) attr_t :
 */
attr_t
termcurs_conv_to_curses_attrs(cct_termattr from) {
	attr_t a = 0;

	if ( from & CCA_NORMAL )
		a |= A_NORMAL;
	if ( from & CCA_STANDOUT )
		a |= A_STANDOUT;
	if ( from & CCA_UNDERLINE )
		a |= A_UNDERLINE;
	if ( from & CCA_REVERSE )
		a |= A_REVERSE;
	if ( from & CCA_BLINK )
		a |= A_BLINK;
	if ( from & CCA_DIM )
		a |= A_DIM;
	if ( from & CCA_BOLD )
		a |= A_BOLD;
#ifdef A_ALTCHARSET
	if ( from & CCA_ALTCHARSET )
		a |= A_ALTCHARSET;
#endif
#ifdef A_INVIS
	if ( from & CCA_INVIS )
		a |= A_INVIS;
#endif
#ifdef A_PROTECT
	if ( from & CCA_PROTECT )
		a |= A_PROTECT;
#endif
#ifdef A_HORIZONTAL
	if ( from & CCA_HORIZONTAL )
		a |= A_HORIZONTAL;
#endif
#ifdef A_LEFT
	if ( from & CCA_LEFT )
		a |= A_LEFT;
#endif
#ifdef A_LOW
	if ( from & CCA_LOW )
		a |= A_LOW;
#endif
#ifdef A_RIGHT
	if ( from & CCA_RIGHT )
		a |= A_RIGHT;
#endif
#ifdef A_TOP
	if ( from & CCA_TOP )
		a |= A_TOP;
#endif
#ifdef A_VERTICAL
	if ( from & CCA_VERTICAL )
		a |= A_VERTICAL;
#endif
	return a;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses_conv.c,v $ */

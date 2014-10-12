/* term_curses_conv.h : Conversion routines for terminal.h to Curses(3X) values
 * Warren W. Gay VE3WWG
 * Thu Sep 27 13:01:17 2007
 * $Id: term_curses_conv.h,v 1.1 2007/09/27 17:17:23 ve3wwg Exp $
 */
#ifndef _term_curses_conv_h_
#define _term_curses_conv_h_

#define IsSpecialChar(ch) (( (ch) <= acs_STERLING ) ? T : F )

/*
 * Use DEFNTHIS when _this is only needed or assert() :
 */
#ifndef NDEBUG
#define DEFNTHIS(obj)	cct_termcurs *_this = (cct_termcurs *)(obj)
#endif

extern cct_mmask termcurs_conv_to_term_mmask(mmask_t mmask);
extern mmask_t termcurs_conv_to_curses_mmask(cct_mmask mmask);
extern void termcurs_conv_to_term_mevent(cct_mevent *term_mevent,MEVENT *curs_mevent);
extern int termcurs_conv_to_term_key(int keycode);
extern char termcurs_conv_to_term_char(chtype ch);
extern chtype termcurs_conv_to_curses_char(char ch);
extern cst_colour termcurs_conv_to_curses_colour(cct_colour colour);
extern cct_colour termcurs_conv_to_term_colour(cst_colour colour);
extern cct_termattr termcurs_conv_to_term_attrs(attr_t from);
extern attr_t termcurs_conv_to_curses_attrs(cct_termattr from);

#endif /* _term_curses_conv_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses_conv.h,v $ */

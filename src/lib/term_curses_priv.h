/* term_curses_priv.h : Curses module for terminal I/O
 * Warren W. Gay VE3WWG
 * Thu Sep  6 16:25:57 2007
 * $Id: term_curses_priv.h,v 1.4 2007/09/28 14:31:02 ve3wwg Exp $
 */
#ifndef _term_curses_priv_h_
#define _term_curses_priv_h_

/*
 * This is the extension info from cct_terminal.extdata :
 */
typedef struct {
	cct_bool		open;		/* True if window is open */
	WINDOW			*win;		/* Main window */
	cct_mmask		mmask;		/* Current mouse mask in event */
	cct_mevent		mevent;		/* Last mouse event (if any) */
	cct_bool		have_mevent;	/* T if we have a mouse event pending */
	cct_mouse_ival		mouse_click_ms;	/* Mouse click interval in ms */
#ifdef COBCURSES_MOUSE_SUPPORT
	mmask_t			mmask_save;	/* To be restored upon closing */
#endif
} cct_curses;

/*
 * This is the cct_termcurs view of cct_terminal :
 */
typedef struct {
	cct_objid		id;		/* Id of this object */
	cct_api_terminal	*api;		/* Api set */
	cct_bool		alloc;		/* True if allocated object by malloc */
	cct_curses		*curses;	/* Curses info */
} cct_termcurs;

#endif /* _term_curses_priv_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses_priv.h,v $ */


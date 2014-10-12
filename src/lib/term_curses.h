/* term_curses.h : Curses module for terminal I/O
 * Warren W. Gay VE3WWG
 * Thu Sep  6 16:25:57 2007
 * $Id: term_curses.h,v 1.6 2007/10/16 02:51:08 ve3wwg Exp $
 */
#ifndef _term_curses_h_
#define _term_curses_h_

#ifdef USE_NCURSES
#ifdef HAVE_NCURSES_H
#include <ncurses.h>
#endif
#ifdef HAVE_NCURSES_NCURSES_H
#include <ncurses/ncurses.h>
#endif
#endif

#ifdef USE_PDCURSES
#ifdef HAVE_PDCURSES_H
#include <pdcurses.h>
#endif
#endif

#ifdef USE_CURSES
#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#endif

#if 1
#include <cc_menu.h>
#else
#if defined(USE_NCURSES) && defined(HAVE_NCURSES_MENU_H)
#include <ncurses/menu.h>
#else
#ifdef HAVE_MENU_H 1
#include <menu.h>
#endif
#endif
#endif

#include <terminal.h>

/*
 * Local Curses(3X) typedefs :
 */
typedef short cst_colour;
typedef short cst_pair;

#include <term_curses_priv.h>
#include <term_curses.h>
#include <term_curses_conv.h>

extern cct_terminal *termcurs_init(cct_terminal *obj);

#endif /* _term_curses_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses.h,v $ */

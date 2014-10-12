/* term_curses_menu.h : Menu support from Curses(3X)
 * Warren W. Gay VE3WWG
 * Thu Sep 27 13:23:49 2007
 * $Id: term_curses_menu.h,v 1.8 2007/10/23 16:14:18 ve3wwg Exp $
 */
#ifndef _term_curses_menu_h_
#define _term_curses_menu_h_

/*
 * Menu item :
 */
typedef struct S_MENU_ITEM cct_curses_menu_item;

struct S_MENU_ITEM {
	char		*name;		/* Menu item name */
	char 		*desc;		/* Menu item description */
	ITEM		*item;		/* Item from new_item() */
	struct S_MENU_ITEM *next;	/* Next item in list */
};

/*
 * The menu :
 */
typedef struct {
	cct_termcurs		*term;		/* The terminal object for this menu */
	char			*title;		/* Menu title or 0 */
	cct_term_menu_opts	opts;		/* Menu options */
	cct_termattr		tattr;		/* Title attribute */
	cct_pair		tpair;		/* Title colour pair */
	cct_curses_menu_item	*head;		/* Item list head */
	cct_ushort		nitems;		/* Items count */
	WINDOW			*win;		/* Menu window */
	WINDOW			*sub;		/* Menu subwindow */
	chtype			foreground;	/* Menu foreground attribute */
	chtype			background;	/* Menu background attribute */
	chtype			grey;		/* Menu grey attribute */
	cct_ushort		y;		/* Preferred top left y coord */
	cct_ushort		x;		/* Preferred top left x coord */
	cct_ushort		maxrows;	/* For set_menu_format() */
	cct_ushort		maxcols;	/* Ditto */
	cct_bool		hidden;		/* T when menu has been hidden */
	char			*module;	/* Module name, if this is a dynamic menu */
	cct_ushort		item_limit;	/* Item limit for dynamic menus (0 == no limit) */
	ITEM			**items;
	MENU			*menu;		/* The new_menu() MENU object */
	char			*mvalue;	/* Last multi-valued result returned */
} cct_curses_menu;

extern cct_menu *termcurs_new_menu(cct_terminal *obj);
extern cct_menu *termcurs_set_menu_format(cct_menu *obj,cct_ushort maxrows,cct_ushort maxcols);
extern cct_term_menu_opts termcurs_set_menu_opts(cct_menu *obj,cct_term_menu_opts opts);
extern cct_term_menu_opts termcurs_get_menu_opts(cct_menu *obj);
extern cct_menu *termcurs_new_menu_item(cct_menu *menu,const char *name,const char *desc);
extern cct_bool  termcurs_set_menu_item_sel(cct_menu *obj,const char *name,cct_bool selectable);
extern cct_bool  termcurs_get_menu_item_sel(cct_menu *obj,const char *name);
extern cct_menu *termcurs_set_menu_title(cct_menu *obj,const char *title,cct_termattr attr,cct_pair pair);
extern cct_menu *termcurs_set_menu_coord(cct_menu *menu,cct_ushort y,cct_ushort x);
extern cct_menu *termcurs_set_menu_foreground(cct_menu *obj,cct_termattr attr,cct_pair pair);
extern cct_menu *termcurs_set_menu_background(cct_menu *obj,cct_termattr attr,cct_pair pair);
extern cct_menu *termcurs_set_menu_grey(cct_menu *obj,cct_termattr attr,cct_pair pair);
extern cct_menu *termcurs_show_menu(cct_menu *menu);
extern cct_menu *termcurs_hide_menu(cct_menu *obj);
extern cct_menu *termcurs_delete_menu(cct_menu *menu);
extern char     *termcurs_get_menu_selection(cct_menu *obj);

extern cct_menu *termcurs_set_dynamic_menu(cct_menu *obj,const char *module,cct_ushort item_limit);

#endif /* _term_curses_menu_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses_menu.h,v $ */

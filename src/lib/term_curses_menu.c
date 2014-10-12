/* term_curses_menu.c : Menu support from Curses(3X)
 * Warren W. Gay VE3WWG
 * Thu Sep 27 13:23:49 2007
 * $Id: term_curses_menu.c,v 1.11 2007/10/24 20:20:20 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <term_curses.h>
#include <term_curses_menu.h>
#include <term_curses_conv.h>
#include <misc.h>
#include <dynstr.h>
#include <cobtrace.h>

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef USE_NCURSES
typedef int OPTIONS;				/* ncurses defines this as Item_Options, which is int */
#endif

/*
 * This part is Open-Cobol specific :
 */
typedef int (*cct_menu_module)(const char *req_type,char *name,char *desc);

extern cct_menu_module cob_resolve(const char *module_name);

/*
 * Internal : Refresh menu window on top of main window 
 */
static void
internal_refresh(cct_curses_menu *_this) {

	wnoutrefresh(_this->term->curses->win);	/* Main window */
	if ( _this->win && !_this->hidden )
		wnoutrefresh(_this->win);	/* Menu window */
	doupdate();				/* Do physical screen update */
}

/*
 * Internal : Pop down menu
 */
cct_menu *
termcurs_hide_menu(cct_menu *obj) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;

	if ( _this->hidden )
		return obj;		/* Already hidden */

	if ( _this->menu ) {
		unpost_menu(_this->menu);
		_this->hidden = T;	/* Mark as hidden */
		touchwin(_this->term->curses->win);
		_this->term->api->flush((cct_terminal *)_this->term);
	}
	internal_refresh(_this);	/* Refresh main screen from underneath menu */
	return obj;
}

/*
 * Internal : Create a new cct_curses_menu_item :
 */
static
cct_curses_menu_item *
internal_new_item(const char *name,const char *desc) {
	cct_curses_menu_item *item = malloc(sizeof *item);

	item->name = strdup(name);
	item->desc = malloc(2 + strlen(desc) + 1);
	strcat(strcpy(item->desc,"- "),desc);

	item->next = 0;
	item->item = new_item(item->name,item->desc);
	set_item_userptr(item->item,item->name);

	return item;
}

/*
 * Release menu item :
 */
static
cct_curses_menu_item *
termcurs_delete_item(cct_curses_menu_item *item) {

	if ( item->item )
		free_item(item->item);
	free(item->name);
	free(item->desc);
	memset(item,0,sizeof *item);
	free(item);
	return 0;
}

/*
 * Create a new menu object :
 */
cct_menu *
termcurs_new_menu(cct_terminal *obj) {
	cct_curses_menu *_this = malloc(sizeof *_this);
	attr_t a = termattrs();

	_this->term = (cct_termcurs *) obj; /* Owning terminal object */
	_this->title = 0;		/* No title */
	_this->tattr = CCA_REVERSE;	/* By default reverse */
	_this->tpair = 0;		/* Assume monichrome */
	_this->head = 0;		/* Head of items list */
	_this->items = 0;		/* Items array for new_menu() */
	_this->menu = 0;		/* Menu object from new_menu() */
	_this->nitems = 0;		/* No items currently */
	_this->win = 0;			/* No menu window yet */
	_this->sub = 0;			/* No subwindow yet */
	_this->foreground = A_REVERSE;	/* Selected menu item attribute */
	_this->background = A_NORMAL;	/* Background (unselected) item attribute */
	_this->grey       = A_DIM;	/* Unselectable item attribute */
	_this->y   = 10;		/* Default top left Y */
 	_this->x   = 40;		/* Default top left X */
	_this->maxrows = _this->maxcols = 0;	/* For use with set_menu_format() */
	_this->hidden = T;		/* Mark as hidden for the moment */
	_this->mvalue = 0;		/* No multi-valued data yet */
	_this->module = 0;		/* No registered module yet */
	_this->item_limit = 0;		/* Or corresponding item limit */

	_this->opts = cct_emo_ONEVALUE | cct_emo_ROWMAJOR | cct_emo_IGNORECASE | cct_emo_SHOWDESC
				| cct_emo_SHOWMATCH;

	if ( !(a & A_DIM) )
		_this->grey = A_UNDERLINE;	/* Use underline if no A_DIM capability */
	if ( !(a & A_UNDERLINE) )
		_this->grey = A_NORMAL;		/* Use normal as a last resort */

	return (cct_menu *) _this;
}

/*
 * Set the max menu dimensions :
 */
cct_menu *
termcurs_set_menu_format(cct_menu *obj,cct_ushort maxrows,cct_ushort maxcols) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;

	_this->maxrows = maxrows;
	_this->maxcols = maxcols;
	return obj;
}

/*
 * Set menu options :
 */
cct_term_menu_opts
termcurs_set_menu_opts(cct_menu *obj,cct_term_menu_opts opts) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;
	cct_term_menu_opts orig_opts = _this->opts;

	_this->opts = opts;
	if ( _this->menu )
		set_menu_opts(_this->menu,_this->opts);
	return orig_opts;
}

/*
 * Set menu options :
 */
cct_term_menu_opts
termcurs_get_menu_opts(cct_menu *obj) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;
	cct_term_menu_opts opts = 0;

	if ( _this->menu )
		opts = _this->opts = (cct_term_menu_opts) menu_opts(_this->menu);
	else	opts = _this->opts;
	return opts;
}

/*
 * Add another item to the menu :
 */
cct_menu *
termcurs_new_menu_item(cct_menu *obj,const char *name,const char *desc) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;
	cct_curses_menu_item **pp;
	cct_curses_menu_item *item = internal_new_item(name,desc);

	if ( _this->menu != 0 ) {
		free_menu(_this->menu);
		_this->menu = 0;
		free(_this->items);
		_this->items = 0;
	}

	/*
	 * Attach to the tail end of the items list :
	 */
	for ( pp = &_this->head; *pp != 0; pp = &(*pp)->next )
		;
	*pp = item;

	++_this->nitems;			/* Count a new item added */

	return obj;
}

/*
 * Set a menu title :
 */
cct_menu *
termcurs_set_menu_title(cct_menu *obj,const char *title,cct_termattr attr,cct_pair pair) {
	cct_curses_menu *_this = (cct_curses_menu *)obj;

	if ( _this->title )
		free(_this->title);
	_this->title = title ? strdup(title) : 0;
	_this->tattr = attr;
	_this->tpair = pair;

	return obj;
}

/*
 * Save preferred menu coordinates for top left :
 */
cct_menu *
termcurs_set_menu_coord(cct_menu *obj,cct_ushort y,cct_ushort x) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;

	_this->y = y;
	_this->x = x;
	return obj;
}

static void
draw_title(cct_curses_menu *menu,const char *text) {
	int sy, sx;
	int width;
	char *tbuf;
	cct_ushort ux;
	chtype ch;

	if ( !text )
		text = menu->title;

	if ( !text )
		return;				/* Nothing to draw */

	getmaxyx(menu->win,sy,sx);		/* Get window size */
	width = sx - 2;				/* Get title area width */
	tbuf = ALLOCA(width);			/* Buffer to center text */
	cobcurses_center(tbuf,width,text);	/* Center the text */

	wmove(menu->win,1,1);
	wattr_set(menu->win,
		termcurs_conv_to_curses_attrs(menu->tattr),
		termcurs_conv_to_curses_colour(menu->tpair),
		0);

	for ( ux=0; ux<width; ++ux ) {
		ch = termcurs_conv_to_curses_char(tbuf[ux]);
		waddch(menu->win,ch);
	}

	wattr_set(menu->win,			/* Border colour & attributes */
		A_NORMAL,
		termcurs_conv_to_curses_colour(menu->tpair),
		0);

	/*
	 * Draw extra title area border chars :
	 */
	wmove(menu->win,2,0);
	waddch(menu->win,ACS_LTEE);
	whline(menu->win,ACS_HLINE,width);
	wmove(menu->win,2,width+1);
	waddch(menu->win,ACS_RTEE);

	FREEA(tbuf);
}

/*
 * Create the menu from the previously registered items :
 */
cct_menu *
termcurs_show_menu(cct_menu *obj) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_curses_menu_item *p;
	cct_ushort count, ux;
	int wy, wx, ty, tlen;

	if ( !_this->hidden )
		return obj;				/* Menu is already showing */
	if ( _this->menu )
		goto post;				/* Go un-hide the menu */

	assert(!_this->menu);
	assert(!_this->win);
	assert(!_this->sub);

	/*
	 * Load additional items, if this is a dynamic module,
	 * but only do so once :
	 */
	if ( _this->module != 0 && !_this->items ) {
		/*
		 * This is a dynamic menu :
		 */
		cct_menu_module module = cob_resolve(_this->module);
		char name[33], desc[65];
		cct_ushort limit = !_this->item_limit ? _this->item_limit : ~0;
		int rc;

		cobcurses_trace_printf(0,"\nMenu Module was %s (0x%p)\n",
			module ? "resolved" : "NOT resolved",module);

		if ( !module )
			goto moderr;			/* The module did not resolve */

		rc = module("O",0,0);			/* Open the module and it's file(s) */
		cobcurses_trace_printf(0,"module('O',0,0) returned %d\n",rc);
		if ( rc )
			goto moderr;
			
		while ( !(rc = module("R",name,desc)) ) {	/* While it returns items */
			name[cobcurses_trim_trailing(name,32,' ')] = 0;
			desc[cobcurses_trim_trailing(desc,64,' ')] = 0;
			cobcurses_trace_printf(0,"module('R') returned { '%s', '%s' }\n",name,desc);
			termcurs_new_menu_item(obj,name,desc);
			if ( --limit < 1 )
				break;
		}
		cobcurses_trace_printf(0,"module('R') returned %d\n",rc);

		rc = module("C",0,0);				/* Close the module */
		cobcurses_trace_printf(0,"module('C') returned %d\n",rc);
moderr:		;
	}

	/*
	 * Must create the menu and its items first :
	 */
	for ( count=0, p = _this->head; p != 0; p = p->next )
		++count;

	cobcurses_trace_printf(0,"Found %u menu items\n",count);

	if ( count ) {
		attr_t a;
		cst_pair pr;

		_this->items = (ITEM **) malloc((count + 1) * sizeof (ITEM *));

		for ( ux=0, p = _this->head; p != 0; p = p->next, ++ux )
			_this->items[ux] = p->item;
		_this->items[count] = 0;

		_this->menu = new_menu(_this->items);
		assert(count == _this->nitems);

		set_menu_opts(_this->menu,_this->opts);

		if ( _this->maxrows > 0 ) {
			cct_ushort at = _this->title ? 2 : 0;

			for (;;) {
				set_menu_format(_this->menu,_this->maxrows,_this->maxcols);
				scale_menu(_this->menu,&wy,&wx);
				if ( wx > COLS && _this->maxcols > 1 )
					--_this->maxcols;
				if ( wy + at > LINES && _this->maxrows > 1 )
					--_this->maxrows;
				if ( wx <= COLS && wy + at <= LINES )
					break;
			}
		} else	scale_menu(_this->menu,&wy,&wx);
		
		if ( _this->title ) {
			if ( (tlen = strlen(_this->title)) > wx )
				wx = tlen + 2;		/* Add more width for the title */
			ty = 2;				/* 2 lines needed for title */
		} else	tlen = ty = 0;			/* No title */

		{
		int win_rows = wy + ty + 2;
		int win_cols = wx + 2;
		int sub_row_offset = 1 + ty;
		int begy = _this->y;
		int begx = _this->x;

		if ( begy + win_rows > LINES && (begy = LINES - win_rows) < 0 )
			begy = 0;
		if ( begx + win_cols > COLS && (begx = COLS - win_cols) < 0 )
			begx = 0;

		_this->win = newwin(win_rows,win_cols,begy,begx);
		keypad(_this->win,TRUE);
		_this->sub = derwin(_this->win,wy,wx,sub_row_offset,1);
		}

		set_menu_win(_this->menu,_this->win);
		set_menu_sub(_this->menu,_this->sub);
		set_menu_mark(_this->menu,">");

		wattr_get(_this->win,&a,&pr,0);	/* Save current attributes */

		wattr_set(_this->win,A_NORMAL,termcurs_conv_to_curses_colour(_this->tpair),0);
		box(_this->win,0,0);		/* Draw window box */
		draw_title(_this,0);		/* Draw title area (if there is one) */
		wattr_set(_this->win,a,pr,0);	/* Restore attributes */

		menu_opts_on(_this->menu,O_ONEVALUE|O_SHOWMATCH|O_IGNORECASE|O_SHOWDESC);
		menu_opts_off(_this->menu,O_NONCYCLIC);

		set_menu_fore(_this->menu,_this->foreground);
		set_menu_back(_this->menu,_this->background);
		set_menu_grey(_this->menu,_this->grey);

		assert(_this->hidden == T);
	}

post:	if ( _this->menu ) {
		set_menu_opts(_this->menu,_this->opts);
		post_menu(_this->menu);
		menu_driver(_this->menu,REQ_CLEAR_PATTERN);
		touchwin(_this->win);
		_this->hidden = F;
	}
	internal_refresh(_this);
	return obj;
}

/*
 * Delete the menu & menu object :
 */
cct_menu *
termcurs_delete_menu(cct_menu *obj) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_curses_menu_item *p, *nextp;

	termcurs_hide_menu(obj);
	if ( _this->menu ) {
		if ( _this->sub != 0 ) {
			delwin(_this->sub);
			_this->sub = 0;
		}
		if ( _this->win != 0 ) {
			delwin(_this->win);
			_this->win = 0;
		}
		free_menu(_this->menu);
		_this->menu = 0;
	}


	if ( _this->items ) {
		free(_this->items);
		_this->items = 0;
	}

	for ( p=_this->head; p; p = nextp ) {
		nextp = p->next;
		termcurs_delete_item(p);
	}
	_this->head = 0;

	free(_this);
	return 0;
}

/*
 * Update the title area :
 *
 * This is used when !O_SHOWDESC, and the description
 * can be placed into a title area (this can only happen
 * if a title area was reserved).
 */
static void
update_title(cct_curses_menu *menu) {
	int y, x;
	char *desc;

	if ( menu->title && !(menu->opts & cct_emo_SHOWDESC) ) {
		getyx(menu->win,y,x);
		desc = item_description(current_item(menu->menu));
		if ( !strncmp(desc,"- ",2) )
			desc += 2;
		draw_title(menu,desc);
		wmove(menu->win,y,x);
	}
}

/*
 * Get the menu selection :
 */
char *
termcurs_get_menu_selection(cct_menu *obj) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_key key;
	ITEM *item = 0;
	OPTIONS iopts = 0;
	WINDOW *svwin = _this->term->curses->win;	/* Save main window */
	char *retval = 0;				/* 0 or pointer to item name to be returned */

	if ( !_this->menu )
		return 0;				/* No selection */

	internal_refresh(_this);			/* Make sure all is visible */

	/*
	 * We temporarily give getkey() our menu window, since this window's
	 * content must remain on top. You'll recall that getkey() does
	 * a refresh of the main window, so we override it with the menu
	 * window here. 
	 */
	_this->term->curses->win = _this->win;		/* Fake out getkey() with menu window instead */

	while ( 1 ) {
		update_title(_this);
		wrefresh(_this->win);

		key = _this->term->api->getkey((cct_terminal *)_this->term);

		switch ( key ) {
		case CONTROL('N') :
		case cck_DOWN :
			menu_driver(_this->menu,REQ_DOWN_ITEM);
			break;
		case CONTROL('P') :
		case cck_UP :
			menu_driver(_this->menu,REQ_UP_ITEM);
			break;
		case CONTROL('V') :
		case cck_NPAGE :
			menu_driver(_this->menu,REQ_SCR_DPAGE);
			break;
		case CONTROL('Y') :
		case cck_PPAGE :
			menu_driver(_this->menu,REQ_SCR_UPAGE);
			break;
		case CONTROL('U') :
			menu_driver(_this->menu,REQ_SCR_ULINE);
			break;
		case CONTROL('D') :
			menu_driver(_this->menu,REQ_SCR_ULINE);
			break;
		case cck_LEFT :
			menu_driver(_this->menu,REQ_LEFT_ITEM);
			break;
		case cck_RIGHT :
			menu_driver(_this->menu,REQ_RIGHT_ITEM);
			break;
		case CONTROL('A') :
			menu_driver(_this->menu,REQ_CLEAR_PATTERN);
			break;
		case CONTROL('C') :
		case CONTROL('X') :
			menu_driver(_this->menu,REQ_NEXT_MATCH);
			break;
		case CONTROL('R') :
			menu_driver(_this->menu,REQ_PREV_MATCH);
			break;
		case CONTROL('T') :
		case ' ' :
			menu_driver(_this->menu,REQ_TOGGLE_ITEM);
			break;
		case CONTROL('H') :
		case cck_BACKSPACE :
			menu_driver(_this->menu,REQ_BACK_PATTERN);
			break;
		case cck_HOME :
			menu_driver(_this->menu,REQ_FIRST_ITEM);
			break;
		case cck_END :
			menu_driver(_this->menu,REQ_LAST_ITEM);
			break;
		case cck_ENTER :
			item = current_item(_this->menu);
			iopts = item_opts(item);
			if ( iopts & O_SELECTABLE ) {
				retval = item_userptr(item);
				goto xit;
			}
			beep();					/* Can't select this item */
			break;
		case 033 :					/* Escape key */
		case CONTROL('G') :				/* ^G to exit */
		case CONTROL('O') :
		case '.' :
			retval = 0;
			goto xit;				/* Escape: no selection */
		default :
			if ( key < cck_MIN )
				menu_driver(_this->menu,key);
		}
	}

xit:	_this->term->curses->win = svwin;			/* Restore main window */
	internal_refresh(_this);				/* Make sure terminal shows current state */

	if ( retval && !(_this->opts & cct_emo_ONEVALUE ) ) {
		/*
		 * This is potentially a multi-valued return :
		 */
		cct_strvar mv;
		cct_ushort ux;
		ITEM *item;

		dyn_str_init(&mv);
		dyn_str_assign(&mv,retval);			/* Add CR entry first */
		dyn_str_trim(&mv);

		for ( ux=0; ux < _this->nitems; ++ux ) {
			item = _this->items[ux];
			if ( item->selected && item->name != retval ) {
				dyn_str_append_ch(&mv,',');
				dyn_str_append(&mv,item->name);
				dyn_str_trim(&mv);
			}
		}
		retval = dyn_str_steal(&mv);			/* Return multi-valued result */
	}		

	return retval;						/* Return the selection */
}

/*
 * Set the foreground (selected items) of the menu :
 */
cct_menu *
termcurs_set_menu_foreground(cct_menu *obj,cct_termattr attr,cct_pair pair) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	chtype a = termcurs_conv_to_curses_attrs(attr);
	cst_pair pr = (cst_pair)pair;

	_this->foreground = a | COLOR_PAIR(pr);

	if ( _this->menu )
		set_menu_fore(_this->menu,_this->foreground);
	return obj;
}

/*
 * Set the terminal attributes for the background of the menu
 * (unselected menu items)
 */
cct_menu *
termcurs_set_menu_background(cct_menu *obj,cct_termattr attr,cct_pair pair) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	chtype a = termcurs_conv_to_curses_attrs(attr);
	cst_pair pr = (cst_pair)pair;

	_this->background = a | COLOR_PAIR(pr);

	if ( _this->menu )
		set_menu_back(_this->menu,_this->background);
	return obj;
}

/*
 * Set the terminal attributes for the greyed out menu item :
 */
cct_menu *
termcurs_set_menu_grey(cct_menu *obj,cct_termattr attr,cct_pair pair) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	chtype a = termcurs_conv_to_curses_attrs(attr);
	cst_pair pr = (cst_pair)pair;

	_this->grey = a | COLOR_PAIR(pr);

	if ( _this->menu )
		set_menu_grey(_this->menu,a);
	return obj;
}

/*
 * Set the named item selectable (or not) :
 * Returns the prior status of the item.
 */
cct_bool
termcurs_set_menu_item_sel(cct_menu *obj,const char *name,cct_bool selectable) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_curses_menu_item *p;
	OPTIONS opts = 0;

	for ( p=_this->head; p; p = p->next )
		if ( !strcmp(p->name,name) ) {
			opts = item_opts(p->item);
			if ( selectable )
				item_opts_on(p->item,O_SELECTABLE);
			else	item_opts_off(p->item,O_SELECTABLE);
			return opts & O_SELECTABLE ? T : F;
		}
	return F;
}

/*
 * Get the named item's selectability status :
 */
cct_bool
termcurs_get_menu_item_sel(cct_menu *obj,const char *name) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_curses_menu_item *p;

	for ( p=_this->head; p; p = p->next )
		if ( !strcmp(p->name,name) )
			return item_opts(p->item) & O_SELECTABLE ? T : F;
	return F;
}

/*
 * Configure a dynamic menu :
 *
 * The module name is the COBOL module that will be called to get the items
 * (usually from an indexed file). 
 *
 * The value item_limit is a safety limit, to avoid lockup when the entire
 * database is being returned.
 */
cct_menu *
termcurs_set_dynamic_menu(cct_menu *obj,const char *module,cct_ushort item_limit) {
	cct_curses_menu *_this = (cct_curses_menu *) obj;
	cct_strvar modname;
	
	assert(_this);

	if ( _this->module )
		free(_this->module);

	if ( module ) {
		dyn_str_init(&modname);
		dyn_str_assign(&modname,"COBCURSES-MENU-");
		dyn_str_append(&modname,module);
		dyn_str_trim(&modname);
		_this->module = dyn_str_steal(&modname);
	} else	_this->module = 0;

	_this->item_limit = item_limit;

	return obj;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses_menu.c,v $ */

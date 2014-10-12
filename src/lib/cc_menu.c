/* cc_menu.c : CobCurses replacement for curses menu facilities
 * Warren W. Gay VE3WWG
 * Mon Oct 15 14:06:34 2007
 * $Id: cc_menu.c,v 1.9 2007/10/25 20:11:37 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <term_curses.h>
#include <term_curses_conv.h>

#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>
#include <assert.h>
#include <errno.h>

#include <cc_menu.h>


static void calc_dimensions(MENU *menu,cct_ushort *rows,cct_ushort *cols);
static void redraw_items(MENU *menu);
static void redraw_item(ITEM *item);

/*
 * Allocate a menu item :
 */
ITEM *
new_item(const char *name,const char *desc) {
	ITEM *item = malloc(sizeof *item);

	memset(item,0,sizeof *item);
	item->name = strdup(name);
	item->desc = strdup(desc);
	item->opts = O_SELECTABLE;
	item->up = item->left = item->right = item->down = 0;
	item->selected = F;
	item->x = item->y = 0;
	item->userptr = 0;

	return item;
}

/*
 * Release the allocated item :
 */
int
free_item(ITEM *item) {

	if ( !item )
		return E_BAD_ARGUMENT;
	if ( item->menu != 0 )
		return E_CONNECTED;
	assert(item->name);
	assert(item->desc);

	free(item->name);
	free(item->desc);
	memset(item,0,sizeof *item);
	free(item);

	return E_OK;
}

/*
 * Create a new menu :
 */
MENU *
new_menu(ITEM **items) {
	ITEM **p;
	cct_ushort ux;

	if ( items != 0 && *items != 0 ) {
		MENU *menu = malloc(sizeof *menu);

		memset(menu,0,sizeof *menu);
		menu->opts = O_ONEVALUE | O_ROWMAJOR | O_IGNORECASE | O_SHOWDESC | /* O_NONCYCLIC | */ O_SHOWMATCH;
		menu->win = menu->sub = 0;	/* None set yet */
		menu->n_items = 0;		/* Changed later */
		menu->top_row = 0;		/* No page offset */
		menu->markstr = strdup(">");	/* Default mark string */
		menu->current = 0;		/* No current item yet */
		menu->start   = 0;
		menu->fg_attr = CCA_REVERSE;	/* Foreground attribute */
		menu->bg_attr = CCA_NORMAL;	/* Background attribute */
		menu->gr_attr = CCA_UNDERLINE;	/* Grey attribute */
		menu->max_namelen = menu->max_desclen = 0;
		menu->max_marklen = strlen(menu->markstr);
		menu->max_itemlen = 0;
		menu->patbuf = 0;
		menu->patx   = 0;
		menu->n_rows = 0;
		menu->n_cols = 1;
		menu->o_row  = 0;
		menu->cmpfunc = strncasecmp;	/* When O_IGNORECASEE is active */

		/* Count the # of items => n_items */
		for ( p = items; *p; ++p )
			++menu->n_items;

		/* Allocate and load the list of item refs => menu->items[] */
		menu->items = malloc(menu->n_items * sizeof *items);
		for ( ux=0, p = items; *p; ++p, ++ux ) {
			menu->items[ux] = *p;
			menu->items[ux]->menu = menu;
		}

		menu->current = menu->items[0];
		menu->status = cct_em_init;

		return menu;
	} else	{
		errno = E_NOT_CONNECTED;
		return 0;
	}
}

/*
 * Free a menu :
 */
int
free_menu(MENU *menu) {
	ITEM *p;
	cct_ushort ux;

	if ( !menu )
		return E_BAD_ARGUMENT;

	if ( menu->status == cct_em_posted )
		return E_POSTED;

	if ( !menu->items || menu->n_items < 1 )
		return E_BAD_ARGUMENT;

	if ( menu->items ) {
		/* Disconnect items from the menu */
		for ( ux=0; ux < menu->n_items; ++ux ) {
			p = menu->items[ux];
			p->menu = 0;
			p->up = p->down = p->left = p->right = 0;
		}

		menu->items = 0;
		menu->n_items = 0;
	}

	if ( menu->patbuf )
		free(menu->patbuf);

	memset(menu,0,sizeof *menu);
	free(menu);

	return E_OK;
}

/*
 * Establish the window for the menu :
 */
int
set_menu_win(MENU *menu, WINDOW *win) {

	if ( !menu || !win )
		return E_BAD_ARGUMENT;
	
	if ( menu->status != cct_em_init )
		return E_POSTED;

	if ( !menu->items )
		return E_NOT_CONNECTED;

	menu->win = win;
	return E_OK;
}

/*
 * Establish the subwindow for the menu :
 */
int
set_menu_sub(MENU *menu, WINDOW *sub) {

	if ( !menu || !sub )
		return E_BAD_ARGUMENT;

	if ( menu->status != cct_em_init )
		return E_POSTED;

	if ( !menu->items )
		return E_NOT_CONNECTED;
		
	menu->sub = sub;
	return E_OK;
}

/*
 * Return the mimimum width of the menu :
 */
int
scale_menu(MENU *menu, int *rows, int *columns) {
	cct_ushort lrows, lcols;

	if ( !menu )
		return E_BAD_ARGUMENT;

	if ( !menu->items )
		return E_NOT_CONNECTED;

	calc_dimensions(menu,&lrows,&lcols);
	if ( rows != 0 )
		*rows = (int) lrows;

	if ( columns != 0 )
		*columns = (int) lcols;
	return E_OK;
}

/*
 * Set a user ptr in an item :
 */
int
set_item_userptr(ITEM *item, void *userptr) {
	
	assert(item!=0);
	item->userptr = userptr;
	return E_OK;
}

/*
 * Return the item's userptr :
 */
void *
item_userptr(const ITEM *item) {

	assert(item!=0);
	return item->userptr;
}

/*
 * Return the current item :
 */
ITEM *
current_item(const MENU *menu) {

	assert(menu!=0);

	if ( !menu->items ) {
		errno = E_NOT_CONNECTED;
		return 0;
	}

	switch ( menu->status ) {
	case cct_em_init :
	case cct_em_unposted :
		errno = E_BAD_STATE;
		return 0;
	case cct_em_posted :
		return menu->current;
	default :
		assert(0==1);
	}

	return menu->current;
}

/*
 * Set item options :
 */
int
set_item_opts(ITEM *item, cct_item_opt opts) {
	
	assert(item!=0);
	item->opts = opts;

	if ( item->menu->status == cct_em_posted ) {
		redraw_item(item);
		touchwin(item->menu->win);
	}
	return E_OK;
}

/*
 * Turn on item options :
 */
int
item_opts_on(ITEM *item, cct_item_opt opts) {

	assert(item!=0);

	item->opts |= opts;
	if ( item->menu->status == cct_em_posted ) {
		redraw_item(item);
		touchwin(item->menu->win);
	}
	return E_OK;
}

/*
 * Turn off item options :
 */
int
item_opts_off(ITEM *item, cct_item_opt opts) {

	assert(item!=0);

	item->opts &= ~opts;
	if ( item->menu->status == cct_em_posted ) {
		redraw_item(item);
		touchwin(item->menu->win);
	}

	return E_OK;
}

/*
 * Return Item Options :
 */
cct_item_opt
item_opts(const ITEM *item) {

	assert(item!=0);
	return item->opts;
}

static void
win_move(MENU *menu,cct_ushort y,cct_ushort x) {
	int sy, sx;

	getparyx(menu->sub,sy,sx);
	wmove(menu->win,sy+y,sx+x);
}

/*
 * Draw str padded on the right to make exactly length bytes :
 */
void
draw_padded(WINDOW *win,const char *str,cct_ushort length) {
	cct_ushort slen = strlen(str);
	chtype ch;
	cct_ushort ux;

	if ( slen > length )
		slen = length;

	for ( ux=0; ux < length; ++ux ) {
		ch = termcurs_conv_to_curses_char(str[ux]);
		if ( ux < slen )
			waddch(win,ch);
		else	waddch(win,' ');
	}
}

/*
 * Draw a menu item to the subwindow :
 */
static void
draw_item(ITEM *item,cct_ushort item_offset,attr_t attr) {
	MENU *menu = item->menu;

	if ( menu->opts & O_ROWMAJOR )
		wmove(menu->sub,item_offset,item->x);
	else	wmove(menu->sub,item->y,item_offset + menu->max_marklen);

	wattrset(menu->sub,attr);
	draw_padded(menu->sub,item->name,menu->max_namelen);

	waddstr(menu->sub,A_NORMAL);
	waddch(menu->sub,' ');

	if ( menu->opts & O_SHOWDESC ) {
		wattrset(menu->sub,attr);
		draw_padded(menu->sub,item->desc,menu->max_desclen);
	}
}

static cct_bool
scroll_to(ITEM *item) {
	MENU *menu = item->menu;
	cct_ushort item_offset, my, mx;
	cct_ushort  menu_offset = menu->o_row;

	getmaxyx(menu->sub,my,mx);			/* Get subwindow size (not max y,x)  */

	if ( menu->opts & O_ROWMAJOR ) {
		if ( item->y >= menu_offset && (item_offset = item->y - menu_offset) < my && item->x < mx )
			return F;				/* This is already displayable */
		if ( my > 1 && item->y > 0 )
			menu_offset = item->y - 1;		/* Put item 2nd on the list */
		else 	menu_offset = item->y;			/* Put item at the top of the list */
	} else	{
		if ( item->x >= menu_offset && (item_offset = item->x - menu_offset) < mx && item->y < my )
			return F;				/* This is already displayable */
		menu_offset = item->x - menu->max_marklen;
	}

	menu->o_row = menu_offset;

	/*
	 * Scrolling required :
	 */
	redraw_items(menu);			/* Redraw menu */

	return T;
}

/*
 * Scroll up/down a page :
 */
static void
scroll_page(MENU *menu,int dir) {
	cct_ushort menu_offset = menu->o_row;		/* Menu offset */
	ITEM *item = menu->current;			/* The current item */
	cct_ushort my, mx;				/* Subwindow's size */
	cct_ushort adv;					/* How many items to advance */
	cct_ushort x, y;				/* Item coordinates */

	getmaxyx(menu->sub,my,mx);			/* Get subwindow size */

	if ( menu->opts & O_ROWMAJOR ) {
		adv = my;				/* Advance based upon # of rows */
		if ( dir >= 0 )
			for ( x = item->x; item->down->x == x && item->down->y > item->y && adv > 0; item = item->down, --adv );
		else	for ( x = item->x; item->up->x   == x && item->up->y   < item->y && adv > 0; item = item->up, --adv );
	} else	{
		adv = menu->n_cols;			/* Advance based upon # of columns */
		if ( dir >= 0 )
			for ( y = item->y; item->right->y == y && item->right->x > item->x && adv > 0; item = item->right, --adv );
		else	for ( y = item->y; item->left->y  == y && item->left->x  < item->x && adv > 0; item = item->left, --adv );
	}

	menu->current = item;				/* Make this the current item */
	if ( menu->opts & O_ROWMAJOR )
		menu_offset = item->y;			/* Calculate new y offset */
	else	menu_offset = item->x - menu->max_marklen; /* else x offset */

	menu->o_row = menu_offset;			/* Update offset in MENU object */
	redraw_items(menu);				/* Redraw subwindow */
}

/*
 * Home the cursor in the subwindow, but actually
 * show cursor movement (in the parent window):
 */
static void
home_cursor(MENU *menu) {
	int sy, sx;

	getparyx(menu->sub,sy,sx);
	wmove(menu->win,sy,sx);
}

/*
 * Draw n blanks in the current attribute :
 */
static void
draw_blanks(WINDOW *win,cct_ushort n) {

	for ( ; n > 0; --n )
		waddch(win,' ');
}

/*
 * Draw the mark in front of the item :
 */
static void
draw_mark(ITEM *item,cct_ushort item_offset) {
	MENU *menu = item->menu;

	wattrset(menu->sub,A_NORMAL);

	if ( menu->opts & O_ROWMAJOR ) {
		wmove(menu->sub,item_offset,(int)(item->x - menu->max_marklen));
		if ( item == menu->current ) {
			waddstr(menu->sub,menu->markstr);
			win_move(menu,item_offset,menu->patx + 1 + item->x - menu->max_marklen);
		} else
			draw_blanks(menu->sub,menu->max_marklen);
	} else	{
		wmove(menu->sub,item->y,item_offset);
		if ( item == menu->current ) {
			waddstr(menu->sub,menu->markstr);
			win_move(menu,item->y,item_offset + menu->patx + 1);
		} else
			draw_blanks(menu->sub,menu->max_marklen);
	}
}

/*
 * Redraw a menu item :
 *
 * The menu's coordinate is already calculated in (item->y, item->x),
 * but the menu's current offset (menu->o_row) must be taken into account.
 * Note that for !O_ROWMAJOR, menu->o_row is actually a column offset.
 * Here we copy menu->o_row into local var menu_offset for greater
 * clarity.
 */
static void
redraw_item(ITEM *item) {
	cct_bool sel =						/* T if the item is selected (or current) */
		( item->menu->current == item || item->selected ) ? T : F;
	MENU *menu = item->menu;				/* The menu that the item is connected to */
	cct_ushort my, mx;					/* Subwindows size in row, cols */
	cct_ushort item_offset;					/* Item's row/col offset */
	cct_ushort menu_offset = menu->o_row;			/* Menu's row/col offset for paging */
	attr_t a;						/* The drawing attribute to use for item */

	if ( !(item->opts & O_SELECTABLE ) )			/* Is the item selectable? */
		a = menu->gr_attr;				/* Not: use grey attribute */
	else	a = sel ? menu->fg_attr : menu->bg_attr;	/* Use fg attr if selected else bg */

	getmaxyx(menu->sub,my,mx);				/* Get subwindow size */

	if ( menu->opts & O_ROWMAJOR ) {
		/*
		 * Here items go across (in order), but paging goes
		 * up and down within the subwindow :
		 */
		if ( item->y >= menu_offset && (item_offset = item->y - menu_offset) < my && item->x < mx ) {
			draw_item(item,item_offset,a);		/* This item is visible */
			draw_mark(item,item_offset);
		} else if ( item == menu->current || menu->current->y < menu_offset)
			home_cursor(menu);			/* The item is not displayable */
	} else	{
		/*
		 * !O_ROWMAJOR :
		 *
		 * Here items go down (in order), but paging goes
		 * left to right within the subwindow.
		 */
		if ( item->x >= menu_offset && (item_offset = item->x - menu_offset - menu->max_marklen) < mx && item->y < my ) {
			draw_item(item,item_offset,a);		/* This item is visible */
			draw_mark(item,item_offset);
		} else if ( item == menu->current || menu->current->x < menu_offset)
			home_cursor(menu);			/* The item is not displayable */
	}
}

/*
 * Redraw all visible items on the current menu's subwindow :
 */
static void
redraw_items(MENU *menu) {
	cct_ushort ux;

	wclear(menu->sub);
	for ( ux=0; ux<menu->n_items; ++ux )
		redraw_item(menu->items[ux]);
	touchwin(menu->win);
}

/*
 * Calculate the menu's row x col requirements :
 */
static void
calc_dimensions(MENU *menu,cct_ushort *rows,cct_ushort *cols) {
	cct_ushort ux, nlen, dlen, len;
	ITEM *item;
	cct_ushort n_cols = menu->n_cols <= 1 ? 1 : menu->n_cols;
	cct_ushort n_rows = menu->n_rows < 1 ? ( menu->n_items - 1 )/ n_cols + 1 : menu->n_rows;
	cct_ushort r_rows, r_cols;

	/*
	 * Calculate the size of names and descriptions :
	 */
	for ( ux=nlen=dlen=0; ux < menu->n_items; ++ux ) {
		item = menu->items[ux];
		if ( nlen < (len = strlen(item->name)) )
			nlen = len;
		if ( dlen < (len = strlen(item->desc)) )
			dlen = len;
	}
	menu->max_marklen = strlen(menu->markstr);
	menu->max_namelen = nlen;
	menu->max_desclen = dlen;
	menu->max_itemlen = 1 + menu->max_marklen + menu->max_namelen + 1
		+ ( menu->opts & O_SHOWDESC ? menu->max_desclen : 0 );

	/*
	 * Calculate row/column dimenions of the menu :
	 */
	r_rows = ( menu->n_items - 1 ) / n_cols + 1;
	if ( menu->opts & O_ROWMAJOR )
		r_cols = minimum(menu->n_items,n_cols);
	else
		r_cols = ( menu->n_items - 1 ) / r_rows + 1;

	r_rows = minimum(r_rows,n_rows);
	r_cols = minimum(r_cols,n_cols) * menu->max_itemlen;

	if ( rows )
		*rows = r_rows;
	if ( cols )
		*cols = r_cols;
}

/*
 * Post a menu :
 */
int
post_menu(MENU *menu) {

	assert(menu!=0);
	assert(menu->win!=0);
	assert(menu->sub!=0);

	if ( !menu->items )
		return E_BAD_STATE;	/* No menu items */

	switch ( menu->status ) {
	case cct_em_init :
	case cct_em_unposted :
		break;
	case cct_em_posted :
		return E_POSTED;
	default :
		assert(1==0);
	}

	if ( !menu->patbuf ) {
		menu->patbuf = malloc(menu->max_namelen+1);

		if ( !menu->n_rows )
			menu->n_rows = menu->n_items;

	}
	menu->patx   = 0;

	scale_menu(menu,0,0);

	/*
	 * Check menu dimenions :
	 */
	{
		cct_ushort maxcols, maxrows;

		getmaxyx(menu->sub,maxrows,maxcols);

		/*
		 * Adjust if the subwindow is too small :
		 */
		if ( menu->n_cols * menu->max_itemlen > maxcols ) {
			if ( !(menu->n_cols = maxcols / menu->max_itemlen) )
				menu->n_cols = 1;
		}
		if ( menu->n_rows > maxrows )
			menu->n_rows = maxrows;

		/*
		 * Now fix the item pointers :
		 */
		if ( menu->opts & O_ROWMAJOR ) {
			cct_ushort uy, ux, uz;
			ITEM *item, *first, *last;

			first = menu->items[0];
			last  = menu->items[menu->n_items-1];
			for ( uz = uy = 0; uz < menu->n_items; ++uy ) {
				for ( ux = 0; ux < menu->n_cols && uz < menu->n_items; ++ux, ++uz ) {
					item = menu->items[uz];
					item->y = uy;
					item->x = menu->max_marklen + ux * menu->max_itemlen;

					item->right = uz + 1 < menu->n_items ? menu->items[uz + 1] : first;
					item->left  = uz > 0 ? menu->items[uz - 1] : last;

					item->up    = uz >= menu->n_cols ? menu->items[uz-menu->n_cols] 
							: uz + menu->n_items / menu->n_cols * menu->n_cols < menu->n_items
								? menu->items[uz + menu->n_items / menu->n_cols * menu->n_cols]
								: menu->items[uz + menu->n_items / menu->n_cols * menu->n_cols - menu->n_cols];
					item->down  = uz + menu->n_cols < menu->n_items ? menu->items[uz + menu->n_cols]
							: ux < menu->n_items ? menu->items[ux] : first;
				}
			}
		} else	{
			cct_ushort uy, ux, uz;
			ITEM *item, *first, *last;

			first = menu->items[0];
			last  = menu->items[menu->n_items-1];
			for ( uz = ux = 0; uz < menu->n_items; ++ux )
				for ( uy = 0; uy < menu->n_rows && uz < menu->n_items; ++uy, ++uz ) {
					item = menu->items[uz];
					item->y = uy;
					item->x = menu->max_marklen + ux * menu->max_itemlen;

					item->down = uz + 1 < menu->n_items ? menu->items[uz + 1] : first;
					item->up   = uz > 0 ? menu->items[uz - 1] : last;

					item->left    = uz >= menu->n_rows ? menu->items[uz-menu->n_rows] : last;
					item->right   = uz + menu->n_rows < menu->n_items ? menu->items[uz + menu->n_rows]
                                                       	: uy < menu->n_items ? menu->items[uy] : first;
				}
		}
	}

	/*
	 * Draw the items :
	 */
	redraw_items(menu);
	menu->status = cct_em_posted;

	return E_OK;
}

/*
 * Unpost a menu :
 */
int
unpost_menu(MENU *menu) {

	assert(menu!=0);
	assert(menu->items!=0);

	switch ( menu->status ) {
	case cct_em_init :
	case cct_em_unposted :
		return E_NOT_POSTED;
	case cct_em_posted :
		break;
	default :
		assert(1==0);
	}

	assert(menu->win);
	assert(menu->sub);

	wclear(menu->sub);
	touchwin(menu->sub);
	menu->status = cct_em_unposted;

	return E_OK;
}

/*
 * Configure the menu mark string :
 */
int
set_menu_mark(MENU *menu, const char *mark) {

	assert(menu);
	assert(menu->markstr);
	assert(menu->status==cct_em_init);

	if ( strcmp(menu->markstr,mark) != 0 ) {
		free(menu->markstr);
		menu->markstr = strdup(mark);
		menu->max_marklen = strlen(menu->markstr);
	}
	return E_OK;
}

int
set_menu_fore(MENU *menu, attr_t attr) {

	assert(menu);
	menu->fg_attr = attr;
	return E_OK;
}

int
set_menu_back(MENU *menu, attr_t attr) {

	assert(menu);
	menu->bg_attr = attr;
	return E_OK;
}

int
set_menu_grey(MENU *menu, attr_t attr) {

	assert(menu);
	menu->gr_attr = attr;
	return E_OK;
}

int
set_menu_opts(MENU *menu, cct_menu_opt opts) {
	assert(menu);
	
	menu->opts = opts;
	menu->cmpfunc = menu->opts & O_IGNORECASE ? strncasecmp : strncmp;

	return E_OK;
}

int
menu_opts_on(MENU *menu, cct_menu_opt opts) {
	assert(menu);
	assert(menu->status != cct_em_posted);

	menu->opts |= opts;
	return E_OK;
}

int
menu_opts_off(MENU *menu, cct_menu_opt opts) {
	assert(menu);
	assert(menu->status != cct_em_posted);

	menu->opts &= ~opts;
	return E_OK;
}

cct_menu_opt
menu_opts(const MENU *menu) {
	assert(menu);
	return menu->opts;
}

static ITEM *
next_item(ITEM *item,cct_bool next) {
	if ( item->menu->opts & O_ROWMAJOR )
		return next ? item->right : item->left;
	else	return next ? item->down  : item->up;
}

static cct_bool
pattern_next_match(MENU *menu,cct_bool next) {
	cct_ushort ux;	
	ITEM *item = menu->current;

	if ( next )
		item = next_item(item,T);

	for ( ux=0; ux<menu->n_items; ++ux ) {
		if ( !menu->cmpfunc(item->name,menu->patbuf,menu->patx) ) {
			menu->current = item;
			return T;
		}
		item = next_item(item,T);
	}
	return F;
}

static cct_bool
pattern_add_search(MENU *menu,char ch) {
	cct_ushort x = menu->patx;

	if ( !menu->patx )
		menu->start = menu->current;

	if ( x < menu->max_namelen ) {
		menu->patbuf[x++] = ch;
		++menu->patx;

		if ( pattern_next_match(menu,F) )
			return T;	/* Pattern match was found */
		--menu->patx;		/* Pattern not found */
	}
	return F;
}

static cct_bool
pattern_back_match(MENU *menu,cct_bool next) {
	ITEM *item = menu->current;
	cct_ushort ux;

	if ( next )
		item = next_item(item,F);
	for ( ux=0; ux<menu->n_items; ++ux ) {
		if ( !menu->cmpfunc(item->name,menu->patbuf,menu->patx) ) {
			menu->current = item;
			return T;
		}
		item = next_item(item,F);
	}
	return F;
}

static cct_bool
pattern_back(MENU *menu) {

	if ( menu->patx < 1 )
		return F;

	if ( --menu->patx < 1 ) {
		if ( menu->start )
			menu->current = menu->start;
		return T;
	}

	return pattern_back_match(menu,F);
}

/*
 * The menu driver itself :
 */
int
menu_driver(MENU *menu, int ch) {
	ITEM *item;
	int rc = E_OK;

	assert(menu);
	assert(menu->status == cct_em_posted);
	assert(menu->items);
	assert(menu->current);

	item = menu->current;

	if ( ch > KEY_MAX && ch <= MAX_MENU_COMMAND ) {
		switch ( ch ) {
		case REQ_NEXT_ITEM :
			if ( menu->opts & O_ROWMAJOR )
				ch = REQ_RIGHT_ITEM;
			else	ch = REQ_DOWN_ITEM;
			break;
		case REQ_PREV_ITEM :
			if ( menu->opts & O_ROWMAJOR )
				ch = REQ_LEFT_ITEM;
			else	ch = REQ_UP_ITEM;
			break;
		default :
			;
		}

		/* Menu command */
		switch ( ch ) {
		case REQ_UP_ITEM :
			menu->current = menu->current->up;
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_DOWN_ITEM :
			menu->current = menu->current->down;
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_FIRST_ITEM :
			menu->current = menu->items[0];
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_LAST_ITEM :
			menu->current = menu->items[menu->n_items-1];
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_LEFT_ITEM :
			menu->current = menu->current->left;
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_RIGHT_ITEM :
			menu->current = menu->current->right;
			menu->patx = 0;
			scroll_to(menu->current);
			break;
		case REQ_TOGGLE_ITEM :
			if ( !(menu->opts & O_ONEVALUE) ) {
				menu->current->selected ^= T;
				redraw_item(menu->current);
				touchwin(menu->win);
			}
			break;
		case REQ_SCR_UPAGE :
			scroll_page(menu,-1);
			break;
		case REQ_SCR_DPAGE :
			scroll_page(menu,1);
			break;
		case REQ_NEXT_MATCH :
			if ( pattern_next_match(menu,T) ) {
				scroll_to(menu->current);
				redraw_item(menu->current);
				touchwin(menu->win);
			}
			break;
		case REQ_PREV_MATCH :
			if ( pattern_back_match(menu,T) ) {
				scroll_to(menu->current);
				redraw_item(menu->current);
				touchwin(menu->win);
			}
			break;
		case REQ_BACK_PATTERN :
			if ( pattern_back(menu) ) {
				scroll_to(menu->current);
				redraw_item(menu->current);
			}
			break;
		case REQ_CLEAR_PATTERN :
			menu->patx = 0;
			redraw_item(menu->current);
			touchwin(menu->win);
			break;
		case REQ_SCR_ULINE :
			if ( menu->o_row > 0 ) {
				--menu->o_row;
				redraw_items(menu);
			}
			break;
		case REQ_SCR_DLINE :
			if ( menu->o_row + 1 < menu->n_items ) {
				++menu->o_row;
				redraw_items(menu);
			}
			break;
		default :
			rc = E_UNKNOWN_COMMAND;		
		}
		if ( menu->current != item ) {
			redraw_item(item);
			redraw_item(menu->current);
			touchwin(menu->win);
		}
	} else if ( ch == KEY_MOUSE ) {
		/* Mouse event */
		rc = E_REQUEST_DENIED;		/* FIX ME */
	} else	{
		/* Probably a regular character */
		if ( ch <= KEY_MIN && ( isprint(ch) || ch == ' ' ) ) {
			if ( !pattern_add_search(menu,ch) )
				rc = E_NO_MATCH;
			else	scroll_to(menu->current);
			if ( rc != E_NO_MATCH ) {
				redraw_item(item);
				redraw_item(menu->current);
			}
			touchwin(menu->win);
		} else	rc = E_NO_MATCH;
	}

	return rc;
}

int
set_menu_format(MENU *menu, int rows, int cols) {

	assert(menu);
	assert(rows > 0);
	assert(cols > 0);
	assert(menu->status == cct_em_init);

	menu->n_rows = rows;
	menu->n_cols = cols;
	return E_OK;
}

void
menu_format(const MENU *menu, int *rows, int *cols) {

	assert(menu);

	*rows = menu->n_rows;
	*cols = menu->n_cols;
}

/*
 * Return the item's name :
 */
char *
item_name(const ITEM *item) {

	assert(item);
	return item->name;
}

/*
 * Return the item's description :
 */
char *
item_description(const ITEM *item) {

	assert(item);
	return item->desc;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/cc_menu.c,v $ */

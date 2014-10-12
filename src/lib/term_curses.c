/* term_curses.c : Curses module for terminal I/O
 * Warren W. Gay VE3WWG
 * Thu Sep  6 16:25:57 2007
 * $Id: term_curses.c,v 1.13 2007/10/29 14:56:15 ve3wwg Exp $
 */
#include <stdlib.h>
#include <memory.h>
#include <assert.h>

#include <cobcurses.h>
#include <terminal.h>
#include <term_curses.h>
#include <term_curses_menu.h>

static cct_api_terminal *termcurs_init_api(void);	/* Initializes the api struct */

static cct_api_terminal api = { 0, 0 };			/* The api struct */
static cct_api_terminal *super = 0;			/* Base class methods */
static cct_bool is_open = F;				/* There can only be 1 initscr() */

/*
 * Internal routine to get mouse event :
 */
#ifdef COBCURSES_MOUSE_SUPPORT
static void
internal_get_mouse_event(cct_termcurs *_this) {
	MEVENT m;

	if ( getmouse(&m) == OK ) {
		termcurs_conv_to_term_mevent(&_this->curses->mevent,&m);
		_this->curses->have_mevent = T;
	} else
		_this->curses->have_mevent = F;
	return;
}
#endif

/*
 * Initialize a pre-allocated cct_termcurs object :
 */
cct_terminal *
termcurs_init(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	if ( !super )
		super = obj->api;		/* Grab parent API methods */

	_this->id  = id_termcurs;
	_this->api = termcurs_init_api();

	assert(!_this->curses);

	_this->curses = malloc(sizeof *_this->curses);
	_this->curses->open = F;			/* Mark as not open */
	_this->curses->win = 0;				/* No curses window yet */
	_this->curses->mmask = 0;			/* Turn off all events initially */
	_this->curses->have_mevent = F;			/* No pending mouse event */

	return obj;
}

/*
 * Dispose of a cct_termcurs object :
 */
static
cct_terminal *
termcurs_dispose(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(_this->id==id_termcurs);

	if ( _this->curses->open )
		_this->api->close(obj);

	if ( _this->curses ) {
		free(_this->curses);
		_this->curses = 0;
	}

	return super->dispose(obj);
}

/*
 * Free an allocated cct_termcurs object :
 */
static
cct_terminal *
termcurs_delete(cct_terminal *obj) {
	DEFNTHIS(obj);

	assert(_this->id==id_termcurs);
	termcurs_dispose(obj);			/* Dispose of this object */
	return super->_delete(obj);		/* Housekeeping */
}

/*
 * Open the curses window stdscr :
 */
static
cct_terminal *
termcurs_open(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(_this->id==id_termcurs);
	assert(Is_False(is_open));	/* There can only be one initscr() outstanding */
	assert(Is_False(_this->curses->open));	/* Don't do this twice */

	initscr();
	_this->curses->win = stdscr;		/* Use the main screen for this */

	if ( has_colors() ) {
		start_color();		/* Crank up colour support */
		if ( can_change_color() && COLORS > 0 ) {
			short full = 850; /* Full, but not "bold" */

			/* Max brightness is 1000, 0 is black */
			init_color(COLOR_BLACK,0,0,0);
			init_color(COLOR_GREEN,100,full,0);
			init_color(COLOR_RED,full,0,0);
			init_color(COLOR_BLUE,300,300,full);
			init_color(COLOR_CYAN,0,full,full);
			init_color(COLOR_MAGENTA,full,0,full);
			init_color(COLOR_YELLOW,full,full,0);
			init_color(COLOR_WHITE,full,full,full);
		}
	}

	intrflush(_this->curses->win,FALSE);	/* Don't flush screen changes */
	keypad(_this->curses->win,TRUE);	/* Enable keypad processing */
	noecho();
	noraw();
	cbreak();
	nonl();
	def_prog_mode();			/* Save tty modes */

#ifdef COBCURSES_MOUSE_SUPPORT
	mousemask(0,&_this->curses->mmask_save);	/* Save value to restore upon close */
	_this->curses->mmask = 0;			/* Mark as requiring no mouse events */
	_this->curses->mouse_click_ms = (cct_mouse_ival) mouseinterval(-1);
#endif

	is_open = _this->curses->open = T;		/* Mark as initscr() issued */
	return obj;
}

/*
 * Terminate curses support :
 */
static
cct_terminal *
termcurs_close(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(_this->id==id_termcurs);
	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));

#ifdef COBCURSES_MOUSE_SUPPORT	
	mousemask(_this->curses->mmask_save,0);	/* Restore original mouse mask for xterm/whatever */
#endif
	_this->api->flush(obj);			/* Do final screen refresh */
	_this->curses->win = 0;			/* Forget we had a window */
	endwin();				/* Close curses window support */
	is_open = _this->curses->open = F;	/* Mark as closed */

	return obj;
}

/*
 * Flush out unwritten changes to terminal :
 */
static
cct_terminal *
termcurs_flush(cct_terminal *obj) {
	DEFNTHIS(obj);

	assert(_this->id==id_termcurs);
	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));

	wrefresh(_this->curses->win);
	return obj;
}

/*
 * Write 1 character to the open terminal :
 */
static
cct_terminal *
termcurs_putch(cct_terminal *obj,char ch) {
	DEFNTHIS(obj);
	chtype c;

	assert(_this->id==id_termcurs);
	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));

	if ( !IsSpecialChar(ch) )
		c = (chtype)ch;
	else	c = termcurs_conv_to_curses_char(ch);
	waddch(_this->curses->win,c);

	return obj;
}

/*
 * Write text to the open terminal :
 */
static
cct_terminal *
termcurs_putsn(cct_terminal *obj,const char *text,cct_ushort n) {
	DEFNTHIS(obj);
	char ch;
	chtype c;

	assert(_this->id==id_termcurs);
	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));

	for ( ; n > 0; --n ) {
		ch = *text++;
		if ( !IsSpecialChar(ch) )
			c = (chtype)ch;
		else	c = termcurs_conv_to_curses_char(ch);
		waddch(_this->curses->win,c);
	}

	return obj;
}

/*
 * Write text to the open terminal :
 */
static
cct_terminal *
termcurs_puts(cct_terminal *obj,const char *text) {
	return termcurs_putsn(obj,text,strlen(text));
}

/*
 * Get a terminal keystroke or function key :
 */
static
cct_key
termcurs_getkey(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cct_key key;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	_this->api->flush(obj);	
	key = termcurs_conv_to_term_key(wgetch(_this->curses->win));

	switch ( key ) {
	case CONTROL('H') :
		key = KEY_BACKSPACE;
		break;
	case '\n' :
	case '\r' :
		key = cck_ENTER;
		break;
	default :
		break;
	}		

#ifdef COBCURSES_MOUSE_SUPPORT
	if ( key == cck_MOUSE && !_this->curses->have_mevent )
		internal_get_mouse_event(_this);	/* Stow current mouse event info */
#endif
	return key;
}

/*
 * Return T if we have underline capability :
 */
static
cct_bool
termcurs_has_underline_cap(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);
	
	{
		chtype attrs = termattrs();		/* Supported attributes */
		return ( attrs & A_UNDERLINE ) ? T : F;
	}
}

/*
 * Return the number of lines :
 */
static
cct_ushort
termcurs_lines(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return (cct_ushort)LINES;
}

/*
 * Return the number of columns :
 */
static
cct_ushort
termcurs_columns(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return (cct_ushort)COLS;
}

static
cct_bool
termcurs_has_colour_cap(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return has_colors() ? T : F;
}

static
cct_bool
termcurs_colour_pairs(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return termcurs_has_colour_cap(obj) ? COLOR_PAIRS : 0;
}

/*
 * Return the y,x coordinates :
 */
static
cct_ushort
termcurs_getyx(cct_terminal *obj,cct_ushort *py) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	int y, x;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	getyx(_this->curses->win,y,x);
	if ( py )
		*py = (cct_ushort) y;
	return (cct_ushort) x;
}

/*
 * Return T if the terminal can change its colours :
 */
static
cct_bool
termcurs_has_change_colour_cap(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return can_change_color() ? T : F;
}

/*
 * Return attribute capabilities :
 */
static
cct_terminal *
termcurs_cap_attr(cct_terminal *obj,cct_termattr *attrs) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	attr_t a;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#if HAVE_TERM_ATTRS
	a = term_attrs();
#else
	a = (attr_t) termattrs();
#endif
#ifdef BUILD_MINGW
	a &= ~((attr_t)A_UNDERLINE);		/* No underline for DOS console window */
#endif
	*attrs = termcurs_conv_to_term_attrs(a);
	return obj;
}

/*
 * Get current attributes :
 */
static
cct_terminal *
termcurs_get_attr(cct_terminal *obj,cct_termattr *attrs) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cst_pair pair;
	attr_t a;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wattr_get(_this->curses->win,&a,&pair,0);
	*attrs = termcurs_conv_to_term_attrs(a);
	return obj;
}

/*
 * Get current attributes & colour pair :
 */
static
cct_terminal *
termcurs_get_attr_and_pair(cct_terminal *obj,cct_termattr *attrs,cct_pair *pair) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cst_pair pr;
	attr_t a;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wattr_get(_this->curses->win,&a,&pr,0);
	*attrs = termcurs_conv_to_term_attrs(a);
	*pair = (cct_pair)pr;
	return obj;
}

/*
 * Set new attributes :
 */
static
cct_terminal *
termcurs_set_attr(cct_terminal *obj,const cct_termattr attrs) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	attr_t a;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	a = termcurs_conv_to_curses_attrs(attrs);
	wattrset(_this->curses->win,(int)a);
	return obj;
}

/*
 * Set new attributes & colour pair :
 */
static
cct_terminal *
termcurs_set_attr_and_pair(cct_terminal *obj,const cct_termattr attrs,cct_pair pair) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	attr_t a;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	a = termcurs_conv_to_curses_attrs(attrs);
	wattr_set(_this->curses->win,a,(cst_pair)pair,0);
	return obj;
}

/*
 * Move to location (y,x) on screen :
 */
static
cct_terminal *
termcurs_move(cct_terminal *obj,cct_ushort y,cct_ushort x) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wmove(_this->curses->win,(int)y,(int)x);

	return obj;
}

/*
 * Clear screen :
 */
static
cct_terminal *
termcurs_clear(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wclear(_this->curses->win);

	return obj;
}

/*
 * Clear to end of line :
 */
static
cct_terminal *
termcurs_clrtoeol(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wclrtoeol(_this->curses->win);

	return obj;
}

/*
 * Clear to bottom of screen :
 */
static
cct_terminal *
termcurs_clrtobot(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wclrtobot(_this->curses->win);

	return obj;
}

/*
 * Beep
 */
static
cct_terminal *
termcurs_beep(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	beep();

	return obj;
}

/*
 * Initialize a colour pair :
 */
static
cct_terminal *
termcurs_init_pair(cct_terminal *obj,cct_colour foreground,cct_colour background,cct_pair pair) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cst_colour fg, bg;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	fg = termcurs_conv_to_curses_colour(foreground);
	bg = termcurs_conv_to_curses_colour(background);
	init_pair(pair,fg,bg);
	return obj;
}

/*
 * Set the colour pair to use :
 */
static
cct_terminal *
termcurs_set_pair(cct_terminal *obj,cct_pair pair) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	wcolor_set(_this->curses->win,(cst_pair)pair,0);
	return obj;
}

/*
 * Draw a horizontal line :
 */
static
cct_terminal *
termcurs_hline(cct_terminal *obj,char ch,cct_ushort n) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	chtype ich;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	ich = termcurs_conv_to_curses_char(ch);
	whline(_this->curses->win,ich,(int)n);
	return obj;
}

/*
 * Draw a vertical line :
 */
static
cct_terminal *
termcurs_vline(cct_terminal *obj,char ch,cct_ushort n) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	chtype ich;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	ich = termcurs_conv_to_curses_char(ch);
	wvline(_this->curses->win,ich,(int)n);
	return obj;
}

/*
 * Suspend curses to allow the caller to call system() :
 */
static
cct_terminal *
termcurs_suspend(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	def_prog_mode();				/* Save tty settings */
	endwin();					/* Suspend curses operation */
	return obj;
}

/*
 * Resume curses after the caller has called system() :
 */
static
cct_terminal *
termcurs_resume(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	reset_prog_mode();			/* Restore tty settings */
	refresh();				/* Refresh terminal */
	return obj;
}

/*
 * Return T if we have mouse support :
 */
static
cct_bool
termcurs_have_mouse(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#ifdef COBCURSES_MOUSE_SUPPORT
	return T;
#else
	return F;
#endif
}

/*
 * Set the mouse mask for events :
 */
static
cct_mmask
termcurs_set_mouse_mask(cct_terminal *obj,cct_mmask mmask) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cct_mmask old_mmask = _this->curses->mmask;
#ifdef COBCURSES_MOUSE_SUPPORT
	mmask_t new_mmask;
#endif
	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#ifdef COBCURSES_MOUSE_SUPPORT
	_this->curses->mmask = mmask;			/* Save new mask in our object */
	new_mmask = termcurs_conv_to_curses_mmask(mmask); /* Convert to curses(3X) bit mask */
	mousemask(new_mmask,0);				/* Establish settings withing curses */
#endif
	return old_mmask;				/* Return original bit mask */
}

/*
 * Get the mouse mask for events :
 */
static
cct_mmask
termcurs_get_mouse_mask(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return _this->curses->mmask;
}

/*
 * Return T if we have a pending mouse event :
 */
static
cct_bool
termcurs_have_mouse_event(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#ifdef COBCURSES_MOUSE_SUPPORT
	if ( !_this->curses->have_mevent )		/* No pending event? */
		internal_get_mouse_event(_this);	/* Poll for pending data */
#endif
	return _this->curses->have_mevent;		/* T if mouse event is pending */
}

/*
 * Return mouse event info :
 */
static
cct_mevent *
termcurs_get_mouse_event(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#ifdef COBCURSES_MOUSE_SUPPORT
	if ( !_this->curses->have_mevent )
		internal_get_mouse_event(_this);

	if ( _this->curses->have_mevent )
		return &_this->curses->mevent;
#endif
	return 0;
}

/*
 * Clear last mouse event info :
 */
static
cct_terminal *
termcurs_clear_mouse_event(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	_this->curses->have_mevent = 0;
	return obj;
}

/*
 * Get mouse click interval :
 */
static
cct_mouse_ival
termcurs_get_mouse_ival(cct_terminal *obj) {
	cct_termcurs *_this = (cct_termcurs *)obj;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

	return _this->curses->mouse_click_ms;
}

/*
 * Set mouse click interval :
 */
static
cct_mouse_ival
termcurs_set_mouse_ival(cct_terminal *obj,cct_mouse_ival new_ival) {
	cct_termcurs *_this = (cct_termcurs *)obj;
	cct_mouse_ival old_ival;

	assert(Is_True(is_open));
	assert(Is_True(_this->curses->open));
	assert(_this->id==id_termcurs);

#ifdef COBCURSES_MOUSE_SUPPORT
	old_ival = (cct_mouse_ival) mouseinterval((int)new_ival);
	_this->curses->mouse_click_ms = (cct_mouse_ival) mouseinterval(-1);
#else
	old_ival = _this->curses->mouse_click_ms = new_ival;
#endif
	return old_ival;
}



/*
 * Initialize the API pointers :
 */
static
cct_api_terminal *
termcurs_init_api(void) {
	if ( !api.curses_init ) {
		memset(&api,0,sizeof api);
		api.curses_init		= termcurs_init;
		api.dispose		= termcurs_dispose;
		api._delete		= termcurs_delete;
		api.open		= termcurs_open;
		api.close		= termcurs_close;
		api.flush		= termcurs_flush;
		api.putch		= termcurs_putch;
		api.puts		= termcurs_puts;
		api.putsn		= termcurs_putsn;
		api.move                = termcurs_move;
		api.getkey		= termcurs_getkey;
		api.cap_attr		= termcurs_cap_attr;
		api.get_attr		= termcurs_get_attr;
		api.get_attr_and_pair	= termcurs_get_attr_and_pair;
		api.set_attr		= termcurs_set_attr;
		api.set_attr_and_pair	= termcurs_set_attr_and_pair;
		api.lines               = termcurs_lines;
		api.columns             = termcurs_columns;
		api.colour_pairs        = termcurs_colour_pairs;
		api.getyx               = termcurs_getyx;
		api.has_underline_cap	= termcurs_has_underline_cap;
		api.has_colour_cap	= termcurs_has_colour_cap;
		api.has_change_colour_cap = termcurs_has_change_colour_cap;
		api.clear		= termcurs_clear;
		api.clrtoeol		= termcurs_clrtoeol;
		api.clrtobot		= termcurs_clrtobot;
		api.beep		= termcurs_beep;
		api.init_pair		= termcurs_init_pair;
		api.set_pair		= termcurs_set_pair;
		api.hline		= termcurs_hline;
		api.vline		= termcurs_vline;
	
		api.have_mouse		= termcurs_have_mouse;
		api.set_mouse_mask	= termcurs_set_mouse_mask;
		api.get_mouse_mask	= termcurs_get_mouse_mask;
		api.have_mouse_event	= termcurs_have_mouse_event;
		api.get_mouse_event	= termcurs_get_mouse_event;
		api.clear_mouse_event	= termcurs_clear_mouse_event;
		api.get_mouse_ival	= termcurs_get_mouse_ival;
		api.set_mouse_ival	= termcurs_set_mouse_ival;

		api.new_menu		= termcurs_new_menu;
		api.set_menu_format	= termcurs_set_menu_format;
		api.set_menu_opts	= termcurs_set_menu_opts;
		api.get_menu_opts	= termcurs_get_menu_opts;
		api.set_dynamic_menu	= termcurs_set_dynamic_menu;

		api.new_menu_item	= termcurs_new_menu_item;
		api.set_menu_item_sel   = termcurs_set_menu_item_sel;
		api.get_menu_item_sel	= termcurs_get_menu_item_sel;
		api.set_menu_title	= termcurs_set_menu_title;
		api.set_menu_coord      = termcurs_set_menu_coord;
		api.set_menu_foreground = termcurs_set_menu_foreground;
		api.set_menu_background = termcurs_set_menu_background;
		api.set_menu_grey       = termcurs_set_menu_grey;

		api.show_menu		= termcurs_show_menu;
		api.hide_menu		= termcurs_hide_menu;
		api.delete_menu		= termcurs_delete_menu;
		api.get_menu_selection  = termcurs_get_menu_selection;

		api.suspend		= termcurs_suspend;
		api.resume		= termcurs_resume;
	}
	return &api;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/term_curses.c,v $ */

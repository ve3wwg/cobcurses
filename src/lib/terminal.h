/* terminal.h : General base class for terminal interfaces
 * Warren W. Gay VE3WWG
 * Thu Sep  6 15:42:53 2007
 * $Id: terminal.h,v 1.13 2007/10/29 14:56:15 ve3wwg Exp $
 */
#ifndef _terminal_h_
#define _terminal_h_

/*
 * Colour Definitions :
 */
typedef enum {
	clr_BLACK=0,
	clr_RED,
	clr_GREEN,
	clr_YELLOW,
	clr_BLUE,
	clr_MAGENTA,
	clr_CYAN,
	clr_WHITE
} cct_colour;

/*
 * Colour Pair :
 */
typedef short int	cct_pair;

/*
 * Terminal attributes :
 */
typedef cct_unsigned	cct_termattr;

#define CCA_NORMAL	((cct_termattr)0x000001)
#define CCA_STANDOUT	((cct_termattr)0x000002)
#define CCA_UNDERLINE	((cct_termattr)0x000004)
#define CCA_REVERSE	((cct_termattr)0x000008)
#define CCA_BLINK	((cct_termattr)0x000010)
#define CCA_DIM		((cct_termattr)0x000020)
#define CCA_BOLD	((cct_termattr)0x000040)
#define CCA_ALTCHARSET	((cct_termattr)0x000080)
#define CCA_INVIS	((cct_termattr)0x000100)
#define CCA_PROTECT	((cct_termattr)0x000200)
#define CCA_HORIZONTAL	((cct_termattr)0x000400)
#define CCA_LEFT	((cct_termattr)0x000800)
#define CCA_LOW		((cct_termattr)0x001000)
#define CCA_RIGHT	((cct_termattr)0x002000)
#define CCA_TOP		((cct_termattr)0x004000)
#define CCA_VERTICAL	((cct_termattr)0x008000)

#define CCA_ATTRIBUTES	((cct_termattr)0x00FFFF)

/*
 * CobCurses key definitions :
 */
typedef enum {
	cck_BREAK	=0x0100,
	cck_SRESET,
	cck_RESET,
	cck_DOWN,
	cck_UP,
	cck_LEFT,
	cck_SLEFT,
	cck_RIGHT,
	cck_SRIGHT,
	cck_HOME,
	cck_SHOME,
	cck_BACKSPACE,
	cck_F0,
	cck_F1,
	cck_F2,
	cck_F3,
	cck_F4,
	cck_F5,
	cck_F6,
	cck_F7,
	cck_F8,
	cck_F9,
	cck_F10,
	cck_F11,
	cck_F12,
	cck_DL,
	cck_SDL,
	cck_IL,
	cck_DC,
	cck_SDC,
	cck_IC,
	cck_SIC,
	cck_EIC,
	cck_CLEAR,
	cck_EOS,
	cck_EOL,
	cck_SEOL,
	cck_SF,
	cck_SR,
	cck_NPAGE,
	cck_PPAGE,
	cck_STAB,
	cck_CTAB,
	cck_CATAB,
	cck_ENTER,
	cck_PRINT,
	cck_SPRINT,
	cck_LL,
	cck_A1,
	cck_A3,
	cck_B2,
	cck_C1,
	cck_C3,
	cck_BTAB,
	cck_BEG,
	cck_SBEG,
	cck_CANCEL,
	cck_SCANCEL,
	cck_CLOSE,
	cck_COMMAND,
	cck_SCOMMAND,
	cck_COPY,
	cck_SCOPY,
	cck_CREATE,
	cck_SCREATE,
	cck_END,
	cck_EXIT,
	cck_SEXIT,
	cck_FIND,
	cck_SFIND,
	cck_HELP,
	cck_SHELP,
	cck_MARK,
	cck_MESSAGE,
	cck_SMESSAGE,
	cck_MOVE,
	cck_SMOVE,
	cck_NEXT,
	cck_SNEXT,
	cck_OPEN,
	cck_OPTIONS,
	cck_SOPTIONS,
	cck_PREVIOUS,
	cck_SPREVIOUS,
	cck_REDO,
	cck_SREDO,
	cck_REFERENCE,
	cck_REFRESH,
	cck_REPLACE,
	cck_SREPLACE,
	cck_RESTART,
	cck_RESUME,
	cck_SRSUME,
	cck_SAVE,
	cck_SSAVE,
	cck_SELECT,
	cck_SEND,
	cck_SUNDO,
	cck_SUSPEND,
	cck_SSUSPEND,
	cck_UNDO,
	cck_MOUSE,
	cck_RESIZE,
	cck_EVENT,
	cck_UNDEFINED,
	cck_IDLE				/* Equiv to ERR from curses getch() */
} cct_key;

#define cck_MIN		cck_BREAK
#define cck_MAX		cck_IDLE

/*
 * S P E C I A L   C H A R A C T E R S :
 *
 * These are mapped to control-codes. The terminal output routines will
 * re-map them to the correct character code necessary to display them.
 *
 * VT100 symbols :
 */
#define acs_ULCORNER	((char)0x01) 	/* upper left corner */
#define acs_LLCORNER	((char)0x02)	/* lower left corner */
#define acs_URCORNER	((char)0x03)	/* upper right corner */
#define acs_LRCORNER	((char)0x04)	/* lower right corner */
#define acs_LTEE	((char)0x05)	/* tee pointing right */
#define acs_RTEE	((char)0x06)	/* tee pointing left */
#define acs_BTEE	((char)0x07)	/* tee pointing up */
#define acs_TTEE	((char)0x08)	/* tee pointing down */
#define acs_HLINE	((char)0x09)	/* horizontal line */
#define acs_VLINE	((char)0x0A)	/* vertical line */
#define acs_PLUS	((char)0x0B)	/* large plus or crossover */
#define acs_S1		((char)0x0C)	/* scan line 1 */
#define acs_S9  	((char)0x0D)	/* scan line 9 */
#define acs_DIAMOND	((char)0x0F)	/* diamond */
#define acs_CKBOARD	((char)0x10)	/* checker board (stipple) */
#define acs_DEGREE	((char)0x11)	/* degree symbol */
#define acs_PLMINUS	((char)0x12)	/* plus/minus */
#define acs_BULLET	((char)0x13)	/* bullet */
/*
 * Teletype 5410v1 symbols :
 */
#define acs_LARROW	((char)0x14)	/* arrow pointing left */
#define acs_RARROW	((char)0x15)	/* arrow pointing right */
#define acs_DARROW	((char)0x16)	/* arrow pointing down */
#define acs_UARROW	((char)0x17)	/* arrow pointing up */
#define acs_BOARD	((char)0x18)	/* board of squares */
#define acs_LANTERN	((char)0x19)	/* lantern symbol */
#define acs_BLOCK	((char)0x1A)	/* solid square block */
/*
 * Undocumented syms :
 */
#if 0	                /* These are not supported here */
#define acs_S3		((char)0x..)	/* scan line 3 */
#define acs_S7		((char)0x..)	/* scan line 7 */
#endif

#define acs_LEQUAL	((char)0x1B)	/* less/equal */
#define acs_GEQUAL	((char)0x1C)	/* greater/equal */
#define acs_PI		((char)0x1D)	/* Pi */
#define acs_NEQUAL	((char)0x1E)	/* not equal */
#define acs_STERLING	((char)0x1F)	/* UK pound sign (^^) */

#define acs_MAX		acs_STERLING

#define is_acs(ch)	((cct_bool)( (ch) <= acs_MAX ? T : F ))

/*
 * Mouse data types and constants :
 */
typedef cct_unsigned		cct_mmask;	/* Mouse button state info mask */
typedef cct_short		cct_mouseid;	/* Mouse id */
typedef cct_unsigned		cct_mouse_ival;	/* Mouse click interval in ms */

typedef struct {
	cct_mouseid	id;			/* Id of mouse causing event */
	cct_ushort	x;			/* X value (column) */
	cct_ushort	y;			/* Y value (line) */
	cct_ushort	z;			/* Z value (not used) */
	cct_mmask	bstate;			/* Button states */
} cct_mevent;

#define __MSHIFTED__(mask,x)	(((x)>1)?(((cct_mmask)(mask))<<(((x)-1)*8)):((cct_mmask)(mask)))

#define BSTATE_PRESSED(x)		__MSHIFTED__(1,x)
#define BSTATE_RELEASED(x)		__MSHIFTED__(2,x)
#define BSTATE_CLICKED(x)		__MSHIFTED__(4,x)
#define BSTATE_DOUBLE_CLICKED(x)	__MSHIFTED__(8,x)
#define BSTATE_TRIPLE_CLICKED(x)	__MSHIFTED__(0x10,x)

/*
 * Menu Options :
 */
typedef enum {
	cct_emo_ONEVALUE      = 0x01,		/* Only one item may be selected (default) */
	cct_emo_ROWMAJOR      = 0x02,		/* Items are row major order (default) */
	cct_emo_IGNORECASE    = 0x04,		/* Ignore case in menu item matching (default) */
	cct_emo_SHOWDESC      = 0x08,		/* Show descriptive text (default) */
	/* These options are not supported (ignored) */
	cct_emo_NONCYCLIC     = 0x10,		/* This option is ignored */
	cct_emo_SHOWMATCH     = 0x20		/* This option is ignored */
} cct_term_menu_opts;

/*
 * Terminal data types :
 */
typedef struct S_TERMINAL 	cct_terminal;	/* The terminal object (partially opaque) */
typedef struct S_TERM_MENU 	cct_menu;	/* The opaque menu object */

/*
 * The terminal API :
 */
typedef cct_terminal * (*api_terminal_curses_init)(cct_terminal *obj);

typedef cct_terminal * (*api_terminal_dispose)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_delete)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_open)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_close)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_flush)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_putch)(cct_terminal *obj,		char ch);
typedef cct_terminal * (*api_terminal_puts)(cct_terminal *obj,const char *text);
typedef cct_terminal * (*api_terminal_putsn)(cct_terminal *obj,const char *text,cct_ushort n);
typedef cct_key        (*api_terminal_getkey)(cct_terminal *obj);
typedef cct_bool       (*api_terminal_has_underline_cap)(cct_terminal *obj);
typedef cct_bool       (*api_terminal_has_colour_cap)(cct_terminal *obj);
typedef cct_bool       (*api_terminal_has_change_colour_cap)(cct_terminal *obj);
typedef cct_ushort     (*api_terminal_lines)(cct_terminal *obj);
typedef cct_ushort     (*api_terminal_columns)(cct_terminal *obj);
typedef cct_ushort     (*api_terminal_colour_pr)(cct_terminal *obj);
typedef cct_ushort     (*api_terminal_getyx)(cct_terminal *obj,cct_ushort *py);
typedef cct_terminal * (*api_terminal_move)(cct_terminal *obj,cct_ushort y, cct_ushort x);
typedef cct_terminal * (*api_terminal_attribute_cap)(cct_terminal *obj,cct_termattr *attrs);
typedef cct_terminal * (*api_terminal_attribute_set)(cct_terminal *obj,const cct_termattr attrs);
typedef cct_terminal * (*api_terminal_attribute_get)(cct_terminal *obj,cct_termattr *attrs);
typedef cct_terminal * (*api_terminal_attribute_get_pair)(cct_terminal *obj,cct_termattr *attrs,cct_pair *pair);
typedef cct_terminal * (*api_terminal_attribute_set_pair)(cct_terminal *obj,const cct_termattr attrs,cct_pair pair);
typedef cct_terminal * (*api_terminal_clear)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_clrtoeol)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_clrtobot)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_beep)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_init_pair)(cct_terminal *obj,cct_colour foreground,cct_colour background,cct_pair pair);
typedef cct_terminal * (*api_terminal_set_pair)(cct_terminal *obj,cct_pair pair);
typedef cct_terminal * (*api_terminal_hline)(cct_terminal *obj,char ch,cct_ushort n);
typedef cct_terminal * (*api_terminal_vline)(cct_terminal *obj,char ch,cct_ushort n);

typedef cct_bool       (*api_terminal_have_mouse)(cct_terminal *obj);
typedef cct_mmask      (*api_terminal_set_mouse_mask)(cct_terminal *obj,cct_mmask mask);
typedef cct_mmask      (*api_terminal_get_mouse_mask)(cct_terminal *obj);
typedef cct_bool       (*api_terminal_have_mouse_event)(cct_terminal *obj);
typedef cct_mevent *   (*api_terminal_get_mouse_event)(cct_terminal *obj);
typedef cct_terminal * (*api_terminal_clear_mouse_event)(cct_terminal *obj);
typedef cct_mouse_ival (*api_terminal_get_mouse_ival)(cct_terminal *obj);
typedef cct_mouse_ival (*api_terminal_set_mouse_ival)(cct_terminal *obj,cct_mouse_ival new_ival);

typedef cct_menu *     (*api_terminal_new_menu)(cct_terminal *obj);
typedef cct_menu *     (*api_terminal_set_menu_format)(cct_menu *obj,cct_ushort maxrows,cct_ushort maxcols);
typedef cct_term_menu_opts (*api_terminal_set_menu_opts)(cct_menu *obj,cct_term_menu_opts opts);
typedef cct_term_menu_opts (*api_terminal_get_menu_opts)(cct_menu *obj);
typedef cct_menu *     (*api_terminal_set_dynamic_menu)(cct_menu *obj,const char *module,cct_ushort item_limit);
typedef cct_menu *     (*api_terminal_new_menu_item)(cct_menu *obj,const char *name,const char *desc);
typedef cct_bool       (*api_terminal_set_menu_item_sel)(cct_menu *obj,const char *name,cct_bool selectable);
typedef cct_bool       (*api_terminal_get_menu_item_sel)(cct_menu *obj,const char *name);
typedef cct_menu *     (*api_terminal_set_menu_title)(cct_menu *obj,const char *title,cct_termattr attr,cct_pair pair);
typedef cct_menu *     (*api_terminal_set_menu_coord)(cct_menu *obj,cct_ushort y,cct_ushort x);
typedef cct_menu *     (*api_terminal_set_menu_foreground)(cct_menu *obj,cct_termattr attr,cct_pair pair);
typedef cct_menu *     (*api_terminal_set_menu_background)(cct_menu *obj,cct_termattr attr,cct_pair pair);
typedef cct_menu *     (*api_terminal_set_menu_grey)(cct_menu *obj,cct_termattr attr,cct_pair pair);
typedef cct_menu *     (*api_terminal_show_menu)(cct_menu *obj);
typedef cct_menu *     (*api_terminal_hide_menu)(cct_menu *obj);
typedef cct_menu *     (*api_terminal_delete_menu)(cct_menu *obj);
typedef char *         (*api_terminal_get_menu_sel)(cct_menu *obj);

typedef cct_terminal * (*api_terminal_suspend_resume)(cct_terminal *obj);

/*
 * Terminal API :
 */
typedef struct {
	api_terminal_curses_init curses_init;	/* Initialize curses object (choose curses interface) */
	api_terminal_dispose	dispose;	/* Dispose of objects contents */
	api_terminal_delete	_delete;	/* Dispose of objects contents & free object */
	api_terminal_open	open;		/* Open terminal */
	api_terminal_close	close;		/* Close terminal */
	api_terminal_flush	flush;		/* Refresh terminal with changes */
	api_terminal_putch	putch;		/* Put 1 character */
	api_terminal_puts	puts;		/* Put text */
	api_terminal_putsn	putsn;		/* Put up to n chars of text */
	api_terminal_move	move;		/* Move to (y,x) */
	api_terminal_getkey	getkey;		/* Get a keystroke */
	api_terminal_lines	lines;		/* Get number of lines */
	api_terminal_columns	columns;	/* Get number of columns */
	api_terminal_colour_pr	colour_pairs;	/* Get max number of colour pairs */
	api_terminal_getyx      getyx;          /* Get (y,x) coordinates */
	api_terminal_clear	clear;		/* Clear screen */
	api_terminal_clrtoeol	clrtoeol;	/* Clear to end of line */
	api_terminal_clrtobot	clrtobot;	/* Clear to bottom of screen */
	api_terminal_beep	beep;		/* Beep */
	api_terminal_init_pair	init_pair;	/* Initialize colour pair */
	api_terminal_set_pair	set_pair;	/* Set colour pair */
	api_terminal_hline	hline;		/* Draw horizontal line */
	api_terminal_vline	vline;		/* Draw vertical line */

	api_terminal_attribute_cap 	   cap_attr;		/* Get attribute caps */
	api_terminal_attribute_get 	   get_attr;	 	/* Get attributes */
	api_terminal_attribute_get_pair	   get_attr_and_pair;	/* Get attributes and colour pair */
	api_terminal_attribute_set 	   set_attr;		/* Set attributes */
	api_terminal_attribute_set_pair	   set_attr_and_pair;	/* Set attribute and colour pair */
	api_terminal_has_underline_cap has_underline_cap; 	/* Has underline capability */
	api_terminal_has_colour_cap has_colour_cap;	  	/* Has colour capability */
	api_terminal_has_change_colour_cap has_change_colour_cap; /* Has ability to change colours */

	api_terminal_have_mouse		have_mouse;		/* Return T if we have mouse support */
	api_terminal_set_mouse_mask	set_mouse_mask;		/* Set mouse mask for events */
	api_terminal_get_mouse_mask	get_mouse_mask;		/* Get mouse mask for events */
	api_terminal_have_mouse_event	have_mouse_event;	/* Return T if we have a mouse event */
	api_terminal_get_mouse_event	get_mouse_event;	/* Return pending mouse event info */
	api_terminal_clear_mouse_event	clear_mouse_event;	/* Release last mouse event */
	api_terminal_get_mouse_ival	get_mouse_ival;		/* Get mouse click interval in ms */
	api_terminal_set_mouse_ival	set_mouse_ival;		/* Set mouse click interval in ms */

	api_terminal_new_menu		new_menu;		/* create new menu object */
	api_terminal_set_menu_format	set_menu_format;	/* Set menu's max dimensions */
	api_terminal_set_menu_opts	set_menu_opts;		/* Set menu options */
	api_terminal_get_menu_opts	get_menu_opts;		/* Get menu options */
	api_terminal_set_dynamic_menu	set_dynamic_menu;	/* Configure a dynamic menu */

	api_terminal_new_menu_item	new_menu_item;		/* add menu item */
	api_terminal_set_menu_item_sel	set_menu_item_sel;	/* Set menu item selectable */
	api_terminal_get_menu_item_sel  get_menu_item_sel;	/* Get menu item selectability */
	api_terminal_set_menu_title	set_menu_title;		/* Set menu title */
	api_terminal_set_menu_coord	set_menu_coord;		/* Set menu top left coordinates */
	api_terminal_set_menu_foreground set_menu_foreground;	/* Set attributes for menu foreground */
	api_terminal_set_menu_background set_menu_background;	/* Set attributes for menu foreground */
	api_terminal_set_menu_grey  	set_menu_grey;		/* Set attributes for menu foreground */
	api_terminal_show_menu		show_menu;		/* show menu object */
	api_terminal_hide_menu		hide_menu;		/* hide menu */
	api_terminal_delete_menu	delete_menu;		/* delete menu */
	api_terminal_get_menu_sel       get_menu_selection;	/* get menu selection */

	api_terminal_suspend_resume	suspend;		/* Suspend terminal to allow system() call */
	api_terminal_suspend_resume	resume;			/* Resume terminal after suspend */
} cct_api_terminal;

/*
 * Terminal object :
 */
struct S_TERMINAL {
	cct_objid		id;		/* Id of this object */
	cct_api_terminal	*api;		/* Api set */
	cct_bool		alloc;		/* True if allocated object by malloc */
	void			*extdata;	/* Extension of this object */
};

extern cct_terminal *terminal_new(void);		/* Create new terminal object */
extern cct_terminal *terminal_init(cct_terminal *obj);	/* Initialize a terminal object */

/*
 * The user's view of the menu object :
 */
struct S_TERM_MENU {
	cct_terminal		*term;		/* The owning terminal object for this menu */
	/* The rest remains private */
};

#endif /* _terminal_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/terminal.h,v $ */

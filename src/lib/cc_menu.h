/* cc_menu.h : CobCurses replacement for curses menu facilities
 * Warren W. Gay VE3WWG
 * Mon Oct 15 14:06:34 2007
 * $Id: cc_menu.h,v 1.6 2007/10/18 17:15:53 ve3wwg Exp $
 */
#ifndef _cc_menu_h_
#define _cc_menu_h_

#define E_OK                    (0)
#define E_BAD_ARGUMENT          (-1)
#define E_BAD_STATE             (-2)
#define E_UNKNOWN_COMMAND       (-3)
#define E_POSTED                (-4)
#define E_CONNECTED             (-5)
#define E_SYSTEM_ERROR          (-6)
#define E_NO_ROOM               (-7)
#define E_NOT_POSTED            (-8)
#define E_NO_MATCH              (-9)
#define E_NOT_CONNECTED         (-10)
#define E_REQUEST_DENIED        (-11)
#define E_INVALID_FIELD         (-12)
#define E_CURRENT               (-13)
#define E_NOT_SELECTABLE        (-14)

/*
 * Define menu commands :
 */
#define REQ_LEFT_ITEM           (KEY_MAX + 1)
#define REQ_RIGHT_ITEM          (KEY_MAX + 2)
#define REQ_UP_ITEM             (KEY_MAX + 3)
#define REQ_DOWN_ITEM           (KEY_MAX + 4)
#define REQ_FIRST_ITEM          (KEY_MAX + 5)
#define REQ_LAST_ITEM           (KEY_MAX + 6)
#define REQ_NEXT_ITEM           (KEY_MAX + 7)
#define REQ_PREV_ITEM           (KEY_MAX + 8)
#define REQ_SCR_UPAGE           (KEY_MAX + 9)
#define REQ_SCR_ULINE           (KEY_MAX + 10)
#define REQ_SCR_DPAGE           (KEY_MAX + 11)
#define REQ_SCR_DLINE           (KEY_MAX + 12)
#define REQ_TOGGLE_ITEM         (KEY_MAX + 13)
#define REQ_NEXT_MATCH          (KEY_MAX + 14)
#define REQ_PREV_MATCH          (KEY_MAX + 15)
#define REQ_BACK_PATTERN        (KEY_MAX + 16)
#define REQ_CLEAR_PATTERN       (KEY_MAX + 17)

#define MIN_MENU_COMMAND        REQ_LEFT_ITEM
#define MAX_MENU_COMMAND        REQ_CLEAR_PATTERN


/*
 * Internal Data types :
 */
typedef cct_ushort		cct_menu_opt;
typedef cct_ushort		cct_item_opt;

/*
 * Menu options :
 *
 * Note: These values must agree with term_curses_menu.h values
 *	 in data type cct_term_menu_opts.
 */
#define O_ONEVALUE      ((cct_menu_opt)(0x01))
#define O_ROWMAJOR      ((cct_menu_opt)(0x02))
#define O_IGNORECASE    ((cct_menu_opt)(0x04))
#define O_SHOWDESC      ((cct_menu_opt)(0x08))
#define O_NONCYCLIC     ((cct_menu_opt)(0x10))
#define O_SHOWMATCH     ((cct_menu_opt)(0x20))

/*
 * Item options :
 */
#define O_SELECTABLE    ((cct_item_opt)(0x01))

typedef struct S_CC_ITEM ITEM;
typedef struct S_CC_MENU MENU;

/*
 * The ITEM data type :
 */
struct S_CC_ITEM {
	char		*name;		/* Menu item name */
	char		*desc;		/* Menu item descriptive text */
	cct_item_opt	opts;		/* Menu item options */
	ITEM		*up;		/* Next item up */
	ITEM		*left;		/* Next item left */
	ITEM		*right;		/* Next item right */
	ITEM		*down;		/* Next item down */
	cct_bool	selected;	/* T if item is selected */
	cct_ushort	y;		/* y coordinate */
	cct_ushort	x;		/* x coordinate */
	void		*userptr;	/* User data pointer */
	MENU		*menu;		/* Null or ptr to owning menu */
};

/*
 * Menu status :
 */
typedef enum {
	cct_em_init,			/* Newly initialized menu */
	cct_em_posted,			/* Menu is posted */
	cct_em_unposted			/* Menu is unposted */
} cct_menu_status;

typedef int (*cct_strncmp)(const char *str1,const char *str2,size_t n);

/*
 * The MENU data type :
 */
struct S_CC_MENU {
	cct_menu_opt	opts;		/* Menu options */
	WINDOW		*win;		/* Menu's window */
	WINDOW		*sub;		/* Menu's subwindow */
	ITEM		**items;	/* List of all items */
	cct_ushort	n_items;	/* No. of items in items[] */
	cct_ushort	top_row;	/* Top row of menu */
	char		*markstr;	/* Mark string */
	ITEM		*current;	/* Ptr to current item */
	ITEM		*start;		/* Ptr to item with no pattern match */
	attr_t		fg_attr;	/* Foreground attribute */
	attr_t		bg_attr;	/* Background attribute */
	attr_t		gr_attr;	/* Grey attribute */
	char		*patbuf;	/* Pattern buffer */
	cct_ushort	patx;		/* Pattern index */
	cct_ushort	n_rows;		/* # of menu rows, max (configured) */
	cct_ushort	n_cols;		/* # of menu cols, max (configured) */
	cct_ushort	o_row;		/* Row offset for paging through large menus */
	/**/
	cct_ushort	max_namelen;	/* Max width of the item name */
	cct_ushort	max_desclen;	/* Max width of the item descriptions */
	cct_ushort	max_marklen;	/* Max width of the mark string */
	cct_ushort	max_itemlen;	/* Max width of item (mark + name + space + desc) */
	cct_menu_status status;		/* Menu status */
	cct_strncmp	cmpfunc;	/* String compare function for item matching */
};

extern ITEM *new_item(const char *name,const char *desc);
extern int   free_item(ITEM *item);
extern MENU *new_menu(ITEM **items);
extern int   free_menu(MENU *menu);
	
extern int set_menu_win(MENU *menu, WINDOW *win);
extern int set_menu_sub(MENU *menu, WINDOW *sub);
extern int scale_menu(MENU *menu, int *rows, int *columns);

extern int set_item_userptr(ITEM *item, void *userptr);
extern void *item_userptr(const ITEM *item);
extern ITEM *current_item(const MENU *menu);

extern int set_item_opts(ITEM *item, cct_item_opt opts);
extern int item_opts_on(ITEM *item, cct_item_opt opts);
extern int item_opts_off(ITEM *item, cct_item_opt opts);
extern cct_item_opt item_opts(const ITEM *item);

extern char *item_name(const ITEM *item);
extern char *item_description(const ITEM *item);

extern int post_menu(MENU *menu);
extern int unpost_menu(MENU *menu);

extern int set_menu_mark(MENU *menu, const char *mark);

extern int set_menu_fore(MENU *menu, attr_t attr);
extern int set_menu_back(MENU *menu, attr_t attr);
extern int set_menu_grey(MENU *menu, attr_t attr);

extern int set_menu_opts(MENU *menu, cct_menu_opt opts);
extern int menu_opts_on(MENU *menu, cct_menu_opt opts);
extern int menu_opts_off(MENU *menu, cct_menu_opt opts);
extern cct_menu_opt menu_opts(const MENU *menu);
                           
extern int menu_driver(MENU *menu, int c);

extern int set_menu_format(MENU *menu, int rows, int cols);
extern void menu_format(const MENU *menu, int *rows, int *cols);

extern void draw_padded(WINDOW *win,const char *str,cct_ushort length);

#endif /* _cc_menu_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/cc_menu.h,v $ */

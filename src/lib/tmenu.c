/*
 * This is just a grotty little test program for testing
 * menu functions :
 *
 * Warren Gay
 */
#include <cobcurses.h>
#include <terminal.h>
#include <string.h>

#include "debug.h"

DBG_DEF;

static cct_menu *
New_Menu(cct_terminal *term) {
	cct_menu *menu;

	menu = term->api->new_menu(term);

	term->api->new_menu_item(menu,"Apple","An apple");
	term->api->new_menu_item(menu,"Pear","A pair of pears");
	term->api->new_menu_item(menu,"Banana","Big banana");
	term->api->new_menu_item(menu,"Orange","An orange orange");
	term->api->new_menu_item(menu,"Anchovies","Something for pizza");
	term->api->new_menu_item(menu,"Grapes","A bunch of grapes");
	term->api->new_menu_item(menu,"Peach","A peck of peaches");
	term->api->new_menu_item(menu,"Cherry","A quart of cherries");
	term->api->new_menu_item(menu,"Raspberry","A red raspberry");
	term->api->new_menu_item(menu,"Pumpkin","A pumpkin for pie");
	term->api->new_menu_item(menu,"Squash","A squished squash");
	term->api->new_menu_item(menu,"Tomato","A red tomato");
	term->api->new_menu_item(menu,"Strawberry","A sweet strawberry");
	term->api->new_menu_item(menu,"Cherry Tomato","A cherry tomato");
	term->api->new_menu_item(menu,"Cucumber","Sliced cucumbers");
	term->api->new_menu_item(menu,"Radish","A horse radish");
	term->api->new_menu_item(menu,"Lettuce","Lettuce for salads");
	term->api->new_menu_item(menu,"Corn","A cob of corn");
	term->api->new_menu_item(menu,"Carrots","Carrots are not for rabbits");
	term->api->new_menu_item(menu,"Catnip","Catnip is for cats");
	term->api->new_menu_item(menu,"Turnip","Turnips are tasty");
	term->api->new_menu_item(menu,"Potatoes","Potatoes for dinner");
	term->api->new_menu_item(menu,"Tangerine","Tangerines are sweet");
	term->api->new_menu_item(menu,"!Catnip","Toggle catnip on/off");
	term->api->new_menu_item(menu,"!Corn","Toggle corn on/off");
	term->api->new_menu_item(menu,"TRMajor","Toggle row-major order");
	term->api->new_menu_item(menu,"TFormat","Toggle menu format");
	term->api->new_menu_item(menu,"TDesc","Toggle descriptive format");
	term->api->new_menu_item(menu,"TMulti","Toggle multi selection");
	term->api->new_menu_item(menu,"TICase","Toggle ignore case");
	
	term->api->set_menu_title(menu,"A Test Menu",CCA_REVERSE,0);
	term->api->set_menu_coord(menu,20,12);

	return menu;
}

int
main(int argc,char **argv) {
	cct_terminal *term = terminal_new();
	cct_menu *menu;
	char *msel;
	cct_bool mcatnip = T;
	cct_bool mformat = F;
	cct_term_menu_opts mopts = 0;
	cct_ushort fmtrows = 4;
	cct_ushort fmtcols = 5;

	term->api->curses_init(term);
	term->api->open(term);

	menu = New_Menu(term);

	term->api->show_menu(menu);
	mopts = term->api->get_menu_opts(menu);

	while ( (msel = term->api->get_menu_selection(menu)) ) {
DBG "  msel = '%s'\n", msel END
		if ( *msel == '!' ) {
			mcatnip = term->api->get_menu_item_sel(menu,msel+1) ^ T;
			term->api->set_menu_item_sel(menu,msel+1,mcatnip);
		} else if ( !strncasecmp(msel,"TFormat",7) ) {
			term->api->delete_menu(menu);

			mformat ^= T;
			menu = New_Menu(term);
			term->api->set_menu_opts(menu,mopts);
			if ( mformat )
				term->api->set_menu_format(menu,fmtrows,fmtcols);
			term->api->show_menu(menu);
		} else if ( !strncasecmp(msel,"TDesc",5) ) {
			term->api->delete_menu(menu);

			mopts ^= cct_emo_SHOWDESC;
			menu = New_Menu(term);
			term->api->set_menu_opts(menu,mopts);
			if ( mformat )
				term->api->set_menu_format(menu,fmtrows,fmtcols);
			term->api->show_menu(menu);
		} else if ( !strncasecmp(msel,"TMulti",6) ) {
			mopts ^= cct_emo_ONEVALUE;
			term->api->set_menu_opts(menu,mopts);
		} else if ( !strncasecmp(msel,"TRMajor",7) ) {
			mopts ^= cct_emo_ROWMAJOR;

			term->api->delete_menu(menu);
			menu = New_Menu(term);
			term->api->set_menu_opts(menu,mopts);
			if ( mformat )
				term->api->set_menu_format(menu,fmtrows,fmtcols);
			term->api->show_menu(menu);
		} else if ( !strncasecmp(msel,"TICase",6) ) {
			mopts ^= cct_emo_IGNORECASE;
			term->api->set_menu_opts(menu,mopts);
		}
		mopts = term->api->get_menu_opts(menu);
DBG "  menu opts in effect: 0x%04X\n\n", mopts END
	}

	term->api->delete_menu(menu);
	term->api->close(term);
	term->api->_delete(term);
	return 0;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/tmenu.c,v $ */

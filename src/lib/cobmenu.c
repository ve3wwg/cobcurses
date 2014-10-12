#include <stdio.h>

/* cobmenu.c : COBOL menu support
 * Warren W. Gay VE3WWG
 * Wed Oct 10 15:46:35 2007
 * $Id: cobmenu.c,v 1.8 2007/10/25 18:53:19 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <terminal.h>
#include <cobmenu.h>
#include <misc.h>
#include <dynstr.h>
#include <cobtrace.h>

#include <stdlib.h>
#include <memory.h>
#include <ctype.h>

/*
 * Create a menu from a COBOL menu definition :
 */
cct_menu *
cobcurses_COBOL_menu(cct_terminal *term,const void *cobol_definition,cct_pair menu_pair,cct_termattr menu_tattr) {
	char *p = (char *)cobol_definition;
	cct_strvar name, pic9, title, item, desc, module_name;
	cct_ushort y, x, title_length, desc_length, name_length;
	cct_menu *menu = 0;
	cct_bool selectable;
	cct_ushort safety = 0;
        cct_term_menu_opts opts = 0;
	cct_ushort opt_rows = 0;
	cct_ushort opt_cols = 0;
	cct_ushort menu_type = 0;
	cct_ushort item_limit = 0;

	dyn_str_init(&name);
	dyn_str_init(&pic9);
	dyn_str_init(&title);
	dyn_str_init(&item);
	dyn_str_init(&desc);
	dyn_str_init(&module_name);

	dyn_str_PICX(&name,p,16);		/* Menu Name is PIC X(16) */
	p += 16;

	cobcurses_trace_printf(2,"COBOL Menu name is '%s'\n",dyn_str_string(&name));

	if ( !dyn_str_PIC9(&pic9,p,2) )		/* PIC 9(2) for Menu Type */
		goto bad;
	p += 2;
	menu_type = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	cobcurses_trace_printf(2,"  menu type = %02u\n", menu_type);

	/*
	 * Dynamic menus have a module name and an item limit :
	 */
	if ( menu_type == 02 ) {
		dyn_str_PICX(&module_name,p,16); /* PIC X(16) module name */
		p += 16;
		cobcurses_trace_printf(2,"  module name is '%s'\n",dyn_str_string(&module_name));

		if ( !dyn_str_PIC9(&pic9,p,4) ) { /* PIC 9999 item limit */
			cobcurses_trace_printf(2,"  bad item limit value '%s'\n",dyn_str_string(&pic9));
			goto bad;
		}
		p += 4;
		item_limit = (cct_ushort) dyn_str_to_ulong(&pic9,0);
		cobcurses_trace_printf(2,"  item limit is %u\n",item_limit);
	}

	if ( !dyn_str_PIC9(&pic9,p,3) )	{	/* PIC 9(3) for X */
		cobcurses_trace_printf(2,"  bad x value '%s'\n",dyn_str_string(&pic9));
		goto bad;
	}	
	x = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	if ( x > 0 )
	--x;					/* Curses is zero based */
	p += 3;

	if ( !dyn_str_PIC9(&pic9,p,3) )	{	/* PIC 9(3) for Y*/
		cobcurses_trace_printf(2,"  bad y value '%s'\n",dyn_str_string(&pic9));
		goto bad;			/* Bad menu */
	}
	y = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	if ( y > 0 )
		--y;				/* COBOL is 1-based */
	p += 3;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_ONEVALUE;
        ++p;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_ROWMAJOR;
        ++p;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_IGNORECASE;
        ++p;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_SHOWDESC;
        ++p;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_NONCYCLIC;
        ++p;

        if ( TOBOOL(*p) != F )
            opts |= cct_emo_SHOWMATCH;
        ++p;

	if ( !dyn_str_PIC9(&pic9,p,2) ) {	/* PIC 9(2) for rows or 00 */
		cobcurses_trace_printf(2,"  bad format rows value '%s'\n",dyn_str_string(&pic9));
		goto bad;
	}
	opt_rows = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	p += 2;

	if ( !dyn_str_PIC9(&pic9,p,2) ) {	/* PIC 9(2) for cols or 00 */
		cobcurses_trace_printf(2,"  bad format cols value '%s'\n",dyn_str_string(&pic9));
		goto bad;
	}
	opt_cols = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	p += 2;

	if ( !dyn_str_PIC9(&pic9,p,2) )	{	/* PIC 9(2) for title_length */
		cobcurses_trace_printf(2,"  bad title length '%s'\n",dyn_str_string(&pic9));
		goto bad;
	}
	title_length = (cct_ushort) dyn_str_to_ulong(&pic9,0);
	p += 2;
	
	dyn_str_PICX(&title,p,title_length);	/* PIC X(title_length) for title */
	p += title_length;
	cobcurses_trace_printf(2,"  menu title is '%s'\n",dyn_str_string(&title));

	menu = term->api->new_menu(term);
	term->api->set_menu_title(menu,dyn_str_string(&title),menu_tattr,menu_pair);
	term->api->set_menu_coord(menu,y,x);
        term->api->set_menu_opts(menu,opts);
	if ( opt_rows || opt_cols )
		term->api->set_menu_format(menu,opt_rows,opt_cols);
	if ( menu_type == 02 )
		term->api->set_dynamic_menu(menu,dyn_str_string(&module_name),item_limit);

	/*
	 * Static menus have a list of items :
	 */
	while ( ISBOOL(*p) && ++safety < 200 ) {
		selectable = TOBOOL(*p);	/* PIC X is Y/N/X for selectable */
		++p;

		if ( !dyn_str_PIC9(&pic9,p,2) ) {
			cobcurses_trace_printf(2,"  bad item name length '%s'\n",dyn_str_string(&pic9));
			break;
		}
		name_length = dyn_str_to_ulong(&pic9,0);
		p += 2;

		dyn_str_PICX(&item,p,name_length);	/* PIC X(name_length) for item name */
		dyn_str_trim(&item);			/* Trim trailing blanks */
		p += name_length;

		cobcurses_trace_printf(2,"  item name is '%s'\n",dyn_str_string(&item));

		if ( !dyn_str_PIC9(&pic9,p,2) ) {	/* PIC 9(2) for desc length */
			cobcurses_trace_printf(2,"  bad item desc length '%s'\n",dyn_str_string(&pic9));
			break;
		}
		desc_length = dyn_str_to_ulong(&pic9,0);
		p += 2;

		dyn_str_PICX(&desc,p,desc_length); /* PIC X(desc_length) for desc */
		dyn_str_trim(&desc);
		p += desc_length;

		cobcurses_trace_printf(2,"  item desc is '%s'\n",dyn_str_string(&desc));

		term->api->new_menu_item(menu,dyn_str_string(&item),dyn_str_string(&desc));
		if ( !selectable )
			term->api->set_menu_item_sel(menu,dyn_str_string(&item),selectable);
	}

	cobcurses_trace_printf(2,"  ended menu item list with '%c' (0x%02X)\n",
		isprint(*p) ? *p : '?', (unsigned)*(unsigned char*)p);


xit:	dyn_str_clear(&name);
	dyn_str_clear(&pic9);
	dyn_str_clear(&title);
	dyn_str_clear(&item);
	dyn_str_clear(&desc);
	dyn_str_clear(&module_name);
	return menu;

bad:	if ( menu ) {
		term->api->delete_menu(menu);
		menu = 0;
	}
	cobcurses_trace_printf(2,"  bad menu.. no menu created.\n");
	goto xit;
}

#if 0
/*
 * Return a list of items :
 */
static
cct_cobol_menu_item *
internal_load_items(char *cobol_ptr) {
	cct_cobol_menu_item root, *obj;
	cct_cobol_menu_item **pnode = &root.next;
	
	*pnode = 0;

	while ( *cobol_ptr != 'X' ) {
		*pnode = obj = malloc(sizeof *obj);
		memset(obj,0,sizeof *obj);

		obj->selectable = ( *cobol_ptr == 'Y' || *cobol_ptr == 'y' ) ? T : F;
		cobol_ptr	+= 1;
		obj->name	= cobol_ptr;		/* PIC X(08) */
		obj->name_len	= 8;			/* Fixed at PIC X(8) */
		cobol_ptr	+= 8;
		obj->desc_len   = PIC9(cobol_ptr,2);	/* PIC 9(2) */
		cobol_ptr	+= 2;
		obj->desc	= cobol_ptr;
		cobol_ptr	+= obj->desc_len;	/* Point at next item */

		pnode		= &obj->next;
	}

	return root.next;
}
#endif

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/cobmenu.c,v $ */

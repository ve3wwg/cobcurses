/* terminal.c : General base class for terminal interfaces
 * Warren W. Gay VE3WWG
 * Thu Sep  6 15:42:53 2007
 * $Id: terminal.c,v 1.2 2007/09/27 15:49:26 ve3wwg Exp $
 */
#include <cobcurses.h>

#include <stdlib.h>
#include <memory.h>
#include <assert.h>

#include <terminal.h>

#ifdef USE_TERM_CURSES
#include <term_curses.h>
#endif

static cct_api_terminal *terminal_init_api(void);	/* Initializes the api struct */

static cct_api_terminal api = { 0, 0 };			/* The api struct */

/*
 * Initialize a pre-allocated cct_terminal object :
 */
cct_terminal *
terminal_init(cct_terminal *obj) {

	memset(obj,0,sizeof *obj);
	obj->id  = id_terminal;				/* Baseclass id */
	obj->api = terminal_init_api();			/* Baseclass API */
	obj->alloc = F;					/* Assume this is not allocated */
	return obj;
}

/*
 * Allocate a new cct_terminal object :
 */
cct_terminal *
terminal_new(void) {
	cct_terminal *obj = malloc(sizeof *obj);

	terminal_init(obj);
	obj->alloc = T;					/* This is an allocated object */
	return obj;
}

/*
 * Dispose of a cct_terminal object :
 */
static cct_terminal *
terminal_dispose(cct_terminal *obj) {
	if ( !obj->api )
		abort();	/* This object has been disposed of already! */
	if ( Is_False(obj->alloc) )
		obj->api = 0;	/* Mark this object as finalized if not allocated */
	return obj;
}

/*
 * Free an allocated cct_terminal object :
 */
static cct_terminal *
terminal_delete(cct_terminal *obj) {
	if ( Is_False(obj->alloc) )	
		abort();	/* It never was allocated */
	obj->api = 0;		/* Mark this object as finalized if not allocated */
	free(obj);		/* Free it */
	return 0;		/* Return nothing */
}

/*
 * Initialize the API pointers :
 */
static
cct_api_terminal *
terminal_init_api(void) {

	if ( !api._delete ) {
		memset(&api,0,sizeof api);
#ifdef USE_TERM_CURSES
		api.curses_init		= termcurs_init;
#endif
		api.dispose		= terminal_dispose;
		api._delete		= terminal_delete;
	}
	return &api;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/terminal.c,v $ */

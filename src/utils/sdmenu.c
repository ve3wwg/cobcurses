/* sdmenu.c : A COBOL dynamic menu test utility
 * Warren W. Gay VE3WWG
 * Mon Oct 29 14:52:25 2007
 * $Id: sdmenu.c,v 1.4 2007/10/30 19:56:27 ve3wwg Exp $
 *
 *	The purpose of this utility is to test the shared/dll dynamic
 *	menu modules. See lib/cobcurses_menu_sd_fields.cob as an
 *	example of what the COBOL module should look like (this one
 *	has context). A context free dynamic menu example would be
 *	lib/cobcurses_menu_sd_menus.cob.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <config.h>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

typedef int (*cct_menu_module)(const char *req_type,char *name,char *desc);

extern cct_menu_module cob_resolve(const char *module_name);

static void
usage(void) {
	puts("Usage: sdmenu [-x 'context'] { [ -m 'name' ] | module_names.. }");
	puts("where:");
	puts("\t-x 'string'\tSets the context for the menu call (optional)");
	puts("\t-m 'name'\tSpecifies the module name");
	puts("\tmodule_names\tArgs are treated the same as the -m name option");
	puts(" ");
	puts("Option -x can be specified between module names to change the");
	puts("context for each test. Specify a null string to cancel context.");
	puts(" ");
	puts("Example:");
	puts("--------");
	puts("sdmenu -x 'SD002010' SD-MENU-FIELDS");
	puts("sdmenu SD-SCREENS");
}

static int
invoke_module(const char *module,const char *context) {
	char mbuf[1024];
	char nbuf[64], dbuf[1024];
	cct_menu_module mmod;
	int needs_close = 1;
	int rc = 0;
	int mrc = 0;

	strcat(strcpy(mbuf,"COBCURSES-MENU-"),module);

	if ( context ) 
		printf("\nMODULE '%s', CONTEXT='%s' :\n\n",mbuf,context);
	else	printf("\nMODULE '%s' with no context :\n\n",mbuf);

	if ( !(mmod = cob_resolve(mbuf)) ) {
		fprintf(stderr,"Error: Unable to resolve module '%s'\n",mbuf);
		fprintf(stderr,"Check COB_LIBRARY_PATH.\n");
		rc = 1;
	} else	{
		if ( context ) {
			printf("  CALL '%s' USING 'X', '%s', OMITTED.\n",mbuf,context);
			fflush(stdout);
			memset(dbuf,' ',sizeof dbuf);
			strncpy(dbuf,context,strlen(context));
			mrc = mmod("X",dbuf,0);
			printf("    returned %d\n",mrc);
		}
		if ( !mrc ) {
			printf("  CALL '%s' USING 'O', OMITTED, OMITTED.\n",mbuf);
			fflush(stdout);
			mrc = mmod("O",0,0);
			printf("    returned %d\n",mrc);
			if ( mrc )
				needs_close = 0;
		} else	needs_close = 0;
		while ( !mrc ) {
			printf("  CALL '%s' USING 'R', NAME, DESC.\n",mbuf);
			fflush(stdout);
			mrc = mmod("R",nbuf,dbuf);
			printf("    returned %d  { '%.32s', '%.64s' }\n",mrc,nbuf,dbuf);
		}
		fflush(stdout);
		if ( needs_close ) {
			printf("  CALL '%s' USING 'C', OMITTED, OMITTED.\n",mbuf);
			fflush(stdout);
			mrc = mmod("C",0,0);
			printf("    returned %d\n",mrc);
		}
		fflush(stdout);
		if ( mrc != 0 )
			rc = 1;
	}
	fflush(stdout);
	return rc;
}

int
main(int argc,char **argv) {
	int rc = 0;
	static char opts[] = "m:x:h";
	char *context = 0;
	char *module = 0;
	int flag;
	extern void cob_init(int,int);

	cob_init(0,0);			/* Initialize COBOL libraries */

	opterr = 0;			/* We'll report our own errors */

	while ( !rc && (flag = getopt(argc,argv,opts)) != -1 ) {
		switch ( flag ) {
		case 'm' :
			module = optarg;
			if ( invoke_module(module,context) )
				rc |= 1;
			break;
		case 'x' :
			if ( *optarg )
				context = optarg;
			else	context = 0;
			break;
		case 'h' :
			usage();
			goto xit;
		case '?' :
		default :
			rc = 1;
			fprintf(stderr,"Error: Unknown option -%c\n",optopt);
		}
	}

	for ( ; !rc && optind < argc; ++optind ) {
		module = argv[optind]++;
		if ( invoke_module(module,context) )
			rc |= 1;
	}
			
	if ( !module ) {
		rc = 1;
		fprintf(stderr,"Warning: no module names were given.\n");
	}

xit:	return rc;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/utils/sdmenu.c,v $ */

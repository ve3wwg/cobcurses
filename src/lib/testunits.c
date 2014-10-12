/* testunits.c : Test a units specification string for errors
 * Warren W. Gay VE3WWG
 * Mon Sep 17 09:13:59 2007
 * $Id: testunits.c,v 1.3 2007/09/21 19:55:39 ve3wwg Exp $
 */
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <assert.h>

#include <cobcurses.h>
#include <misc.h>
#include <units.h>

/*
 * Test a unit specification string :
 *
 * 	This routine is designed mainly to be used by the screen designer.
 *	This allows verification and testing of a specification string.
 *
 * Returns :
 *	RET_OK			if string passed format parsing test.
 *	  count of options
 *	  count of unit specs
 *	
 *	RET_FAILED
 *	  1-based error offset
 *	  error message text
 */
int
COBCURSES_TEST_UNITS_STRING(
  char *	pin_units_string,	/* Input string to be processed (must have blank at end) */
  cct_ushort *	pout_count_opts,	/* Output returned count of options */
  cct_ushort *	pout_count_units,	/* Output returned count of unit specs */
  cct_ushort *  pout_error_offset,	/* Output 1-based offset where error occurred */
  char *	pout_error_msgbuf,	/* Output buffer for error message, if any */
  cct_ushort *	pin_error_msgbuflen	/* Input buffer length for message */
) {
	cct_unit_option opts;
	cct_ushort units_length = cobcurses_strlen(pin_units_string,' ');
	cct_unit_enum u;
	cct_ushort count_opts = 0;
	cct_ushort count_units = 0;
	cct_ushort emsg_buflen = *pin_error_msgbuflen;
	int rc = RET_OK;

	assert(emsg_buflen < 99 && emsg_buflen > 0);

	cobcurses_unit_option_init(&opts);
	cobcurses_unit_option_open(&opts,pin_units_string,units_length);

	*pout_count_opts = 0;
	*pout_count_units = 0;
	*pout_error_offset = 0;
	memset(pout_error_msgbuf,' ',*pin_error_msgbuflen);

	for ( u = cobcurses_unit_option_next(&opts); u != uo_null && u != uo_error;
        	u = cobcurses_unit_option_next(&opts) ) {
		switch ( u ) {
		case uo_error :
			break;	/* Should not get here */
		case uo_unit :
			++count_units;
			break;
		case uo_append :
		case uo_asis :
		case uo_ucase :
		case uo_lcase :
		default :
			++count_opts;
		}
	}	                	

	if ( u == uo_error ) {
		char *emsg = cobcurses_unit_option_value(&opts);
		cct_ushort elen = strlen(emsg);

		memcpy(pout_error_msgbuf,emsg,elen <= emsg_buflen ? elen : emsg_buflen);
		*pout_error_offset = cobcurses_unit_option_offset(&opts) + 1;
		rc = RET_FAILED;
	}

	*pout_count_opts = count_opts;		/* Return count of options found */
	*pout_count_units = count_units;	/* Return count of units found */

	cobcurses_unit_option_dispose(&opts);
	return rc;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/testunits.c,v $ */

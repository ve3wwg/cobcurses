/* CobCurses : Edit a value, possibly with units
 * Warren W. Gay
 * Tue Aug  7 10:08:10 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <units.h>
#include <misc.h>
#include <enumber.h>
#include <eunits.h>

/*
 * Process buf for buflen characters, for a possibly scientific notation
 * number, and possibly adjusting the exponent if unit characters are
 * matched :
 */
cct_bool
cobcurses_edit_units(
    cct_strvar	*numb,		/* Output: returned extracted number */
    const char	*buf,		/* Input: text buffer to parse */
    cct_ushort	buflen,		/* Input: length of buffer */
    const char	*units_config	/* Input: Units config string (term with blank) (can be NULL) */
) {
	cct_bool ok = 0;
	cct_short expon = 0;
	cct_strvar units;
	cct_unit_option opts;
	cct_unit_enum u;
	const char *unit_sym;
	int unit_exp;
	char unit_case = 0;
	cct_ushort ulen;

	dyn_str_init(&units);
	cobcurses_unit_option_init(&opts);

	if ( !cobcurses_edit_number(numb,&expon,&units,buf,buflen) )
		goto errxit;

	cobcurses_unit_option_open(&opts,
		units_config,(cct_ushort)cobcurses_strlen(units_config,' '));

	for ( u = cobcurses_unit_option_next(&opts); u != uo_null && u != uo_error;
		u = cobcurses_unit_option_next(&opts) ) {

		switch ( u ) {
		case uo_unit :
			unit_sym = cobcurses_unit_option_name(&opts);
                        unit_exp = cobcurses_unit_option_expon(&opts);
			unit_case = cobcurses_unit_case(&opts);

			ulen = strlen(unit_sym);
			assert(ulen > 0);

			switch ( unit_case ) {
			case 'L' :
				dyn_str_lcase(&units);
				break;
			case 'U' :
				dyn_str_ucase(&units);
				break;
			case 'A' :
			default :
				;
			}
			if ( !strncmp(dyn_str_string(&units),unit_sym,ulen) ) {
				expon += unit_exp;
				ok = 1;
				goto xit;
			}
			break;
		default :
			;
		}
	}

	ok = 1;

xit:	dyn_str_clear(&units);
	cobcurses_unit_option_dispose(&opts);
	
	if ( expon != 0 )
		dyn_str_appendf(numb,"E%d",(int)expon);

	return ok;

errxit:	ok = 0;
	goto xit;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/eunits.c,v $ */

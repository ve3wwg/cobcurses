/* CobCurses : Edit a value with units into a COMP-1
 * Warren W. Gay
 * Thu Aug  2 13:41:28 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <units.h>
#include <misc.h>
#include <eunits.h>

/*
 * Edit input buffer into a COMP-1 (float) value :
 */
int
NC_EDIT_COMP1(
    cct_float  	*comp1,			/* Returned: Extracted COMP-1 value */
    const char	**pin_text,		/* Input: Text to parse */
    cct_ushort	*pin_len,		/* Input: Text length */
    const char	**punits_config		/* Input: blank terminated (or null) */
) {
	const char *in_text = *pin_text;
	cct_ushort in_len = *pin_len;
	const char *units_config = *punits_config;
	cct_strvar numb;
	cct_bool ok = 0;
	float fv;

	dyn_str_init(&numb);

	ok = cobcurses_edit_units(&numb,in_text,in_len,units_config);
	if ( !ok )
		goto xit;		/* Bad value */

	fv = dyn_str_to_float(&numb,&ok);
	if ( !ok )
		goto xit;		/* Bad value */

	*comp1 = fv;			/* Return COMP-1 value */
	ok = 1;				/* Successful */

xit:	dyn_str_clear(&numb);
	return ok ? RET_OK : RET_FAILED;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/ecomp1.c,v $ */

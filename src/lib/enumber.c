/* CobCurses : Edit a number
 * Warren W. Gay
 * Tue Aug  7 10:02:36 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <units.h>
#include <misc.h>
#include <enumber.h>

/*
 * Edit a number into a number component (numb), an exponent (pexpon),
 * and units (units).
 *
 * The returned pointer is null if the format of the value is in bad
 * form.
 */
cct_strvar *
cobcurses_edit_number(
    cct_strvar	*numb,			/* Returned: Extracted number */
    cct_short	*pexpon,		/* Returned: Number's exponent */
    cct_strvar	*units,			/* Returned: Extracted units text */
    const char	*in_text,		/* Input: Text to parse */
    cct_ushort	tlen			/* Input: Text length */
) {
	const char *cp = in_text;		/* Current pointer to the text */
	const char *endp = cp + tlen;		/* Pointer to the end of the string */
	char ch;				/* Current character */
	cct_ushort digits = 0;			/* # of digits capture (not incl exponent) */
	cct_bool non_zero = 0;			/* Value is non-zero */
	cct_bool got_decpt = 0;			/* Got decimal point flag */
	cct_bool issued_decpt = 0;		/* Issued '.' flag */
	cct_strvar estr;			/* Exponent buffer */
	cct_strvar *retp = 0;			/* Return pointer */
	cct_short expon = 0;			/* Exponent value to return */

	dyn_str_clear(numb);			/* Clear returned number */
	*pexpon = 0;
	dyn_str_clear(units);			/* Clear returned number */
	dyn_str_init(&estr);			/* Clear internal exponent buffer */

	/*
	 * Skip leading blanks
	 */
	for ( ; cp < endp && (ch = *cp) != 0; ++cp )
		if ( ch != ' ' )
			break;
	if ( cp >= endp )
		goto xit;			/* No number present */
	
	/*
	 * Check for optional sign character :
	 */
	if ( (ch = *cp) == '+' || ch == '-' ) {
		if ( ch != '+' )
			dyn_str_append_ch(numb,ch);
		++cp;
	}

	/*
	 * Copy leading digits
	 */
	for ( ; cp < endp && (ch = *cp) != 0; ++cp, ++digits ) {
		if ( !isdigit(ch) ) {
			if ( ch != ',' )	/* Ignore commas */
				break;
		} else 	{
			dyn_str_append_ch(numb,ch);
			if ( ch != '0' )
				non_zero = 1;
		}
	}
	if ( cp >= endp ) {
		if ( !digits )
			goto xit;		/* No number present */
		retp = numb;			/* Integer value */
		goto xit;
	}

	/*
	 * Check for decimal point :
	 */
	if ( (ch = *cp) == '.' ) {
		got_decpt = 1;			/* Yes, got a decpt */
		++cp;
	}
	if ( cp >= endp ) {
		if ( !digits )
			goto xit;		/* No number present */
		retp = numb;			/* Integer value */
		goto xit;
	}

	/*
	 * Copy digits trailing the decimal point
	 */
	for ( ; cp < endp && (ch = *cp) != 0; ++cp, ++digits ) {
		if ( !isdigit(ch) )
			break;
		if ( !issued_decpt ) {
			dyn_str_append_ch(numb,'.');
			issued_decpt = 1;
		}
		dyn_str_append_ch(numb,ch);
		if ( ch != '0' )
			non_zero = 1;
	}
	if ( cp >= endp || !digits )
		goto xit;

	/*
	 * Check for exponent :
	 */
	ch = *cp;
	if ( ch == 'e' || ch == 'E' ) {
		char sgn = '+';

		if ( ++cp >= endp ) {
			retp = numb;		/* Error really, but got a number */
			goto xit;
		}

		/*
		 * Test for exponent sign character :
		 */
		ch = *cp++;
		if ( ch == '-' || ch == '+' )
			sgn = ch;
		else	--cp;
		dyn_str_append_ch(&estr,sgn);

		/*
		 * Copy exponent digits :
		 */
		for ( ; cp < endp && (ch = *cp) != 0; ++cp ) {
			if ( !isdigit(ch) )
				break;
			dyn_str_append_ch(&estr,ch);
		}
	}

	/*
	 * Skip trailing blanks :
	 */
	for ( ; cp < endp && (ch = *cp) != 0; ++cp )
		if ( ch != ' ' )
			break;

	/*
	 * Copy units related text :
	 */
	for ( ; cp < endp && (ch = *cp) != 0; ++cp ) {
		if ( ch == ' ' )
			break;
		else	dyn_str_append_ch(units,ch);
	}

xit:	if ( digits < 1 )
		retp = 0;			/* No number at all */
	else	retp = numb;			/* We did get a number */

	/*
	 * Convert the exponent to a signed short :
	 */
	if ( retp != 0 && dyn_str_length(&estr) > 0 ) {
		cct_long lexp;
		cct_bool ok;

		lexp = dyn_str_to_long(&estr,&ok);
		assert(ok!=0);
		if ( lexp >= MINIMUM_EE && lexp <= MAXIMUM_EE )
			expon = (cct_short) lexp;
		else	retp = 0;		/* Bad exponent range */
	}

	dyn_str_clear(&estr);			/* Dispose of buffer */

	*pexpon = expon;			/* Return exponent value */
	return retp;				/* Return edited value */
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/enumber.c,v $ */

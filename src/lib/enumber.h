/* CobCurses : Edit a number
 * Warren W. Gay
 * Tue Aug  7 10:02:36 2007
 */
#ifndef _enumber_h_
#define _enumber_h_

extern cct_strvar *
cobcurses_edit_number(
    cct_strvar	*numb,			/* Returned: Extracted number */
    cct_short	*pexpon,		/* Returned: Number's exponent */
    cct_strvar	*units,			/* Returned: Extracted units text */
    const char	*in_text,		/* Input: Text to parse */
    cct_ushort	tlen			/* Input: Text length */
);

#endif /* _enumber_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/enumber.h,v $ */

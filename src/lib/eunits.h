/* CobCurses : Edit a value, possibly with units
 * Warren W. Gay
 * Tue Aug  7 14:05:22 2007
 */
#ifndef _eunits_h_
#define _eunits_h_

cct_bool
cobcurses_edit_units(
    cct_strvar	*numb,		/* Output: returned extracted number */
    const char	*buf,		/* Input: text buffer to parse */
    cct_ushort	buflen,		/* Input: length of buffer */
    const char	*units_config	/* Input: Units config string (term with blank) (can be NULL) */
);

#endif /* _eunits_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/eunits.h,v $ */

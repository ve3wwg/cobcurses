/* codegen.h : Code generator support
 * Warren W. Gay VE3WWG
 * Wed Oct 24 10:45:38 2007
 * $Id: codegen.h,v 1.3 2007/10/30 14:26:19 ve3wwg Exp $
 */
#ifndef _codegen_h_
#define _codegen_h_

extern int
COBCURSES_SPECIAL_CHARS(
    const char		*in_text,
    cct_ushort		*in_length,
    char                *ot_buffer,
    cct_ushort          *ot_buflen
);

extern int
COBCURSES_HEX2ASCII(
    const unsigned char	*in_hex,		/* Ptr to input hex buffer */
    cct_ushort		*in_hexlen,		/* Input hex buffer length (bytes) */
    char                *ot_ascbuf,		/* Ptr to output ascii buffer */
    cct_ushort          *ot_ascbuflen		/* Output buffer length */
);

extern int
COBCURSES_SPECIAL2ASCII(
    unsigned char	*io_text,		/* Buffer to be edited */
    cct_ushort		*io_length		/* Buffer's length */
);

#endif /* _codegen_h_ */

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/codegen.h,v $ */

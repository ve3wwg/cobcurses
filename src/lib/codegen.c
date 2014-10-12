/* codegen.c : Code generator support
 * Warren W. Gay VE3WWG
 * Wed Oct 24 10:45:38 2007
 * $Id: codegen.c,v 1.4 2007/10/30 14:31:10 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <codegen.h>

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

/*
 * Check in_text for special characters. If none, return with 0.
 * A return value > 0 indicates that ot_buffer was populated with
 * hexadecimal specification of the in_text.
 */
int
COBCURSES_SPECIAL_CHARS(
    const char		*in_text,
    cct_ushort		*in_length,
    char                *ot_buffer,
    cct_ushort          *ot_buflen
) {
	const char *cp, *ep;
	char *op, tbuf[3];
	unsigned char ch;
	int rc = 0;
	
	for ( cp=in_text, ep=in_text+*in_length; cp < ep; ++cp ) {
		ch = (unsigned char)*cp;
		if ( ch < 0x20 || ch > 0x7F ) {
			rc = 1;
			break;
		}
	}

	if ( !rc )
		return 0;			/* No special characters */

	if ( !ot_buffer || *ot_buflen < *in_length * 2 )
		return 0;			/* Can't use the output buffer */	
	
	memset(ot_buffer,' ',*ot_buflen);	/* INITIALIZE ot_buffer */

	for ( cp=in_text, ep=in_text+*in_length, op=ot_buffer; cp < ep; ++cp ) {
		sprintf(tbuf,"%02X",(unsigned)*(unsigned char *)cp);
		*op++ = tbuf[0];
		*op++ = tbuf[1];
	}
	return (int)(op - ot_buffer);		/* Return output length */
}

static cct_ushort
hex_digit(const unsigned char ch) {
	static const unsigned char zero = '0';
	static const unsigned char nine = '9';
	static const unsigned char ch_A = 'A';
	static const unsigned char ch_F = 'F';
	static const unsigned char ch_a = 'a';
	static const unsigned char ch_f = 'f';
	
	if ( ch >= zero && ch <= nine )
		return ch - zero;
	if ( ch >= ch_A && ch <= ch_F )
		return (ch - ch_A) + 10u;
	if ( ch >= ch_a && ch <= ch_f )
		return (ch - ch_a) + 10u;
	return 16;				/* Error! */
}

/*
 * Convert HEX characters back to ASCII characters :
 */
int
COBCURSES_HEX2ASCII(
    const unsigned char	*in_hex,		/* Ptr to input hex buffer */
    cct_ushort		*in_hexlen,		/* Input hex buffer length (bytes) */
    char                *ot_ascbuf,		/* Ptr to output ascii buffer */
    cct_ushort          *ot_ascbuflen		/* Output buffer length */
) {
	cct_ushort inlen = *in_hexlen;		/* Input length in bytes */
	char *endp = ot_ascbuf + *ot_ascbuflen;	/* Ptr past end of output buffer */
	char *outp = ot_ascbuf;			/* Ptr to out buffer */
	cct_ushort ux, ch, ich;

	for ( ux=0; ux<inlen && outp<endp; ) {
		ch = ich = hex_digit(in_hex[ux++]);
		if ( ich >= 16 )
			return 0;		/* Error! Bad character */
		if ( ux < inlen ) {
			ich = hex_digit(in_hex[ux++]);
			if ( ich >= 16 )
				return 0; 	/* bad char */
			ch = (ch << 4) | ich;
		}
		*outp++ = ch;
	}
	return (int)(outp - ot_ascbuf);		/* Return length */
}

/*
 * Convert any special graphics character to ASCII equivalents if we can,
 * and splat out the remainder. This is intended for use in the text file
 * that is the "image" file for the generated screen.
 */
int
COBCURSES_SPECIAL2ASCII(
    unsigned char	*io_text,		/* Buffer to be edited */
    cct_ushort		*io_length		/* Buffer's length */
) {
	cct_ushort ux;
	unsigned char ch;
	
	for ( ux=0; ux < *io_length; ++ux ) {
		switch ( (ch = io_text[ux]) ) {
		case acs_ULCORNER :
		case acs_LLCORNER :
		case acs_URCORNER :
		case acs_LRCORNER :
		case acs_PLUS :
		case acs_PLMINUS :
			ch = '+';
			break;
		case acs_VLINE :
		case acs_LTEE :
		case acs_RTEE :
			ch = '|';
			break;
		case acs_BTEE :
		case acs_TTEE :
		case acs_HLINE :
		case acs_S1 :
			ch = '-';
			break;
			break;
		case acs_S9 :
			ch = '_';
			break;
		case acs_DIAMOND :
		case acs_BULLET :
			ch = '*';
			break;
		case acs_DEGREE :
			ch = 'o';
			break;
		case acs_LARROW :
			ch = '<';
			break;
		case acs_RARROW :
			ch = '>';
			break;
		case acs_DARROW :
			ch = 'v';
			break;
		case acs_UARROW :
			ch = '^';
			break;
			break;
		case acs_BLOCK :
		case acs_BOARD :
		case acs_CKBOARD :
		case acs_STERLING :
			ch = '#';
			break;
		default :
			if ( ch < ' ' )
				ch = '@';
		}
		io_text[ux] = ch;
	}

	return RET_OK;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/codegen.c,v $ */

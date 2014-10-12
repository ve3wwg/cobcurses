/* CobCurses : Edit a value with units
 * Warren W. Gay
 * Thu Aug  2 13:41:28 2007
 */
#ifndef _units_h_
#define _units_h_

#include <dynstr.h>

typedef enum {
	uo_null,	/* No entry */
	uo_error,	/* Bad format/value */
	uo_append,	/* Append option */
	uo_asis,	/* As is option */
	uo_ucase,	/* Ucase option */
	uo_lcase,	/* Lcase option */
	uo_unit		/* Unit specification */
} cct_unit_enum;

typedef struct {
	char		*txt;			/* Text being parsed */
	cct_ushort	txt_length;		/* Length of text */
	char		chcase;			/* 'A'=asis,'U'=ucased,'L'=lcase */
	cct_ushort	txt_offset;		/* Current offset in txt */
	cct_unit_enum	utype;			/* Option type */
	cct_strvar	unit_name;		/* Unit name */
	cct_strvar	unit_value;		/* Option/unit value */
	int		expon;			/* Exponent's value */
	char		range;			/* 0 or '/' if has range */
	int		low_range;		/* Low range for output exponent */
	int		high_range;		/* High range for output exponent */
} cct_unit_option;

extern cct_unit_option *cobcurses_unit_option_init(cct_unit_option *uo);
extern void cobcurses_unit_option_dispose(cct_unit_option *uo);
extern cct_unit_option *cobcurses_unit_option_open(cct_unit_option *uo,const char *input,cct_ushort in_length);
extern cct_unit_enum cobcurses_unit_option_next(cct_unit_option *uo);
extern char *cobcurses_unit_option_text(cct_unit_option *uo);
extern cct_unit_enum cobcurses_unit_option_type(cct_unit_option *uo);
extern char *cobcurses_unit_option_name(cct_unit_option *uo);
extern char *cobcurses_unit_option_value(cct_unit_option *uo);
extern int cobcurses_unit_option_expon(cct_unit_option *uo);
extern int cobcurses_unit_option_low(cct_unit_option *uo);
extern int cobcurses_unit_option_high(cct_unit_option *uo);
extern cct_ushort cobcurses_unit_option_offset(cct_unit_option *uo);
extern char cobcurses_unit_case(cct_unit_option *uo);

#endif /* _units_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/units.h,v $ */

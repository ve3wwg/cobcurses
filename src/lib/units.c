/* CobCurses : Edit a value with units
 * Warren W. Gay
 * Thu Aug  2 13:41:28 2007
 */
#include <cobcurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#include <units.h>
#include <misc.h>

/*
 *	Option format:	"-option_name"
 *		or	"-option_name=value"
 *	Unit format:	"unit_name=exponent"
 *		or	"unit_name=exponent:exponent/exponent"
 *	where:
 *		exponent is a possibly signed integer
 *
 *	Example Formats:
 *		"-ncase,m=6,k=3 "
 *		"-asis,-append=ohms,k=3,M=6 "
 *		"-ncase,p=-9,uu=-9,mm=-6,u=-3:-1/-5 "
 *
 */
static struct	{
	const char	*opt_name;
	cct_unit_enum	opt_enum;
} optnames[] = {
	{ "-append",	uo_append },
	{ "-asis",	uo_asis },
	{ "-ucase",	uo_ucase },
	{ "-lcase",	uo_lcase },
	{ 0,		uo_null }
};

/*
 * Convert text string into enum for recognized options :
 */
static
cct_unit_enum
option_enum_of(const char *text) {
	cct_ushort x;

	for ( x=0; optnames[x].opt_enum != uo_null; ++x )
		if ( !strcasecmp(text,optnames[x].opt_name) )
			return optnames[x].opt_enum;
	return uo_null;
}

/*
 * Initialize a cct_unit_option type :
 */
cct_unit_option *
cobcurses_unit_option_init(cct_unit_option *uo) {

	memset(uo,0,sizeof *uo);
	uo->txt = 0;
	uo->utype = uo_null;
	uo->chcase = 'A';
	dyn_str_init(&uo->unit_name);
	dyn_str_init(&uo->unit_value);
	return uo;
}

/*
 * Dispose of a cct_unit_option type :
 */
void
cobcurses_unit_option_dispose(cct_unit_option *uo) {

	if ( uo->txt != 0 )
		free(uo->txt);
	dyn_str_clear(&uo->unit_name);
	dyn_str_clear(&uo->unit_value);
	memset(uo,0,sizeof *uo);
	uo->utype = uo_null;
}

/*
 * (Re)Open a string for unit options processing :
 */
cct_unit_option *
cobcurses_unit_option_open(cct_unit_option *uo,const char *input,cct_ushort in_length) {

	if ( input != 0 ) {
		uo->txt = realloc(uo->txt,in_length+1);
		memcpy(uo->txt,input,in_length);
		uo->txt_length = in_length;
		uo->txt[uo->txt_length] = 0;
	} else	{
		uo->txt = 0;
		uo->txt_length = 0;
	}

	uo->txt_offset = 0;
	uo->utype = uo_null;
	dyn_str_clear(&uo->unit_name);
	dyn_str_clear(&uo->unit_value);
	return uo;
}	

/*
 * Extract a possibly signed integer :
 */
static
char *
extract_int(int *inumb,const char *cp,const char *endp) {
	char ch;
	cct_ushort digits = 0;
	cct_strvar numb;

	if ( cp >= endp )
		return 0;	/* No number */

	dyn_str_init(&numb);

	if ( ( (ch = *cp) == '-' || ch == '+' ) ) {
		++cp;
		dyn_str_append_ch(&numb,ch);
	}
	while ( cp < endp && (ch = *cp) != 0 && isdigit(ch) ) {
		dyn_str_append_ch(&numb,ch);
		++digits;
		++cp;
	}
	if ( !digits ) {
		dyn_str_clear(&numb);
		return 0;	/* No number */
	} else 	{
		cct_bool ok;
		long lv = dyn_str_to_long(&numb,&ok);
		int iv;

		dyn_str_clear(&numb);

		if ( !ok || (iv = (int)lv) != lv )
			return 0;	/* Range error */
		*inumb = iv;		/* Return int value */
	}

	return (char *)cp;		/* Return point where we left off */
}

static
char *
extract_exp(int *inumb,const char *cp,const char *endp) {
	char *rp = extract_int(inumb,cp,endp);

	if ( rp != 0 ) {
		if ( *inumb < -999 || *inumb > 999 )
			return 0;	/* Out of range for exponent */
	}
	return rp;
}

/*
 * Internal support routine :
 */
static
cct_unit_enum
cobcurses_unit_option_next_internal(cct_unit_option *uo) {
	const char *cp = uo->txt + uo->txt_offset;
	char *endp = uo->txt + uo->txt_length;
	char ch;
	cct_bool is_opt = 0;
	cct_bool have_eq = 0;

	/*
	 * Skip leading blanks and comma's :
	 */
	for ( ; cp < endp && (*cp == ' ' || *cp == ','); ++cp)
		;
	if ( cp >= endp )
		goto end_opts;

	/*
	 * Must start with alpha or '-' :
	 */
	is_opt = ( (ch = *cp) == '-' );

	if ( !isalpha(ch) && ch != '-' ) {
		uo->txt_offset = cp - uo->txt;	/* Update offset */
		cp = "ERROR: Bad format- need alpha or '-'.";
		goto err;
	} else if ( is_opt )
		dyn_str_append_ch(&uo->unit_name,*cp++);

	/*
	 * Capture unit or option name :
	 */
	for ( ; cp < endp && (ch = *cp) != 0 && isalnum(ch); ++cp )
		dyn_str_append_ch(&uo->unit_name,ch);
		
	/*
	 * Capture value, if any :
	 */
	if ( ch == '=' ) {
		have_eq = 1;
		for ( ++cp; cp < endp && (ch = *cp) != 0 && ch != ',' && ch != ':'; ++cp )
			dyn_str_append_ch(&uo->unit_value,ch);
	}

	uo->txt_offset = cp - uo->txt;		/* Update offset */

	/*
	 * Check for proper termination of option :
	 */
	if ( cp < endp && !( *cp == ',' || ( !is_opt && *cp == ':' ) ) ) {
		cp = "ERROR: Bad foramt- not at end, or ','.";
		goto err;
	}

	/*
	 * If we got a ':', extract range :
	 */
	if ( *cp == ':' ) {
		cp = extract_exp(&uo->low_range,++cp,endp);
		if ( !cp || *cp != '/' ) {
badexp:			cp = "ERROR: Bad format- exponent range.";
			goto err;
		}
		cp = extract_exp(&uo->high_range,++cp,endp);
		if ( !cp || *cp != ',' )
			goto badexp;
		uo->range = '/';		/* Range info is present */
		if ( uo->low_range > uo->high_range ) {
			int tmp = uo->high_range;

			uo->high_range = uo->low_range;
			uo->low_range  = tmp;
		}
		uo->txt_offset = cp - uo->txt;	/* Update offset */
	} else	uo->range = 0;			/* No range info present */

	/*
	 * Now process captured option :
	 */
	switch ( uo->chcase ) {
	case 'A' :				/* As is (no case change) */
		break;
	case 'U' :				/* Uppercase */
		if ( !is_opt )
			dyn_str_ucase(&uo->unit_name);
		dyn_str_ucase(&uo->unit_value);
		break;
	case 'L' :				/* Lower case */
		if ( !is_opt )
			dyn_str_lcase(&uo->unit_name);
		dyn_str_lcase(&uo->unit_value);
		break;
	}

	if ( is_opt ) {
		/* Process an option */
		uo->utype = option_enum_of(dyn_str_string(&uo->unit_name));
		if ( uo->utype == uo_null ) {
			cp = "ERROR: Unknown option name.";
			goto err;
		}
		/*
		 * Process certain options here :
		 */
		switch ( uo->utype ) {
		case uo_asis :
			uo->chcase = 'A';
			break;
		case uo_lcase :
			uo->chcase = 'L';
			break;
		case uo_ucase :
			uo->chcase = 'U';
			break;
		default :
			;
		}

		/*
		 * Check for values to options that don't take values, or
		 * missing values for other options :
		 */
		switch ( uo->utype ) {
		case uo_append :
			if ( dyn_str_length(&uo->unit_value) < 1 ) {
				cp = "ERROR: Option requires a value.";
				goto err;
			}
			break;
		case uo_asis :
		case uo_ucase :
		case uo_lcase :
			if ( dyn_str_length(&uo->unit_value) > 0 || have_eq ) {
				cp = "ERROR: Option does not take a value.";
				goto err;
			}
			break;
		default :
			abort();	/* Should not get here */
		}
	} else	{
		long lv;
		cct_bool ok;

		uo->utype = uo_unit;		/* Processing user defined units */
		lv = dyn_str_to_long(&uo->unit_value,&ok);
		if ( !ok || lv < -999 || lv > 999 ) {
			cp = "ERROR: Bad value or range of value.";
			goto err;
		}
		uo->expon = (int) lv;

		if ( !uo->range )
			uo->low_range = uo->high_range = uo->expon;
	}

xit:	return uo->utype;

end_opts:
	dyn_str_assign(&uo->unit_value,"End of options.");
	uo->utype = uo_null;
	goto xit;

err:	dyn_str_assign(&uo->unit_value,cp);
	uo->utype = uo_error;
	goto xit;
}

/*
 * Parse the next unprocessed option :
 *
 * Returns uo_null if there is no next option (end of list)
 */
cct_unit_enum
cobcurses_unit_option_next(cct_unit_option *uo) {

	/*
	 * Clear out current option :
	 */
	uo->utype = uo_null;
	dyn_str_clear(&uo->unit_name);
	dyn_str_clear(&uo->unit_value);

	if ( !uo->txt ) {
		dyn_str_assign(&uo->unit_value,"End of options.");
		return uo->utype;
	} else
		return cobcurses_unit_option_next_internal(uo);
}

/*
 * Return the text being parsed :
 */
char *
cobcurses_unit_option_text(cct_unit_option *uo) {
	return uo->txt ? uo->txt : (char *)"";
}

/*
 * Return the current option type :
 */
cct_unit_enum
cobcurses_unit_option_type(cct_unit_option *uo) {
	return uo->utype;
}

/*
 * Return the current option/unit name :
 */
char *
cobcurses_unit_option_name(cct_unit_option *uo) {
	return dyn_str_string(&uo->unit_name);
}

/*
 * Return the current option/unit value :
 */
char *
cobcurses_unit_option_value(cct_unit_option *uo) {
	return dyn_str_string(&uo->unit_value);
}

/*
 * Return the current unit value's exponent :
 */
int
cobcurses_unit_option_expon(cct_unit_option *uo) {
	return uo->expon;
}

/*
 * Return the current unit value's low exponent range :
 */
int
cobcurses_unit_option_low(cct_unit_option *uo) {
	return uo->low_range;
}

/*
 * Return the current unit value's high exponent range :
 */
int
cobcurses_unit_option_high(cct_unit_option *uo) {
	return uo->high_range;
}

/*
 * Return the current offset into the parsed text :
 */
cct_ushort
cobcurses_unit_option_offset(cct_unit_option *uo) {
	return uo->txt_offset;
}

/*
 * Return the state of the case option :
 */
char
cobcurses_unit_case(cct_unit_option *uo) {
	return uo->chcase;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/units.c,v $ */

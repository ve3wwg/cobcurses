/* CobCurses : Edit a value with units
 * Warren W. Gay
 * Thu Aug  2 13:41:28 2007
 */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#include <cobcurses.h>
#include <units.h>
#include <misc.h>

/*
 * INTERNAL : Edit a COMP-2 into a string, stripping the exponent, and
 * unnecessary trailing zeros etc.  The %G format comes close, but it
 * needs to remove trailing zeros on large integers, and remove "000"
 * from values that format as "0.0009999".
 *
 * This routine always strips the 'E+99' from the %G result, and returns
 * the exponent in the argument 'pexponent' :
 */
static
cct_strvar *
g_format(cct_strvar *v,cct_double comp2,cct_ushort digits,cct_int *pexponent,cct_bool prune) {
	char *tbuf;				/* Temp buffer (in v) */
	char *ee;				/* Ptr to 'E' in exponent */
	cct_ushort tlen;			/* Temp length */
	cct_ushort doff = comp2 < 0 ? 1 : 0;	/* Decimal offset */
	cct_int eadj = 0;			/* Exponent adjustment */

	dyn_str_sprintf(v,"%.*G",(unsigned)digits,comp2);
	tbuf = dyn_str_string(v);		/* Ptr to the text */
	tlen = dyn_str_length(v);		/* It's length */

	if ( !(ee = strchr(tbuf,'E')) ) {
		/*
		 * No exponent came out in this edit :
		 *
		 * Now remove trailing zeros, if the integer has any.
		 * Note that %G removed trailing zeros after the decimal already.
		 */
		while ( dyn_str_length(v) > 1 && *dyn_str_right(v,1) == '0' ) {
			++eadj;
			dyn_str_truncate(v,dyn_str_length(v)-1);
		}

		/*
		 * A number could also start 0.000999 or -0.000999
		 * Note: %g format guarantees E format if exponent < 4.
		 */
		tlen = dyn_str_length(v);
		if ( tlen > doff + 1 + 1 + 3 && !strncmp(dyn_str_string(v)+doff,"0.000",5) ) {
			dyn_str_delete(v,doff+1+1,3);
			eadj -= 3;
		} else if ( prune &&
		      tlen > doff + 1 + 1 + 2 && !strncmp(dyn_str_string(v)+doff,"0.00",4) ) {
			dyn_str_delete(v,doff+1+1,2);
			eadj -= 2;
		} else if ( prune &&
		      tlen > doff + 1 + 1 + 1 && !strncmp(dyn_str_string(v)+doff,"0.0",3) ) {
			dyn_str_delete(v,doff+1+1,1);
			eadj -= 1;
		}

		/*
		 * Append an exponent, if one is required :
		 */
		*pexponent = eadj;		/* pass back the exponent */
	} else	{
		/*
		 * We have an exponent starting at ee :
		 */
		*pexponent = atoi(ee+1);	/* Extract its value */
		dyn_str_truncate(v,ee-tbuf);	/* Remove the 'E+99' from the string */
	}

	return v;
}

/*
 * Apply some minor reformat tweaks, depending upon the exponent
 * and tolerance for trailing zeros :
 */
static
cct_strvar *
g_reformat(cct_strvar *v,cct_int *expon,cct_short n) {
	char zeros[32];

	if ( n > 0 && *expon > 0 && *expon <= n ) {
		memset(zeros,'0',n);
		zeros[*expon] = 0;
		dyn_str_append(v,zeros);
		*expon = 0;
	}
	if ( *expon != 0 )
		dyn_str_appendf(v,"E%+02d",*expon);
	return v;
}

/*
 * Format a COMP-2 item into exponential format (optionally in engineering
 * format), with or without units :
 */
int
NC_FORMAT_COMP2(
    cct_double	*pcomp2,		/* Input: Number to format */
    char	*pengineering_fmt,	/* Input: 'Y' if to use engineering format */	
    cct_ushort	*pdigits,		/* Input: N significant digits */
    const char	**punits_config,	/* Input: Units config string or NULL */
    char	**pbuf,			/* Output: Buffer for results */
    cct_ushort	*pbuflen		/* Input: Buffer length */
) {
	cct_double comp2 = *pcomp2;			/* Value to format result for */
	cct_bool eng_flag = TOBOOL(*pengineering_fmt);	/* T if we want engineering format */
	cct_ushort digits = *pdigits;			/* # of significant digits */
	const char *units_config = *punits_config;	/* Units config string */
	char *cobol_buf = *pbuf;			/* Receiving COBOL buffer */
	cct_ushort cobol_buflen = *pbuflen;		/* Receiving COBOL buflen */
	cct_bool got_best = 0;				/* T if we matched a unit */
	cct_short best_exp = 0;				/* Best exponent value */
	cct_int expon;					/* Exponent's value */
	cct_int low, high;				/* Low/High exp value of unit */
	const char *unit_sym;				/* Symbol for this unit */
	cct_unit_option opts;				/* Options processor */
	cct_unit_enum u;				/* Options iterator */
	int unit_exp = 0;				/* Exponent for unit */
	cct_strvar best_unit;				/* Best unit (symbol) to use */
	cct_strvar unit_name;				/* Name of the units (eg "ohms") */
	cct_short diff_exp;				/* abs() diff of nearest exponent */
	cct_strvar nb_unit;				/* Next best unit selected */
	cct_short nb_exp = 9999;			/* Next best exponent value */
	cct_strvar tmpbuf;				/* Numeric work buffer */
	char *tbuf;					/* tmpbuf.string() */
	cct_ushort tlen;				/* tmpbuf.length() */
	cct_short zallow = 6;				/* How many zeros to allow */

	/*
	 * Initialize dynamic objects an the COBOL receiving buffer :
	 */
	dyn_str_init(&tmpbuf);
	dyn_str_init(&best_unit);
	dyn_str_init(&nb_unit);
	dyn_str_init(&unit_name);
	cobcurses_unit_option_init(&opts);
	memset(cobol_buf,' ',cobol_buflen);

	/*
	 * Try to avoid truncation when the calling user supplies
	 * *small* buffers :
	 *
	 * By default we allow up to 6 zeros to be right padded to
	 * an integer, but for smaller buffers, we scale back (below):
	 */
	if ( digits < 8 || cobol_buflen < 10 )
		zallow = 3;
	if ( digits < 6 || cobol_buflen < 8 )
		zallow = 2;
	if ( digits < 5 || cobol_buflen < 6 )
		zallow = 1;
	if ( digits < 4 || cobol_buflen < 5 )
		zallow = 0;

	/*
	 * Use a modified %G format to eliminate ugly representation :
	 */
	g_format(&tmpbuf,comp2,digits,&expon,T);

	/*
	 * Start processing the options string (if any) :
	 * Note: No processing occurs here if units_config is null.
	 */
	diff_exp = abs(expon);				/* Initial difference */
	cobcurses_unit_option_open(
		&opts,
		units_config,
                units_config ? (cct_ushort)cobcurses_strlen(units_config,' ') : (cct_ushort) 0);

	for ( u = cobcurses_unit_option_next(&opts); u != uo_null && u != uo_error;
		u = cobcurses_unit_option_next(&opts) ) {

		switch ( u ) {
		case uo_append :
			/*
			 * Name to append to value (like "ohms") :
			 */
			dyn_str_assign(&unit_name,cobcurses_unit_option_value(&opts));
			break;

		case uo_unit :
			/*
			 * A unit specification like "p" in pF (picofarads), or "k" in "kohms"
			 */
			unit_sym = cobcurses_unit_option_name(&opts);	/* Unit symbol */
                        unit_exp = cobcurses_unit_option_expon(&opts);	/* Unit exponent */
			low = cobcurses_unit_option_low(&opts);		/* Lowest exponent */
			high = cobcurses_unit_option_high(&opts);	/* Highest exponent */

			assert(unit_sym && *unit_sym != 0);
			if ( !got_best ) {
				/*
				 * No best value detected yet, so this will do:
				 * (only the first "best" encountered is selected)
				 */
				if ( expon >= low && expon <= high ) {
					/*
					 * This value falls in range :
					 */
					dyn_str_assign(&best_unit,unit_sym);
					best_exp  = unit_exp;
					got_best = 1;
				} else if ( abs(unit_exp - expon) < diff_exp ) {
					/*
					 * This is not in range, but the exponent brings
					 * us closer -- consider this the "next best" :
					 */
					dyn_str_assign(&nb_unit,unit_sym);
					nb_exp = unit_exp;
					diff_exp = abs(unit_exp - expon);
				}
			}
			break;
		default :
			;
		}
	}

	/*
	 * If we didn't find a "best" unit, then apply the "next best" :
	 */
	if ( !got_best && dyn_str_length(&nb_unit) > 0 ) {
		dyn_str_assign(&best_unit,dyn_str_string(&nb_unit));
		best_exp = nb_exp;
	}

	if ( best_exp != 0 ) {
		/*
		 * See if the exponent goes away if we use the units selected :
		 */
		g_format(&tmpbuf,comp2 * pow(10,-best_exp),digits,&expon,F);
		g_reformat(&tmpbuf,&expon,zallow);
	} else	{
		/*
		 * This value either has no suitable units, or has no exponent.
		 * Reformat it again, for returning to the caller :
		 */
		g_format(&tmpbuf,comp2,digits,&expon,F);
		if ( expon != 0 ) {
			if ( eng_flag ) {
				cct_int emod = expon % 3;

				if ( emod < 0 )
					emod += 3;
				if ( emod ) {
					dyn_str_append(&tmpbuf,emod==1?"0":"00");
					expon -= emod;
				}
				g_reformat(&tmpbuf,&expon,zallow-emod);
			} else
				g_reformat(&tmpbuf,&expon,zallow);
		}
	}

	/*
	 * If a "unit" was selected, append it after the value :
	 */
	if ( dyn_str_length(&best_unit) > 0 )
		dyn_str_append_s(&tmpbuf,&best_unit);

	/*
	 * If the options indicate a unit name to append (like ohms), then do
	 * that now :
	 */
	if ( dyn_str_length(&unit_name) > 0 )
		dyn_str_append_s(&tmpbuf,&unit_name);

	/*
	 * Transfer our edited result to the caller's COBOL buffer :
	 */
	tbuf = dyn_str_string(&tmpbuf);		/* Ptr to the text */
	tlen = dyn_str_length(&tmpbuf);		/* It's length */
	memcpy(cobol_buf,tbuf,cobol_buflen > tlen ? tlen : cobol_buflen);

	/*
	 * Dispose of dynamic objects :
	 */
	dyn_str_clear(&tmpbuf);
	dyn_str_clear(&best_unit);
	dyn_str_clear(&unit_name);
	dyn_str_clear(&nb_unit);
	cobcurses_unit_option_dispose(&opts);

	return tlen <= cobol_buflen ? RET_OK : RET_TRUNCATED;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/fcomp2.c,v $ */

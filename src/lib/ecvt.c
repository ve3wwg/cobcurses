/* CobCurses : Edit double/float types into exponential display forms
 * Warren W. Gay
 * Wed Aug  1 09:16:50 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <string.h>

/*
 * CONVERT COMP-2 INTO EXPONENTIAL FORMAT TEXT :
 */
int
NC_ECVT_COMP2(
    double	*pcomp2,	/* Pointer to input COMP-2 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    cct_short	*pexpon		/* Pointer to cct_ushort to receive exponent */
) {
	char *bufp = (*(char **)pbuffer) + 1;	/* Where the digits will go */
	int digits = (int)*pbuflen - 1;		/* Reserve first char for sign */
	int decpt, sgn;
	char *cp;

	if ( !pbuffer || !*pbuffer || !pbuflen || *pbuflen < 1 || !pexpon )
		return RET_BADPARM;

	decpt = sgn = 0;
	cp = ecvt(*pcomp2,digits,&decpt,&sgn);
	memcpy(bufp,cp,(size_t)digits);
	bufp[-1] = !sgn ? '+' : '-';
	*pexpon = (cct_short)decpt;

	return RET_OK;
}

/*
 * CONVERT COMP-1 INTO EXPONENTIAL FORMAT TEXT :
 */
int
NC_ECVT_COMP1(
    float	*pcomp1,	/* Pointer to input COMP-1 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    cct_short	*pexpon		/* Pointer to cct_ushort to receive exponent */
) {
	double comp2 = (double) *pcomp1;

	return NC_ECVT_COMP2(&comp2,pbuffer,pbuflen,pexpon);
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/ecvt.c,v $ */

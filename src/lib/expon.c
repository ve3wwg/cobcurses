/* CobCurses : Edit double/float types into exponential display forms
 * Warren W. Gay
 * Wed Aug  1 09:16:50 2007
 */
#include <cobcurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>

/*
 * CONVERT COMP-2 INTO EXPONENTIAL FORMAT WITH/WITHOUT ENGINEERING EXPONENTS :
 */
int
NC_SCINOTATION_COMP2(
    double	*pcomp2,	/* Pointer to input COMP-2 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    const char	*peng_flag,	/* Pointer to 'Y' if to use Engineering exponents */
    cct_short   *pexpon,	/* Pointer to receiving exponent */
    cct_ushort  *E_offset	/* Poitner to offset var for 'E' (returned) */
) {
	char *pbuf = *pbuffer + 1;
	cct_ushort digits;
	cct_short expon;
	cct_bool eng_flag = TOBOOL(*peng_flag);
	short emod = 0;
	char tmp[32];
	int rc;

	if ( !pbuflen || *pbuflen < 8 || !pcomp2 || !pbuffer || !*pbuffer )
		return RET_BADPARM;
		
	if ( pexpon )
		*pexpon = 0;

	digits = *pbuflen - 5;
	
	rc = NC_ECVT_COMP2(pcomp2,&pbuf,&digits,&expon);
	if ( rc != RET_OK ) {
		memset(*pbuffer,'*',(size_t)*pbuflen);
		return rc;
	}

	--expon;
	if ( eng_flag ) {
		if ( (emod = expon % 3) < 0 )
			emod = 3 + emod;
		if ( emod == 1 )
			--expon;
		else if ( emod == 2 )
			expon -= 2;
	}

	pbuf[-1] = pbuf[0];			/* Sign */
	pbuf[0] = pbuf[1];			/* Digit 1 */

	switch ( emod ) {
	case 0 :
		pbuf[1] = '.';
		break;
	case 1 :
		pbuf[1] = pbuf[2];		/* Digit 2 */
		pbuf[2] = '.';
		break;
	case 2 :
		pbuf[1] = pbuf[2];		/* Digit 2 */
		pbuf[2] = pbuf[3];		/* Digit 3 */
		pbuf[3] = '.';
	}

	if ( expon > 999 || expon < -999 ) {
		memset(*pbuffer,'*',(size_t)*pbuflen);
		if ( E_offset != 0 )
			*E_offset = 0;		/* There is no 'E' */
	} else if ( expon >= -99 ) {
		sprintf(tmp,"E%03d",(int)expon);
		memcpy(pbuf+digits,tmp,strlen(tmp));
		if ( expon >= 0 && expon < 100 )
			pbuf[digits+1] = '+';
		if ( E_offset != 0 )
			*E_offset = digits + 2;	/* Return offset to 'E' */
	} else	{
		sprintf(tmp,"E%04d",(int)expon);
		memcpy(pbuf+digits-1,tmp,strlen(tmp));
		if ( E_offset != 0 )
			*E_offset = digits + 1;	/* Return offset to 'E' */
	}

	if ( pexpon )
		*pexpon = (short)expon;

	return rc;
}

/*
 * CONVERT COMP-2 INTO EXPONENTIAL FORMAT WITH/WITHOUT ENGINEERING EXPONENTS :
 */
int
NC_SCINOTATION_COMP1(
    float	*pcomp1,	/* Pointer to input COMP-1 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    const char	*peng_flag,	/* Pointer to 'Y' if to use Engineering exponents */
    cct_short	*pexpon,	/* Pointer to receiving exponent value */
    cct_ushort   *E_offset	/* Poitner to offset var for 'E' (returned) */
) {
	double comp2 = (double)*pcomp1;

	return NC_SCINOTATION_COMP2(&comp2,pbuffer,pbuflen,peng_flag,pexpon,E_offset);
}

/* End */

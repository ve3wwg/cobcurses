/* CobCurses : pathname edit routines for OpenCobol
 * Warren W. Gay
 * Wed Jul 25 10:05:09 2007
 */
#include <cobcurses.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>

#include <misc.h>
#include <dynstr.h>
#include <environ.h>

/*
 * Return the index and variable name length of a substitutable
 * value. If *pvarlen is returned as zero, there is no more
 * substitutions left to be performed.
 */
static cct_ushort
find_subst(const char *s,cct_ushort *pvarlen) {
	const char *cp, *start, *end, *env, *rp, *resume;
	cct_ushort varlen;

	resume = s;
	do	{
		/* Skip to '$' */
		for ( cp = resume; *cp && (*cp != '$' || cp[1] != '{'); ++cp );
		if ( *cp == '$' ) {
			rp = cp;
			resume = start = ( cp += 2 );
			end = strchr(start,'}');
			if ( end != 0 ) {
				resume = end + 1;
				cp = end + 1;
				*pvarlen = varlen = end - start;
				if ( varlen > 0 ) {
					char *envname = ALLOCA(varlen + 1);

					strncpy(envname,start,varlen);
					envname[varlen] = 0;
					/*
					 * See if the variable is defined :
					 */
					if ( (env = cobcurses_getenv(envname)) != 0 )
						return rp - s;
					FREEA(envname);
				}
			}
		}
	} while ( cp && *cp );

	*pvarlen = 0;
	return 0;
}

/*
 * Perform one substitution, if necessary. Returns zero if
 * there were no substitutions possible or performed. 1 if
 * a substitution occurred.
 */
static int
do_subst(cct_strvar *v) {
	cct_ushort varlen = 0;
	cct_ushort offset = find_subst(dyn_str_string(v),&varlen);

	if ( !varlen )
		return 0;
	else	{
		cct_strvar left, right, varname;

		dyn_str_init(&left);
		dyn_str_init(&right);
		dyn_str_init(&varname);
		dyn_str_assign_substr(&left,dyn_str_string(v),offset);
		dyn_str_assign_substr(&varname,dyn_str_string(v)+offset+2,varlen);
		dyn_str_assign(&right,dyn_str_string(v)+offset+2+varlen+1);
		dyn_str_assign(v,dyn_str_string(&left));
		dyn_str_append(v,cobcurses_getenv(dyn_str_string(&varname)));
		dyn_str_append(v,dyn_str_string(&right));
	}
	
	return 1;
}

/*
 * Perform ${VAR} substitutions on pathname, until no
 * more ${VAR} occurences remain
 */
int
cobcurses_substitute(char *pathname,cct_ushort pathlen) {
	cct_strvar s;
	cct_ushort flen, tlen;
	int rc = RET_OK;

	tlen = (cct_ushort) cobcurses_trim_trailing((char *)pathname,pathlen,' ');
	dyn_str_assign_substr(dyn_str_init(&s),(const char *)pathname,tlen);

	while ( do_subst(&s) != 0 )
		;

	flen = dyn_str_length(&s);
	memcpy(pathname,dyn_str_string(&s),flen > pathlen ? pathlen : flen);
	if ( flen < pathlen )
		memset(pathname+flen,' ',pathlen-flen);
	else if ( flen > pathlen )
		rc = RET_TRUNCATED;

	dyn_str_clear(&s);
	return rc;
}

/*
 * Substitute in the user's pathname any values of the form
 * ${VARIABLE}, until none remain. Variables that are undefined
 * will be left as is.
 */
int
NC_PATHNAME(
  char		**ppathname,	/* Input/Output pathname buffer NC-PATHNAME */
  cct_ushort	**ppathlen	/* Length of the buffer */
) {
	char *pathname = *ppathname;
        cct_ushort pathlen = **ppathlen;
	int rc;

	rc = cobcurses_substitute(pathname,pathlen);
	if ( rc != 0 )
		return rc;

#ifdef BUILD_MINGW
	/*
	 * The MinGW environment needs "nul" or "NUL" to operate correctly.
	 * However, under MSYS, paths hardcoded as /dev/null do not get
	 * translated (in libcobcurses_codegen), so we must do it for the
	 * caller:
	 */
	if ( pathlen == 9 && !strncmp(pathname,"/dev/null",pathlen) ) {
		memset(pathname,' ',pathlen);
		strncpy(pathname,"NUL ",4);
	}
#endif
	return rc;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/pathname.c,v $ */

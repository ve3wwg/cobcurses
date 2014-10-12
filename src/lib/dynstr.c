/* CobCurses : pathname edit routines for OpenCobol
 * Warren W. Gay
 * Wed Jul 25 10:05:09 2007
 */
#include <cobcurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <dynstr.h>
#include <misc.h>
#include <errno.h>
#include <stdarg.h>

/* 
 * Don't use the strtof() function:
 * Linux includes the prototype in stdlib.h only if __USE_ISOC99 is defined, but
 * what happens on other platforms?  Just use the strtod() function instead here.
 */
#undef HAVE_STRTOF

/*
 * Initialize a string variable :
 */
cct_strvar *
dyn_str_init(cct_strvar *v) {
	memset(v,0,sizeof *v);
	return v;
}

/*
 * Clear a string variable to the null string :
 */
cct_strvar *
dyn_str_clear(cct_strvar *v) {
	if ( v->str )
		free(v->str);
	memset(v,0,sizeof *v);
	return v;
}

/*
 * Uppercase a string variable :
 */
cct_strvar *
dyn_str_ucase(cct_strvar *v) {
	if ( v->str )
		cobcurses_ucase((char *)(v->str),strlen((char *)(v->str)));
	return v;
}

/*
 * Lowercase a string variable :
 */
cct_strvar *
dyn_str_lcase(cct_strvar *v) {
	if ( v->str )
		cobcurses_lcase((char *)(v->str),strlen((char *)(v->str)));
	return v;
}

/*
 * Lowercase a string variable :
 */
cct_strvar *
dyn_str_trim(cct_strvar *v) {
	cct_ushort x;

	if ( v->str && v->slen > 0 ) {
		for ( x=v->slen; x > 0 && v->str[--x] == ' '; ) 
			v->str[v->slen = x] = 0;
	}
	return v;
}

/*
 * Assign a substring of a C string to a string variable :
 */
cct_strvar *
dyn_str_assign_substr(cct_strvar *v,const char *string,cct_ushort slen) {
	static const cct_ushort assign_mod = 64;
	cct_ushort slop = v->slen % assign_mod;

	dyn_str_clear(v);
	v->slen = slen;
	v->alen = (v->slen + 1) + slop + assign_mod;
	v->str  = malloc(v->alen);
	memcpy(v->str,string,v->slen);
	v->str[v->slen] = 0;
	return v;
}

/*
 * Delete n characters starting at offset :
 */
cct_strvar *
dyn_str_delete(cct_strvar *v,cct_ushort offset,cct_ushort n) {

	if ( offset + n > v->slen )
		abort();
	if ( offset + n == v->slen )
		return dyn_str_truncate(v,offset);

	memmove(v->str+offset,v->str+offset+n,v->slen-offset-n);
	v->slen -= n;
	v->str[v->slen] = 0;
	return v;
}

/*
 * Return a C-string from a string variable :
 */
char *
dyn_str_string(cct_strvar *v) {
	return v->str ? (char *) v->str : (char *) "";
}

/*
 * Return the rightmost n characters :
 */
char *
dyn_str_right(cct_strvar *v,cct_ushort n) {

	if ( v->slen < n )
		abort();
	return dyn_str_string(v) + v->slen - n;
}

/*
 * Assign a C-string to a string variable :
 */
cct_strvar *
dyn_str_assign(cct_strvar *v,const char *string) {
	return dyn_str_assign_substr(v,string,strlen(string));
}

/*
 * Truncate a string to a new smaller length :
 */
cct_strvar *
dyn_str_truncate(cct_strvar *v,cct_ushort new_length) {
	if ( new_length > v->slen )
		abort();
	v->slen = new_length;
	if ( v->str )
		v->str[v->slen] = 0;
	return v;
}

/*
 * Append a C-string to a string variable :
 */
cct_strvar *
dyn_str_append(cct_strvar *v,const char *string) {
	static const cct_ushort assign_mod = 64;
	cct_ushort slen = strlen(string);
	cct_ushort newlen = v->slen + slen;
	cct_ushort slop = v->slen % assign_mod;

	if ( v->alen <= newlen ) {
		v->alen = (newlen + 1) + slop + assign_mod;
		v->str = realloc(v->str,v->alen);
	}
	memcpy(v->str + v->slen, string, slen);
	v->slen = newlen;
	v->str[v->slen] = 0;
	return v;
}

/*
 * Append from another cct_strvar :
 */
cct_strvar *
dyn_str_append_s(cct_strvar *v,cct_strvar *string) {
	return dyn_str_append(v,dyn_str_string(string));
}

/*
 * Append a n bytes of a C-string to a string variable :
 */
cct_strvar *
dyn_str_append_n(cct_strvar *v,const char *string,cct_ushort n) {
	static const cct_ushort assign_mod = 64;
	cct_ushort newlen = v->slen + n;
	cct_ushort slop = v->slen % assign_mod;

	if ( v->alen <= newlen ) {
		v->alen = (newlen + 1) + slop + assign_mod;
		v->str = realloc(v->str,v->alen);
	}
	memcpy(v->str + v->slen, string, n);
	v->slen = newlen;
	v->str[v->slen] = 0;
	return v;
}

/*
 * Append 1 character :
 */
cct_strvar *
dyn_str_append_ch(cct_strvar *v,const char ch) {
	return dyn_str_append_n(v,&ch,1);
}

/*
 * Return the length of a string variable :
 */
cct_ushort
dyn_str_length(cct_strvar *v) {
	return v->slen;
}

/*
 * Convert v to a signed long :
 */
cct_long
dyn_str_to_long(cct_strvar *v,cct_bool *okflag) {
	long lv = 0L;
	char *ep = 0;
	cct_bool ok = 0;

	if ( !v->str || dyn_str_length(v) < 1 )
		goto err;

	errno = 0;
	lv = strtol(dyn_str_string(v),&ep,10);
	if ( errno == ERANGE )
		goto err;

	/* Ignore trailing blanks, if any */
	for ( ; ep && *ep == ' '; ++ep ) ;
	ok = ( ep && *ep != 0 ) ? 0 : 1;

xit:	if ( !ok )
		lv = 0L;
	if ( okflag )
		*okflag = ok;
	return lv;

err:	ok = 0;
	goto xit;
}

/*
 * Convert v to a unsigned long :
 */
cct_ulong
dyn_str_to_ulong(cct_strvar *v,cct_bool *okflag) {
	long ulv = 0;
	char *ep = 0;
	cct_bool ok = 0;

	if ( !v->str || dyn_str_length(v) < 1 )
		goto err;

	errno = 0;
	ulv = strtoul(dyn_str_string(v),&ep,10);
	if ( errno == ERANGE )
		goto err;

	/* Ignore trailing blanks, if any */
	for ( ; ep && *ep == ' '; ++ep ) ;
	ok = ( ep && *ep != 0 ) ? 0 : 1;

xit:	if ( !ok )
		ulv = 0;
	if ( okflag )
		*okflag = ok;
	return ulv;

err:	ok = 0;
	goto xit;
}

/*
 * Convert v to a double :
 */
cct_double
dyn_str_to_double(cct_strvar *v,cct_bool *okflag) {
	double dv = 0.0;
	char *ep = 0;
	cct_bool ok = 0;
	
	if ( !v->str || dyn_str_length(v) < 1 )
		goto err;

	errno = 0;
	dv = strtod(dyn_str_string(v),&ep);
	if ( errno == ERANGE )
		goto err;

	/* Ignore trailing blanks, if any */
	for ( ; ep && *ep == ' '; ++ep ) ;
	ok = ( ep && *ep != 0 ) ? 0 : 1;

xit:	if ( !ok )
		dv = 0.0;
	if ( okflag )
		*okflag = ok;
	return dv;

err:	ok = 0;
	goto xit;
}

cct_double
dyn_str_to_float(cct_strvar *v,cct_bool *okflag) {
	float fv = 0.0;
	char *ep = 0;
	cct_bool ok = 0;
	const char *s = 0;
	
	if ( !v->str || dyn_str_length(v) < 1 )
		goto err;

	errno = 0;
	s = dyn_str_string(v);
#ifdef HAVE_STRTOF
	fv = strtof(s,&ep);
#else
	fv = (float) strtod(s,&ep);
#endif
	if ( errno == ERANGE )
		goto err;

	/* Ignore trailing blanks, if any */
	for ( ; ep && *ep == ' '; ++ep ) ;
	ok = ( ep && *ep != 0 ) ? 0 : 1;

xit:	if ( !ok )
		fv = 0.0;
	if ( okflag )
		*okflag = ok;
	return fv;

err:	ok = 0;
	goto xit;
}

static
cct_strvar *
dyn_str_appendv(cct_strvar *v,const char *format,va_list ap) {
	char buf[2048];

	vsnprintf(buf,sizeof buf-1,format,ap);
	buf[sizeof buf-1] = 0;
	return dyn_str_append(v,buf);
}

cct_strvar *
dyn_str_appendf(cct_strvar *v,const char *format,...) {
	va_list ap;

	va_start(ap,format);
	dyn_str_appendv(v,format,ap);
	va_end(ap);
	return v;
}

cct_strvar *
dyn_str_sprintf(cct_strvar *v,const char *format,...) {
	va_list ap;

	va_start(ap,format);
	dyn_str_clear(v);
	dyn_str_appendv(v,format,ap);
	va_end(ap);
	
	return v;
}

/*
 * Return offset into v where ch occurs, else -1 :
 */
int
dyn_str_strchr(cct_strvar *v,char ch) {
	char *s = dyn_str_string(v);
	char *rp = strchr(s,ch);

	if ( !rp )
		return -1;
	return (int) ( rp - s );
}

/*
 * Load PIC 9(n) into v, returning T if the digits
 * were all numeric, else returns F.
 */
cct_bool
dyn_str_PIC9(cct_strvar *v,char *str,cct_ushort n) {
	cct_ushort ux;
	int ch;

	dyn_str_assign_substr(v,str,n);
	for ( ux=0; ux<n; ++ux ) {
		ch = str[ux];
		if ( !isdigit(ch) )
			return F;
	}
	return T;
}

/*
 * Allow the caller to steal our dynamically allocated
 * string :
 *
 * Note: This effectively leaves the object in a dyn_str_clear()
 *       state.
 */
char *
dyn_str_steal(cct_strvar *v) {
	char *rval = (char *)v->str;

	if ( !rval )
		rval = strdup("");
	v->str = 0;
	v->slen = v->alen = 0;
	return rval;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/dynstr.c,v $ */

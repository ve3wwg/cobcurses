/* CobCurses : Misc utility routines for CobCurses
 * Warren W. Gay
 * Wed Jul 18 09:59:41 2007
 *
 * $Id: misc.c,v 1.16 2007/10/18 21:12:53 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <misc.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * Determine the switch character from the pathname, if possible,
 * else assume the given default. This allows code that runs in
 * both MSYS and MinGW environments to dynamically adjust :
 */
char
cobcurses_switchchar(const char *pathname,cct_ushort slen,char swdef) {
	const char *endp = pathname + slen;
	const char *cp;
	char ch;

	for ( cp = pathname; cp < endp; ++cp )
		if ( (ch = *cp) == '/' || ch == '\\' )
			return ch;		/* Found it in the pathname */
	return swdef;				/* Assume this default */
}

/*
 * Return a temporary null terminated string of n chars :
 */
char *
cobcurses_strn(const char *sbuf,size_t slen) {
	static char *buf = 0;

	buf = realloc(buf,slen + 1);
	memcpy(buf,sbuf,slen);
	buf[slen] = 0;
	return buf;
}

/*
 * Return a temporary null terminated string between
 * two char pointers :
 */
char *
cobcurses_strp(const char *sbuf,const char *endp) {
	size_t slen = endp - sbuf;

	return cobcurses_strn(sbuf,slen);
}

/*
 * Uppercase a string buffer :
 */
char *
cobcurses_ucase(char *sbuf,size_t slen) {
	char *cp, ch;

	for ( cp=sbuf; slen > 0; ++cp, --slen ) {
		ch = *cp;
		if ( islower(ch) )
			*cp = toupper(ch);
	}
	return sbuf;
}

/*
 * Uppercase a string buffer :
 */
char *
cobcurses_lcase(char *sbuf,size_t slen) {
	char *cp, ch;

	for ( cp=sbuf; slen > 0; ++cp, --slen ) {
		ch = *cp;
		if ( isupper(ch) )
			*cp = tolower(ch);
	}
	return sbuf;
}

/*
 * Trim trailing characters from a buffer :
 * (Return length of string minus trailing characters)
 */
size_t
cobcurses_trim_trailing(const char *buffer,size_t buflen,char blank) {
	size_t x;

	for ( x = buflen-1; x > 0; --x )
		if ( buffer[x] != blank )
			return x+1;
	return buffer[0] == blank ? 0 : 1;
}

/*
 * Return the string length based upon a delimiter (other than 0) :
 */
size_t
cobcurses_strlen(const char *buffer,char delim) {
	int delim_ch = (int) delim;
	const char *cp = strchr(buffer,delim_ch);

	if ( !cp )
		return 0;
	return (size_t)( cp - buffer );
}

/*
 * Return the string length based upon a delimiter (other than 0),
 * but search for a maximum of buflen characters :
 */
size_t
cobcurses_nstrlen(const char *buffer,size_t buflen,char delim) {
	const char *ep = buffer + buflen;
	const char *cp;

	for ( cp = buffer; cp < ep; ++cp )
		if ( *cp == delim )
			return (size_t)( cp - buffer );
	return buflen;
}

/*
 * Do a strchr() search, stopping after a max of buflen chars :
 */
char *
cobcurses_strnchr(const char *buffer,size_t buflen,char ch) {
	const char *cp = buffer;
	const char *endp = buffer + buflen;

	for ( ; cp < endp; ++cp )
		if ( *cp == ch )
			return (char *)cp;
	return 0;
}

/*
 * Perform a strchr() with an end pointer :
 */
char *
cobcurses_strnchrp(const char *buffer,const char *endp,char ch) {
	size_t slen = endp - buffer;

	return cobcurses_strnchr(buffer,slen,ch);
}

/*
 * Eat the tail end of a string, replacing with blanks :
 */
size_t
cobcurses_eat_tail(char *buffer,size_t start_offset,char eat) {
	char *cp = buffer + start_offset;

	for ( ; cp >= buffer; --cp )
		if ( *cp == eat )
			*cp = ' ';
		else
			return (size_t) (cp - buffer);
	return 0;
}

/*
 * Append a string, truncating if necessary, not to exceed
 * the buffer's length limit :
 */
size_t
cobcurses_append_trunc(char *buf,size_t buflen,size_t start,const char *appstr) {
	size_t avail = buflen - start;
	size_t cpylen = strlen(appstr);
	
	if ( cpylen > avail )
		cpylen = avail;
	if ( cpylen > 0 )
		memcpy(buf+start,appstr,cpylen);

	return start + cpylen;
}

/*
 * Determine the names's length :
 * A return value of zero, indicates that now switch
 * character was found.
 */
size_t
cobcurses_dirname(const char *pathname) {
#ifdef BUILD_MINGW
	char defswch = '\\';
#else
	char defswch = '/';
#endif
	size_t slen = strlen(pathname);
	char swch = cobcurses_switchchar(pathname,slen,defswch);
	const char *cp;
	size_t dirlen = 0;

	if ( !slen )
		return 0;

	for ( cp = pathname + slen - 1; cp >= pathname; --cp )
		if ( *cp == swch ) {
			dirlen = (size_t) ( cp - pathname + 1 );
			break;
		}

	if ( dirlen == 1 && *pathname == swch )
		dirlen = 0;		/* Don't return '/' as a dirname */

#ifdef BUILD_MINGW
	if ( dirlen == 3 && *pathname != swch ) {
		if ( isalpha(pathname[0]) && pathname[1] == ':' && pathname[2] == swch )
			dirlen = 0;	/* Don't count root directory as a dirname */
	}
#endif
	return dirlen;
}

/*
 * Make directory, and subdirectories if necessary :
 */
int
cobcurses_mkdir_p(const char *pathname,mode_t mode) {
	int rc = 0;

	if ( MKDIR(pathname,mode) != 0 ) {
		size_t dirlen = cobcurses_dirname(pathname);

		if ( dirlen > 1 ) {
			char *parentdir;

			SUBSTRA(parentdir,pathname,dirlen-1);
			if ( !(rc = cobcurses_mkdir_p(parentdir,mode)) ) {
				if ( MKDIR(pathname,mode) != 0 )
					rc = errno;
				else	rc = 0;
			}
			FREEA(parentdir);
		}
	}
	return rc;
}

/*
 * Return true if a field is entirely blank :
 */
cct_bool
cobcurses_is_blank(char *s,cct_ushort len) {
	cct_ushort x;

	for ( x=0; x<len; ++x )
		if ( s[x] != ' ' )
			return 0;	/* Not entirely blank */
	return 1;			/* Is entirely blank */
}

/*
 * Return 0 if the character set is ok,
 * else return 1-based offset to bad character :
 */
cct_ushort
cobcurses_is_charset(char *buf,cct_ushort len,char *chrset) {
	cct_ushort x, y;
	cct_ushort blen;
	cct_ushort rlen = 0;

	/* Minimum chrset length is 1 (put blank in first) */
	for ( rlen=1; chrset[rlen] != ' '; ++rlen );

	/* Get buffer's trimmed length */
	for ( blen = len; blen > 0 && buf[blen-1] == ' '; --blen );

	/* Test character set */
	for ( x=0; x<blen; ++x ) {
		for ( y=0; y<rlen; ++y )
			if ( buf[x] == chrset[y] )
				break;
		if ( y >= rlen )
			return x + 1;	/* Bad character */
	}
	return 0;			/* Charset ok */
}

/*
 * Center text within a buffer. If text is too long, it is
 * truncated :
 */
char *
cobcurses_center(char *buf,cct_ushort buflen,const char *text) {
	cct_ushort tlen, offset;

	assert(buf);
	assert(text);

	tlen = strlen(text);
	if ( tlen > buflen )
		tlen = buflen;

	memset(buf,' ',buflen);
	offset = (buflen - tlen) / 2;

	strncpy(buf+offset,text,tlen);
	return buf;
}

/*
 * Return the length of the string, but using a double-blank
 * as the string terminator :
 */
unsigned
cobcurses_2blen(const char *cobol_text) {
	unsigned x = 0;

	for ( x=0; ; ++x )
		if ( cobol_text[x] == ' ' && cobol_text[x+1] == ' ' )
			return x;
	return 0;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/misc.c,v $ */

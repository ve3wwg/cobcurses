/* initcobc.c : Replacement for init-cobcurses script.
 * Warren W. Gay VE3WWG
 * Tue Sep  4 11:24:20 2007
 * $Id: initcobc.c,v 1.6 2007/09/27 15:49:26 ve3wwg Exp $
 */
#include <cobcurses.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <misc.h>
#include <environ.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * Check if a pathname exists and is a Directory or File :
 */
int
NC_check_path(
  char 		*datadir,
  cct_ushort 	*pdatadirlen,
  char 		*ftype
) {
	char *ppath = datadir;
	size_t len = cobcurses_trim_trailing(ppath,*pdatadirlen,' ');
	char *path = ALLOCA(len + 1);
	char file_type = *ftype;	/* 'D' for directory, or 'F' for reg file */
	struct stat sbuf;
	int rc = RET_OK;

	strncpy(path,ppath,len);
	path[len] = 0;
	
	if ( stat(path,&sbuf) != 0 ) {
		rc = RET_FAILED;			/* Path does not exist */
	} else	{
		mode_t ftype = sbuf.st_mode & S_IFMT;

		switch ( file_type ) {
		case 'D' :
			if ( ftype != S_IFDIR )
				rc = RET_BADPARM;	/* Path is not a directory */
			break;
		case 'F' :
			if ( ftype != S_IFREG )
				rc = RET_BADPARM;	/* Path is not a file */
			break;
		default :
			rc = RET_BADPARM;
		}
	}

	FREEA(path);
	return rc;
}

/*
 * Remove a requested file, within a directory :
 */
int
NC_mk_path(
  char		*datadir,		/* Input directory */
  cct_ushort	*pdatadirlen,		/* Input dir length */
  char		*filename,		/* Input file name */
  cct_ushort	*pfilenamelen,		/* Input file length */
  char		*outpath,		/* Out buffer area (can be same as datadir) */
  cct_ushort	*poutlen		/* Out buffer length */
) {
	char *ppath = datadir;		/* Copy to aligned ptr */
	size_t len = cobcurses_trim_trailing(ppath,*pdatadirlen,' ');
	char *path = ALLOCA(len + *pfilenamelen);
	char *pfile = filename;		/* Copy to aligned ptr */
	size_t flen = cobcurses_trim_trailing(pfile,*pfilenamelen,' ');
	char swch = '/';
	int rc = RET_OK;

	strncpy(path,ppath,len);
#ifdef BUILD_MINGW
	swch = '\\';			/* This still might be '/' in MSYS environment */
#endif
	swch = cobcurses_switchchar(path,len,swch);
	path[len++] = swch;		/* Separator */
	strncpy(path+len,pfile,flen);
	path[len += flen] = 0;

	ppath = outpath;		/* Get aligned pointer */
	memset(ppath,' ',*poutlen);	/* Blank fill the buffer */
	strncpy(ppath,path,len < (size_t) *poutlen ? len : (size_t) *poutlen);

	FREEA(path);
	return rc;
}

/*
 * Create a directory and its subdirs :
 */
int
NC_mkdir_p(
  const char 	*pathname,		/* Ptr to cobol pathname buffer */
  cct_ushort	*ppathlen		/* Length of pathname buffer */
) {
	const char *apath = pathname;	/* Copy an aligned pointer */
	size_t plen = cobcurses_trim_trailing(apath,*ppathlen,' ');
	char *directory_path;
	int rc;

	SUBSTRA(directory_path,apath,plen);
	rc = cobcurses_mkdir_p(directory_path,0700);
	FREEA(directory_path);

	return rc;
}

/*
 * Remove a requested file, within a directory :
 */
int
NC_rm_file(
  char		*datadir,
  cct_ushort	*pdatadirlen
) {
	char *ppath = datadir;			/* Copy to aligned ptr */
	size_t len = cobcurses_trim_trailing(ppath,*pdatadirlen,' ');
	char *path;
	int rc = RET_OK;

	SUBSTRA(path,ppath,len);

	if ( unlink(path) != 0 ) {
		if ( errno == ENOENT )
			rc = RET_NOTFOUND;	/* File was simply not there */
		else	rc = RET_FAILED;	/* Some other complication occurred */
	}

	FREEA(path);
	return rc;
}

/*
 * Allow the Cobol program to modify the environment :
 */
int
NC_setenv(
  const char	*pvarname,		/* Variable to set */
  cct_ushort	*pvarname_len,		/* Max length of name */
  const char	*pvalue,		/* Value to set */
  cct_ushort	*pvalue_len		/* Max length of value */
) {
	const char *varname = pvarname;	/* Aligned pointer */
	const char *value   = pvalue;	/* Aligned pointer */
	size_t vnlen = cobcurses_trim_trailing(varname,*pvarname_len,' ');
	size_t valen = cobcurses_trim_trailing(value,*pvalue_len,' ');
	char *avarname, *avalue;
	int rc;

	SUBSTRA(avarname,varname,vnlen);
	SUBSTRA(avalue,value,valen);
	rc = cobcurses_setenv(avarname,avalue);
	FREEA(avarname);
	FREEA(avalue);

	return rc == 0 ? RET_OK : RET_FAILED;
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/initcobc.c,v $ */

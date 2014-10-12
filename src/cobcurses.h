/* cobcurses.h : Top level header file for CobCurses
 * Warren Gay    Tue May 29 10:34:55 2007
 */
#ifndef _cobcurses_h_
#define _cobcurses_h_ "$Id: cobcurses.h,v 1.45 2007/10/31 14:56:12 ve3wwg Exp $"

#include <config.h>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#endif

#ifdef HAVE_ALLOCA
#define ALLOCA(bytes)		alloca(bytes)
#define FREEA(ptr)		/* no free() required */
#else
#define ALLOCA(bytes)		malloc(bytes)
#define FREEA(ptr)		free(ptr)
#endif

#define SUBSTRA(var,str,n)	{ var = ALLOCA((n)+1); strncpy(var,str,n)[n] = 0; }

#undef HAVE_SOME_CURSES

#ifdef USE_NCURSES
#undef HAVE_CURSES_H
#undef HAVE_PDCURSES_H
#ifdef HAVE_NCURSES_H
#define HAVE_SOME_CURSES 1
#endif
#ifdef HAVE_NCURSES_NCURSES_H
#define HAVE_SOME_CURSES 1
#endif
#endif

#ifdef USE_PDCURSES
#undef HAVE_NCURSES_H
#undef HAVE_CURSES_H
#define HAVE_SOME_CURSES 1
#endif

#ifdef USE_CURSES
#undef HAVE_NCURSES_H
#undef HAVE_PDCURSES_H
#ifdef HAVE_CURSES_H
#define HAVE_SOME_CURSES 1
#endif
#endif

#ifndef HAVE_SOME_CURSES
#error "*** You do not have any curses support: curses/ncurses/pdcurses ***"
#else
#define USE_TERM_CURSES 1			/* We will be using a curses interface */
#endif

/*
 * Environment variables :
 */
#define COBCURSES_TOPDIR_ENVNAME	"COBCURSES_TOPDIR"
#define COBCURSES_DATADIR_ENVNAME	"COBCURSES_DATADIR"
#define COBCURSES_NORECOVERY_ENVNAME	"COBCURSES_NORECOVERY"
#define COBCURSES_SHAREDIR_ENVNAME      "COBCURSES_SHAREDIR"
#define COBCURSES_TRACE			"COBCURSES_TRACE"			/* Pathname */
#define COBCURSES_TRACE_LEVEL		"COBCURSES_TRACE_LEVEL"			/* Level (0=most detailed, 5 default) */
#define USER_SHELL_ENVNAME		"USER_SHELL"

/*
 * Types used within CobCurses :
 */
typedef unsigned char		cct_uchar;
typedef short int		cct_short;
typedef int                     cct_int;
typedef unsigned short		cct_ushort;
typedef unsigned int		cct_unsigned;
typedef unsigned long		cct_ulong;
typedef long			cct_long;
typedef float			cct_float;
typedef double			cct_double;
typedef unsigned short		cct_bool;

typedef cct_unsigned		cct_date;
typedef cct_unsigned		cct_time;

#define T			((cct_bool)1)
#define F			((cct_bool)0)

#define Is_True(f)	( (f) != 0 ? T : F )
#define Is_False(f)	( (f) == 0 ? T : F )

#undef minimum
#define minimum(a,b) 	((a) < (b) ? (a) : (b))

/*
 * Object ID Values :
 */
typedef enum {
	id_terminal,				/* cct_terminal */
	id_termcurs				/* cct_termcurs */
} cct_objid;

#include <terminal.h>

/*
 * Exponent limits :
 */
#define MAXIMUM_EE		(999)		/* Temporary hacks */
#define MINIMUM_EE		(-999)

/*
 * RETURN-CODE Values :
 */
#define RET_OK		0		/* Successful */
#define RET_FAILED	1		/* Failed */
#define RET_OPEN	2		/* Was already open/was not open */
#define RET_NSUPPORT	3		/* Not supported */
#define RET_TRUNCATED	4		/* Results were truncated */
#define RET_NOTFOUND	5		/* Entry not found */
#define RET_BADPARM	6		/* Bad (or null) argument provided */
#define RET_END		7		/* End was reached (no data returned) */
#define RET_RESOURCE	8		/* There are no resources left */

/*
 * COBOL Types :
 */
#define CBL_NON_COMP	((cct_ushort)0)	/* Non-COMP types */
#define CBL_COMP_1	((cct_ushort)1)	/* COMP-1 */
#define CBL_COMP_2	((cct_ushort)2)	/* COMP-2 */

/*
 * Bool support :
 */
#define ISBOOL(ch)	((cct_bool)( (ch) == 'Y' || (ch) == 'y' || (ch) == 'N' || (ch) == 'n' ))
#define TOBOOL(ch)	((ch) == 'Y' || (ch) == 'y' ? (cct_bool)1 : (cct_bool)0)
#define BOOLCH(b)	((b) ? (cct_uchar)'Y' : (cct_uchar)'N' )

/*
 * Control Character Macro :
 */
#define CONTROL(ch)	(((cct_uchar)(ch)) & ((cct_uchar)0x1F) )

/*
 * External Function prototypes :
 */
extern int
NC_OPEN(
    cct_ushort  *pcols,			/* Pass back # columns in display */
    cct_ushort  *plines, 		/* Pass back # lines in display */
    char        *phas_colour,		/* Pass back 'Y' if colour is supported */
    char        *pchg_colour,		/* Pass back 'Y' if colour can be changed */
    cct_ushort  *pcolour_pairs,    	/* Pass back # of colour pairs supported */
    char        *phave_mouse,		/* Pass back 'Y' if we have mouse support */
    char        *pmouse_click_ms,	/* Pass back mouse click ms */
    char        *phas_underline		/* Pass back 'Y' if we can support underline */
);

extern int NC_REFRESH(void);
extern int NC_PAUSE(void);
extern int NC_MOVE(cct_ushort *py,cct_ushort *px);
extern int NC_ADDCH(char *pch);
extern int NC_ADDSTR(char **ppstring,cct_ushort *pslen);
extern int NC_CLOSE(void);
extern int NC_CLEAR(void);
extern int NC_CLRTOBOT(void);
extern int NC_CLRTOEOL(void);

extern int
NC_GETCH(
  cct_ushort 	*pkey_code,	/* RETURNED: Key code */
  char		*pasc_char,	/* RETURNED: Character */
  char		*pis_key_code	/* RETURNED: 'Y' if a key code, else 'N' */
);

extern int
NC_GETTEXT(
    cct_ushort		*py,		/* Field's Y position */
    cct_ushort		*px,		/* Field's X position */
    cct_termattr	*pfb_attr,	/* Field's fallback attribute if no colour */
    cct_pair		*ppair,		/* Field colour pair to use else 0 */
    cct_ushort		*pfield_length,	/* Field's length */
    cct_ushort		*pwindow_length, /* Field's window length */
    char 		**pfield_text,	/* Pointer to Field's buffer address */
    char		*pclear,	/* Clear field upon entry? */
    char		*puppercase,	/* Uppercase field if 'Y' */
    char		*pwmask,	/* Mask as a password field when 'Y' */
    char		*pnotblank,	/* When 'Y' do not allow FWD movement with blank field */
    char		**prestrict,	/* Restrict to these characters */
    char		*psignyn,	/* When 'Y', allow a sign character in numeric field */
    cct_ushort		*pnumdigits,	/* Max Number of digits for numeric field */
    cct_ushort		*pnumdecplaces,	/* Max number of decimal places in numeric field */
    char		*pmode,		/* 'A' = action mode, 'F' = 3270 FKEY mode */
    cct_ushort		*pxpos,		/* RETURNED: Relative position within the field */
    char		*pfield_exit,	/* RETURNED: How the field was exited */
    char		*pfb,		/* RETURNED: general exit field code */
    char		*pchgflag,	/* RETURNED: 'Y' if field content has changed */
    char		*pmouseflag,	/* RETURNED: 'Y' if exit due to a mouse event */
    char		*paction,	/* RETURNED: action character, if any */
    cct_ushort		*psearch,	/* RETURNED: Search field # in action fields */
    cct_ushort		*pcomp_type,	/* Input: COMP-2/COMP-1 type (CBL_COMP_2 etc.) */
    void      		**ppcomp_x,	/* RETURNED: Ptr to COMP-1 or COMP-2 data item */
    char		**pmenu,	/* Pointer to optional menu structure */
    cct_pair            *pmenu_pair,    /* The colour pair to use for menu frame */
    cct_termattr	*pmenu_attr	/* The menu title attributes to use */
);

extern int
NC_VERIFY(
    cct_ushort	*field_length,	/* Field's length */
    char 	**pfield_text,	/* Pointer to Field's buffer address */
    char 	*pnotblank,	/* When 'Y' do not allow FWD movement with blank field */
    char	**prestrict,	/* Restrict to these characters */
    char	*psignyn,	/* When 'Y', allow a sign character in numeric field */
    cct_ushort	*pnumdigits,	/* Max Number of digits for numeric field */
    cct_ushort	*pnumdecplaces	/* Max number of decimal places in numeric field */
);

extern int
NC_PUTTEXT(
    cct_ushort	*py,		/* Field's Y position */
    cct_ushort	*px,		/* Field's X position */
    cct_pair	*ppair,		/* Colour pair if non-zero */
    cct_ushort	*pfield_length,	/* Field's length */
    cct_ushort	*pwindow_length, /* Field's window length */
    char 	**pfield_text	/* Pointer to Field's buffer address */
);

extern int
NC_PUTTEXTNORM(
    cct_ushort	*py,		/* Field's Y position */
    cct_ushort	*px,		/* Field's X position */
    cct_pair	*ppair,		/* Colour pair if non-zero */
    cct_ushort	*pfield_length,	/* Field's length */
    cct_ushort	*pwindow_length, /* Field's window length */
    char 	**pfield_text	/* Pointer to Field's buffer address */
);

extern int NC_SETATTR(cct_termattr *pattr);

extern int
NC_INITCOLOUR(
  cct_pair	*ppair_no,		/* Pair number to assign colour to */
  cct_ushort	*pforeground,		/* Foreground colour for pair */
  cct_ushort	*pbackground		/* Background colour for pair */
);

extern int NC_SETCOLOUR(cct_pair *ppair_no);

extern int
NC_DRAW_BOX(
    cct_ushort	*ptop_line,		/* Top line # */
    cct_ushort	*pleft_col,		/* Top left col # */
    cct_ushort	*pbot_line,		/* Bottom line # */
    cct_ushort	*pright_col		/* Bottom right col # */
);

extern int
NC_TITLE(
    cct_ushort	*py,		/* Line number for title */
    char	**ptext,	/* Title text */
    cct_ushort	*plength,	/* Length of title text */
    cct_pair	*ppair,		/* Colour pair or zero if none */
    char 	*pbold,		/* Y for bold */
    char 	*punderline,	/* Y for underline */
    char	*preverse,	/* Y for reverse */
    cct_date	*pdate,		/* Ptr to date for title only */
    cct_time	*ptime		/* Ptr to time for title only */
);

extern int NC_TITLE_ATTRS(cct_termattr *pattr);

extern int
NC_MSG(
    char	**ptext,			/* Pointer to message */
    cct_ushort	*tlen,				/* Text length */
    cct_pair	*ppair,				/* Colour pair to use if non-zero */
    char	*reqpause			/* 'Y' if pause required */
);

extern int
NC_BLANK(
    char	**ppfield,
    cct_ushort	*plength
);

extern int
NC_EXTRACT_SEGMENT(
    char        *pinbuf,    /* Input buffer */
    cct_ushort  *pinlen,    /* Input buffer width */
    char        *potbuf,    /* Output buffer */
    cct_ushort  *potlen,    /* Output buffer's max length */
    cct_ushort  *pcolumn,   /* Returned offset from inbuf */
    cct_ushort  *pseglen    /* Segment's length in otbuf */
);

extern int
NC_STRIP(
    char	*buf,
    cct_ushort	*pbuflen
);

int
NC_MOUSE_INTERVAL(
    cct_int *pmillisecs
);

extern int
NC_MOUSE_MASK(
    char *pb1_pressed,		/* 'Y' / 'N' : Mouse button 1, pressed  */
    char *pb1_released,		/* 'Y' / 'N' : Mouse button 1, released */
    char *pb1_clicked,		/* 'Y' / 'N' : Mouse button 1, clicked  */
    char *pb1_d_clicked,	/* 'Y' / 'N' : Mouse button 1, double-clicked */
    char *pb1_t_clicked,	/* 'Y' / 'N' : Mouse button 1, triple-clicked */
    char *pb2_pressed,		/* 'Y' / 'N' : Mouse button 2, etc. */
    char *pb2_released,		/* 'Y' / 'N' */
    char *pb2_clicked,		/* 'Y' / 'N' */
    char *pb2_d_clicked,	/* 'Y' / 'N' */
    char *pb2_t_clicked,	/* 'Y' / 'N' */
    char *pb3_pressed,		/* 'Y' / 'N' : Mouse button 3, etc. */
    char *pb3_released,		/* 'Y' / 'N' */
    char *pb3_clicked,		/* 'Y' / 'N' */
    char *pb3_d_clicked,	/* 'Y' / 'N' */
    char *pb3_t_clicked		/* 'Y' / 'N' */
);

extern int
NC_MOUSE_EVENT(
    cct_ushort 	*pid,		/* RETURNED : Mouse ID */
    cct_ushort 	*px,		/* RETURNED : X (1-based) */
    cct_ushort 	*py,		/* RETURNED : Y (1-based) */
    cct_ushort 	*pz,		/* RETURNED : Z (1-based) */
    char 	*pb1_pressed,	/* RETURNED : Button 1 Pressed Y/N */
    char 	*pb1_released,	/* RETURNED : Button 1 Released Y/N */
    char 	*pb1_clicked,	/* RETURNED : Button 1 Clicked Y/N */
    char 	*pb1_d_clicked,	/* RETURNED : Button 1 Double clicked Y/N */
    char 	*pb1_t_clicked,	/* RETURNED : Button 1 Triple clicked Y/N */
    char 	*pb2_pressed,	/* RETURNED : Button 2 Pressed Y/N */
    char 	*pb2_released,	/* RETURNED : Button 2 Released Y/N */
    char 	*pb2_clicked,	/* RETURNED : Button 2 Clicked Y/N */
    char 	*pb2_d_clicked,	/* RETURNED : Button 2 Double clicked Y/N */
    char 	*pb2_t_clicked,	/* RETURNED : Button 2 Triple clicked Y/N */
    char 	*pb3_pressed,	/* RETURNED : Button 3 Pressed Y/N */
    char 	*pb3_released,	/* RETURNED : Button 3 Released Y/N */
    char 	*pb3_clicked,	/* RETURNED : Button 3 Clicked Y/N */
    char 	*pb3_d_clicked,	/* RETURNED : Button 3 Double clicked Y/N */
    char 	*pb3_t_clicked 	/* RETURNED : Button 3 Triple clicked Y/N */
);

extern int NC_LIBCOBCURSES(void);
extern int NC_NORECOVERY(char *pflag);
	
extern int
NC_SHOW_MENU(
    const char 		*pmenu_defn,	/* Ptr to COBOL menu definition */
    char        	*psel_buffer,	/* Ptr to receiving buffer PIC X(*pbuflen) */
    cct_ushort  	*pbuflen,	/* Ptr to PIC 9999 COMP-5 SYNCHRONIZED length of buffer */
    cct_pair		*pmenu_pair,	/* The colour pair to use for menu frame */
    cct_termattr	*pmenu_attr	/* The menu title attributes to use */
);

/********************************************************************************
 *	PATHNAME SUPPORT
 ********************************************************************************/

extern int
NC_PATHNAME(
  char		**ppathname,	/* Input/Output pathname buffer NC-PATHNAME */
  cct_ushort	**ppathlen	/* Length of the buffer */
);

extern int NC_check_path(char *datadir,cct_ushort *pdatadirlen,char *ftype);

extern int
NC_mk_path(
  char		*datadir,		/* Input directory */
  cct_ushort	*pdatadirlen,		/* Input dir length */
  char		*filename,		/* Input file name */
  cct_ushort	*pfilenamelen,		/* Input file length */
  char		*outpath,		/* Out buffer area (can be same as datadir) */
  cct_ushort	*poutlen		/* Out buffer length */
);

extern int
NC_mkdir_p(
  const char 	*pathname,		/* Ptr to cobol pathname buffer */
  cct_ushort	*ppathlen		/* Length of pathname buffer */
);

extern int NC_rm_file(
  char          *datadir,
  cct_ushort    *pdatadirlen
);

extern int
NC_setenv(
  const char	*pvarname,		/* Variable to set */
  cct_ushort	*pvarname_len,		/* Max length of name */
  const char	*pvalue,		/* Value to set */
  cct_ushort	*pvalue_len		/* Max length of value */
);
	
extern int cobcurses_substitute(char *pathname,cct_ushort pathlen);

extern int NC_SUSPEND(void);		/* Suspend terminal to allow system() call */
extern int NC_RESUME(void);		/* Resume terminal */

/********************************************************************************
 *	CSV SUPPORT
 ********************************************************************************/

extern int
NC_EXTRACT_CSV(
    char 	**pcsv_text,		/* Pointer to *.csv text line */
    cct_ushort	*pcsv_length,		/* Pointer to Length of *.cvs text line */
    cct_ushort  *pfno,			/* Pointer to field number to extract */
    char	**pbuffer,		/* Pointer to extract buffer */
    cct_ushort	*pbuflen,		/* Pointer to extract buffer's length */
    char   	*poptions		/* Pointer to options */
);

extern int NC_CLEAR_HEADINGS(void);    /* Clear CSV heading associations */

extern int
NC_LOAD_COLUMN_HEADINGS(
    char	**pbuffer,			/* Input CSV text pointer */
    cct_ushort	*pbuflen,			/* Input CSV text length */
    char   	*poptions			/* Options */
);

extern int
NC_REGISTER_COLUMN(
    char	**pheading,		/* Pointer to column heading name */
    cct_ushort	*pheading_length,	/* Length of column heading name */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
);

extern int
NC_REGISTER_COLUMN_NO(
    cct_ushort  *pcolumn_no,		/* Column number */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
);

extern int
NC_REGISTER_COLUMN_HEADING(
    char	**pheading,		/* Pointer to column heading name */
    cct_ushort	*pheading_length,	/* Length of column heading name */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
);

extern int
NC_EXTRACT_CSV_RECORD(
    char	**pinbuf,		/* Pointer to the input buffer CSV text */
    cct_ushort	*pinlen,		/* Input buffer's length */
    char   	*poptions		/* Options */
);

extern int
NC_EMIT_CSV_HEADINGS(
    char 	**pout_buffer,		/* Pointer to *.csv output text line buffer */
    cct_ushort	*pout_length,		/* Pointer to Max Length of text buffer */
    char   	*poptions		/* Pointer to options */
);

extern int
NC_EMIT_CSV_RECORD(
    char 	**pout_buffer,		/* Pointer to *.csv output text line buffer */
    cct_ushort	*pout_length,		/* Pointer to Max Length of text buffer */
    char   *poptions			/* Pointer to options */
);

extern int
NC_COUNT_CSV(
    char 	**pcsv_text,		/* Pointer to *.csv text line */
    cct_ushort	*pcsv_length,		/* Pointer to Length of *.cvs text line */
    char   	*poptions,		/* Pointer to options */
    cct_ushort	*pcount 		/* Pointer to RETURNED count */
);

/********************************************************************************
 *	COBOL ASSOC ARRAY API
 ********************************************************************************/

#include <assoc.h>

extern int
NC_ASSOC_ASSIGN(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char   			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
);

extern int
NC_ASSOC_FETCH(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
);

int
NC_ASSOC_DELETE(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen 	/* Pointer to key length in bytes */
);

extern int
NC_ASSOC_CLEAR(
    cct_assoc_instance_key	*pinstance	/* Pointer to instance to use */
);

extern int
NC_ASSOC_COUNT(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_count		*pcount		/* Pointer to returned count */
);

extern int
NC_ASSOC_FETCH_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char   			**pdata,		/* Pointer to data area */
    cct_ushort   		*pdatlen	/* Pointer to data area length in bytes */
);

extern int
NC_ASSOC_FETCH_KEY_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char	                **pkey,		/* Pointer to key area */
    cct_ushort      	        *pkeylen	/* Pointer to key length in bytes */
);

extern int
NC_ASSOC_FETCH_DATA_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char 			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
);

extern int
NC_ASSOC_FIRST(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to first handle for instance (OUT) */
);

extern int
NC_ASSOC_NEXT(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to next handle for instance (IN/OUT) */
);

extern int
NC_LAST_ASSOC(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to receiving handle */
);

extern int
NC_ASSIGN_INSTANCE(
    cct_assoc_instance_key	*pinstance
);

extern int
NC_ASSOC_DELETE_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to handle to delete */
);

extern int NC_ASSOC_CLEAR_ALL(void);

/********************************************************************************
 *	FORMAT COMP-1 / COMP-2 AS TEXT
 ********************************************************************************/
extern int
NC_ECVT_COMP1(
    float	*pcomp1,	/* Pointer to input COMP-1 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    cct_short	*pexpon		/* Pointer to cct_ushort to receive exponent */
);

extern int
NC_ECVT_COMP2(
    double	*pcomp2,	/* Pointer to input COMP-2 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    cct_short	*pexpon		/* Pointer to cct_ushort to receive exponent */
);

extern int
NC_SCINOTATION_COMP1(
    float	*pcomp1,	/* Pointer to input COMP-1 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    const char	*peng_flag,	/* Pointer to 'Y' if to use Engineering exponents */
    cct_short	*pexpon,	/* Pointer to receiving exponent value */
    cct_ushort   *E_offset	/* Poitner to offset var for 'E' (returned) */
);

extern int
NC_SCINOTATION_COMP2(
    double	*pcomp2,	/* Pointer to input COMP-2 item */
    char	**pbuffer,	/* Pointer to receiving buffer */
    cct_ushort	*pbuflen,	/* Pointer to cct_ushort indicating buffer length */
    const char	*peng_flag,	/* Pointer to 'Y' if to use Engineering exponents */
    cct_short   *pexpon,	/* Pointer to receiving exponent */
    cct_ushort   *E_offset	/* Poitner to offset var for 'E' (returned) */
);

/********************************************************************************
 * EDIT TEXT POSSIBLY IN EXPONENTIAL FORMAT, POSSIBLY WITH UNITS, INTO A
 * COMP-1 / COMP-2 DATA VALUE :
 ********************************************************************************/

int
NC_EDIT_COMP1(
    cct_float  	*comp1,			/* Returned: Extracted COMP-1 value */
    const char	**pin_text,		/* Input: Text to parse */
    cct_ushort	*pin_len,		/* Input: Text length */
    const char	**punits_config		/* Input: blank terminated (or null) */
);

int
NC_EDIT_COMP2(
    cct_double  *comp2,			/* Returned: Extracted COMP-2 value */
    const char	**pin_text,		/* Input: Text to parse */
    cct_ushort	*pin_len,		/* Input: Text length */
    const char	**punits_config		/* Input: blank terminated (or null) */
);

/********************************************************************************
 * FORMAT A COMP-2 ITEM WITH/WITHOUT UNITS, IN POSSIBLY EXPONENTIAL
 * (OPTIONALLY ENGINEERING) DISPLAY FORMAT.
 ********************************************************************************/

extern int
NC_FORMAT_COMP2(
    cct_double	*pcomp2,		/* Input: Number to format */
    char	*pengineering_fmt,	/* Input: 'Y' if to use engineering format */	
    cct_ushort	*pdigits,		/* Input: N significant digits */
    const char	**punits_config,	/* Input: Units config string or NULL */
    char	**pbuf,			/* Output: Buffer for results */
    cct_ushort	*pbuflen		/* Input: Buffer length */
);

/********************************************************************************
 * MISC : For screen designer 
 ********************************************************************************/

int
COBCURSES_TEST_UNITS_STRING(
  char *	pin_units_string,	/* Input string to be processed (must have blank at end) */
  cct_ushort *	pout_count_opts,	/* Output returned count of options */
  cct_ushort *	pout_count_units,	/* Output returned count of unit specs */
  cct_ushort *  pout_error_offset,	/* Output 1-based offset where error occurred */
  char *	pout_error_msgbuf,	/* Output buffer for error message, if any */
  cct_ushort *	pout_error_msgbuflen	/* Output buffer length for message */
);

/********************************************************************************
 * Trace : Active when COBCURSES_TRACE="some-filename"
 ********************************************************************************/

extern int NC_TRACE_STATE(cct_ushort *pfseq,cct_ushort *pfseq_next,cct_ushort *pfno,char *pverify_flag);
extern int NC_TRACE_MSG(const char *pmsg);

#endif /* _cobcurses_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/cobcurses.h,v $ */

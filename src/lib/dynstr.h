/* dynstr.h : 	C dynamic string support
 * Warren Gay   Wed Jul 25 10:10:26 2007
 */
#ifndef _dynstr_h_
#define _dynstr_h_

typedef struct {
	cct_uchar	*str;
	cct_ushort	slen;
	cct_ushort	alen;
} cct_strvar;

extern cct_strvar *dyn_str_init(cct_strvar *v);
extern cct_strvar *dyn_str_clear(cct_strvar *v);
extern cct_strvar *dyn_str_ucase(cct_strvar *v);
extern cct_strvar *dyn_str_lcase(cct_strvar *v);
extern cct_strvar *dyn_str_assign_substr(cct_strvar *v,const char *string,cct_ushort slen);
extern char *dyn_str_string(cct_strvar *v);
extern char *dyn_str_right(cct_strvar *v,cct_ushort n);
extern cct_strvar *dyn_str_assign(cct_strvar *v,const char *string);
extern cct_strvar *dyn_str_truncate(cct_strvar *v,cct_ushort new_length);
extern cct_strvar *dyn_str_append(cct_strvar *v,const char *string);
extern cct_strvar *dyn_str_append_s(cct_strvar *v,cct_strvar *string);
extern cct_strvar *dyn_str_append_n(cct_strvar *v,const char *string,cct_ushort n);
extern cct_strvar *dyn_str_append_ch(cct_strvar *v,const char ch);
extern cct_ushort  dyn_str_length(cct_strvar *v);
extern cct_strvar *dyn_str_delete(cct_strvar *v,cct_ushort offset,cct_ushort n);

extern cct_strvar *dyn_str_appendf(cct_strvar *v,const char *format,...);
extern cct_strvar *dyn_str_sprintf(cct_strvar *v,const char *format,...);

extern cct_ulong dyn_str_to_ulong(cct_strvar *v,cct_bool *okflag);
extern cct_long dyn_str_to_long(cct_strvar *v,cct_bool *okflag);
extern cct_double dyn_str_to_double(cct_strvar *v,cct_bool *okflag);
extern cct_double dyn_str_to_float(cct_strvar *v,cct_bool *okflag);

extern cct_bool dyn_str_PIC9(cct_strvar *v,char *str,cct_ushort n);
#define dyn_str_PICX(v,str,n) dyn_str_assign_substr(v,str,n)

extern int dyn_str_strchr(cct_strvar *v,char ch);
extern cct_strvar *dyn_str_trim(cct_strvar *v);

extern char *dyn_str_steal(cct_strvar *v);

#endif

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/dynstr.h,v $ */

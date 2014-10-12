/* csvsupport.c : *.csv file support
 * Warren W. Gay VE3WWG
 * Tue Jul  3 11:12:05 2007
 * $Id: csvsupport.c,v 1.20 2007/10/30 19:09:09 ve3wwg Exp $
 */
#include <cobcurses.h>

#include <assert.h>
#include <misc.h>
#include <assoc.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <dynstr.h>

typedef struct {
	char    	*buffer;	/* Pointer to user's buffer */
	cct_ushort	buflen;		/* Buffer's length */
	cct_ushort	colno;		/* Column number */
} cct_col_assoc;

typedef struct {
	char		delimiter;		/* Delimiter character */
	cct_bool	mult_del_as_one;	/* Treat multiple delimiters as one */
	cct_bool	dbl_quote;		/* Using double quote convention */
	cct_bool	backslash;		/* Using the backslash convention */
} cct_extra_options;

static cct_bool headings_inited = 0;		/* True if &headings has been initialized */
static cct_assoc_array headings = ASSOC_INIT;	/* Headings assoc array */

/********************************************************************************
 * C String keyed assoc array API :
 ********************************************************************************/

static cct_assoc_key *assoc_key_init_str(cct_assoc_key *genkey,void *ptr,cct_ushort length);
static short assoc_key_strcmp(cct_assoc_key *k1, cct_assoc_key *k2);
static cct_assoc_key *assoc_key_dispose_str(cct_assoc_key *k);
static void assoc_obj_free(void *ptr,cct_ushort length);

static cct_assoc_api str_api = {
	assoc_key_init_str,		/* Key initialization */
	assoc_key_strcmp,		/* Key comparison */
	assoc_key_dispose_str,		/* Key disposal */
	assoc_obj_free,			/* Object disposal */
	0				/* Aux object disposal */
};

/********************************************************************************
 * C String keyed API routines :
 ********************************************************************************/

/*
 * Initialize a key as a C string :
 */
static cct_assoc_key *
assoc_key_init_str(cct_assoc_key *genkey,void *ptr,cct_ushort length) {

	/* length arg is ignored */
	memset(genkey,0,sizeof *genkey);
	genkey->key = strdup((char *)ptr);
	genkey->keylen = strlen((char *)genkey->key);

	/* Uppercase so checksum comes out right */
	cobcurses_ucase((char *)genkey->key,(size_t)genkey->keylen);

	genkey->kcksum = assoc_key_checksum(genkey->key,genkey->keylen);
	return genkey;
}

/*
 * Internal: This treats the key as simple null terminated strings :
 */
static short
assoc_key_strcmp(cct_assoc_key *k1, cct_assoc_key *k2) {
	return (short) strcmp((char *)k1->key,(char *)k2->key);
}

/*
 * Dispose of the object :
 */
static void
assoc_obj_free(void *ptr,cct_ushort length) {
	if ( ptr )
		free(ptr);
}

/*
 * Dispose of key content :
 */
static cct_assoc_key *
assoc_key_dispose_str(cct_assoc_key *k) {

	if ( k->key != 0 )
		free(k->key);
	k->key = 0;
	return k;
}

/*
 * Initialize a "C string key" associative array :
 */
static cct_assoc_array *
assoc_array_init_str(cct_assoc_array *a) {
	return assoc_array_init(a,&str_api);
}

/*
 * Initialize assoc array if required :
 */
static short
headings_cond_init(void) {

	if ( !headings_inited ) {
		assoc_array_init_str(&headings);
		headings_inited = 1;
		return 1;
	}
	return 0;
}

/********************************************************************************
 * END OF C String keyed assoc array API
 ********************************************************************************/

/*
 * Convert COBOL PIC X(3) string of options into cct_extra_options :
 */
static cct_extra_options *
extra_opts(cct_extra_options *opts,char *poptions) {

	memset(opts,0,sizeof *opts);
	opts->delimiter = (char) poptions[0];
	opts->mult_del_as_one = TOBOOL(poptions[1]);
	switch ( poptions[2] ) {
	case '"' :
		opts->dbl_quote = 1;
		break;
	case '\\' :
		opts->backslash = 1;
		break;
	default :
		/* Unknown convention - ignored */
		;
	}
	return opts;
}

/*
 * Skip further delimiters, if the option calls for it :
 */
static char *
skip_multi_delim(char *cp,char *endp,cct_extra_options *opts) {

	if ( !opts->mult_del_as_one )
		return cp;			/* No multi delimiter option set */
	
	for ( ; cp < endp && *cp == opts->delimiter; ++cp )
		;
	return cp;
}

/*
 * Locate the end of the current field :
 */
static char *
end_field(char *pfield,char *endp,cct_extra_options *opts) {
	char *cp = pfield;		/* Current char ptr */
	cct_bool quoted = *cp == '"';	/* True if quoted */
	cct_bool end = 0;		/* True if end found */

	if ( quoted )
		++cp;			/* Skip opening quote */

	/*
	 * Now locate the end of the field :
	 */
	for ( end = 0; cp < endp && !end; ) {
		if ( opts->dbl_quote && *cp == '"' && cp + 1 < endp && cp[1] == '"' )
			cp += 2;
		else if ( opts->backslash && *cp == '\\' && cp + 1 < endp )
			cp += 2;
		else if ( quoted && *cp == '"' ) {
			end = 1;
			for ( ++cp; cp < endp && *cp != opts->delimiter ; ++cp )
				++cp;
		} else if ( !quoted && *cp == opts->delimiter )
			end = 1;
		else 	++cp;
	}

	return skip_multi_delim(cp,endp,opts);	/* Possibly skip over more delimters */
}

/*
 * Extract the data into buffer for buflen max, from
 * the csv field pointed by fstart, ending at fend.
 */
static int
extract_field(
    char	*fstart,		/* Start of CSV field text */
    char	*fend,			/* End of CSV field text + 1 */
    char	*buffer,		/* Return buffer */
    cct_ushort	buflen,			/* Return buffer length */
    cct_extra_options *opts		/* Options */
) {
	char *cp = fstart;
	cct_bool quoted = *fstart == '"';
	cct_uchar ch;
	int rc = RET_OK;

	if ( quoted )
		++cp;			/* Skip opening quote */

	while ( cp < fend ) {
		if ( opts->dbl_quote && *cp == '"' && cp + 1 < fend && cp[1] == '"' ) {
			cp += 2;
			ch = '"';
		} else if ( opts->backslash && *cp == '\\' && cp + 1 < fend ) {
			ch = cp[1];
			cp += 2;
		} else if ( quoted && *cp == '"' )
			break;
		else if ( !quoted && *cp == opts->delimiter )
			break;
		else 	ch = *cp++;
		if ( buflen > 0 ) {
			*buffer++ = ch;
			--buflen;
		} else
			rc = RET_TRUNCATED;
	}
	return rc;
}

/*
 * Allocate a buffer and extract a null delimited text for the
 * column delimited by pointers fstart & fend :
 */
static char *
extract_text(
    char	*fstart,		/* Start of CSV field text */
    char	*fend,			/* End of CSV field text + 1 */
    cct_extra_options *opts		/* Options */
) {
	cct_ushort buflen = fend - fstart + 1;	/* Max size needed */
	char *buf = malloc(buflen);		/* Allocate a buffer */

	memset(buf,0,buflen);			/* Zero out the buffer */
	extract_field(fstart,fend,buf,buflen,opts);
	return buf;
}

/*
 * Given some CSV text, extract a field by 1-based column number :
 */
int
NC_EXTRACT_CSV(
    char 	**pcsv_text,		/* Pointer to *.csv text line */
    cct_ushort	*pcsv_length,		/* Pointer to Length of *.cvs text line */
    cct_ushort  *pfno,			/* Pointer to field number to extract */
    char	**pbuffer,		/* Pointer to extract buffer */
    cct_ushort	*pbuflen,		/* Pointer to extract buffer's length */
    char   	*poptions		/* Pointer to options */
) {
	char *csv_text = *pcsv_text;
	cct_ushort csv_length = *pcsv_length;
	char *buffer = *pbuffer;
	cct_ushort buflen = *pbuflen;
	char *endp;
	char *fstart;
	char *fend;
	cct_ushort fno = *pfno;
	cct_ushort cfno;
	cct_extra_options opts;

	extra_opts(&opts,poptions);
		
	if ( !csv_text || !buffer || buflen < 1 || fno < 1 )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	endp = csv_text + csv_length;

	memset(buffer,' ',buflen);	/* Clear return buffer with blanks */

	for ( fstart = fend = csv_text, cfno = 0; cfno < fno && fend < endp ; ++cfno ) {
		fstart = fend;
		fend = end_field(fstart,endp,&opts);
		if ( cfno + 1 < fno && fend < endp && *fend == opts.delimiter )
			++fend;		/* Skip over comma */
	}

	if ( cfno < fno || fstart >= endp )
		return RET_OK;		/* No data to return */

	return extract_field(fstart,fend,buffer,buflen,&opts);
}

/*
 * Free all prior column associations
 */
int
NC_CLEAR_HEADINGS(void) {

	if ( !headings_cond_init() )		/* Initialize &headings if required */
		assoc_array_clear(&headings);	/* Clear out existing array content */
	return 0;
}	

int
NC_LOAD_COLUMN_HEADINGS(
    char	**pbuffer,			/* Input CSV text pointer */
    cct_ushort	*pbuflen,			/* Input CSV text length */
    char   	*poptions			/* Options */
) {
	char *buffer = *pbuffer;		/* Input buffer */
	cct_ushort buflen = *pbuflen;		/* Input buffer length */
	char *fstart, *fend, *endp, *text;
	cct_ushort colno = 0;			/* Column number */
	cct_col_assoc *colent = 0;		/* Column entry */
	cct_extra_options opts;			/* Options */
	int rc = RET_OK;			/* Return code */

	extra_opts(&opts,poptions);

	if ( !buffer )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	endp = buffer + buflen;		/* End pointer */
	NC_CLEAR_HEADINGS();		/* Clear associations */

	for ( fstart = fend = buffer; fend < endp ; ) {
		fstart = fend;
		fend = end_field(fstart,endp,&opts);
		text = extract_text(fstart,endp,&opts);
		colent = malloc(sizeof(cct_col_assoc));
		colent->buffer = 0;
		colent->buflen = 0;
		colent->colno = ++colno;
		assoc_array_assign(&headings,colent,sizeof *colent,0,0,text,strlen((char *)text));
		free(text);
		if ( fend < endp && *fend == opts.delimiter )
			++fend;		/* Skip over comma */
	}
	return rc;
}

/*
 * Return string without trailing blanks :
 */
static char *
trimmed_name(char *name,cct_ushort length) {
	static char *tname = 0;
	cct_ushort x = length;

	tname = realloc(tname,length + 1);
	strncpy(tname,name,length);
	tname[length] = 0;
	while ( x > 0 ) {
		if ( tname[--x] != ' ' )
			break;
		tname[x] = 0;
	}
	return tname;
}

/*
 * Register a receiving buffer by column number:
 */
int
NC_REGISTER_COLUMN_NO(
    cct_ushort  *pcolumn_no,		/* Column number */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
) {
	cct_ushort column_no = *pcolumn_no;
	char *buffer = *pbuffer;
	cct_ushort buflen = *pbuflen;
	cct_col_assoc *colent = malloc(sizeof(cct_col_assoc));
	char col_name[7];

	if ( !buffer || !buflen || column_no < 1 )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	colent->colno = column_no;
	colent->buffer = buffer;
	colent->buflen = buflen;

	sprintf(col_name,"%06u",column_no);
	assoc_array_assign(&headings,colent,sizeof *colent,0,0,col_name,0);
	return RET_OK;
}

/*
 * Register a receiving buffer with a column :
 */
int
NC_REGISTER_COLUMN(
    char	**pheading,		/* Pointer to column heading name */
    cct_ushort	*pheading_length,	/* Length of column heading name */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
) {
	char *heading = *pheading;
	cct_ushort heading_length = *pheading_length;
	char *buffer = *pbuffer;
	cct_ushort buflen = *pbuflen;
	char *name;
	cct_col_assoc *colent;
	cct_assoc_iter_handle x;

	if ( !heading || heading_length < 1 || !buffer || !buflen )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	name = trimmed_name((char *)heading,heading_length);
	x = assoc_array_locate(&headings,name,strlen(name));
	if ( x == ASSOC_NOENT )
		return RET_NOTFOUND;

	/*
	 * Add/replace the current association :
	 */
	colent = assoc_array_object(&headings,x);
	colent->buffer = buffer;
	colent->buflen = buflen;
	return RET_OK;
}

/*
 * Register a receiving buffer with a column and register heading as well :
 */
int
NC_REGISTER_COLUMN_HEADING(
    char	**pheading,		/* Pointer to column heading name */
    cct_ushort	*pheading_length,	/* Length of column heading name */
    char	**pbuffer,		/* Pointer to receiving buffer */
    cct_ushort	*pbuflen		/* Receiving buffer's name */
) {
	char *heading = *pheading;
	cct_ushort heading_length = *pheading_length;
	char *buffer = *pbuffer;
	cct_ushort buflen = * pbuflen;
	char *name;
	cct_col_assoc *colent;

	if ( !heading || heading_length < 1 || !buffer || !buflen )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	name = trimmed_name((char *)heading,heading_length);
        if ( !*name )
            return RET_BADPARM;     /* No heading name */

	/*
	 * Add/replace the current association :
	 */
	colent = malloc(sizeof(cct_col_assoc));
	colent->buffer = buffer;
	colent->buflen = buflen;
	colent->colno = ((cct_ushort) assoc_array_count(&headings)) + 1;
        assoc_array_assign(&headings,colent,sizeof *colent,0,0,name,strlen((char *)name));

	return RET_OK;
}

/*
 * Extract the CSV record into the registered
 * column buffers :
 */
int
NC_EXTRACT_CSV_RECORD(
    char	**pinbuf,		/* Pointer to the input buffer CSV text */
    cct_ushort	*pinlen,		/* Input buffer's length */
    char   	*poptions		/* Options */
) {
	char *inbuf = *pinbuf;		/* Input buffer pointer */
	cct_ushort inlen = *pinlen;	/* Input buffer length */
	cct_assoc_iter_handle x;
	cct_col_assoc *colent;
	cct_extra_options opts;
	int rc = RET_OK;
	int trc;

	extra_opts(&opts,poptions);	/* Get options */

	if ( !inbuf )
		return RET_BADPARM;	/* No input buffer */

	headings_cond_init();		/* Initialize &headings if required */

	inlen = (cct_ushort)cobcurses_trim_trailing((char *)inbuf,(size_t)inlen,' ');

	for ( x=assoc_array_first(&headings); x != ASSOC_NOENT; x=assoc_array_next(&headings,x) ) {
		colent = assoc_array_object(&headings,x);
		if ( colent->buffer && colent->buflen > 0 ) {
			trc = NC_EXTRACT_CSV(pinbuf,&inlen,
				&colent->colno,&colent->buffer,&colent->buflen,poptions);
			if ( trc == RET_TRUNCATED )
				rc = RET_TRUNCATED;
		}
	}
	return rc;
}

/*
 * Given some CSV text, count how many columns there are :
 */
int
NC_COUNT_CSV(
    char 	**pcsv_text,		/* Pointer to *.csv text line */
    cct_ushort	*pcsv_length,		/* Pointer to Length of *.cvs text line */
    char   	*poptions,		/* Pointer to options */
    cct_ushort	*pcount 		/* Pointer to RETURNED count */
) {
	char *csv_text = *pcsv_text;
	cct_ushort csv_length = *pcsv_length;
	char *endp, *fstart, *fend;
	cct_ushort cfno;
	cct_extra_options opts;

	extra_opts(&opts,poptions);
		
	if ( !csv_text )
		return RET_BADPARM;

	headings_cond_init();		/* Initialize &headings if required */

	if ( csv_length < 1 ) {
		cfno = 0;		/* There is no content */
	} else	{
		endp = csv_text + csv_length;

		for ( fstart = fend = csv_text, cfno = 0; fend < endp ; ++cfno ) {
			fstart = fend;
			fend = end_field(fstart,endp,&opts);
			if ( fend < endp && *fend == opts.delimiter )
				++fend;		/* Skip over comma */
		}
	}

	*pcount = cfno;
	return RET_OK;
}

/*
 * Return 1 if the field for flen bytes needs CSV quoting :
 */
static short
needs_quoting(char *field,cct_ushort flen,cct_extra_options *opts) {
	cct_ushort cx;
	short nq = 0;

	for ( cx = 0; cx < flen; ++cx ) {
		if ( field[cx] == ' ' || field[cx] == opts->delimiter )
			nq |= 1;
		if ( field[cx] == '"' || ( opts->backslash && field[cx] == '\\' ) )
			nq |= 2;
	}
	return nq;
}

/*
 * Escape quote characters :
 */
static void
esc_quote(cct_strvar *tstr,char *text,cct_ushort tlen) {
	char ch;

	dyn_str_clear(tstr);
	for ( ; tlen > 0; --tlen ) {
		ch = *text++;
		switch ( (char) ch ) {
		case '"' :
			dyn_str_append_n(tstr,"\"\"",2);
			break;
		case '\\' :
			dyn_str_append_n(tstr,"\\\\",2);
			break;
		default :
			dyn_str_append_ch(tstr,ch);
		}
	}
}

/*
 * Emit one possibly quoted *.CSV field :
 */
static cct_strvar *
emit(
    cct_strvar	*outbuf,
    char   	*ptext,
    cct_ushort	text_length,
    cct_extra_options *opts
) {
	short nq;
	cct_strvar tstr;
	cct_ushort tlen = (cct_ushort)cobcurses_trim_trailing((char *)ptext,text_length,' ');	

	dyn_str_init(&tstr);	

	if ( (nq = needs_quoting(ptext,tlen,opts)) != 0 ) {
		dyn_str_append(outbuf,"\"");
		if ( !(nq & 2) ) {
			dyn_str_append_n(outbuf,(char *)ptext,tlen);
		} else	{
			esc_quote(&tstr,ptext,tlen);
			dyn_str_append(outbuf,dyn_str_string(&tstr));
		}
		dyn_str_append(outbuf,"\"");
	} else
		dyn_str_append_n(outbuf,(char *)ptext,tlen);

	dyn_str_clear(&tstr);
	return outbuf;
}

/*
 * Emit CSV headings or content :
 */
static int
nc_emit_csv_record(
    char		*outbuf, 	/* Output buffer pointer */
    cct_ushort		outlen,  	/* Output buffer maximum length */
    cct_extra_options	*opts,		/* Options */
    cct_bool 		emit_headings	/* True if emitting headings else content */
) {
	cct_assoc_count count;		/* Count of columns required */
	cct_assoc_iter_handle x;	/* Iterator handle */
	cct_ushort cx = 0;		/* Column index */
	cct_col_assoc *colent;		/* Heading column heading */
	cct_bool cf;			/* Change flag (bubble sort) */
	cct_ushort colno;		/* Column number */
	cct_strvar str;			/* Dynamic string */
	cct_ushort slen = 0;		/* Dynamic string length */
	char *hp;			/* Temp heading pointer */
	int rc = RET_OK;		/* Return code */

	dyn_str_init(&str);

	if ( !outbuf || outlen < 1 || !headings_inited ) {
		rc = RET_BADPARM;	/* No input buffer */
		goto xit;
	}

	count = assoc_array_count(&headings);	/* How many columns are we processing? */
	{
		cct_uchar delim[2];
		cct_col_assoc **colents = ALLOCA(count * sizeof (cct_col_assoc *));
                char **hdgs = ALLOCA(count * sizeof (cct_uchar *));

		delim[0] = opts->delimiter;
		delim[1] = 0;

		/* Get the list of column entries (unsorted) */
		for ( x=assoc_array_first(&headings); x != ASSOC_NOENT; x=assoc_array_next(&headings,x) ) {
			assert((cct_assoc_count) cx < count);
			colent = assoc_array_object(&headings,x);
			colents[cx] = colent;
			hdgs[cx++] = assoc_array_key(&headings,x);
		}

		assert(count <= (cct_assoc_count)0xFFFF);

		/*
		 * Now sort the column entries by column # :
		 */
		do 	{
			cf = 0;
			for ( cx=0; cx < (cct_ushort)count-1; ++cx ) {
				if ( colents[cx]->colno > colents[cx+1]->colno ) {
					cf            = 1;
					colent        = colents[cx+1];
					colents[cx+1] = colents[cx];
					colents[cx]   = colent;
					hp            = hdgs[cx+1];
					hdgs[cx+1]    = hdgs[cx];
					hdgs[cx]      = hp;
				}
			}
		} while ( cf != 0 );
		/*
		 * Now format content of each column into the output buffer :
		 */
		colno = colents[0]->colno;
		for ( cx=0; cx < (cct_ushort)count; ++cx ) {
			if ( cx > 0 ) {
				dyn_str_append(&str,(char *)delim);
				if ( !opts->mult_del_as_one )
					for ( ++colno; colno < colents[cx]->colno; ++colno )
						dyn_str_append(&str,(char *)delim);
				else	colno = colents[cx]->colno;
			}
			if ( !emit_headings ) {
				if ( colents[cx]->buffer != 0 && colents[cx]->buflen > 0 )
					emit(&str,colents[cx]->buffer,colents[cx]->buflen,opts);
			} else	{
				emit(&str,hdgs[cx],strlen((char *)hdgs[cx]),opts);
			}
		}

		/*
		 * Copy the content from str to the output buffer :
		 */
		if ( (slen = dyn_str_length(&str)) > outlen ) {
			slen = outlen;
			rc = RET_TRUNCATED;	/* Result is truncated */
		}

		memcpy(outbuf,dyn_str_string(&str),slen);
		if ( slen < outlen )
			memset(outbuf+slen,' ',outlen-slen);
		
		FREEA(colents);
		FREEA(hdgs);
	}

xit:	dyn_str_clear(&str);
	return rc;
}

/*
 * Given existing CSV column definitions, create a text record with the
 * column information encoded in *.CSV format :
 */
int
NC_EMIT_CSV_RECORD(
    char 	**pout_buffer,		/* Pointer to *.csv output text line buffer */
    cct_ushort	*pout_length,		/* Pointer to Max Length of text buffer */
    char   *poptions			/* Pointer to options */
) {
	char *outbuf = *pout_buffer; 	/* Output buffer pointer */
	cct_ushort outlen = *pout_length; /* Output buffer maximum length */
	cct_extra_options opts;		/* Options */

	extra_opts(&opts,poptions);	/* Get options */

	return nc_emit_csv_record(outbuf,outlen,&opts,0);
}

/*
 * Given existing CSV column definitions, create a text record with the
 * column headings encoded in *.CSV format :
 */
int
NC_EMIT_CSV_HEADINGS(
    char 	**pout_buffer,		/* Pointer to *.csv output text line buffer */
    cct_ushort	*pout_length,		/* Pointer to Max Length of text buffer */
    char   	*poptions		/* Pointer to options */
) {
	char *outbuf = *pout_buffer;	/* Output buffer pointer */
	cct_ushort outlen = *pout_length; /* Output buffer maximum length */
	cct_extra_options opts;		/* Options */

	extra_opts(&opts,poptions);	/* Get options */

	return nc_emit_csv_record(outbuf,outlen,&opts,1);
}

/* End $Source: /cvsroot/cobcurses/cobcurses/src/lib/csvsupport.c,v $ */

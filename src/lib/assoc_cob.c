/* assoc_cob.c : Associative array support for COBOL
 * Warren W. Gay VE3WWG
 * Tue Jul 10 10:50:31 2007
 * $Id: assoc_cob.c,v 1.12 2007/10/30 19:09:09 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RETURN(retcode)	{ rc = (retcode); goto xit; }
#define EXIT		xit: return rc;

static cct_assoc_array instances = ASSOC_INIT;	/* The array of instances */
static cct_bool instances_inited = 0;		/* True if the array has been initialized */
static cct_assoc_instance_key next_instance = 100000; /* Next unassigned instance ID */

/********************************************************************************
 * Instance Associative Array support :
 ********************************************************************************/

#include <assoc.h>

static cct_assoc_key *assoc_key_init_instance(cct_assoc_key *genkey,void *ptr,cct_ushort length);
static short assoc_key_cmp_instance(cct_assoc_key *k1, cct_assoc_key *k2);
static cct_assoc_key *assoc_key_dispose_instance(cct_assoc_key *k);
static void assoc_obj_dispose_instance(void *obj,cct_ushort objlen);

static cct_assoc_api assoc_instance_api = {
	assoc_key_init_instance,		/* Key initialization */
	assoc_key_cmp_instance,			/* Key comparison */
	assoc_key_dispose_instance,		/* Key disposal */
	assoc_obj_dispose_instance,		/* Object disposal */
	0					/* Aux object disposal */
};

/*
 * Module initialization :
 */
static void
initialize(void) {
	if ( instances_inited )
		return;
	assoc_array_init(&instances,&assoc_instance_api);
	instances_inited = 1;
}

#define INITIALIZE() { if ( !instances_inited ) initialize(); }

/*
 * Create an instance cct_assoc_key structure from ptr :
 * NB: length is ignored, as it is assumed to be sizzeof (cct_assoc_instance_key).
 */
static cct_assoc_key *
assoc_key_init_instance(cct_assoc_key *genkey,void *ptr,cct_ushort length) {

	genkey->keylen = sizeof (cct_assoc_instance_key);
	genkey->key = malloc(genkey->keylen);
	memcpy(genkey->key,ptr,genkey->keylen);

	genkey->kcksum = assoc_key_checksum(genkey->key,genkey->keylen);
	return genkey;
}

/*
 * Compare two instance keys :
 */
static short
assoc_key_cmp_instance(cct_assoc_key *k1, cct_assoc_key *k2) {
	cct_assoc_instance_key inst1, inst2;

	if ( k1->keylen != k2->keylen )
		abort();
	if ( k1->keylen != sizeof(cct_assoc_instance_key) )
		abort();
	inst1 = 777;
	inst2 = 666;
	memcpy(&inst1,k1->key,k1->keylen);
	memcpy(&inst2,k2->key,k2->keylen);
	if ( inst1 < inst2 )
		return -1;
	return inst1 == inst2 ? 0 : 1;
}

/*
 * Dispose of a key :
 */
static cct_assoc_key *
assoc_key_dispose_instance(cct_assoc_key *k) {

	if ( !k )
		abort();
	free(k->key);
	k->key = 0;
	k->keylen = 0;
	return k;
}

/*
 * Clear the associative array object pointed to by obj,
 * and release the object.
 */
static void
assoc_obj_dispose_instance(void *obj,cct_ushort objlen) {

	if ( !obj )
		abort();
	if ( objlen != sizeof (cct_assoc_array) )
		abort();
	assoc_array_clear((cct_assoc_array *)obj);	/* Release contents of object */
	free(obj);					/* Release object */
}

/********************************************************************************
 * End of Instance Associative Array support.
 *
 * Start of (internal) Instance Array API :
 ********************************************************************************/

static cct_assoc_key *cob_assoc_key_init(cct_assoc_key *genkey,void *ptr,cct_ushort length);
static short cob_assoc_key_cmp(cct_assoc_key *k1, cct_assoc_key *k2);
static cct_assoc_key *cob_assoc_key_dispose(cct_assoc_key *k);
static void cob_free_object(void *objptr,cct_ushort objlen);

static cct_assoc_api cob_assoc_api = {
	cob_assoc_key_init,		/* Key initialization */
	cob_assoc_key_cmp,		/* Key comparison */
	cob_assoc_key_dispose,		/* Key disposal */
	cob_free_object,		/* Object disposal */
	0				/* Aux object disposal */
};

/*
 * Initialise a key for COBOL associations :
 */
static cct_assoc_key *
cob_assoc_key_init(cct_assoc_key *genkey,void *ptr,cct_ushort length) {

	if ( length < 1 )
		abort();

	genkey->keylen = length;
	genkey->key    = malloc(genkey->keylen);
	memcpy(genkey->key,ptr,genkey->keylen);
	genkey->kcksum = assoc_key_checksum(genkey->key,genkey->keylen);

	return genkey;
}

/*
 * Binary compare of COBOL keys :
 */
static short
cob_assoc_key_cmp(cct_assoc_key *k1, cct_assoc_key *k2) {
	cct_ushort length = k1->keylen < k2->keylen ? k1->keylen : k2->keylen;
	int diff = memcmp(k1->key,k2->key,length);
		
	if ( length < 1 )
		abort();
	if ( !diff )
		return k1->keylen == k2->keylen ? 0
			: ( k1->keylen < k2->keylen ? -1 : 1 );
	else	return diff < 0 ? -1 : 1;
}

/*
 * Dispose of a key's content :
 */
static cct_assoc_key *
cob_assoc_key_dispose(cct_assoc_key *k) {

	if ( !k->key )
		abort();
	free(k->key);
	k->key = 0;
	k->keylen = 0;
	return k;
}

/*
 * Free the object's contents :
 */
static void
cob_free_object(void *objptr,cct_ushort objlen) {

	if ( !objptr )
		abort();
	free(objptr);
}

/*
 * Get pointer to instance :
 * Returns 0, if not found.
 */
static cct_assoc_array *
cob_assoc_get_instance(cct_assoc_array *ia,cct_assoc_instance_key instance) {
	cct_assoc_iter_handle x = assoc_array_locate(ia,&instance,sizeof instance);

	if ( x == ASSOC_NOENT )
		return 0;		/* Not found */
	return (cct_assoc_array *)assoc_array_object(ia,x);
}

/*
 * New instance array :
 * If the array previously existed, it returns that one instead.
 */
static cct_assoc_array *
cob_assoc_new_instance(cct_assoc_array *ia,cct_assoc_instance_key instance) {
	cct_assoc_array *a = cob_assoc_get_instance(ia,instance);

	if ( !a ) {
		a = assoc_array_new(&cob_assoc_api);
		assoc_array_assign(ia,a,sizeof *a,0,0,&instance,sizeof instance);
	}
	return a;
}

/*
 * Clear and delete the specified instance array :
 * Returns:
 *	0 - successful
 *	1 - not found
 */
static short
cob_assoc_del_instance(cct_assoc_array *ia,cct_assoc_instance_key instance) {
	cct_assoc_iter_handle x = assoc_array_locate(ia,&instance,sizeof instance);

	if ( x == ASSOC_NOENT )
		return 1;
	assoc_array_delete_x(ia,x);
	return 0;
}

/*
 * Copy the user data into allocated storage :
 */
static void *
cob_new_data(void *data,cct_ushort datlen) {
	cct_uchar *obj = malloc(datlen);
	
	memcpy(obj,data,datlen);
	return obj;
}

/********************************************************************************
 * End of (internal) Instance API.
 *
 * Start of COBOL API :
 ********************************************************************************/

/*
 * Associate a value, by instance :
 */
int
NC_ASSOC_ASSIGN(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char   			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	char *key = *pkey;
	cct_ushort keylen = *pkeylen;
	char *data = *pdata;
	cct_ushort datlen = *pdatlen;
	cct_assoc_array *a;
	cct_assoc_iter_handle h;
	int rc = RET_OK;

	INITIALIZE();

	if ( keylen < 1 || datlen < 1 || !key || !data )
		RETURN(RET_BADPARM);

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		a = cob_assoc_new_instance(&instances,instance);

	h = assoc_array_assign(a,cob_new_data(data,datlen),datlen,0,0,key,keylen);

	if ( h == ASSOC_NOENT )
		RETURN(RET_RESOURCE) 		/* Resource has been exhausted */
	else	RETURN(RET_OK);			/* Assignment was successful */
	EXIT;
}

/*
 * Return the last handle assigned by an assignment :
 */
int
NC_LAST_ASSOC(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to receiving handle */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_array *a;

	INITIALIZE();

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		return RET_NOTFOUND;

	*phandle = a->handle;
	return a->handle == ASSOC_NOENT ? RET_END : RET_OK;
}

/*
 * Internal code to return the data content of an assoc entry :
 */
static int
cob_return_content(cct_assoc_array *a,cct_assoc_iter_handle x,char *data,cct_ushort *pdatlen) {
	cct_ushort datlen = *pdatlen;
	void      *obj;				/* Pointer to user data content */
	cct_ushort objlen;			/* Length of user data content */
	int rc = RET_OK;

	if ( datlen < 1 )
		RETURN(RET_BADPARM);		/* This shouldn't happen in here */

	obj = assoc_array_object(a,x);		/* Get access to object pointer */
	if ( !obj )
		RETURN(RET_NOTFOUND);		/* Bad index */

	objlen = assoc_array_objlen(a,x);	/* Get length of this object */

	if ( objlen < datlen ) {
		memset(data,0,datlen);		/* Clear out larger return buffer */
		datlen = objlen;		/* This is the length we are returning */
	}

	if ( datlen > 0 )
		memcpy(data,obj,datlen);	/* Return the data content */
	*pdatlen = datlen;			/* Return content's length */

	EXIT;
}

/*
 * Fetch an associated value, by instance and key :
 */
int
NC_ASSOC_FETCH(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	char *key = *pkey;
	cct_ushort keylen = *pkeylen;
	char *data = *pdata;
	cct_ushort datlen = *pdatlen;
	cct_assoc_array *a;
	cct_assoc_iter_handle x;
	int rc = RET_OK;

	INITIALIZE();

	if ( keylen < 1 || datlen < 1 || !key || !data )
		RETURN(RET_BADPARM);

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* Instance is not known */

	x = assoc_array_locate(a,key,keylen);
	if ( x == ASSOC_NOENT )
		RETURN(RET_NOTFOUND);	/* Key is not known */

	rc = cob_return_content(a,x,data,&datlen);
	*pdatlen = datlen;	/* Return to caller the object's content length */
	EXIT;
}

/*
 * Delete an association by instance and key :
 */
int
NC_ASSOC_DELETE(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen 	/* Pointer to key length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	char *key = *pkey;
	cct_ushort keylen = *pkeylen;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	if ( keylen < 1 || !key )
		RETURN(RET_BADPARM);

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* No such association */

	if ( assoc_array_delete(a,key,keylen) )
		RETURN(RET_OK)		/* Association and data deleted */
	else	RETURN(RET_NOTFOUND);	/* Key value was not found */

	EXIT;
}

/*
 * Delete an item by its handle :
 */
int
NC_ASSOC_DELETE_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to handle to delete */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_iter_handle handle = *phandle;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* Instance is not known */

	if ( !assoc_array_isvalid(a,handle) )
		RETURN(RET_NOTFOUND);	/* Invalid handle */
	
	assoc_array_delete_x(a,handle);
	RETURN(RET_OK);

	EXIT;
}

/*
 * Clear an instance (release memory used by it) :
 */
int
NC_ASSOC_CLEAR(
    cct_assoc_instance_key	*pinstance	/* Pointer to instance to use */
) {
	cct_assoc_instance_key instance = *pinstance;

	if ( instances_inited )
		return cob_assoc_del_instance(&instances,instance)
			? RET_NOTFOUND : RET_OK;
	return RET_OK;
}

/*
 * Return the number of items used by instance :
 */
int
NC_ASSOC_COUNT(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_count		*pcount		/* Pointer to returned count */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a ) {
		*pcount = 0;		/* Return a zeroed count */
		RETURN(RET_NOTFOUND);	/* No such instance */
	}

	*pcount = assoc_array_count(a);

	RETURN(RET_OK);
	EXIT;
}

/*
 * Fetch an association's key by index :
 */
int
NC_ASSOC_FETCH_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char			**pkey,		/* Pointer to key area */
    cct_ushort			*pkeylen,	/* Pointer to key length in bytes */
    char   			**pdata,		/* Pointer to data area */
    cct_ushort   		*pdatlen	/* Pointer to data area length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_iter_handle handle = *phandle;	/* Iter index into the assoc array */
	char *key = *pkey;	        		/* Ptr to receiving key buffer */
	cct_ushort keylen = *pkeylen;   		/* Max length of key buffer */
	char *data = *pdata;	       			/* Ptr to data receiving buffer */
	cct_ushort datalen = *pdatlen;  		/* Max length of data receiving buffer */
	cct_uchar *akey;		/* Ptr to actual key content */
	cct_ushort akeylen;		/* Length of actual key */
	cct_uchar *aobj;		/* Ptr to object content */
	cct_ushort aobjlen;		/* Length of object content */
	cct_assoc_array *a;		/* Associative array */
	int rc = RET_OK;

	INITIALIZE();

	if ( keylen < 1 || datalen < 1 || !key || !data )
		RETURN(RET_BADPARM);

	if ( handle == ASSOC_NOENT )
		RETURN(RET_NOTFOUND);	/* Key is not known */

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* Instance is not known */

	akey = assoc_array_key(a,handle);
	akeylen = assoc_array_keylen(a,handle);
	aobj = assoc_array_object(a,handle);
	aobjlen = assoc_array_objlen(a,handle);

	if ( akeylen < keylen ) {
		memset(key,0,keylen);	/* Zero the return buffer for short key */
		keylen = akeylen;	/* Use the short length for content transfer */
	}
	if ( aobjlen < datalen ) {
		memset(data,0,datalen);	/* Zero the return buffer for short object */
		datalen = aobjlen;	/* Use the short length for content transfer */
	}

	memcpy(key,akey,keylen);	/* Return key content to user */
	memcpy(data,aobj,datalen);	/* Return data content */

	*pkeylen = keylen;		/* Return key length to user */
	*pdatlen = datalen;		/* Return object length to user */

	RETURN(RET_OK);

	EXIT;
}

/*
 * Fetch an association's key by index :
 */
int
NC_ASSOC_FETCH_KEY_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char	                **pkey,		/* Pointer to key area */
    cct_ushort      	        *pkeylen	/* Pointer to key length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_iter_handle handle = *phandle;	/* Iter index into the assoc array */
	char *key = *pkey; 				/* Ptr to receiving key buffer */
	cct_ushort keylen = *pkeylen;   		/* Max length of key buffer */
	cct_uchar *akey;	        		/* Ptr to actual key content */
	cct_ushort akeylen;				/* Length of actual key */
	cct_assoc_array *a;				/* Associative array to use */
	int rc = RET_OK;

	INITIALIZE();

	if ( keylen < 1 || !key )
		RETURN(RET_BADPARM);

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* Instance is not known */

	if ( handle == ASSOC_NOENT )
		RETURN(RET_NOTFOUND);	/* Key is not known */

	akey = assoc_array_key(a,handle);
	akeylen = assoc_array_keylen(a,handle);

	if ( akeylen < keylen ) {
		memset(key,0,keylen);
		keylen = akeylen;
	}

	memcpy(key,akey,keylen);	/* Return key content to user */
	*pkeylen = keylen;	        /* Return key length to user */

	RETURN(RET_OK);
	EXIT;
}

/*
 * Fetch an association's data by index :
 */
int
NC_ASSOC_FETCH_DATA_X(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle,	/* Pointer to handle to fetch */
    char 			**pdata,	/* Pointer to data area */
    cct_ushort			*pdatlen	/* Pointer to data area length in bytes */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_iter_handle handle = *phandle;
	char *data = *pdata;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	if ( *pdatlen < 1 || !*pdata )
		RETURN(RET_BADPARM);

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a )
		RETURN(RET_NOTFOUND);	/* Instance is not known */

	rc = cob_return_content(a,handle,data,pdatlen);

	RETURN(rc);
	EXIT;
}

/*
 * Return an iterator handle to the first assoc array item by instance :
 */
int
NC_ASSOC_FIRST(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to first handle for instance (OUT) */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a ) {
		*phandle = ASSOC_NOENT;
		RETURN(RET_NOTFOUND);	/* No such instance */
	}

	*phandle = assoc_array_first(a);
	RETURN((*phandle) == ASSOC_NOENT ? RET_END : RET_OK);
	EXIT;
}

/*
 * Return the next iterator handle that occurs after handle, by index :
 */
int
NC_ASSOC_NEXT(
    cct_assoc_instance_key	*pinstance,	/* Pointer to instance to use */
    cct_assoc_iter_handle	*phandle	/* Pointer to next handle for instance (IN/OUT) */
) {
	cct_assoc_instance_key instance = *pinstance;
	cct_assoc_iter_handle handle = *phandle;
	cct_assoc_array *a;
	int rc = RET_OK;

	INITIALIZE();

	a = cob_assoc_get_instance(&instances,instance);
	if ( !a ) {
		*phandle = ASSOC_NOENT;
		RETURN(RET_NOTFOUND);	/* No such instance */
	}

	*phandle = assoc_array_next(a,handle);
	RETURN((*phandle) == ASSOC_NOENT ? RET_END : RET_OK);
	EXIT;
}

/*
 * Return a free instance number (assign an instance)
 */
int
NC_ASSIGN_INSTANCE(
    cct_assoc_instance_key	*pinstance
) {
	cct_assoc_iter_handle h;
	cct_assoc_instance_key safety_count = 0;

	INITIALIZE();

	for ( h = 0; h != ASSOC_NOENT; next_instance = ( next_instance + 1 ) % 1000000000U ) {
		h = assoc_array_locate(&instances,&next_instance,sizeof next_instance);
		if ( ++safety_count > 999999999 )
			break;			/* No free instances available */
	}

	if ( h == ASSOC_NOENT ) {
		/* Create the instance to avoid another call returning the same instance */
		(void) cob_assoc_new_instance(&instances,next_instance);
		*pinstance = next_instance++;
		return RET_OK;
	} else	{
		*pinstance = ASSOC_NOENT;
		return RET_RESOURCE;		/* No free instances */
	}
}

/*
 * Clear out all associative arrays :
 */
int
NC_ASSOC_CLEAR_ALL(void) {
	cct_assoc_iter_handle h;
	cct_assoc_instance_key instance;

	if ( instances_inited ) {
		for ( h = assoc_array_first(&instances); h != ASSOC_NOENT;
			h = assoc_array_next(&instances,h) ) {
			instance = *(cct_assoc_instance_key *)
				assoc_array_key(&instances,h);
			cob_assoc_del_instance(&instances,instance);
		}
	}
	return RET_OK;
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/assoc_cob.c,v $ */

/* assoc.h : Associative array support
 * Warren W. Gay VE3WWG
 * Tue Jul  3 11:12:05 2007
 * $Id: assoc.h,v 1.8 2007/09/26 18:17:31 ve3wwg Exp $
 */
#ifndef _assoc_h_
#define _assoc_h_

struct S_CCT_API;
struct S_CCT_ASSOC_ARRAY;

typedef struct S_CCT_API		cct_assoc_api;
typedef struct S_CCT_ASSOC_ARRAY	cct_assoc_array;

typedef cct_unsigned cct_assoc_count;		/* Count type */
typedef cct_assoc_count cct_assoc_iter_handle;	/* Iterator handle */
typedef cct_ushort cct_hash;			/* Hash index */
typedef cct_assoc_count cct_assoc_instance_key;	/* Type for instance key */

typedef struct {
	void		*key;		/* Lookup key */
	cct_ushort	keylen;		/* Key's byte length */
	cct_uchar	kcksum;		/* Key checksum */
} cct_assoc_key;

typedef cct_assoc_key * (*cct_key_init)(cct_assoc_key *key,void *ptr,cct_ushort length);
typedef short (*cct_key_cmp)(cct_assoc_key *key1,cct_assoc_key *key2);
typedef cct_assoc_key * (*cct_key_dispose)(cct_assoc_key *key);
typedef void (*cct_obj_dispose)(void *obj,cct_ushort objlen);
typedef void (*cct_aux_dispose)(void *aux,cct_ushort auxlen);
typedef cct_assoc_array * (*cct_assign)(cct_assoc_array *a,void *object,void *aux,void *ptr,cct_ushort length);
typedef cct_assoc_array * (*cct_delete)(cct_assoc_array *a,void *ptr,cct_ushort length);
typedef cct_assoc_array * (*cct_locate)(cct_assoc_array *a,void *ptr,cct_ushort length);

typedef struct {
	cct_assoc_key	key;		/* Entry key */
	void		*object;	/* Pointer to object */
	cct_ushort	objlen;		/* Object's length */
	void 		*aux;		/* Pointer to aux object */
	cct_ushort	auxlen;		/* Aux object length */
} cct_assoc_ent;

#define ASSOC_KEY_INIT	{0,0,0}
#define ASSOC_ENT_INIT	{ASSOC_KEY_INIT,0,0,0,0} 

struct S_CCT_API {
	cct_key_init	key_init;	/* Create a generic key */
	cct_key_cmp	key_cmp;	/* Compare key */
	cct_key_dispose	key_dispose;	/* Key disposal function to use (optional) */
	cct_obj_dispose	obj_dispose;	/* Object disposal function (optional) */
	cct_aux_dispose aux_dispose;	/* Aux object disposal function (optional) */
};

#define ASSOC_MODBITS	5
#define ASSOC_MODULUS	31
#define ASSOC_MODMASK	0x001F
#define ASSOC_INDEXMSK	((cct_assoc_iter_handle)0x00FFFFFF)
#define ASSOC_HASHSHIFT	(29-ASSOC_MODBITS)

#define ASSOC_MKHANDLE(h,x)	((((cct_assoc_iter_handle)(h))<<(ASSOC_HASHSHIFT))|((cct_assoc_iter_handle)(x)))
#define ASSOC_INDEX(h)		((cct_assoc_iter_handle)(h)&ASSOC_INDEXMSK)
#define ASSOC_HASH(h)		((cct_hash)(((h)>>ASSOC_HASHSHIFT)&ASSOC_MODMASK))

typedef struct S_CCT_ASSOC_LIST {
	cct_assoc_ent	**entries;	/* List of entries for this hash */
	cct_assoc_count	n;		/* Active length of list */
	cct_assoc_count	alloc;		/* Allocated length of list */
	cct_assoc_count	holes;		/* Number of holes within the list */
} cct_assoc_list;

struct S_CCT_ASSOC_ARRAY {
	cct_assoc_list	list[ASSOC_MODULUS]; /* The collection of lists by hash */
	cct_assoc_count	count;		/* Count of items held */
	cct_assoc_api	api;		/* Function pointers to use for API */
	cct_bool	dirty;		/* Dirty flag (for count) */
	cct_assoc_iter_handle handle;	/* Last handle assigned */
};

#define ASSOC_API_INIT	{ 0, 0, 0, 0, 0 }
#define ASSOC_LIST_ENT_INIT { 0,0,0,0 }
#define ASSOC_LIST_INIT { \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, \
	ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT, ASSOC_LIST_ENT_INIT }
#define ASSOC_INIT  	{ ASSOC_LIST_INIT,0,ASSOC_API_INIT,0 }

#define ASSOC_NOENT 	((cct_assoc_iter_handle)999999999)

extern cct_ushort assoc_key_checksum(void *ptr,cct_ushort length);

extern cct_assoc_array *assoc_array_init(cct_assoc_array *a,cct_assoc_api *api);
extern cct_assoc_array *assoc_array_new(cct_assoc_api *api);
extern void assoc_array_free(cct_assoc_array *a);

extern cct_assoc_array *assoc_array_clear(cct_assoc_array *a);
extern cct_assoc_iter_handle assoc_array_locate(cct_assoc_array *a,void *keyptr,cct_ushort keylen);
extern cct_assoc_array *assoc_array_delete(cct_assoc_array *a,void *keyptr,cct_ushort keylen);
extern cct_assoc_array *assoc_array_delete_x(cct_assoc_array *a,cct_assoc_iter_handle x);

extern cct_assoc_iter_handle
assoc_array_assign(
    cct_assoc_array *a,
    void            *object,
    cct_ushort      objlen,
    void            *aux,
    cct_ushort      auxlen,
    void            *keyptr,
    cct_ushort      keylen
);

extern cct_bool assoc_array_isvalid(cct_assoc_array *a,cct_assoc_iter_handle x);
extern void *assoc_array_key(cct_assoc_array *a,cct_assoc_iter_handle x);
extern cct_ushort assoc_array_keylen(cct_assoc_array *a,cct_assoc_iter_handle x);
extern void *assoc_array_object(cct_assoc_array *a,cct_assoc_iter_handle x);
extern cct_ushort assoc_array_objlen(cct_assoc_array *a,cct_assoc_iter_handle x);
extern void *assoc_array_aux(cct_assoc_array *a,cct_assoc_iter_handle x);
extern cct_ushort assoc_array_auxlen(cct_assoc_array *a,cct_assoc_iter_handle x);

extern cct_assoc_count assoc_array_count(cct_assoc_array *a);
extern cct_assoc_iter_handle assoc_array_first(cct_assoc_array *a);
extern cct_assoc_iter_handle assoc_array_next(cct_assoc_array *a,cct_assoc_iter_handle x);

extern cct_bool assoc_array_isvalid_handle(cct_assoc_iter_handle h);

#endif /* _assoc_h_ */

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/assoc.h,v $ */

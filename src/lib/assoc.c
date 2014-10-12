/* assoc.c : Associative array support
 * Warren W. Gay VE3WWG
 * Tue Jul  3 11:12:05 2007
 * $Id: assoc.c,v 1.10 2007/07/19 20:52:46 ve3wwg Exp $
 */
#include <cobcurses.h>
#include <assoc.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/********************************************************************************
 * SUPPORT ROUTINES 
 ********************************************************************************/

/*
 * Generate a key check sum :
 */
cct_ushort
assoc_key_checksum(void *ptr,cct_ushort length) {
	cct_assoc_iter_handle x;
	cct_uchar *cp = ptr;
	cct_ushort cksum = 0;

	for ( x=0; x<length; ++x )
		cksum = (cksum << 1) + *cp++;
	return cksum;
}

/********************************************************************************
 * INTERNAL SUPPORT ROUTINES  
 ********************************************************************************/

/*
 * Generic new assoc ent :
 */
static cct_assoc_ent *
assoc_ent_new(cct_assoc_key *key,void *object,cct_ushort objlen,void *aux,cct_ushort auxlen) {
	cct_assoc_ent *p = malloc(sizeof (cct_assoc_ent));

	memcpy(&p->key,key,sizeof p->key);	/* Copy in the key contents */
	p->object = object;
	p->objlen = objlen;
	p->aux    = aux;
	p->auxlen = auxlen;
	return p;
}

/*
 * Delete the contents of an assoc array entry :
 */
static cct_assoc_ent *
assoc_ent_delete(cct_assoc_ent *p,cct_assoc_api *api) {

	if ( api->key_dispose )
		api->key_dispose(&p->key);		/* Dispose of key content */
	if ( api->obj_dispose && p->object != 0 )
		api->obj_dispose(p->object,p->objlen);	/* Clear out object content */
	if ( api->aux_dispose && p->aux != 0 )
		api->aux_dispose(p->aux,p->auxlen);	/* Clear out aux object content */

	p->object = p->aux = 0;	
	free(p);				/* Free this entry */
	return 0;
}

static cct_assoc_ent *
assoc_ent_replace(
    cct_assoc_ent *p,
    cct_assoc_api *api,
    void	  *obj,
    cct_ushort	  objlen,
    void	  *aux,
    cct_ushort    auxlen) {

	if ( api->obj_dispose && p->object != 0 )
		api->obj_dispose(p->object,p->objlen);	/* Clear out object content */
	if ( api->aux_dispose && p->aux != 0 )
		api->aux_dispose(p->aux,p->auxlen);	/* Clear out aux object content */

	p->object = obj;
	p->objlen = objlen;
	p->aux    = aux;
	p->auxlen = auxlen;

	return p;
}

/*
 * Locate a free cell, or allocate a new
 * one for an associative array item :
 */
static cct_assoc_iter_handle
assoc_list_alloc(cct_assoc_list *lst) {
	cct_assoc_iter_handle x;

	if ( lst->holes > 0 ) 
		for ( x=0; x<lst->n; ++x )
			if ( !lst->entries[x] ) {
				--lst->holes;
				return x;
			}

	if ( lst->n >= lst->alloc ) {
		lst->alloc += 64;
		lst->entries = realloc(lst->entries,lst->alloc * sizeof (cct_assoc_ent));
	}

	if ( lst->n + 1 > ASSOC_INDEXMSK )
		return ASSOC_NOENT;	/* Resource has been exhausted for this list */

	return lst->n++;
}

/*
 * Reset all but api info :
 */
static cct_assoc_list *
assoc_list_reset(cct_assoc_list *lst) {
	lst->n       = 0;			/* List is empty */
	lst->alloc   = 0;			/* Nothing is allocated */
	lst->entries = 0;			/* There is no list yet */
	lst->holes   = 0;			/* There are no holes yet */
	return lst;
}

static cct_assoc_array *
assoc_array_reset(cct_assoc_array *a) {
	cct_ushort lx;

	for ( lx=0; lx<ASSOC_MODULUS; ++lx )
		assoc_list_reset(&a->list[lx]);
	a->count = 0;
	a->dirty = 0;
	a->handle = ASSOC_NOENT;
	return a;
}

/********************************************************************************
 * EXTERNAL API ROUTINES  
 ********************************************************************************/

/*
 * Initialize an assoc array data item :
 */
cct_assoc_array *
assoc_array_init(cct_assoc_array *a,cct_assoc_api *api) {

	memset(a,0,sizeof *a);		/* Clear object to zeros */
	a->api     = *api;		/* Copy over the api set to use */
	return assoc_array_reset(a);	/* Reset all but api info */
}

/*
 * Allocate and return an associative array :
 */
cct_assoc_array *
assoc_array_new(cct_assoc_api *api) {
	cct_assoc_array *a = malloc(sizeof (cct_assoc_array));

	return assoc_array_init(a,api);
}

/*
 * Free an allocated associative array :
 */
void
assoc_array_free(cct_assoc_array *a) {

	assoc_array_clear(a);
	free(a);
}

/*
 * Generic locate item by key :
 */
static cct_assoc_iter_handle
assoc_list_locate(cct_assoc_list *lst,void *keyptr,cct_ushort keylen,cct_assoc_api *api) {
	cct_assoc_key key;
	cct_assoc_iter_handle lx;

	api->key_init(&key,keyptr,keylen);
	for ( lx=0; lx<lst->n; ++lx )
		if ( lst->entries[lx] && !api->key_cmp(&lst->entries[lx]->key,&key) )
			goto xit;
	lx = ASSOC_NOENT;

xit:	if ( api->key_dispose )
		api->key_dispose(&key);
	return lx;
}

/*
 * Locate an item in the array by key :
 */
static cct_assoc_iter_handle
assoc_array_locate_hx(cct_assoc_array *a,void *keyptr,cct_ushort keylen,cct_hash *phx) {
	cct_assoc_key key;
	cct_hash hx;
	cct_assoc_iter_handle lx;

	a->api.key_init(&key,keyptr,keylen);
	hx = key.kcksum % ASSOC_MODULUS;

	lx = assoc_list_locate(&a->list[hx],keyptr,keylen,&a->api);

	if ( phx != 0 )
		*phx = hx;

	if ( a->api.key_dispose )
		a->api.key_dispose(&key);
	return lx;
}

cct_assoc_iter_handle
assoc_array_locate(cct_assoc_array *a,void *keyptr,cct_ushort keylen) {
	cct_hash hx;
	cct_assoc_iter_handle lx;

	lx = assoc_array_locate_hx(a,keyptr,keylen,&hx);
	if ( lx == ASSOC_NOENT )
		return ASSOC_NOENT;
	return ASSOC_MKHANDLE(hx,lx);
}

/*
 * Generic delete item by index :
 */
static cct_assoc_list *
assoc_list_delete_x(cct_assoc_list *lst,cct_assoc_iter_handle lx,cct_assoc_api *api) {

	if ( lx >= lst->n )
		abort();
	if ( !lst->entries[lx] )
		abort();

	lst->entries[lx] = assoc_ent_delete(lst->entries[lx],api);
	++lst->holes;
	return lst;
}

cct_assoc_array *
assoc_array_delete_x(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx;
	cct_assoc_iter_handle lx;

	if ( !assoc_array_isvalid(a,x) )
		return 0;

	hx = ASSOC_HASH(x);
	lx = ASSOC_INDEX(x);
	assoc_list_delete_x(&a->list[hx],lx,&a->api);
	a->dirty = 1;
	if ( x == a->handle )
		a->handle = ASSOC_NOENT;
	return a;
}

/*
 * Generic locate and delete item by key :
 */
cct_assoc_array *
assoc_array_delete(cct_assoc_array *a,void *keyptr,cct_ushort keylen) {
	cct_hash hx;
	cct_assoc_iter_handle lx;

	lx = assoc_array_locate_hx(a,keyptr,keylen,&hx);

	if ( lx == ASSOC_NOENT )
		return 0;			/* No matching entry was found */

	assoc_list_delete_x(&a->list[hx],lx,&a->api);
	a->dirty = 1;
	return a;
}

/*
 * Generic assign new value by key :
 */
cct_assoc_iter_handle
assoc_array_assign(
    cct_assoc_array *a,
    void            *object,
    cct_ushort      objlen,
    void            *aux,
    cct_ushort      auxlen,
    void            *keyptr,
    cct_ushort      keylen
) {
	cct_assoc_key key;
	cct_hash hx;
	cct_assoc_iter_handle lx;

	a->api.key_init(&key,keyptr,keylen);
	hx = key.kcksum % ASSOC_MODULUS;
	lx = assoc_list_locate(&a->list[hx],keyptr,keylen,&a->api);

	if ( lx == ASSOC_NOENT ) {
		/* New Entry to be created */
		lx = assoc_list_alloc(&a->list[hx]);
		if ( lx == ASSOC_NOENT )
			return lx;		/* Resource is full! */
 		a->list[hx].entries[lx] = assoc_ent_new(&key,object,objlen,aux,auxlen);
		/* Don't dispose of key -- contents stolen by assoc_ent_new() above!! */
		++a->count;
	} else	{
		/* Replace values in existing object */
		assoc_ent_replace(a->list[hx].entries[lx],&a->api,object,objlen,aux,auxlen);
		if ( a->api.key_dispose )
			a->api.key_dispose(&key);
	}
	a->handle = ASSOC_MKHANDLE(hx,lx);
	return a->handle;
}

/*
 * Test if a subscript is valid :
 */
static cct_bool
assoc_list_isvalid(cct_assoc_list *lst,cct_assoc_iter_handle lx) {
	return ( lx == ASSOC_NOENT || !lst->n || lx > lst->n || !lst->entries || !lst->entries[lx] ) ? 0 : 1;
}

cct_bool
assoc_array_isvalid(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( hx >= ASSOC_MODULUS )
		return 0;
	return assoc_list_isvalid(&a->list[hx],lx);
}

#if 0
/*
 * Range check a handle for validity, without regard for
 * array affinity.
 */
cct_bool
assoc_array_isvalid_handle(cct_assoc_iter_handle h) {
	cct_hash hx = ASSOC_HASH(h);
	cct_assoc_iter_handle lx = ASSOC_INDEX(h);

	if ( hx >= ASSOC_MODULUS || lx > ASSOC_INDEXMSK )
		return 0;
	return 1;
}
#endif

/*
 * Return generic pointer to key value :
 */
void *
assoc_array_key(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->key.key;
}

/*
 * Return generic pointer to key length :
 */
cct_ushort
assoc_array_keylen(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->key.keylen;
}

/*
 * Return the object by subscript :
 */
void *
assoc_array_object(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->object;
}

/*
 * Return the object's length :
 */
cct_ushort
assoc_array_objlen(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->objlen;
}

/*
 * Return aux object by subscript :
 */
void *
assoc_array_aux(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->aux;
}

/*
 * Return aux object's length by subscript :
 */
cct_ushort
assoc_array_auxlen(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);

	if ( !assoc_array_isvalid(a,x) )
		abort();
	return a->list[hx].entries[lx]->auxlen;
}

/*
 * Clear the contents of an assoc array list :
 */
static cct_assoc_list *
assoc_list_clear(cct_assoc_list *lst,cct_assoc_api *api) {
	cct_assoc_iter_handle x;

	for ( x=0; x<lst->n; ++x )
		if ( lst->entries[x] != 0 )
			lst->entries[x] = assoc_ent_delete(lst->entries[x],api);
	free(lst->entries);
	return assoc_list_reset(lst);
}

/*
 * Clear the contents of an assoc array :
 */
cct_assoc_array *
assoc_array_clear(cct_assoc_array *a) {
	cct_hash hx;

	for ( hx=0; hx<ASSOC_MODULUS; ++hx )
		assoc_list_clear(&a->list[hx],&a->api);
	return assoc_array_reset(a);
}

/*
 * Return the count of items in this list :
 */
static cct_assoc_count
assoc_list_count(cct_assoc_list *lst) {
	return lst->n - lst->holes;
}

/*
 * Return the count of items in this array :
 */
cct_assoc_count
assoc_array_count(cct_assoc_array *a) {
	cct_hash hx;

	if ( a->dirty ) {
		a->count = 0;
		for ( hx=0; hx<ASSOC_MODULUS; ++hx )
			a->count += assoc_list_count(&a->list[hx]);
	}
	return a->count;
}

/*
 * Return the next list subscript after x :
 */
static cct_assoc_iter_handle
assoc_list_next(cct_assoc_list *lst,cct_assoc_iter_handle lx) {
	
	if ( lx == ASSOC_NOENT )
		lx = 0;
	else	++lx;

	for ( ; lx < lst->n; ++lx )
		if ( lst->entries[lx] )
			return lx;
	return ASSOC_NOENT;
}

/*
 * Return the next array subscript after x :
 */
cct_assoc_iter_handle
assoc_array_next(cct_assoc_array *a,cct_assoc_iter_handle x) {
	cct_hash hx = ASSOC_HASH(x);
	cct_assoc_iter_handle lx = ASSOC_INDEX(x);
	cct_assoc_iter_handle h = ASSOC_NOENT;

	if ( x == ASSOC_NOENT ) {
		hx = 0;				/* We want first entry */
		lx = ASSOC_NOENT;		/* Hint to assoc_list_next() */
	} else	{
		lx = ASSOC_INDEX(x);		/* Else after this one */
	}

	for (; hx<ASSOC_MODULUS; ++hx ) {
		lx = assoc_list_next(&a->list[hx],lx);
		if ( lx != ASSOC_NOENT ) {
			h = ASSOC_MKHANDLE(hx,lx);
			break;			
		}
	}
	return h;
}

/*
 * Return the first array subscript, if any :
 */
cct_assoc_iter_handle
assoc_array_first(cct_assoc_array *a) {
	return assoc_array_next(a,ASSOC_NOENT);
}

/* $Source: /cvsroot/cobcurses/cobcurses/src/lib/assoc.c,v $ */

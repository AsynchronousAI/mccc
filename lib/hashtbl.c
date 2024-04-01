/* File:      hashtbl.c
 * Author(s):
 * Contact:
 * 
 */

#include <stdlib.h>

#include "hashtbl.h"

//Hashtbl * ht_create(int logsize, void (*destroy)(void *data))
Hashtbl * ht_create(int logsize)
{
    Hashtbl *htbl;
    int i;
    ub4 len = ((ub4)1 << logsize);
 
    /* Allocate space for the hash table. */
    if ((htbl = (Hashtbl *) malloc(sizeof(Hashtbl))) == NULL)
	return NULL;

    htbl->table = (HashItem **) malloc(len * sizeof(HashItem *));
    if (htbl->table == NULL) {
	free(htbl);
	return NULL;
    }

    /* Initialize the hash table. */
    for (i=0; i<len; i++) 
	htbl->table[i] = NULL;
    htbl->logsize = logsize;
    htbl->mask    = len - 1;
    htbl->count   = 0;
//    htbl->destroy = destroy;

    return htbl;
}

void ht_destroy(Hashtbl *htbl) 
{
    HashItem *h, *next;
    int i;
    ub4 len = (ub4)1 << htbl->logsize;

    /* Destroy each bucket. */
    for (i = 0; i < len; i++) {
	h = htbl->table[i];
	while (h) {
	    next = h->next;
//	    htbl->destroy(h->object);
	    free(h);
	    h = next;
	}
    }

    /* Free the storage allocated for the hash table. */
    free(htbl->table);
    free(htbl);

    return;
}

int ht_exist(const Hashtbl *htbl, ub4 key) 
{
    HashItem *h;
    int bucket;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    for (h = htbl->table[bucket]; h; h = h->next) {
	if (key == h->key) {
	    /* Return that the item has been found. */
	    return 1;
	}
    }

    /* Return that the item was not found. */
    return 0;
}

void * ht_find(const Hashtbl *htbl, ub4 key) 
{
    HashItem *h;
    int bucket;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    for (h = htbl->table[bucket]; h; h = h->next) {
	if (key == h->key) {
	    /* Pass back the object from the item. */
	    return h->object;
	}
    }

    /* Return that the item was not found. */
    return NULL;
}

int ht_add(Hashtbl *htbl, ub4 key, const void *object) 
{
    HashItem *h, **hp;
    int bucket;

    /* Do nothing if the item is already in the table. */
    if (ht_find(htbl, key) != NULL)
	return 1;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Allocate space for the new item */
    if ((h = (HashItem *) malloc(sizeof(HashItem))) == NULL)
	return -1;

    htbl->count++;

    /* Make the hash table bigger if it is getting full. */
    /*
    if (htbl->count > (ub4)1 << (htbl->logsize)) {
	ht_grow(htbl);
    }
    */

    /* Add the new item to the table */
    h->key = key;
    h->object = (void *) object;
    hp = &htbl->table[bucket];
    h->next = *hp;
    *hp = h;

    return 0;
}

int ht_remove(Hashtbl *htbl, ub4 key) 
{
    HashItem *h, *prev;
//    void *object;
    int bucket;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    prev = NULL;

    for (h = htbl->table[bucket]; h; h = h->next) {
	if (key == h->key) {
	    /* Remove the item from the bucket */
	    if (prev) 
		prev->next = h->next;
	    else 
		htbl->table[bucket] = h->next;
//	    htbl->destroy(h->object);
	    free(h);
	    htbl->count--;
	    return 0;
	}
	prev = h;
    }

    /* Return that the item was not found. */
    return -1;
}

void ht_iter(Hashtbl *htbl, void (*process)(ub4 key, void *object)) 
{
    HashItem *h;
    int i, j;
    ub4 len = (ub4)1 << htbl->logsize;

    /* Walk over each bucket. */
    for (i = 0; i < len; i++) {
	for (h = htbl->table[i]; h; h = h->next) {
	    process(h->key, h->object);
	}
    }

    return;
}

void ht_stat(const Hashtbl *htbl)
{
    HashItem *h;
    int i, j;
    ub4 len = (ub4)1 << htbl->logsize;
    int items[10];      /* The nth element stores the number of buckets,
			 * which have n (or >=9 when n=9) items.
			 */

    for (i = 0; i < 10; i++)
	items[i] = 0;

    /* Walk over each bucket. */
    for (i = 0; i < len; i++) {
	for (h = htbl->table[i], j=0; h; ++j, h = h->next)
	    ;
	if (j > 9) j = 9;
	items[j]++;
    }

    /* Print statistics */
    printf("\n");
    for (i = 0; i < 10; i++) {
	printf("items %ld:  %ld buckets\n", i, items[i]);
    }
    printf("buckets: %ld    items: %ld      load factor: %g\n\n",
	   len, htbl->count, (double)htbl->count/len);

    return;
    
}

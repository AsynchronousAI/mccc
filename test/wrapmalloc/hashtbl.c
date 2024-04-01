#include <stdio.h>
#include <stdlib.h>
#include "hashtbl.h"

void ht_init(Hashtbl *htbl)
{
    int i, j;

    /* Initialize the hash table. */
    for (i=0; i<HASH_TABLE_SIZE; i++) 
        for (j=0; j<HASH_COLLIST_SIZE; j++)
            htbl->table[i][j].key = 0;
    htbl->logsize = HASH_BITS;
    htbl->mask    = HASH_MASK;
    htbl->count   = 0;
}


int ht_exist(const Hashtbl *htbl, ub4 key) 
{
    int bucket, j;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    for (j = 0; j < HASH_COLLIST_SIZE; j++) {
        if (key == htbl->table[bucket][j].key) {
	    /* Return that the item has been found. */
	    return 1;
	}
    }

    /* Return that the item was not found. */
    return 0;
}

ub4 ht_find(const Hashtbl *htbl, ub4 key) 
{
    int bucket, j;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    for (j = 0; j < HASH_COLLIST_SIZE; j++) {
        if (key == htbl->table[bucket][j].key) {
	    /* Pass back the object from the item. */
	    return htbl->table[bucket][j].data;
	}
    }

    /* Return zero when the item was not found. */
    return 0;
}

int ht_add(Hashtbl *htbl, ub4 key, ub4 data) 
{
    HashItem *h;
    int bucket, j;

    /* Do nothing if the item is already in the table. */
    if (ht_find(htbl, key) != 0)
	return 1;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    htbl->count++;

    /* Add the new item to the table */
    for (j = 0; j < HASH_COLLIST_SIZE; j++) {
        if (0 == htbl->table[bucket][j].key) {
	    /* Return that the item has been found. */
	    h = &(htbl->table[bucket][j]);
            break;
	}
    }

    if (j == HASH_COLLIST_SIZE) {
        fprintf(stderr, "Exceeds the limit of collision list\n");
        return -1;
    } 

    h->key = key;
    h->data = data;

    return 0;
}

int ht_remove(Hashtbl *htbl, ub4 key) 
{
    HashItem *h;
    int bucket, j;

    /* Hash the key. */
    bucket = ht_hash(key, htbl->logsize, htbl->mask);

    /* Search for the item in the bucket. */
    for (j = 0; j < HASH_COLLIST_SIZE; j++) {
        if (key == htbl->table[bucket][j].key) {
	    /* Return that the item has been found. */
	    h = &(htbl->table[bucket][j]);
            break;
	}
    }

    if (j == HASH_COLLIST_SIZE) {
        /* Not found */
        return -1;
    } 

    h->key = 0;
    h->data = 0;

    htbl->count--;
    
    return 0;
}


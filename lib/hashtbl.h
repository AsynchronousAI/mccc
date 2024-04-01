/* File:      hashtbl.h
 * Author(s): 
 * Contact:
 */

#ifndef HASHTBL_H
#define HASHTBL_H

#include <stdlib.h>

/* Shrink the hash table when its load factor is less than MIN_LOAD_FACTOR;
 * grow the hash table when its load factor is greater than MAX_LOAD_FACTOR.
 */
#define MIN_LOAD_FACTOR         0.5
#define MAX_LOAD_FACTOR         3.0


/****************************************************************************
 * Hashtable data structure
 * Use chaining to resolve conflicts.
 ***************************************************************************/

typedef unsigned long int ub4;

typedef struct HashItem_ {
    ub4 	      key; 	/* unique key */
    void             *object; 	/* actual object */
    struct HashItem_ *next;	/* next HashItem in list */
} HashItem;

typedef struct Hashtbl_ {
    HashItem        **table;
    int 	      logsize; 	/* log of number of buckets */
    ub4		      mask;  	/* (hval & mask) is position in table */
    int               count;	/* number of items */
//    void (*destroy) (void *object);  
//    		    	/* user-defined function for destroying an object */
} Hashtbl;


/* Hash function */
#define ht_hash(key, bits, mask) (((key) ^ ((key) >> (bits))) & (mask)) 
 
/* Number of items in a hash table. */
#define ht_count(htbl) ((htbl)->count)

#ifdef __cplusplus
extern "C" {
#endif
    
    /* Create a hash table. */
//    Hashtbl * ht_create(int logsize, void (*destroy)(void *object));
    Hashtbl * ht_create(int logsize);

    /* Destroy a hash table. */
    void ht_destroy(Hashtbl *htbl);

    /* Search for an element in a hash table. */
    int ht_exist(const Hashtbl *htbl, ub4 key);
    void * ht_find(const Hashtbl *htbl, ub4 key);

    /* Add an element to a hash table. */
    int ht_add(Hashtbl *htbl, ub4 key, const void *object);

    /* Remove an element from a hash table. */
    int ht_remove(Hashtbl *htbl, ub4 key);

    void ht_iter(Hashtbl *htbl, void (*process)(ub4 key, void *object));

    void ht_stat(const Hashtbl *htbl);

#ifdef __cplusplus
}
#endif
    

#endif


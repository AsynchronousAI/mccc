#ifndef __HASHTBL_H__
#define __HASHTBL_H__

#define HASH_BITS               20
#define HASH_TABLE_SIZE         (1 << HASH_BITS)
#define HASH_MASK		(HASH_TABLE_SIZE - 1)
#define HASH_COLLIST_SIZE	8

typedef unsigned long int ub4;

typedef struct HashItem_ {
    ub4 	      key; 	/* unique key, or pointer address */
    ub4               data;     /* data, or allocated size */
} HashItem;

typedef struct Hashtbl_ {
    HashItem    table[HASH_TABLE_SIZE][HASH_COLLIST_SIZE];
    ub4         logsize;
    ub4         mask;
    ub4         count;	/* number of items */
} Hashtbl;


/* Hash function */
#define ht_hash(key, bits, mask) (((key) ^ ((key) >> (bits))) & (mask)) 
 
/* Number of items in a hash table. */
#define ht_count(htbl) ((htbl)->count)

void ht_init(Hashtbl *htbl);

/* Search for an element in a hash table. */
int ht_exist(const Hashtbl *htbl, ub4 key);
ub4 ht_find(const Hashtbl *htbl, ub4 key);

/* Add an element to a hash table. */
int ht_add(Hashtbl *htbl, ub4 key, ub4 data);

/* Remove an element from a hash table. */
int ht_remove(Hashtbl *htbl, ub4 key);

#endif

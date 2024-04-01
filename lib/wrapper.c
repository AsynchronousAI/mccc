#include <stdio.h>

#include "cmemsafe.h"
#include "hashtbl.h"

Hashtbl * heap_table;

int wrapper_init()
{
/*     heap_table = ht_create(12); */
/*     return (heap_table == NULL); */
}

void wrapper_cleanup()
{
/*     ht_destroy(heap_table); */
}

void *wrapper_calloc(unsigned long nmemb, unsigned long size,
                     csafe_capability ** cap_ptr)
{
    struct csafe_header *p;

    p = calloc(1, nmemb * size + sizeof(struct csafe_header));

    if (!p) {
        printf("Memory allocation failed\n");
        exit(1);
    }

    p->_base = p + 1;
    p->_size = size;
#ifdef CSAFE_NEED_TEMPORAL
    *cap_ptr = csafe_HCS_allocate();
    p->_cap_index = **cap_ptr;
#endif
/*    if (p) ht_add(heap_table, (ub4) p, (void *) 0); */
    return (void *)(p+1);
}

void *wrapper_malloc(unsigned long size, csafe_capability ** cap_ptr)
{
    struct csafe_header *p;

    p = malloc(size + sizeof(struct csafe_header));

    if (!p) {
	printf("Memory allocation failed\n");
	exit(1);
    }
/*    ht_add(heap_table, (ub4) p, (void *) 0); */
    p->_base = p+1;
    p->_size = size;
#ifdef CSAFE_NEED_TEMPORAL
    *cap_ptr = csafe_HCS_allocate();
    p->_cap_index = **cap_ptr;
#endif
    return (void *)(p+1);

}

void *wrapper_realloc(void *ptr, unsigned long size,
                      csafe_capability ** cap_ptr)
{
    struct csafe_header *p;

    p = realloc(ptr, size + sizeof(struct csafe_header));

    if (!p) {
	printf("Memory allocation failed\n");
	exit(1);
    }
/*    ht_add(heap_table, (ub4) p, (void *) 0); */
    p->_base = p+1;
    p->_size = size;
#ifdef CSAFE_NEED_TEMPORAL
    *cap_ptr = csafe_HCS_allocate();
    p->_cap_index = **cap_ptr;
#endif
    return (void *)(p+1);
}

void wrapper_free(void *ptr, csafe_capability * cap_ptr)
{
    struct csafe_header *p;

    if (ptr) {
	p = (struct csafe_header *)ptr - 1;
/*	if (ht_exist(heap_table, (ub4) p)) { */
#ifdef CSAFE_NEED_TEMPORAL
            csafe_HCS_release(cap_ptr);
#endif
/*	    ht_remove(heap_table, (ub4) p); */
	    free(p);
	    return;
/*	} */
    }
    
    fprintf(stderr, "free: invalid heap pointer\n"); 
}

/* #define my_malloc malloc */
/* #define my_free free */

/* #define my_malloc wrapper_malloc */
/* #define my_free wrapper_free */

/* int main() */
/* { */
/*    char **p; */
/*    unsigned long i, num = 0x4ffff; */
   
/*    csafe_init(); */

/*    p = (char **) my_malloc (num * sizeof (char **)); */

/*    for (i = 0; i < num; i++) */
/*        p[i] = (char *) my_malloc(1024); */

/*    for (i=0; i<num; i++) */
/*        my_free(p[i]); */

/*    my_free(p); */

/*    csafe_cleanup(); */
/* } */

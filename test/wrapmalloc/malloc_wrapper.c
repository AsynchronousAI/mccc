#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#define __USE_GNU
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>

#include "hashtbl.h"

Hashtbl  allocation_table;

typedef void *(*calloc_type)(size_t nmemb, size_t size);
typedef void *(*malloc_type)(size_t size);
typedef void *(*realloc_type)(void *ptr, size_t size);
typedef void (*free_type)(void *ptr);
typedef void (*exit_type)(int status);

calloc_type  orig_calloc  = 0;
malloc_type  orig_malloc  = 0;      
realloc_type orig_realloc = 0;
free_type    orig_free    = 0;
exit_type    orig_exit    = 0;

unsigned long max_heap_usage = 0;
unsigned long last_max_heap_usage = 0;
unsigned long cur_heap_usage = 0;
unsigned long calloc_heap = 0;
unsigned long malloc_heap = 0;
unsigned long realloc_heap = 0;
unsigned long free_heap = 0;

unsigned long calloc_counter = 0;
unsigned long malloc_counter = 0;
unsigned long realloc_counter = 0;
unsigned long free_counter = 0;

FILE * logchannel;

void __attribute__ ((constructor)) initialize()
{
    // __USE_GNU must be defined to use RTLD_NEXT
    orig_calloc  = (calloc_type) dlsym(RTLD_NEXT, "calloc") ;
    orig_malloc  = (malloc_type) dlsym(RTLD_NEXT, "malloc") ;
    orig_realloc = (realloc_type) dlsym(RTLD_NEXT, "realloc") ;
    orig_free    = (free_type) dlsym(RTLD_NEXT, "free") ;
    orig_exit    = (exit_type) dlsym(RTLD_NEXT, "exit") ;

    ht_init(&allocation_table);
/*     if ((logchannel = fopen("/tmp/mscc_heap_usage.txt", "rw")) == NULL) { */
/*         fprintf(stderr, "Failed to open log channel\n"); */
/*         exit(1); */
/*     } */
    logchannel = stderr;

    fprintf(logchannel, "Heap Usage for Process %d\n", getpid());
}

void record_memusage(int force)
{
    if (force || (max_heap_usage - last_max_heap_usage > 1024L)) {
        last_max_heap_usage = max_heap_usage;
        fprintf(logchannel, 
                "\nLibrary wrapper: maximum heap usage = %ld bytes.\n", 
                max_heap_usage);
        fprintf(logchannel, 
                "Library wrapper: calloc heap = %ld bytes.\n", 
                calloc_heap);
        fprintf(logchannel, 
                "Library wrapper: malloc heap = %ld bytes.\n", 
                malloc_heap);
        fprintf(logchannel, 
                "Library wrapper: realloc heap = %ld bytes.\n", 
                realloc_heap);
        fprintf(logchannel, 
                "Library wrapper: freed heap = %ld bytes.\n", 
                free_heap);
        fprintf(logchannel, 
                "Library wrapper: calloc counter = %ld.\n", 
                calloc_counter);
        fprintf(logchannel, 
                "Library wrapper: malloc counter = %ld.\n", 
                malloc_counter);
        fprintf(logchannel, 
                "Library wrapper: realloc counter = %ld.\n", 
                realloc_counter);
        fprintf(logchannel, 
                "Library wrapper: free counter = %ld.\n\n", 
                free_counter);
    }
}

void *calloc(size_t nmemb, size_t size)
{
    void *p;
    ub4 alloc_size;

    alloc_size = nmemb * size;
    calloc_heap += alloc_size;
    calloc_counter++;

    cur_heap_usage += alloc_size;

    if (cur_heap_usage > max_heap_usage) {
        max_heap_usage = cur_heap_usage;
        record_memusage(0);
    }

    if (orig_calloc != 0) {
	p = orig_calloc(nmemb, size);
        ht_add (&allocation_table, (ub4)p, alloc_size);
        return p;
    }

    return 0;
}

void *malloc(size_t size)
{
    void *p;

    malloc_heap += size;
    malloc_counter++;

    cur_heap_usage += size;
    if (cur_heap_usage > max_heap_usage) {
        max_heap_usage = cur_heap_usage;
        record_memusage(0);
    }

    if (orig_malloc != 0) {
	p = orig_malloc(size);
        ht_add(&allocation_table, (ub4)p, size);
        return p;

    }
    return 0;
}

void *realloc(void *ptr, size_t size)
{
    void *p;
    ub4 oldsize;

    realloc_heap += size;
    realloc_counter++;

    oldsize = ht_find (&allocation_table, (ub4)ptr);
    cur_heap_usage += (size - oldsize);
    ht_remove (&allocation_table, (ub4)ptr);

    if (cur_heap_usage > max_heap_usage) {
        max_heap_usage = cur_heap_usage;
        record_memusage(0);
    }

    if (orig_realloc != 0) {
	p = orig_realloc(ptr, size);
        ht_add(&allocation_table, (ub4)p, size);
        return p;
    }
    return 0;
}

void free(void *ptr)
{
    ub4 size;

    free_counter++;

    if (orig_free != 0) {
        size = ht_find (&allocation_table, (ub4) ptr);
        cur_heap_usage -= size;
        free_heap += size;
        ht_remove (&allocation_table, (ub4) ptr);
	orig_free(ptr);
    }
}

__attribute__((__noreturn__)) void exit(int status) 
{
    record_memusage(1);

    if (orig_exit != 0) 
        orig_exit(status);
}

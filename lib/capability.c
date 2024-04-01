#include <stdio.h>
#include "cmemsafe.h"

#if (CSAFE_NEED_TEMPORAL)

csafe_capability csafe_stack_cap_counter = CAPVAL_STACK_INIT;
csafe_capability csafe_heap_cap_counter = CAPVAL_HEAP_INIT;

struct stack_cap_store csafe_SCS;
struct heap_cap_store csafe_HCS;
  


/* inline csafe_capability * cap_store_get(struct cap_store *store, UI32 index) */
/* { */
/*     csafe_capability * page; */

/*     CSAFE_ASSERT (index < store->size); */
/*     page = store->pages[index >> STORE_PAGE_BITS]; */
/*     return &(page[index & STORE_PAGE_MASK]); */
/* } */


/***********************************************************************
 * Stack Capability Store (SCS)
 * 
 * SCS[0]        CSAFE_CAP_FOREVER associated with every global
 * SCS[SCS.top]  CSAFE_CAP_FRAME associated with current function
 * SCS[i] (i>0) = 
 *      a valid frame capability 
 * or   CAPVAL_INVALID
 **********************************************************************/

/*
 * Allocate and initialize one capability page for SCS.
 */
static inline csafe_capability * csafe_SCS_get_one_page()
{
    csafe_capability * page;
    
    page = (csafe_capability *)
	calloc(CAPSTORE_PAGE_SIZE, sizeof(csafe_capability));
    CSAFE_ASSERT(page != NULL);

    /* No need to have additional initialization code because
     * CAPVAL_INVALID is defined as zero. 
     */

    return page;
}

static inline void csafe_SCS_free_one_page(csafe_capability * page)
{
    free(page);
}

void csafe_SCS_init()
{
    csafe_capability * capptr;

    csafe_SCS.store.pages[0] = csafe_SCS_get_one_page();
    csafe_SCS.store.size = CAPSTORE_PAGE_SIZE;
    csafe_SCS.store.pgnum = 1;

    CAPVAL_SET(*(CSAFE_CAP_NEVER), CAPVAL_INVALID);
    CAPVAL_SET(*(CSAFE_CAP_FOREVER), CAPVAL_FOREVER);

    csafe_SCS.top = 1;
}

void csafe_SCS_cleanup()
{
    int i;
    
    for (i=0; i<csafe_SCS.store.pgnum; i++) {
	csafe_SCS_free_one_page(csafe_SCS.store.pages[i]);
    }
}

csafe_capability * csafe_SCS_push()
{
    csafe_capability ** page, * frame_capptr;

    CSAFE_INC_CALLS();

    csafe_SCS.top++;

    /* Expand SCS if needed */
    if (csafe_SCS.top == csafe_SCS.store.size) {
	if (csafe_SCS.store.pgnum == CAPSTORE_PAGE_MAX)
	    CSAFE_FATAL("SCS exceeds maximum number of capability pages");
	csafe_SCS.store.pages[csafe_SCS.store.pgnum] =
	    csafe_SCS_get_one_page();
	csafe_SCS.store.pgnum++;
	csafe_SCS.store.size += CAPSTORE_PAGE_SIZE;
    }

    /* Assign unique capability value to the top slot of SCS */
    frame_capptr = CAPSTORE_INDEX(csafe_SCS.store, csafe_SCS.top);
    CAPVAL_SET(*frame_capptr, CAPVAL_STACK_NEXT);

    return frame_capptr;
}

void csafe_SCS_pop()
{
    csafe_capability * capptr;

    CSAFE_ASSERT( csafe_SCS.top > 0 );
    
    capptr = CAPSTORE_INDEX(csafe_SCS.store, csafe_SCS.top);
    CAPVAL_SET(*capptr, CAPVAL_INVALID);
    csafe_SCS.top--;
}

/***********************************************************************
 * Heap Capability Store (HCS)
 * HCS[i] for i in [0, HCS.size) = 
 *      a valid heap capability 
 * or   a pointer to an unused slot 
 **********************************************************************/

/* 
 * Allocate and initialize one capability page for HCS
 */
static inline csafe_capability * csafe_HCS_get_one_page () 
{
    int i;
    csafe_capability * page;

    page = (csafe_capability *)
	calloc(CAPSTORE_PAGE_SIZE, sizeof(csafe_capability));
    CSAFE_ASSERT(page != NULL);
    for (i = 0; i < CAPSTORE_PAGE_SIZE - 1; i++) 
	page[i] = (csafe_capability) &(page[i + 1]);

    page[CAPSTORE_PAGE_SIZE - 1] = (csafe_capability) NULL;

    return page;
}

static inline void csafe_HCS_free_one_page(csafe_capability * page)
{
    free(page);
}

void csafe_HCS_init()
{
    UI32 i;

    csafe_HCS.store.pages[0] = csafe_HCS_get_one_page();
    csafe_HCS.store.size = CAPSTORE_PAGE_SIZE;
    csafe_HCS.store.pgnum = 1;
    csafe_HCS.next = &(csafe_HCS.store.pages[0][0]);
}

void csafe_HCS_cleanup()
{
    int i;

    for (i=0; i<csafe_HCS.store.pgnum; i++) {
	csafe_HCS_free_one_page(csafe_HCS.store.pages[i]);
    }
}

csafe_capability * csafe_HCS_allocate()
{
    UI32 i, j;
    csafe_capability ** page, * allocated;

    CSAFE_INC_ALLOCS();

    /* Expand HCS if needed */
    if (csafe_HCS.next == NULL) {
	if (csafe_HCS.store.pgnum == CAPSTORE_PAGE_MAX)
	    CSAFE_FATAL("HCS exceeds maximum number of capability pages");
	csafe_HCS.store.pages[csafe_HCS.store.pgnum] =
	    csafe_HCS_get_one_page();
	csafe_HCS.next = 
	    &(csafe_HCS.store.pages[csafe_HCS.store.pgnum][0]);
	csafe_HCS.store.pgnum++;
	csafe_HCS.store.size += CAPSTORE_PAGE_SIZE;
    }
    /* Allocate capability slot from the head of free slot list */
    allocated = csafe_HCS.next;
    /* Move head of free slot list to the next free slot */
    csafe_HCS.next = (csafe_capability *) CAPVAL_GET(*allocated);
    /* Assign unique capability value to newly allocated capability slot */
    CAPVAL_SET(*allocated, CAPVAL_HEAP_NEXT);

    return allocated;
}

void csafe_HCS_release(csafe_capability * capptr)
{
    /* Hack: never release CSAFE_CAP_FOREVER */
    if (capptr == CSAFE_CAP_FOREVER) return;
    CAPVAL_SET(*capptr, (csafe_capability) csafe_HCS.next);
    csafe_HCS.next = capptr;
}

#endif

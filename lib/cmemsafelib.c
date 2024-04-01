#include <stdio.h>
#include <stdlib.h>
#include "cmemsafe.h"

int csafe_always_stop_on_errors = 0;

char * csafe_errmsg[] = {
    "dereference of NULL pointer",
    "dereference of uninitialized pointer",
    "dereference of dead storage",
    "dereference beyond boundary of memory block",
    "dereference beyond upper boundary of memory block",
    "dereference beyond lower boundary of memory block",
    "incompatible run-time type",
    "pointer arithmetic on incompatible pointer type",
    0
};

struct csafe_header csafe_forever_blkhdr;
struct csafe_header csafe_never_blkhdr;

#ifdef CSAFE_DEBUG
I32 csafe_num_ptr_assign = 0;
I32 csafe_num_ptr_deref = 0;
I32 csafe_num_calls = 0;
I32 csafe_num_allocs = 0;
#endif


void csafe_init()
{
#if (CSAFE_NEED_TEMPORAL)
    csafe_SCS_init();
    csafe_HCS_init();
#endif
/*     wrapper_init(); */
    
#if (CSAFE_NEED_RTTI)
    csafe_forever_blkhdr._blktid = CSAFE_VALID_TYPE;
#endif
    csafe_forever_blkhdr._base = (void *) 0;
    csafe_forever_blkhdr._size = (UI32) (0xffffffff);
#if (CSAFE_NEED_TEMPORAL)
    csafe_forever_blkhdr._cap_index = CAPVAL_FOREVER;
#endif

#if (CSAFE_NEED_RTTI)
    csafe_never_blkhdr._blktid = CSAFE_INVALID_TYPE;
#endif
    csafe_never_blkhdr._base = (void *) 0;
    csafe_never_blkhdr._size = (UI32) 0;
#if (CSAFE_NEED_TEMPORAL)
    csafe_never_blkhdr._cap_index = CAPVAL_INVALID;
#endif
}

void csafe_cleanup()
{
#if (CSAFE_NEED_TEMPORAL)
    csafe_SCS_cleanup();
    csafe_HCS_cleanup();
#endif
/*     wrapper_cleanup(); */

#ifdef CSAFE_DEBUG
    fprintf(stderr, "Number of pointer assignments:  %ld\n", 
	    csafe_num_ptr_assign);
    fprintf(stderr, "Number of pointer dereferences: %ld\n", 
	    csafe_num_ptr_deref);
    fprintf(stderr, "Number of function calls: %ld\n", 
	    csafe_num_calls);
    fprintf(stderr, "Number of allocations:    %ld\n", 
	    csafe_num_allocs);
#endif
}

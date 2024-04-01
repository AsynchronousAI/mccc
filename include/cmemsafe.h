
#ifndef __CMEMSAFE_H__

#define __CMEMSAFE_H__

#define CSAFE_NEED_HEADER          1
#define CSAFE_NEED_TEMPORAL        1
#define CSAFE_NEED_SPATIAL         1 
#define CSAFE_NEED_RTTI            1

#define CSAFE_SINGLE_USE_CAPABILITY    0

#define CSAFE_SHOW_WARNING         0
#define CSAFE_SHOW_ERROR           1

extern int csafe_always_stop_on_errors;

#ifndef NULL
#define NULL  0
#endif

#define I32   long
#define UI32  unsigned I32
#define I16   short int
#define UI16  unsigned I16

#define CSAFE_NULL       	0
#define CSAFE_UNINITIALIZED     1
#define CSAFE_TEMPORAL   	2
#define CSAFE_SPATIAL    	3
#define CSAFE_SPATIAL_UB 	4
#define CSAFE_SPATIAL_LB 	5
#define CSAFE_RTTI		6
#define CSAFE_PTR_ARITH	        7
#define CSAFE_FREE	        8

#define CSAFE_INVALID_TYPE    	0
#define CSAFE_NONCAST_TYPE	1
#define CSAFE_PRIMITIVE_TYPE	2
#define CSAFE_VALID_TYPE  	3
#define CSAFE_FIRST_CAST_TYPE   4

#define csafe_log_message        printf

#define csafe_terminate()        \
    { if (csafe_always_stop_on_errors) exit(1); }

#define CSAFE_FATAL(errmsg) \
    { \
        csafe_log_message("Fatal error at %s:%d: %s\n", \
		__FILE__, __LINE__, errmsg); \
        csafe_terminate(); \
    }

#if (CSAFE_SHOW_ERROR)
#define CSAFE_ERROR(errmsg) \
    { \
        csafe_log_message("Error at %s:%d: %s\n", \
		__FILE__, __LINE__, errmsg); \
        csafe_terminate(); \
    }
#else
#define CSAFE_ERROR(errmsg)    csafe_terminate()
#endif

#if (CSAFE_SHOW_WARNING)
#define CSAFE_WARNING(msg) \
    { \
        csafe_log_message("Warning at %s:%d: %s\n", \
                __FILE__, __LINE__, msg); \
    }
#define CSAFE_WARNING_LOG  csafe_log_message
#define CSAFE_LOG  csafe_log_message
#else
#define CSAFE_WARNING(msg)
#define CSAFE_WARNING_LOG  
#define CSAFE_LOG  csafe_log_message
#endif


#ifdef CSAFE_DEBUG
#define CSAFE_ASSERT_WLOC(p, file, line) \
    if (p == NULL) { \
        printf("%s:%d: ASSERT failed\n", file, line); \
        exit(1); \
    }
#else
#define CSAFE_ASSERT_WLOC(p, file, line)
#endif

#define CSAFE_ASSERT(pred)  CSAFE_ASSERT_WLOC(pred, __FILE__, __LINE__);


/************************************************************************
 * CAPABILITY DEFINITIONS 
 ************************************************************************/

#if (CSAFE_NEED_TEMPORAL)

#if (CSAFE_SINGLE_USE_CAPABILITY)

/* typedef UI32 csafe_capability; */

/* #define CAPSTORE_SEGMENT_BITS   13 */
/* #define CAPSTORE_PAGE_BITS      16 */
/* #define CAPSTORE_BYTE_BITS      3 */
/* #define CAPSTORE_PAGE_MAX       (1 << CAPSTORE_SEGMENT_BITS) */
/* #define CAPSTORE_PAGE_SIZE      (1 << CAPSTORE_PAGE_BITS) */

/* struct cap_store { */
/*     unsigned char * pages[CAPSTORE_PAGE_MAX]; */
/*     UI32 pgnum; */
/*     UI32 size; */
/* }; */

/* struct stack_cap_store { */
/*     struct cap_store store; */
/*     csafe_capability top; */
/* }; */

/* struct heap_cap_store { */
/*     struct cap_store store; */
/*     csafe_capability next; */
/* }; */

/* extern csafe_capability csafe_stack_cap_counter; */
/* extern csafe_capability csafe_heap_cap_counter; */
/* extern struct stack_cap_store csafe_SCS; */
/* extern struct heap_cap_store  csafe_HCS; */

/* extern void csafe_SCS_init(); */
/* extern void csafe_SCS_cleanup(); */
/* extern csafe_capability * csafe_SCS_push(); */
/* extern void csafe_SCS_pop(); */

/* extern void csafe_HCS_init(); */
/* extern void csafe_HCS_cleanup(); */
/* extern csafe_capability * csafe_HCS_allocate(); */
/* extern void csafe_HCS_release(csafe_capability * capptr); */

/* /\* CAP_STORE is indexed using a scheme which is similar to linear */
/*  * virtual address in page-based memory management.  The highest */
/*  * SEGMENT_BITS bits of an index denote the store page number, the next */
/*  * PAGE_BITS bits denote the byte offset within the page, while the */
/*  * lowest BYTE_BITS denote the bit offset of that byte.  */
/*  *\/ */

/* #define CAPSTORE_INDEX(store, index)  \ */
/*    (&((store).pages[(index) >> CAPSTORE_PAGE_BITS][(index) & CAPSTORE_PAGE_MASK])) */

/* /\* */
/*  * A capability value is a 32-bit integer encoded as below */
/*  * (from the least significant to the most significant bit):  */
/*  *    bit 0: capability flag  */
/*  *           1 - a valid capability;   */
/*  *           0 - a capability pointer, but not a capability */
/*  *    bit 1: heap capability flag */
/*  *           1 - a valid heap capability if bit 0 = 1 */
/*  *           0 - a valid non-heap (global, stack) capability if bit 0 = 1 */
/*  *    bit 2-31: sequence number to distinguish capabilities */
/*  *\/ */

/* #define CAPVAL_SET(cap, val)   (cap = val) */
/* #define CAPVAL_GET(cap)        (cap) */

/* #define CAPVAL_CAP_FLAG        0x00000001 */
/* #define CAPVAL_HEAP_FLAG       0x00000003 */

/* #define CAPVAL_VALID_P(cap, flag)   ((CAPVAL_GET(cap) & (flag)) == (flag)) */

/* #define CAPVAL_INVALID  ((UI32) 0) */

/* #define CAPVAL_FOREVER        ((UI32) 1) */

/* #define CAPVAL_HEAP_INIT     ((UI32) 3) */
/* #define CAPVAL_STACK_INIT     ((UI32) 5) */

/* /\* Ideally CAPVAL_STACK_NEXT and CAPVAL_HEAP_NEXT should be able to */
/*  * detect wraparounds of capability values. */
/*  *\/ */
/* #define CAPVAL_STACK_NEXT  __CAPVAL_STACK_NEXT() */
/* __inline static csafe_capability __CAPVAL_STACK_NEXT() */
/* { */
/*     csafe_stack_cap_counter += 4; */
/*     return csafe_stack_cap_counter; */
/* } */

/* #define CAPVAL_HEAP_NEXT  __CAPVAL_HEAP_NEXT() */
/* __inline static csafe_capability __CAPVAL_HEAP_NEXT() */
/* { */
/*     csafe_heap_cap_counter += 4; */
/*     return csafe_heap_cap_counter; */
/* } */

/* #define CSAFE_CAP_NEVER    (CAPSTORE_INDEX(csafe_SCS.store, 0)) */
/* #define CSAFE_CAP_FOREVER  (CAPSTORE_INDEX(csafe_SCS.store, 1)) */
/* #define CSAFE_CAP_FRAME    (CAPSTORE_INDEX(csafe_SCS.store, csafe_SCS.top)) */

/* #define CAPVAL_FRAME       (*CSAFE_CAP_FRAME) */

#else
/* struct csafe_capability; */
/* struct csafe_capability { */
/*     /\* local counter of current slot *\/ */
/*     UI32 value; */
/*     /\* link to next slot *\/ */
/*     struct csafe_capability  * next; */
/* }; */
/* typedef struct csafe_capability csafe_capability; */

/* #define CAPVAL_SET(cap, val)    ((cap).value = val) */
/* #define CAPVAL_GET(cap)         ((cap).value) */

typedef UI32 csafe_capability;

/* maximum number of pages in each store */
#define CAPSTORE_PAGE_MAX     (1 << 16)
/* size in bytes and bit numbers of each page */
#define CAPSTORE_PAGE_BITS    16
#define CAPSTORE_PAGE_SIZE    (1 << CAPSTORE_PAGE_BITS)
#define CAPSTORE_PAGE_MASK    (CAPSTORE_PAGE_SIZE - 1)

struct cap_store {
    csafe_capability * pages[CAPSTORE_PAGE_MAX];
    UI32 pgnum;
    UI32 size;
};

struct stack_cap_store {
    struct cap_store store;
    /* index of top-most slot of stack_cap_store */
    UI32 top;
};

struct heap_cap_store {
    struct cap_store store;
    /* pointer to the first unused slot of heap_cap_store */  
    csafe_capability * next;
};

extern csafe_capability csafe_stack_cap_counter;
extern csafe_capability csafe_heap_cap_counter;
extern struct stack_cap_store csafe_SCS;
extern struct heap_cap_store  csafe_HCS;

extern void csafe_SCS_init();
extern void csafe_SCS_cleanup();
extern csafe_capability * csafe_SCS_push();
extern void csafe_SCS_pop();

extern void csafe_HCS_init();
extern void csafe_HCS_cleanup();
extern csafe_capability * csafe_HCS_allocate();
extern void csafe_HCS_release(csafe_capability * capptr);

/* CAP_STORE is indexed using a scheme which is similar to linear
 * virtual address in page-based memory management.  The highest
 * (32-STORE_PAGE_BITS) bits of an index denote the store page number,
 * while the lowest STORE_PAGE_BITS bits denote the offset within that
 * page.
 */ 
#define CAPSTORE_INDEX(store, index)  \
   (&((store).pages[(index) >> CAPSTORE_PAGE_BITS][(index) & CAPSTORE_PAGE_MASK]))

/*
 * A capability value is a 32-bit integer encoded as below
 * (from the least significant to the most significant bit): 
 *    bit 0: capability flag 
 *           1 - a valid capability;  
 *           0 - a capability pointer, but not a capability
 *    bit 1: heap capability flag
 *           1 - a valid heap capability if bit 0 = 1
 *           0 - a valid non-heap (global, stack) capability if bit 0 = 1
 *    bit 2-31: sequence number to distinguish capabilities
 */

#define CAPVAL_SET(cap, val)   (cap = val)
#define CAPVAL_GET(cap)        (cap)

#define CAPVAL_CAP_FLAG        0x00000001
#define CAPVAL_HEAP_FLAG       0x00000003

#define CAPVAL_VALID_P(cap, flag)   ((CAPVAL_GET(cap) & (flag)) == (flag))

#define CAPVAL_INVALID  ((UI32) 0)

#define CAPVAL_FOREVER        ((UI32) 1)

#define CAPVAL_HEAP_INIT     ((UI32) 3)
#define CAPVAL_STACK_INIT     ((UI32) 5)

/* Ideally CAPVAL_STACK_NEXT and CAPVAL_HEAP_NEXT should be able to
 * detect wraparounds of capability values.
 */
#define CAPVAL_STACK_NEXT  __CAPVAL_STACK_NEXT()
__inline static csafe_capability __CAPVAL_STACK_NEXT()
{
    csafe_stack_cap_counter += 4;
    return csafe_stack_cap_counter;
}

#define CAPVAL_HEAP_NEXT  __CAPVAL_HEAP_NEXT()
__inline static csafe_capability __CAPVAL_HEAP_NEXT()
{
    csafe_heap_cap_counter += 4;
    return csafe_heap_cap_counter;
}

#define CSAFE_CAP_NEVER    (CAPSTORE_INDEX(csafe_SCS.store, 0))
#define CSAFE_CAP_FOREVER  (CAPSTORE_INDEX(csafe_SCS.store, 1))
#define CSAFE_CAP_FRAME    (CAPSTORE_INDEX(csafe_SCS.store, csafe_SCS.top))

#define CAPVAL_FRAME       (*CSAFE_CAP_FRAME)

#endif  /* CSAFE_SINGLE_USE_CAPABILITY */
#endif  /* CSAFE_NEED_TEMPORAL */


/************************************************************************
 * BLOCK HEADER DEFINITIONS 
 ************************************************************************/

struct csafe_header {
#if (CSAFE_NEED_RTTI)
    long                _blktid;
#endif
    void 	      * _base;  /* base address of memory block */
    unsigned long 	_size;  /* allocated size in bytes */
#if (CSAFE_NEED_TEMPORAL && !CSAFE_SINGLE_USE_CAPABILITY)
    csafe_capability    _cap_index;   /* local copy of capability value */
#endif
};
typedef struct csafe_header csafe_header;

extern csafe_header csafe_forever_blkhdr;
extern csafe_header csafe_never_blkhdr;


/************************************************************************
 * POINTER INFO DEFINITIONS 
 ************************************************************************/

/*
 * All possible fields of csafe_info :
 *     unsigned long   _tid; 
 *
 *     csafe_header *  _blkhdr;
 *     void *          _base;
 *     unsigned long   _size;
 *     unisgned long   _cap_index;
 *
 *     capability      _cap_ptr;
 *     void *          _link;
 */

struct csafe_info {
#if (CSAFE_NEED_RTTI)
    unsigned long      _tid;       /* run-time type information */
#endif

#if (CSAFE_NEED_HEADER)
    csafe_header *     _blkhdr;    /* ptr to block header */
#else
#if (CSAFE_NEED_RTTI)
    long               _blktid;
#endif
    void *	       _base;       
    unsigned long      _size;
#endif

#if (CSAFE_NEED_TEMPORAL)
#if (CSAFE_SINGLE_USE_CAPABILITY)
    unsigned long      _cap_index;
#else
#if (CSAFE_NEED_HEADER)
    csafe_capability * _cap_ptr;   /* ptr to slot in capability store */
#else
    unsigned long      _cap_index; 
    csafe_capability * _cap_ptr;   /* ptr to slot in capability store */
#endif /* CSAFE_NEED_HEADER */
#endif /* CSAFE_SINGLE_USE_CAPABILITY */
#endif /* CSAFE_NEED_TEMPORAL */
    void *             _link;      /* ptr to info for stored pointers */
};
typedef struct csafe_info csafe_info;


struct csafe_typeinfo {
    long   elem_sz;       /* size of each element */
    long   info_sz;       /* size of info for each element */
};
typedef struct csafe_typeinfo csafe_typeinfo;

#ifdef CSAFE_DEBUG
extern long csafe_num_ptr_assign;
extern long csafe_num_ptr_deref;
extern long csafe_num_calls;
extern long csafe_num_allocs;
#define CSAFE_INC_PTR_ASSIGN()  (csafe_num_ptr_assign++)
#define CSAFE_INC_PTR_DEREF()   (csafe_num_ptr_deref++)
#define CSAFE_INC_CALLS()	(csafe_num_calls++)
#define CSAFE_INC_ALLOCS()	(csafe_num_allocs++)
#else
#define CSAFE_INC_PTR_ASSIGN()
#define CSAFE_INC_PTR_DEREF()
#define CSAFE_INC_CALLS()
#define CSAFE_INC_ALLOCS()
#endif


// Assumption:
//   All invalid values. such as NULL, CSAFE_TS_INVALID, are defined as zero.
#define CSAFE_RESET(p_info)        (memset(&(p_info), 0, sizeof(p_info)))
#define CSAFE_RESET_ARRAY(a_info)  (memset(a_info, 0, sizeof(a_info)))

/************************************************************************
 * CHECK ROUTINE DEFINITIONS  
 ************************************************************************/

extern char * csafe_errmsg[];

/* #define CHECK_NULL(p, file, line) */
/* #define CHECK_UNINITIALIZED(blkhdr, file, line) */
/* #define CHECK_TEMPORAL(p, gts_ptr, ts, file, line) */
/* #define CHECK_SPATIAL(p, sz, base, size, file, line) */
/* #define CHECK_RTTI(p1_type, p2_tid, file, line) */
/* #define CHECK_PTR_ARITH( \ */
/*         p, e, link_e, p_stype, p_ssize, p_rtype, p_rblktype, file, line)  */

#define CHECK_NULL(p, file, line) \
    if (p == NULL) { \
        CSAFE_ERROR(csafe_errmsg[CSAFE_NULL]); \
    }

#define CHECK_UNINITIALIZED(blkhdr, file, line) \
    if ((blkhdr) == NULL) { \
        CSAFE_ERROR(csafe_errmsg[CSAFE_UNINITIALIZED]); \
    }

#if (CSAFE_SINGLE_USE_CAPABILITY)
#define CHECK_TEMPORAL(p, cap_ptr, cap_index, file, line) \
    if (!CAPVAL_VALID_P(cap_index, CAPVAL_CAP_FLAG)) { \
        CSAFE_LOG("\n Pointer: %s=%p" \
                  "\n Cap_index: %s=%ld\n\n", \
                   #p, p, #cap_index, cap_index); \
        CSAFE_ERROR(csafe_errmsg[CSAFE_TEMPORAL]); \
    }
#else
#define CHECK_TEMPORAL(p, cap_ptr, cap_index, file, line) \
    if (cap_ptr == NULL) { \
        CSAFE_LOG("\n Pointer: %s=%p" \
                  "\n Cap_ptr: %s=%p\n\n", \
                   #p, p, #cap_ptr, cap_ptr); \
        CSAFE_ERROR(csafe_errmsg[CSAFE_UNINITIALIZED]); \
    } \
    else if (cap_index != *(cap_ptr)) { \
        CSAFE_LOG("\n Pointer:    %s   =%p" \
                  "\n *(Cap_ptr): *(%s)=%p=%ld" \
                  "\n Cap_index:  %s   =%ld\n\n", \
              #p, p, #cap_ptr, cap_ptr, *(cap_ptr), #cap_index, cap_index); \
        CSAFE_ERROR(csafe_errmsg[CSAFE_TEMPORAL]); \
    }
#endif

/* This following trick to combine UB and LB checks does not work. */  
/*     if ((unsigned long)(p) - (unsigned long)(base) > \ */
/*         (long)(size) - (long)(sz)) { \ */

#define CHECK_SPATIAL(p, sz, base, size, file, line) \
    CSAFE_INC_PTR_DEREF(); \
    if (((void *)(p) < (void *)(base)) || \
        ((void *)(p) + (long)(sz) > (void *)(base) + (long)(size))) { \
        CSAFE_LOG("\nPointer:           %s = %p" \
                  "\nSizeof(*Pointer):  %s = %ld" \
                  "\nBase:              %s = %p" \
                  "\nSize:              %s = %ld\n\n", \
                  #p, p, #sz, sz, #base, base, #size, size); \
        CSAFE_ERROR(csafe_errmsg[CSAFE_SPATIAL]); \
    }

#define CHECK_SPATIAL_UB(p, sz, base, size, file, line) \
    if ((void *)(p) + (sz) > (void *)(base) + size) { \
        CSAFE_ERROR(csafe_errmsg[CSAFE_SPATIAL_UB]); \
    }

#define CHECK_SPATIAL_LB(p, sz, base, size, file, line) \
    if ((void *)(p) < (void *) (base)) { \
        CSAFE_ERROR(csafe_errmsg[CSAFE_SPATIAL_LB]); \
    }

/* Is type1 a subtype of type2 ? */

/* UI16 csafe_subtype_table[][] */

/* In csafe_subtype_table, there is one bit for each pair of (type1,
 * type2), which is set to ONE if type1 is a subtype of type2, and ZERO
 * otherwise. 
 */

/* #define CSAFE_IS_SUBTYPE(type1, type2) \ */
/*     (csafe_subtype_table[type1][type2] == 'Y') */

#define CSAFE_IS_SUBTYPE(type1, type2) \
       ((csafe_subtype_table[type1][ (UI16)(type2) >> 4 ] \
            & (0x8000 >> ((UI16)(type2) & 0x000f))) != 0)

/* Is p's run-time type a subtype of p's static type ? */
#define CHECK_RTTI(p, p_stype, p_rtype, file, line) \
    if (p_rtype != CSAFE_VALID_TYPE && \
        p_stype != CSAFE_PRIMITIVE_TYPE && \
        !CSAFE_IS_SUBTYPE(p_rtype, p_stype)) { \
        CSAFE_LOG("\nPointer:        %s = %p" \
                  "\nStatic type:    %s = %ld (%s)" \
                  "\nRun-time type:  %s = %ld (%s)\n\n", \
                   #p, p, #p_stype, p_stype, csafe_type_table[p_stype], \
                   #p_rtype, p_rtype, csafe_type_table[p_rtype]); \
        CSAFE_ERROR(csafe_errmsg[CSAFE_RTTI]); \
    }

/* Is p's run-time type a subtype of p's static type ? */
#define CHECK_RTTI_ADDROF(p, p_stype, p_rtype, tmptid, file, line) \
    if (p_rtype != CSAFE_VALID_TYPE && \
        p_stype != CSAFE_PRIMITIVE_TYPE && \
        !CSAFE_IS_SUBTYPE(p_rtype, p_stype)) { \
        CSAFE_WARNING_LOG("\nPointer:        %s = %p" \
                  "\nStatic type:    %s = %ld (%s)" \
                  "\nRun-time type:  %s = %ld (%s)\n\n", \
                   #p, p, #p_stype, p_stype, csafe_type_table[p_stype], \
                   #p_rtype, p_rtype, csafe_type_table[p_rtype]); \
        CSAFE_WARNING(csafe_errmsg[CSAFE_RTTI]); \
        tmptid = CSAFE_PRIMITIVE_TYPE; \
    }

#define CHECK_PTR_ARITH( \
        p, e, link_e, p_stype, p_ssize, p_rtype, p_rblktype, file, line) \
    if (p_rtype == CSAFE_VALID_TYPE) { \
        link_e = 0; \
    } \
    if (p_rtype == CSAFE_INVALID_TYPE) { \
        link_e = 0; \
    } \
    if (p_stype == p_rblktype || \
             p_ssize == csafe_typeinfo_table[p_rblktype].elem_sz) { \
        link_e = (e) * csafe_typeinfo_table[p_rblktype].info_sz; \
    } \
    else if ((csafe_typeinfo_table[p_rblktype].elem_sz != 0) && \
             (p_ssize * e) % csafe_typeinfo_table[p_rblktype].elem_sz == 0) { \
        link_e = (p_ssize * (e)) / csafe_typeinfo_table[p_rblktype].elem_sz \
                 * csafe_typeinfo_table[p_rblktype].info_sz; \
    } \
    else { \
        CSAFE_WARNING_LOG("\nPointer:             %s = %p" \
                  "\nStatic type:         %s = %ld (%s)" \
                  "\nRun-time block type: %s = %ld (%s)" \
                  "\nOffset to be added:  %s * %s = %ld\n\n", \
                  #p, p, #p_stype, p_stype, csafe_type_table[p_stype], \
                  #p_rblktype, p_rblktype, csafe_type_table[p_rblktype], \
                  #e, #p_ssize, p_ssize * e); \
        CSAFE_WARNING(csafe_errmsg[CSAFE_PTR_ARITH]); \
        p_rtype = CSAFE_INVALID_TYPE; \
        link_e = 0; \
    }

#if (CSAFE_SINGLE_USE_CAPABILITY)
/* #define CHECK_FREE_LOG(p, base, cap_ptr, cap_index) \ */
/*     { \ */
/*         CSAFE_LOG("\nPointer:    %s = %p" \ */
/*                   "\nBase:       %s = %p" \ */
/*                   "\nCapIndex:   %s = %ld\n\n", \ */
/*                   #p, p, #base, base, #cap_index, cap_index); \ */
/*     } */

/* #define CHECK_FREE(p, base, cap_ptr, cap_index, file, line) \ */
/*     if (p != base) { \ */
/*         CHECK_FREE_LOG(p, base, cap_ptr, cap_index); \ */
/*         CSAFE_ERROR("Attempt to free a pointer not returned by malloc"); \ */
/*     } \ */
/*     if (cap_index == NULL) { \ */
/*         CHECK_FREE_LOG(p, base, cap_ptr, cap_index); \ */
/* 	CSAFE_ERROR("Attempt to release an invalid capability"); \ */
/*     } \ */
/*     else if (! CAPVAL_VALID_P(*(cap_ptr), CAPVAL_HEAP_FLAG)) { \ */
/*         CHECK_FREE_LOG(p, base, cap_ptr, cap_index); \ */
/* 	CSAFE_ERROR("Attempt to release a non-heap capability."); \ */
/*     } */
#else
#define CHECK_FREE_LOG(p, base, cap_ptr, cap_val) \
    { \
        CSAFE_LOG("\nPointer:    %s = %p" \
                  "\nBase:       %s = %p" \
                  "\n*CapPtr:    *(%s) = *%p = %ld\n\n", \
                  #p, p, #base, base, #cap_ptr, cap_ptr, cap_val); \
    }

#define CHECK_FREE(p, base, cap_ptr, cap_index, file, line) \
    if (cap_ptr == NULL) { \
        CSAFE_LOG("\nPointer:    %s = %p" \
                  "\nCapPtr:     %s = %p\n\n", \
                  #p, p, #cap_ptr, cap_ptr); \
	CSAFE_ERROR("Attempt to release an unintitialized memory block"); \
    } \
    else if (cap_ptr != CSAFE_CAP_FOREVER) { \
      if (p != base) { \
        CHECK_FREE_LOG(p, base, cap_ptr, *(cap_ptr)); \
        CSAFE_ERROR("Attempt to free a pointer not returned by malloc"); \
      } \
      else if (! CAPVAL_VALID_P(*(cap_ptr), CAPVAL_HEAP_FLAG)) { \
        CHECK_FREE_LOG(p, base, cap_ptr, *(cap_ptr)); \
	CSAFE_ERROR("Attempt to release a non-heap memory block"); \
      } \
    }
#endif

/* #define CHECK_PTR_ARITH(p, p_stype, p_rtype, file, line) \ */
/*     if (p_rtype != CSAFE_INVALID_TYPE && \ */
/*         p_rtype != CSAFE_VALID_TYPE && \ */
/*         p_stype != p_rtype && \ */
/*         (!CSAFE_IS_SUBTYPE(p_stype, p_rtype) || \ */
/*          !CSAFE_IS_SUBTYPE(p_rtype, p_stype))) { \ */
/*         CSAFE_LOG("\nPointer:        %s = %p" \ */
/*                   "\nStatic type:    %s = %d (%s)" \ */
/*                   "\nRun-time type:  %s = %d (%s)\n\n", \ */
/*                   #p, p, #p_stype, p_stype, csafe_type_table[p_stype], \ */
/*                   #p_rtype, p_rtype, csafe_type_table[p_rtype]); \ */
/*         CSAFE_WARNING(csafe_errmsg[CSAFE_PTR_ARITH]); \ */
/*         p_rtype = CSAFE_INVALID_TYPE; \ */
/*     } */


/************************************************************************
 * WRAPPER DEFINITIONS 
 ************************************************************************/

/* extern int wrapper_init(); */
/* extern void wrapper_cleanup(); */

/* extern void *wrapper_malloc(unsigned long size); */
/* #pragma cilnoremove("wrapper_malloc") */
#pragma csafealloc("malloc",nozero,sizein(1))

/* extern void wrapper_free(void *ptr); */
/* #pragma cilnoremove("wrapper_free") */
#pragma csafedealloc("free")

/* #pragma mesacwrapper("wrapper_malloc", of("malloc")) */
/* #pragma mesacwrapper("wrapper_free", of("free")) */

/* extern void *wrapper_calloc(unsigned long nmemb, unsigned long size); */
/* extern void *wrapper_realloc(void *ptr, unsigned long size); */

#pragma csafealloc("alloca", nozero, sizein(1))
#pragma csafealloc("calloc", zero, sizemul(1,2))
#pragma csafealloc("valloc", nozero,sizein(1))

/*
 * Example: How to specify a function that should not be transformed:
 * void *(__attribute__ ((__compat__)) fixup_null_alloc) (size_t n);
 */

/* Example: How to specify user-defined allocation functions:
 * #pragma csafealloc("xmalloc", nozero, sizein(1))
 * #pragma csafealloc("xcalloc", zero, sizemul(1,2))
 */

#endif


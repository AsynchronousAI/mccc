#include <stdlib.h>
#include <malloc.h>
#include <sys/types.h>

/* void *__wrap_malloc(size_t size) */
/* { */
/*     printf("My malloc\n"); */
/*     return __real_malloc(size); */
/* } */

int main()
{
    char * p;

    free(malloc(10));

    free(malloc(20));

    free(malloc(30));

    p = calloc(10, 1);
    p = realloc(p, 30);
 
    free(p);

    exit(0);

    return 0;
}


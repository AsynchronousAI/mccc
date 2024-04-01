#include <stdio.h>

int * alloc(int sz)
{
    return (int *)malloc(sz * sizeof(int));
}
/*
void dealloc(int ** ptr)
{
    free(*ptr);
    *ptr = NULL;
}
*/
int main()
{
    int *p = NULL, *q = NULL;

    p = alloc(100);

    printf("p=%p\t\tq=%p\n", p, q);

    q = p + 5;

    printf("p=%p\t\tq=%p\n", p, q);

/*    dealloc(&p); */
    free(p);
    p = NULL;

    printf("p=%p\t\tq=%p\n", p, q);

/*    dealloc(&q); */
    free(q-5);
    q = NULL;

    printf("p=%p\t\tq=%p\n", p, q);
}


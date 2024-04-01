#include <stdio.h>

int foo(int * q, int n)
{
    int i;
    int **p = & q; 

    for (i=0; i<n; i++)
        **p = i+1;

    return i;
}

main()
{
    int a[20];

    foo(a, 20);
}


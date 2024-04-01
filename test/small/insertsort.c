#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

#define NUM 8000

struct list {
    int  val;
    char pad[16];
    struct list *next;
};

struct list *insertionSort(int *a, int alen) 
{
    int i;
    struct list *head = NULL, **p, *q;

    for (i = 0; i < alen; i++) {
	p = &head;
	q = *p; 	//y

	while(q!=NULL){
	    if (q->val < a[i]){ //y and n
		p = &q->next;
		q = *p; 	//y
	    }
	    else
		break;
	}

        *p = (struct list *) malloc(sizeof(struct list)); //y
        (*p)->val = a[i]; 	// y for p and *p // n for a
        (*p)->next = q; 	// y for p and *p
    }
    return head;
}


main()
{
    int i;
    struct list *sorted, *temp;
    int *a;

    a = malloc(NUM * sizeof(int));
    for (i = 0; i<NUM; i++) {
	//a[i] = rand();
	a[i] = (i%(NUM/4))*4 + i/(NUM/4);
    }

    sorted = insertionSort(a, NUM);
}


#define LOW 300
#define MID 350
#define HIGH 400

struct matrix_item {
    int val;
    int * next;
};

struct matrix_item * 
matrix_mult(struct matrix_item * a, 
            struct matrix_item * b,
            struct matrix_item * r,
            int low, int mid, int high)
{
    int i, j, k, sum;

    for (i=0, k=0; i<low; i++) 
        for (j=0; j<mid; j++, k++) 
            a[i * mid + j].val = k; 
          
    for (i=0; i<mid; i++) 
        for (j=0; j<high; j++, k++) 
            b[i * high + j].val = k; 
                
    for (i=0; i<low; i++) {
	for (j=0; j<high; j++) {
	    sum = 0;
	    for (k=0; k<mid; k++) 
		sum += a[i * mid + k].val * b[k * high + j].val;
	    r[i * high + j].val = sum;
	}
    }

    return r;
}

main()
{
/*
    int A[LOW][MID];
    int B[MID][HIGH];
    int R[LOW][HIGH];
*/
    struct matrix_item *A, *B, *R;

    A = (struct matrix_item *)
        malloc( LOW * MID * sizeof(struct matrix_item));
    B = (struct matrix_item *)
        malloc( MID * HIGH * sizeof(struct matrix_item));
    R = (struct matrix_item *)
        malloc( LOW * HIGH * sizeof(struct matrix_item));

    R = matrix_mult(A, B, R, LOW, MID, HIGH);

    free(A);
    free(B);
    free(R);
}


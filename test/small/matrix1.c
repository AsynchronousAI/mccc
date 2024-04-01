
#define LOW 300
#define MID 350
#define HIGH 400

main()
{
    int a[LOW][MID];
    int b[MID][HIGH];
    int r[LOW][HIGH];
    int i, j, k, sum;

    for (i=0, k=0; i<LOW; i++) 
        for (j=0; j<MID; j++, k++) 
            a[i][j] = k; 
          
    for (i=0; i<MID; i++) 
        for (j=0; j<HIGH; j++, k++) 
            b[i][j] = k; 
                
    for (i=0; i<LOW; i++) {
	for (j=0; j<HIGH; j++) {
	    sum = 0;
	    for (k=0; k<MID; k++) 
		sum += a[i][k] * b[k][j];
	    r[i][j] = sum;
	}
    }
}

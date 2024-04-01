#include <stdio.h>

struct window {
    char * title;
    int  left;
    int  right;
    int  top;
    int  bottom;
    int  status;
    char * data;
} Windows[6];

char * titles[] = { 
        "Window One",
        "Window Two",
        "Window Three",
        "Window Four",
        "Window Five",
        "Window Six"
};

int main()
{
    int i;
    struct window * win_ptr;
    struct window (*wingrp_ptr)[2];

    for (i=0; i<6; i++)
       Windows[i].title = titles[i];
 
    win_ptr = Windows;
    win_ptr++;

    wingrp_ptr = (struct window (*)[2]) Windows;
/*   wingrp_ptr++;
*/
    printf("Windows    = %p\n", Windows);
    printf("win_ptr    = %p\n", win_ptr);
    printf("title      = %s\n", (*win_ptr));
    printf("wingrp_ptr = %p\n", wingrp_ptr);
    printf("title      = %s\n", (*wingrp_ptr)[1]);
}


#include <stdio.h>

char * g1;
char ** g2;

int main(int argc)
{
   char ch;
   char * p;

   if (argc == 1) {
      g1 = p;
      printf("g1=%p\n", g1);
      printf("*g1=%c\n", *g1);
   }
   else {
      p = &ch;
      g2 = (char **) p;
      printf("g2=%p\n", g2);
      printf("*g2=%p\n", *g2);
      printf("**g2=%c\n", **g2);
   }
}


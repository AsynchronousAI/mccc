#include <stdio.h>

struct A {
   int f1;
};

struct B {
   int g1;
   char * g2[3];
};

struct C {
   int h1;
   char * h2;
   char * h3;
};

int main()
{
   struct A * p, a;
   struct B * q, b;
   struct C * r, c;

   a.f1 = 1;
   b.g1 = 2;
   c.h1 = 3;

   p = (struct A *) &c;
   q = (struct B *) &a;
   r = (struct C *) &b;
   
   printf("%d\n", p->f1);
   printf("%d\n", r->h1);
   printf("%d\n", q->g1);
}

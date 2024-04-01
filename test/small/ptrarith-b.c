#include <stdio.h>

struct protocol {
   char magic[4];
   int  version;
   int  command;
   char * data;
   char * private;
   int  reserved;
} theProt[2];

char * mybzero(char *q, int sz) 
{
   int i=0;

   while (i++ < sz) 
        (*q++) = '\0';

   return q;
}

void print_protocol(struct protocol * p)
{
   printf("Version=%d \tCommand=%d \tData=%s\n",
           p->version, p->command, p->data);
}

int main()
{
   char *buf = "Hello, World";
   struct protocol * prot = theProt;

   prot->version = 1;
   prot->command = 3;
   prot->data = buf;
   print_protocol(prot);

   prot = (struct protocol *) 
        mybzero((char *)prot, sizeof(struct protocol));

   prot->version = 2;
   prot->command = 5;
   prot->data = buf;

   print_protocol(prot);

   return 0;
}

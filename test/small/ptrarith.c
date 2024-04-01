#include <stdio.h>

struct protocol {
   char magic[4];
   int  version;
   int  command;
   char * data;
   char * private;
   int  reserved;
} theProt[2];

struct agreement {
   char magic[4];
   int  version;
   int  command;
   char * data;
   char * private;
   int  reserved;
};
   
struct agreement * mybzero(struct protocol *q, int sz)
{
   int i=0;
   struct agreement * ap = (struct agreement *) q;
   
   ap++;

   return ap;
}

void print_protocol(struct protocol * p)
{
   printf("Version=%d \tCommand=%d \tData=%s\n",
           p->version, p->command, p->data);
}

int main()
{
   char *q;
   char *buf = "Hello, World";
   struct protocol * prot = theProt;

   prot->version = 1;
   prot->command = 3;
   prot->data = buf;
   print_protocol(prot);

   prot = (struct protocol *) 
        mybzero(prot, sizeof(struct protocol));

   prot->version = 2;
   prot->command = 5;
   prot->data = buf;

   print_protocol(prot);

   return 0;
}

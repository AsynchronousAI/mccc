#include <stdio.h>

typedef enum msg_kind { MSG1, MSG2 } msg_kind;

typedef struct msg_hdr {
    msg_kind kind;
} msg_hdr;

typedef struct msg1_body {
    char src[4];
    char dst[4];
    char *data;
} msg1_body;

typedef struct msg2_body {
    char sig;
    char *desc;
} msg2_body;

typedef struct msg1 {
  msg_hdr hdr;
  msg1_body body;
} msg1;

typedef struct msg2 {
  msg_hdr hdr;
  msg2_body body;
} msg2;

void processMsg1(msg1 * msg)
{
    printf("MSG1: src=\"%s\"\tdst=\"%s\"\tdata=\"%s\"\n", 
        msg->body.src, msg->body.dst, msg->body.data);
}

void processMsg2(msg2 * msg)
{
    printf("MSG2: sig='%c'\tdesc=\"%s\"\n", 
        msg->body.sig, msg->body.desc);
}

void processMsg(msg_hdr *hdr)
{
  switch (hdr->kind) {
    case MSG1:
        processMsg1((msg1 *)hdr);
        break;
    case MSG2:
        processMsg2((msg2 *)hdr);
        break;
    /* ... */
  }
}

main()
{
    msg1 m1;
    msg2 m2;
    char data[20]; 
    char desc[20]; 
    char hdr[100];

/*     ((msg_hdr *)hdr)->kind = MSG1; */
/*     processMsg((msg_hdr *)hdr); */

    strcpy(data, "Hello, World");
    strcpy(desc, "Welcome!");

    m1.hdr.kind = MSG2;
    strcpy(m1.body.src, "ABC");
    strcpy(m1.body.dst, "XYZ");
    m1.body.data = data;

    m2.hdr.kind = MSG1;
    m2.body.sig = 'S';
    m2.body.desc = desc;

    processMsg( (msg_hdr *) &m1);

    processMsg( (msg_hdr *) &m2);
}

#include <stdio.h>

struct stu {
    int id;
    int grade;
    char *name;
} s;

union U {
    int *p;
    struct stu *p2;
} u;

int main()
{
    int k = 200;

    s.id = 100;
    s.grade = 5;

    u.p2 = &s;
    printf("(u.p2)->grade=%d\n", (u.p2)->grade);
    printf("*(u.p)=%d\n", *u.p);

    u.p = &k;
    printf("(u.p2)->grade=%d\n", (u.p2)->grade);

    return 0;
}
    

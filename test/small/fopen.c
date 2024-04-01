
#define NULL 0
#define OPEN_MAX 10
#define PERMS 0666
#define NUM 3500000
#define DUMMY_FD 8

extern void *memset(void *s, int c, unsigned int n);
extern char *strcpy(char *dest, const char *src);
extern void *malloc(unsigned int size);

/* char *sss[] = {"Hello", "World", 0} ; */

struct FILE {
    int  cnt;
    char *ptr;
    char *base;
    int  flag;
    int  fd;
};

enum _flags {
    _READ  = 01,
    _WRITE = 02
};

struct FILE _iob[OPEN_MAX];

struct FILE *fopen(char *name, char *mode) 
{
    int fd;
    struct FILE *fp;

    if ((*mode != 'r') && (*mode != 'w') && (*mode != 'a'))
	return NULL;

    fp = _iob;
    while(fp < _iob + OPEN_MAX){
	if ((fp->flag & (_READ | _WRITE)) == 0)
	    break;
	fp = fp+1;
    }
    if (fp >= _iob+OPEN_MAX) {
	memset(_iob, 0, sizeof(_iob));
	fp = _iob;
    }

    if (*mode == 'w')
	fd = DUMMY_FD;
    else if (*mode == 'r')
	fd = DUMMY_FD;

    if (fd == -1)
	return NULL;

    fp->fd = fd;
    fp->cnt = 0;
    fp->base = NULL;
    fp->flag = (*mode == 'r') ? _READ : _WRITE;
    return fp;
}

int main()
{
    char *name;
    char *mode;
    struct FILE *fp;
    int i;

/*    printf("%s %s\n", sss[0], sss[1]); */

    name = malloc(8);
    mode = malloc(2);
    strcpy(name, "fname");
    strcpy(mode, "w");

    for (i = 0; i < NUM; i++){
	fp = fopen(name, mode);
    }
}

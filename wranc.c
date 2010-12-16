/*
srio@esrf.eu renames WRAN to WRANC (C-random generator)
renames file from myrand48.c to ranc.c
*/

#include <stdlib.h>

extern double drand48();
extern void srand48(long);

#ifdef _MSC_VER
# define F77Name(name) __stdcall name
#else
# ifndef NO_FORT_UNDERSCORE
#  define F77Name(name) name ## _
# else
#  define F77Name(name) name
# endif
#endif

#ifdef _MSC_VER
# define WRANC F77Name(WRANC)
#else
# define WRANC F77Name(wranc)
#endif

double wranc_(int* iseed) {
    static int first = 1;
    int ntries;
    double rand_val;
/*srio
    printf("in wranc.c first %i \n",first);
    printf("in wranc.c iseed %i \n",*iseed);
*/

    if (first) {
	srand48(*iseed);
	first = 0;
    }
    for (ntries = 0; ntries < 10; ++ntries) {
	rand_val = drand48();
	if (rand_val >= 1.0e-20)
	    break;
    }

    if (ntries == 10) {
	printf("ERROR:  Read past end of RANFILE. Aborting.\n");
	exit(1);
    }
/*srio
    printf("in wranc.c %f rand_val \n",rand_val);
    printf("in wranc.c %i iseed \n",iseed);
*/
    return rand_val;
}


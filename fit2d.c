#define _POSIX_SOURCE 1

#include "syntax.h"

/* Global Variables */
char **arguments; /* Copy of args */
Integer num_arguments; /* Copy of num_args */

/*+---------------------------------------------------------------------------*/
/* fit2d.c: "ANSI-C" wrapper to provide command-line arguments */
main (int num_args, char **args)

/* Description: */
/* Method: */
/* Authors:
     A P Hammersley (Hammersley@esrf.eu) */
/* History:
   21-Oct-2005: V0.1 Original (Hammersley)*/
/*----------------------------------------------------------------------------*/
{

  /* Local Variables */
  int len_dummy = 80; /* Dummy variable */

  /* Copy argument details to internal variables */
  num_arguments = num_args;
  arguments = args;

  /* Call Fortran main routine */
#if defined (WIN32)
  F2D_MAIN();
#elif defined (UNDER)
  f2d_main_();
#else
  f2d_main();
#endif

  return 0;
                    
}
 
 
 
 
 
 
 
 
 
 
 

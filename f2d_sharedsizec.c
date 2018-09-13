/*********1*********2*********3*********4*********5*********6*********7**

***            *********************
***            *                   *
***            * f2d_sharedsizec.c *
***            *                   *
***            *********************

*/

#define _POSIX_SOURCE 1

#include <stdio.h>

/*+ F2D_SHAREDSIZEC: calculate Shared memory Size */
int f2d_sharedsizec   /* Returns number of bytes needed for shared memory */
(int xmaxdat,         /* Dimension size in X-direction for data arrays */
 int ymaxdat,         /* Dimension size in Y-direction for data arrays */
 int memory_exist,    /* "True" if memory arrays are to be created */
 int variances_exist) /* "True", if a data variance arrays are to be  created */

/*    Description:
*       Calculates minimum size for shared memory segment given requirements
*       for memory arrays and variance arrays.
*     Status:
*     Method:
*     Bugs:
*     Authors:
*	A P Hammersley (hammersley@esrf.fr)
*     History:
*	19-Nov-2003: V0.1 Original (Hammersley)
*    Import: */
/*   Status: */
/*   Extra "C" Variables: */
/*--------1---------2---------3---------4---------5---------6---------7--*/

{

  /* Local Variables: */
  int num_bytes; /* Number of bytes necessary for FIT2D arrays */

  /* Basic current data arrays */    
  num_bytes = xmaxdat * ymaxdat * sizeof(float); /* "DATA" */
  num_bytes = num_bytes + xmaxdat * ymaxdat * sizeof(char); /* "MASK" */
  num_bytes = num_bytes + xmaxdat * sizeof(float); /* "XAXIS" */
  num_bytes = num_bytes + ymaxdat * sizeof(float); /* "YAXIS" */

  /* Memory arrays */
  if (memory_exist) {
    num_bytes = num_bytes + xmaxdat * ymaxdat * sizeof(float); /* "MDATA" */
    num_bytes = num_bytes + xmaxdat * sizeof(float); /* "MXAXIS" */
    num_bytes = num_bytes + ymaxdat * sizeof(float); /* "MYAXIS" */
  }

  /* Variance arrays */
  if (variances_exist) {
    num_bytes = num_bytes + xmaxdat * ymaxdat * sizeof(float); /* "VARIANCES" */

    if (memory_exist) { /* "MVARIANCES" */
      num_bytes = num_bytes + xmaxdat * ymaxdat * sizeof(float);
    }

  }

  /* Return number of bytes to allocate */
  return num_bytes;

  /* End of Function f2d_sharedsizec */

}







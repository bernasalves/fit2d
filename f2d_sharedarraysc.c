/********1*********2*********3*********4*********5*********6*********7*********8

***            **************************
***            *                        *
***            * f2d_sharedaddressesc.c *
***            *                        *
***            **************************

*/

#define _POSIX_SOURCE 1
#define _SVID_SOURCE 1
#define TRUE ~0

#include <stdio.h>
#include <stdlib.h>

/*+ F2D_SHAREDADDRESSESC: Set base addresses of program arrays */
#if defined (WIN32)
#define F2D_SHAREDADDRESSESC __stdcall F2D_SHAREDADDRESSESC
#elif defined (UNDER)
#define F2D_SHAREDADDRESSESC f2d_sharedaddressesc_
#else
#define F2D_SHAREDADDRESSESC f2d_sharedaddressesc
#endif

int F2D_SHAREDADDRESSESC /* Returns "TRUE" if successful */
(float base_address[],   /* Base address of shared memory segment to use */
 int xmaxdat,            /* Dimension size in X-direction for data arrays */
 int ymaxdat,            /* Dimension size in Y-direction for data arrays */
 int memory_exist,       /* "TRUE" if memory array exists */
 int variances_exist,    /* "TRUE", if a data variance array is created */
 float *DATA[],          /* Returns base address of array "DATA" */
 char *MASK[],           /* Returns base address of array "MASK" */
 float *XAXIS[],         /* Pointer to dynamic array "XAXIS" */
 float *YAXIS[],         /* Returns base address of array "YAXIS" */
 float *VARIANCES[],     /* Returns base address of array "VARIANCES" */
 float *MDATA[],         /* Returns base address of array "MDATA" */
 float *MXAXIS[],        /* Returns base address of array "MXAXIS" */
 float *MYAXIS[],        /* Returns base address of array "MYAXIS" */
 float *MVARIANCES[])    /* Returns base address of array "MVARIANCES" */

/*    Description:
*       Calculates addresses of different available program arrays given
*       the base address and the sizes of the arrays.
*     Status:
*     Method:
*     Bugs:
*     Authors:
*	A P Hammersley (hammersley@esrf.fr)
*     History:
*	18-Dec-2003: V0.1 Original (Hammersley)
*	22-Dec-2003: V0.2 Correct stupid "clever" "C"'s addressing (Hammersley)
*	09-Jan-2004: V0.3 Tidy code (Hammersley)
*    Import: */
/*   Status: */
/* Deficiency: 
/*   Extra "C" Variables: */
/*-------1---------2---------3---------4---------5---------6---------7--------*/

{

  /* Local Variables: */
  char *address; /* Address pointer, in "char" to stop "C"'s stupid "clever"
		    and confusing address arithmetic logic */

  /* Calculate addresses of arrays */    
  *DATA = base_address;
  address = (char *) base_address;
  *MASK = (char *) (address + xmaxdat * ymaxdat * sizeof(float));
  address = address + xmaxdat * ymaxdat * (sizeof(float) + sizeof(char));
  *XAXIS = (float *) address;
  address = address + xmaxdat * sizeof(float);
  *YAXIS = (float *) address;
  address = address + ymaxdat * sizeof(float);

  if (memory_exist) {
    *MDATA = (float *) address;
    address = address + xmaxdat * ymaxdat * sizeof(float);
    *MXAXIS = (float *) address;
    address = address + xmaxdat * sizeof(float);
    *MYAXIS = (float *) address;
    address = address + ymaxdat * sizeof(float);
  } else {
    *MDATA = NULL;
    *MXAXIS = NULL;
    *MYAXIS = NULL;
  }

  if (variances_exist) {
    *VARIANCES = (float *) address;

    if (memory_exist) {
      *MVARIANCES = (float *) (address + xmaxdat * ymaxdat * sizeof(float));
    } else {
      *MVARIANCES = NULL;
    }

  } else {
    *VARIANCES = NULL;
    *MVARIANCES = NULL;
  }
 

  /* Set good return status */
  return TRUE;

  /* End of Function f2d_sharedaddressesc */

}
/********1*********2*********3*********4*********5*********6*********7*********8

***            ***********************
***            *                     *
***            * f2d_sharedarraysc.c *
***            *                     *
***            ***********************

*/

#define _POSIX_SOURCE 1
#define _SVID_SOURCE 1

#include <stdio.h>
#include <errno.h>

#ifndef WIN32
/* Mac OSX has error in ipc.h which is called by shm.h */
#ifndef MAC
#include <sys/shm.h>
#endif
#endif

#include "syntax.h"

/*+ F2D_SHAREDARRAYSC: Allocate shared memory */
#if defined (WIN32)
#define F2D_SHAREDARRAYSC __stdcall F2D_SHAREDARRAYSC
#elif defined (UNDER)
#define F2D_SHAREDARRAYSC f2d_sharedarraysc_
#else
#define F2D_SHAREDARRAYSC f2d_sharedarraysc
#endif

Subroutine F2D_SHAREDARRAYSC
(Integer *xmaxdat,           /* Dimension size in X-direction for data arrays */
 Integer *ymaxdat,           /* Dimension size in Y-direction for data arrays */
 Boolean *memory_exist,      /* "True" if memory array exists */
 Boolean *variances_exist,   /* "True", if a data variance array is created */
 Boolean *shared_memory,     /* "True", if shared memory is to used */
 Integer *memory_id,         /* Identifier of shared memory (if used) */
 Boolean *mask_exist,        /* "True" if memory array exists */
 Real **pDATA,               /* Pointer to dynamic array "DATA" */
 Character **pMASK,          /* Pointer to dynamic array "MASK" */
 Real **pXAXIS,              /* Pointer to dynamic array "XAXIS" */
 Real **pYAXIS,              /* Pointer to dynamic array "YAXIS" */
 Real **pVARIANCES,          /* Pointer to dynamic array "VARIANCES" */
 Real **pMDATA,              /* Pointer to dynamic array "MDATA" */
 Real **pMXAXIS,             /* Pointer to dynamic array "MXAXIS" */
 Real **pMYAXIS,             /* Pointer to dynamic array "MYAXIS" */
 Real **pMVARIANCES,         /* Pointer to dynamic array "MVARIANCES" */
 Integer *retstat)           /* Return status 0 = good status */

/*    Description:
*       Connects to shared memory segment and calculates addresses of
*       arrays
*     Status:
*     Method:
*     Bugs:
*     Authors:
*	A P Hammersley (hammersley@esrf.eu)
*     History:
*	13-Nov-2003: V0.1 Original (Hammersley)
*	18-Nov-2003: V0.2 Add support for mask array (Hammersley)
*	19-Nov-2003: V0.3 Calculate minimum size for shared segment (Hammersley)
*	25-Nov-2003: V0.4 Debugging (Hammersley)
*	01-Dec-2003: V0.5 Test defined addresses (Hammersley)
*	02-Dec-2003: V0.6 See if values are written in "DATA" array (Hammersley)
*	07-Apr-2005: V0.7 Remove support for Mac OSX which has compile errors
*         in "ipc.h" which is called from "shm.h" (Hammersley)
*    Import: */
/*   Status: */
/*   Extra "C" Variables: */
/*-------1---------2---------3---------4---------5---------6---------7--------*/

{

  /* Local Variables: */
  Real *ARRAYS; /* Pointer to start of program arrays */
  Integer num_bytes; /* Number of bytes needed for shared segment */
  Integer x; /* Loop variable for test purposes */

#if defined (WIN32)
  printf ("ERROR: Shared memory is not presently available under Windows\n");
  exit(0);
#elif defined (MAC)
  printf ("ERROR: Shared memory is not available owing to errors in Mac OSX\n");
  exit(0);
#else

  /* Calculate minimum size of shared segment */
  num_bytes = f2d_sharedsizec (*xmaxdat, *ymaxdat, *memory_exist, 
   *variances_exist);
  printf ("NOTE: Minimum size of shared segment must be %i bytes\n", num_bytes);

  /* DEBUG */
  printf ("f2d_sharedarraysc: memory_id = %i\n", *memory_id);
  /* DEBUG */

  /* Get ID of created shared memory segment */
  *memory_id = shmget (97, num_bytes, 0777);

  if (*memory_id Equals -1) {
    printf("shmget failed\n");

    if (errno Equals EACCES) {
      printf ("shmat failed: EACCESS, Could not access shared memory\n");
    } else if (errno Equals EINVAL) {
      printf ("shmat failed: EINVAL error\n");
    } else if (errno Equals EEXIST) {
      printf ("shmat failed: EEXIST, Memory exists\n");
    } else if (errno Equals ENOSPC) {
      printf ("shmat failed: ENOSPC, all memory id's have been taken\n");
    } else if (errno Equals ENOENT) {
      printf ("shmat failed: ENOENT, no segment exists\n");
    } else if (errno Equals ENOMEM) {
      printf ("shmat failed: ENOMEM, no memory available for segment\n");
    }

    *retstat = -1;
    Return;

  }

  /* DEBUG */
  printf ("f2d_sharedarraysc: internal memory_id = %i\n", *memory_id);
  /* DEBUG */

  /* Attach to shared memory */
  if ((ARRAYS = (Real *) shmat (*memory_id, Null, 0)) Equals Null) {
    printf("Could not attach to shared memory\n");
    exit (0);
  }

  if ((int) ARRAYS Equals -1) {

    if (errno Equals EACCES) {
      printf ("shmat failed: Could not access shared memory\n");
    } else if (errno Equals EINVAL) {
      printf ("shmat failed: Shared memory identifier invalid\n");
    } else if (errno Equals ENOMEM) {
      printf ("shmat failed: Couldn't allocate memory\n");
    }

    *retstat = -1;
    Return;

  }
      
  /***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG*/
  /* printf ("ARRAYS = %i\n", ARRAYS); */
  /*CountUp (x, 0, 9) {
    printf ("ARRAYS[%i] = %f\n", x, ARRAYS[x]);
    }
  */
  /***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG*/

  /* DEBUG */
  /* printf ("f2d_sharedarraysc: attached to shared memory\n", num_bytes); */
  /* printf ("f2d_sharedarraysc: xmaxdat = %i, ymaxdat = %i, real size = %i\n", 
   *xmaxdat, *ymaxdat, sizeof(Real));
  printf ("f2d_sharedarraysc: DATA bytes = %i\n", 
  *xmaxdat * *ymaxdat * sizeof(Real)); */
  /* DEBUG */

  /* Calculate addresses of arrays */    
  if (Not F2D_SHAREDADDRESSESC (ARRAYS, *xmaxdat, *ymaxdat, 
   *memory_exist, *variances_exist, pDATA, pMASK, pXAXIS, pYAXIS, pVARIANCES,
   pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES)) {
    printf ("ERROR: Problem calculating base addresses of program arrays\n");
  }

  /* DEBUG */
  /* printf ("f2d_sharedarraysc: addresses calculated\n", num_bytes); */
  /* DEBUG */

  /* The mask is now defined */
  *mask_exist = True;

  /* Set good return status */
  *retstat = 0;

#endif

  /* End of Function F2D_SHAREDARRAYSC */
}







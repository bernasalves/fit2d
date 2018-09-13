#define _POSIX_SOURCE 1
#define _SVID_SOURCE 1
                       
#include <stdio.h>
#include <errno.h>
#include <sys/shm.h>
#include "../LIBRARIES/I/syntax.h"

/*+---------------------------------------------------------------------------*/
/* RunFIT2D as a sub-process test */
Integer Program (Integer argc, Character **argv)

{
  /* Local Constants */
#define MAX_INPUT 256

  /* Local Variables */
  Real *ARRAYS; /* Pointer to shared memory arrays */
  FILE *command; /* File (pipe) for commands to FIT2D */
  Character string[512]; /* Command string to run FIT2D */
  Integer memory_id; /* Identifier for shared memory */
  Integer num_bytes; /* Number of bytes of memory to allocated */
  Integer x; /* Loop variable for X-direction */
  Integer xdim = 1024; /* X-dimension of array */
  Integer y; /* Loop variable for Y-direction */
  Integer ydim = 1024; /* Y-dimension of array */
   
  /* Calculate size of shared memory segment */
  num_bytes = f2d_sharedsizec (xdim, ydim, True, False);
  printf ("Size of shared memory segment = %i\n", num_bytes);

  /* Create new shared memory section */
  memory_id = shmget(97, num_bytes, 0777 | IPC_CREAT);

  printf ("memory_id = %i\n", memory_id);

  /* Check memory identifier */
  if (memory_id Equals -1) {
    printf ("Problem creating shared memory:\n");

    if (errno Equals EINVAL) {
      printf ("problem with size being out of range\n");
    } else if (errno Equals EEXIST) {
      printf ("problem caused by segment already existing\n");
    }

    exit (0);
  }

  /* Attach to shared memory */
  if ((ARRAYS = shmat (memory_id, (Real *) 0, 0)) Equals Null) {
    printf("Could not attach shared memory\n");
    exit (0);
  }

  printf ("runfit2d: After shmat, base address = %i\n", ARRAYS);

  /* Fill "DATA" array with some values */
  for (y = 0; y LessThan ydim; Increment(y)) {

    /* printf ("y = %i\n", y); */
    for (x = 0; x LessThan xdim; Increment(x)) {
      ARRAYS[x + y * xdim] = Real((x - y) % 512);
    }

  }

  printf ("runfit2d: Ready to call FIT2D\n");

  /* Write FIT2D start-up command string */
  /* sprintf (string, 
   "/users/hammersl/FIT2D/LINUX/fit2d -dim1024x1024 -com -data -id=%i", 
   memory_id); */
  sprintf (string, 
   "/users/hammersl/FIT2D/LINUX/fit2d -dim%ix%i -com -id=%i", 
   xdim, ydim, memory_id);

  /* Open FIT2D as a sub-process with input through a pipe */
  command = (FILE *) popen (string, "w");

  /* Check sub-process has been started O.K. */
  if (command Equals Null) {
    printf ("Problem starting sub-process");
    exit (0);
  }

  /* Send commands to FIT2D */
  fprintf (command, "I ACCEPT\n");
  fflush (command);

  /* Enter Image Processing interface to display data */
  fprintf (command, "IMAGE\n");
  fflush (command);

  /* Input image */
  fprintf (command, "INPUT\n");
  fprintf (command, "/users/hammersl/DATA/TILT_TEST/test_001.mar3450\n");
  fflush (command);

  sleep(4);

  /* Zoom in */
  /* fprintf (command, "ZOOM IN\n");
  fprintf (command, "2\n");
  fprintf (command, "100.0\n");
  fprintf (command, "100.0\n");
  fprintf (command, "900.0\n");
  fprintf (command, "800.0\n");
  fflush (command); */
  
  sleep(4);

  /* Exit interface */
  fprintf (command, "EXIT\n");

  /* Close FIT2D */
  fprintf (command, "EXIT FIT2D\n");
  fprintf (command, "YES\n");
  fflush (command);

  /* Detach from shared memory */
  shmdt (ARRAYS);

  /* Delete shared memory segment */
  if (shmctl (memory_id, IPC_RMID , Null) NotEqual 0) {
    printf ("ERROR: shmctl failed to remove shared memory segment\n");
  }

  /* Close pipe */
  pclose (command);

  exit (0);

}

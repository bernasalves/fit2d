#define _POSIX_SOURCE 1
                       
#include <stdio.h>
#include <errno.h>
#include <sys/shm.h>
#include "../LIBRARIES/I/syntax.h"

/*+---------------------------------------------------------------------------*/
/* run_win32_fit2d: Run on WIN32 FIT2D as a sub-process test */
Integer Program (Integer argc, Character **argv)


{
  /* Local Constants */
#define MAX_INPUT 256

  /* Local Variables */
  HANDLE shared_file; /* Handle to file object for shared memory */
  HANDLE mapped_file; /* Handle to mapped file */

  Real *ARRAYS; /* Pointer to shared memory arrays */
  FILE *command; /* File (pipe) for commands to FIT2D */
  Character string[512]; /* Command string to run FIT2D */
  Universal *shared_id; /* Pointer for shared memory handle*/
  Integer num_bytes; /* Number of bytes of memory to allocated */
  Integer x; /* Loop variable for X-direction */
  Integer xdim = 1024; /* X-dimension of array */
  Integer y; /* Loop variable for Y-direction */
  Integer ydim = 1024; /* Y-dimension of array */
   
  /* Calculate size of shared memory segment */
  num_bytes = f2d_sharedsizec (xdim, ydim, True, False);
  printf ("Size of shared memory segment = %i\n", num_bytes);

  /* Use CreateFile to create "file" for shared memory mapping */
  shared_file = CreateFile ("FIT2D_Shared_Memory_File.bin", 
   GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
   Null, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, FILE_FLAG_DELETE_ON_CLOSE, 
   Null);

  /* Check "file" has been created */
  if (shared_file Equals INVALID_HANDLE_VALUE) {
    printf ("Problem creating file for shared memory mapping:\n");
    exit (0);
  }

  /* Memory map file */
  mapped_file = CreateFileMapping (shared_file, Null, PAGE_READWRITE, 
   num_bytes, num_bytes, Null);

  if (mapped_file Equals Null) {
    printf ("Problem creating mapped file for shared memory mapping:\n");
    exit (0);
  }

  /* Get pointer to mapped file */
  if ((ARRAYS = MapViewOfFile (mapped_file, FILE_MAP_ALL_ACCESS,
   0, 0, 0)) Equals Null) {
    printf ("MapViewOfFile failed: Couldn't get pointer to shared memory\n");
    exit (0);
  }

  printf ("runwin32fit2d: After MapViewOfFile , base address = %i\n", ARRAYS);

  /* Fill "DATA" array with some values */
  for (y = 0; y LessThan ydim; Increment(y)) {

    /* printf ("y = %i\n", y); */
    for (x = 0; x LessThan xdim; Increment(x)) {
      ARRAYS[x + y * xdim] = Real((x - y) % 512);
    }

  }

  printf ("runwin32fit2d: Ready to call FIT2D\n");

  /* Write FIT2D start-up command string */
  /* sprintf (string, 
   "/users/hammersl/FIT2D/LINUX/fit2d -dim1024x1024 -com -data -id=%i", 
   memory_id); */
  sprintf (string, "/users/hammersl/FIT2D/LINUX/fit2d -dim%ix%i -com -id=%i", 
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

  /* Unmapped shared memory file */
  UnmapViewOfFile (ARRAYS);

  /* Delete shared memory segment */
  if (Not DeteteSharedMemory (memory_id)) {
    printf ("ERROR: Failed to delete shared memory segment\n");
  }

  /* Close pipe */
  pclose (command);

  exit (0);

}

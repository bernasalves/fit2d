#define _POSIX_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

main () {

  int i;
  FILE *io_file;
  char file_name[256];
  unsigned char BYTES[1024];
  int num_input;
  
  for (i = 0; i <= 9999; i = i + 1) {

    sprintf (file_name, "X:\\inhouse\\Manfred\\data\\inhouse_10apr09\\setup110409\\edf\\canna_2_mesh_2_%4.4i.edf", i);

    io_file = fopen(file_name, "rb");

    /* Check file pointer is defined O.K. */
    if (io_file == NULL) {
      printf ("Problem opening file\n");

    } else {
      printf ("File opened OK; %s\n", file_name);

      /* read in data from file */
      num_input = 1024;
      while (num_input == 1024) {
	num_input = fread (BYTES, sizeof(char), 1024, io_file);
	/* printf ("num_input = %i\n", num_input); */
      }

    }

    fclose (io_file);

  }

}

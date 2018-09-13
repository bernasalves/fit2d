
#include "user_pipe.h"
#include <sys/shm.h>
#include <stdio.h>

user_code(argc, argv)
char    **argv;
{
  register int    i;
  register char   *p;
  
  int flag=0 , size =0, key=-1, create=0 , put=0, get=0, elems =1;
  int points =0, delete=0, id=0;
  char *shmptr;

  while (argc--) {
    p = *argv++;
    if (*p == '-') {
      /* switch on options */
      switch(*++p) {
      case 's':
	/* The size is given in number of doubles */
	size = sizeof(double)*atoi(p+1);
	break;
      case 'p':
	/* The number of points in data group */
	points = atoi(p+1);
	break;
      case 'e':
	/* The number of elements in data group */
	elems = atoi(p+1);
	break;
      case 'k':
	/* A key can be given */
	key = atoi(p+1);
	break;
      case 'c':
	/* We want to create the shared mem */
	create = 1;
	break;
      case 'd':
	/* We want to delete the shared mem */
	delete = 1;
	break;
      case 'w':
	/* We want to write into shared mem */
	put = 1;
	break;
      case 'r':
	/* We want to read from shared mem */
	get = 1;
	break;
      case 'i':
	/* We want to use shm_id */
	id = atoi(p+1);
	break;
      default:
	set_return_value(-1.);
	fprintf(stderr,"Unknown option %c\n",*p);
	do_error_return();
	/*NOTREACHED*/
      }
    } else {
      fprintf(stderr,
	 "usage: -s<size> -p<points> -e<elems> -i<id> -k<key> -c -w -r -d ");
    }
  }
  
  if (size == 0) {
    size = sizeof(double) * elems * points;
  }
  
  if (create) {
    if (size == 0) {
      fprintf(stderr,"You have to give a size of no of points\n");
      set_return_value(-1.); /* Size is 0 */
      do_error_return();
    }
    flag = 0644;
    if (key == -1) 
      key = IPC_PRIVATE;
    else 
      flag |= IPC_CREAT;
    id = shmget((key_t) key, (size_t) size, flag);
    fprintf(stderr,"shm id %d created\n",id);
  }

  if (put || get) {
    if (id ==0) {
      fprintf(stderr,"Not a valid id %d\n",id);
      set_return_value(-1.); 
      do_error_return();
    }
    
    if ((shmptr = shmat(id,(char*) 0,0)) == (char*) -1) {
      fprintf(stderr,"Could not attach shared mem (id %d)\n",id);
      set_return_value(-1.); 
      do_error_return();
    }
    
    fprintf(stderr,"shm id %d attached (addr 0x%x)\n",id,shmptr);

    if (put) {
      get_input_data((double*) shmptr, points, elems);
      fprintf(stderr,"data written %d points %d elems\n",points,elems);
    }
    else {
      set_return_data((double*)shmptr, points, elems,points);
      fprintf(stderr,"data read %d points %d elems\n",points,elems);
    }


    shmdt(shmptr);
    fprintf(stderr,"shm id %d detached (addr 0x%x)\n",id,shmptr);
  }
    
  if (delete) {
    if (id ==0) {
      fprintf(stderr,"Not a valid id %d\n",id);
      set_return_value(-1.); /* Size is 0 */
      do_error_return();
    }
    
    shmctl(id, IPC_RMID , (struct shmid_ds *)NULL);
    fprintf(stderr,"shm id %d deleted\n",id);
  }

  set_return_value((double)id);
}






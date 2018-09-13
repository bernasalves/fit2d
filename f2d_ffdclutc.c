/*********1*********2*********3*********4*********5*********6*********7**

***            ******************
***            *                *
***            * f2d_ffdclutc.c *
***            *                *
***            ******************
*/

#define _POSIX_SOURCE 1

/* Try to improve "C"'s very vague sense of boolean logic */
#define TRUE !0
#define FALSE 0

/*+    F2D_FFDCLUTC: apply Flat-Field correction to Distortion */
/*     Correction Look-Up-Table  */
#if defined (WIN32)
void __stdcall F2D_FFDCLUTC (int *xmaxdat, int *ymaxdat, 
     int *xnumdat, int *ynumdat, float FLAT_FIELD[],
     int *xmax_lut, int *ymax_lut, int *xnum_lut, int *ynum_lut, 
     char X_SD[], char Y_SD[], 
     unsigned char INT_REBINNED[], int *retstat)
#elif defined (UNDER)
void f2d_ffdclutc_(int *xmaxdat, int *ymaxdat, 
     int *xnumdat, int *ynumdat, float FLAT_FIELD[],
     int *xmax_lut, int *ymax_lut, int *xnum_lut, int *ynum_lut, 
     char X_SD[], char Y_SD[], 
     unsigned char INT_REBINNED[], int *retstat)
#else
void f2d_ffdclutc(int *xmaxdat, int *ymaxdat, 
     int *xnumdat, int *ynumdat, float FLAT_FIELD[],
     int *xmax_lut, int *ymax_lut, int *xnum_lut, int *ynum_lut, 
     char X_SD[], char Y_SD[], 
     unsigned char INT_REBINNED[], int *retstat)
#endif

/*    Description:
*       From Fortran should be called:
*
*         Call F2D_FFDCLUTC (xmaxdat, ymaxdat, 
*           xnumdat, ynumdat, FLAT_FIELD,
*           xmax_lut, ymax_lut, xnum_lut, ynum_lut, 
*           X_SD, Y_SD, INT_REBINNED, retstat)
*
*     Import:
*       Integer xmaxdat ! First dimension (Fortran) of data array
*       Integer ymaxdat ! Second dimension (Fortran) of data array
*       Integer xnumdat ! Number of defined X-elements
*       Integer ynumdat ! Number of defined Y-elements
*       Real FLAT_FIELD(xmaxdat, ymaxdat) ! Flat-field response
*         of each pixel, should have been normalised so basic
*         range is around 1.0, or maybe such that the maximum is 1.0
*       Integer xmax_lut ! First dimension (Fortran) of LUT arrays
*       Integer ymax_lut ! Second dimension (Fortran) of LUT arrays
*       Integer xnum_lut ! Number of defined LUT X-elements
*       Integer ynum_lut ! Number of defined LUT Y-elements
*       Byte X_SD(xmax_lut, ymax_lut) ! X shifts
*       Byte Y_SD(xmax_lut, ymax_lut) ! Y shifts
*     Import/Export:
*       Byte INT_REBINNED(9, xmax_lut, ymax_lut) ! Re-bin fractions
*         for every input pixel. Multiplied by flat-field response
*         on output
*     Export:
*       Integer retstat ! Return status variable:
*         0 = Good status
*         1 = Bad status: LUT and flat-field arrays are of different
*           sizes
*        -1 = Flat-field correction performed, but an overflow was
*             encountered in the output table values
*     Status:
*     Method:
*     Bugs:
*     Authors:
*	A P Hammersley (hammersley@esrf.fr)
*     History:
*	05-Mar-1997: V0.1 Original (HAMMERSLEY)
*       30-Jun-1998: V0.2 Support for WIN32 (HAMMERSLEY)
*    Import: */
/*   Status: */
/*   Extra "C" Variables: */
/*--------1---------2---------3---------4---------5---------6---------7--*/

{

/* Local Variables: */
unsigned char rebin_int; /* Re-bin fraction as a byte */
int in_lut; /* Index to LUT elements */
int new_fraction; /* Output pixel fraction multiplied by appropriate */
                  /* flat-field pixel, to the nearest integer */
int out_pixel; /* Index of output element */
int overflow_warning; /* TRUE is the flat-field correction would make */
                      /* the value too big for the storage element */
int rebin_pixel; /* Pixel in rebin intensities array */
int x; /* Loop variable for X-direction */
int x_base; /* Base output pixel for X-direction */
int x_out; /* Output pixel in X-direction */
int xi; /* Inner loop variable for X-direction */
int y; /* Loop variable for Y-direction */
int y_out; /* Output pixel in Y-direction */
int yi; /* Inner loop variable for Y-direction */

/* Initialise variables */
  overflow_warning = FALSE;

/* Check that the arrays are the same size */
  if (*xnumdat != *xnum_lut || *ynumdat != *ynum_lut) {
     *retstat = 1;
  }
  else {

     *retstat = 0;

   /* Loop through distortion correction look-up table */
     for (y = 0; y < *ynum_lut; y = y + 1) {

        in_lut = y * *xmax_lut;
        rebin_pixel = 9 * in_lut;
        for (x = 0; x < *xnum_lut; x = x + 1) {

         /* Calculate output target pixel */
           x_base = X_SD[in_lut] + x;
           y_out = Y_SD[in_lut] + y;

         /* Loop through the output fractions */
           for (yi = 0; yi < 3; yi = yi + 1) {

              for (xi = 0; xi < 3; xi = xi + 1) {

	         rebin_int = INT_REBINNED[rebin_pixel];

	       /* Only bother with pixels with intensity */
	         if (rebin_int > 0) {

                    x_out = x_base + xi;

		    if (x_out >= 0 && y_out >= 0 &&
                      x_out < *xnum_lut && y_out < *ynum_lut) {

		       out_pixel = y_out * *xmaxdat + x_out;

                     /* Apply flat-field correction to re-bin fraction */
                       new_fraction = (int) ((float) rebin_int * 
                         FLAT_FIELD[out_pixel] + 0.49999);

		       if (new_fraction < 256) {
                          INT_REBINNED[rebin_pixel] = new_fraction;
		       }
                       else {
                          INT_REBINNED[rebin_pixel] = 255;
                          overflow_warning = TRUE;
                       }
		    }  
                 }

	       /* Increment re-bin fraction element */
	         rebin_pixel = rebin_pixel + 1;
	       
	      }

	    /* Increment Y output pixel */
	      y_out = y_out + 1;
	      
           }

	 /* Increment array element pointers */
	   in_lut = in_lut + 1;

        }
     }
  }

  if (overflow_warning) {
     *retstat = -1;
  }

/* End of Function F2D_FFDCLUTC */
}










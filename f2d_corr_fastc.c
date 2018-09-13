/*********1*********2*********3*********4*********5*********6*********7**

***            ********************
***            *                  *
***            * f2d_corr_fastc.c *
***            *                  *
***            ********************
*/

#define _POSIX_SOURCE 1

/*+ F2D_CORR_FASTC: CORRECTION FAST (in C because of unsigned bytes)  */
#if defined (WIN32)
void __stdcall F2D_CORR_FASTC (int *xmax_lut, int *ymax_lut, 
     char X_SD[], char Y_SD[], unsigned char INT_REBINNED[],
     float *overload_value, int *xmaxdat, int *ymaxdat, 
     int *xstrelm, int *ystrelm, int *xendelm, int *yendelm,
     float DATA[],
     int *mxstrelm, int *mystrelm, int *mxendelm, int *myendelm,
     float MDATA[])
#elif defined (UNDER)
void f2d_corr_fastc_(int *xmax_lut, int *ymax_lut, 
     char X_SD[], char Y_SD[], unsigned char INT_REBINNED[],
     float *overload_value, int *xmaxdat, int *ymaxdat, 
     int *xstrelm, int *ystrelm, int *xendelm, int *yendelm,
     float DATA[],
     int *mxstrelm, int *mystrelm, int *mxendelm, int *myendelm,
     float MDATA[])
#else
void f2d_corr_fastc(int *xmax_lut, int *ymax_lut, 
     char X_SD[], char Y_SD[], unsigned char INT_REBINNED[],
     float *overload_value, int *xmaxdat, int *ymaxdat, 
     int *xstrelm, int *ystrelm, int *xendelm, int *yendelm,
     float DATA[],
     int *mxstrelm, int *mystrelm, int *mxendelm, int *myendelm,
     float MDATA[])
#endif

/*    Description:
*       From Fortran should be called:
*
*         Call F2D_CORR_FASTC (xmax_lut, ymax_lut, 
*           X_SD, Y_SD, INT_REBINNED,
*           overload_value, xmaxdat, ymaxdat, 
*           xstrelm, ystrelm, xendelm, yendelm,
*           DATA,
*           mxstrelm, mystrelm, mxendelm, myendelm,
*           MDATA)
*
*     Import:
*       Integer xmax_lut ! First dimension (Fortran) of LUT arrays
*       Integer ymax_lut ! Second dimension (Fortran) of LUT arrays
*       Byte X_SD(xmax_lut, ymax_lut) ! X shifts
*       Byte Y_SD(xmax_lut, ymax_lut) ! Y shifts
*       Byte INT_REBINNED(9, xmax_lut, ymax_lut) ! re-bin fractions
*       Integer xmaxdat ! First dimension (Fortran) of data array
*       Integer ymaxdat ! Second dimension (Fortran) of data array
*       Integer xstrelm ! Starting X-element of input region
*       Integer ystrelm ! Starting Y-element of input region
*       Integer xendelm ! End X-element of input region
*       Integer yendelm ! End Y-element of input region
*       Real DATA(xmaxdat, ymaxdat) ! Input data value
*     Export:
*       Integer mxstrelm ! Starting X-element of re-binned  region
*       Integer mystrelm ! Starting Y-element of re-binned region
*       Integer mxendelm ! End X-element of re-binned region
*       Integer myendelm ! End Y-element of re-binned region
*       Real MDATA(xmaxdat, ymaxdat) ! Re-binned data values
*     Status:
*     Method:
*     Bugs:
*     Authors:
*	A P Hammersley (hammersley@esrf.fr)
*     History:
*	04-Oct-1996: V0.1 Original (HAMMERSLEY)
*       25-Mar-1998: V0.2 Restructure, remove inner loop (HAMMERSLEY)
*       30-Jun-1998: V0.3 Support for WIN32 (HAMMERSLEY)
*    Import: */
/*   Status: */
/*   Extra "C" Variables: */
/*--------1---------2---------3---------4---------5---------6---------7--*/

{

/* Local Variables: */
  int rebin_int; /* Re-bin fraction as a byte */
  int in_lut; /* Index to LUT elements */
  int in_pixel; /* Index of input element */
  int mxend; /* Local version of 'mxstrelm' */
  int mxstr; /* Local version of 'mxendelm' */
  int myend; /* Local version of 'mystrelm' */
  int mystr; /* Local version of 'myendelm' */
  int out_base; /* Base index for output pixels */
  int out_pixel; /* Index of output element */
  int rebin_pixel; /* Pixel in rebin intensities array */
  int x; /* Loop variable for X-direction */
  int x_base; /* Base output pixel for X-direction */
  int x_out; /* Output pixel in X-direction */
  int xe; /* Local version of 'xendelm' */
  int xi; /* Inner loop variable for X-direction */
  int xmaxd; /* Local version of 'xmaxdat' */
  int xmaxlut; /* Local version of 'xmax_lut' */
  int xs; /* Local version of 'xstrelm' */
  int y; /* Loop variable for Y-direction */
  int y_end; /* Y-end sub-pixel */
  int y_out; /* Output pixel in Y-direction */
  int y_start; /* Y-starting sub-pixel */
  int ye; /* Local version of 'yendelm' */
  int yi; /* Inner loop variable for Y-direction */
  int ys; /* Local version of 'ystrelm' */
  float intensity; /* Intensity of an input pixel */
  float overload; /* Local version of the overload value */

/* Initialise variables */
  overload = *overload_value;
  mxstr = *mxstrelm - 1;
  mystr = *mxstrelm - 1;
  mxend = *mxendelm - 1;
  myend = *myendelm - 1;
  xmaxlut = *xmax_lut;
  xmaxd = *xmaxdat;
  xs = *xstrelm - 1;
  xe = *xendelm;
  ys = *ystrelm - 1;
  ye = *yendelm;

/* Perform spatial correction */
  for (y = ys; y < ye; y = y + 1) {

     in_pixel = y * xmaxd + xs;
     in_lut = y * xmaxlut + xs;
     rebin_pixel = 9 * (y * xmaxlut + xs);

     for (x = xs; x < xe; x = x + 1) {

        intensity = DATA[in_pixel];
	x_base = X_SD[in_lut] + x;
	y_out = Y_SD[in_lut] + y;

        if (intensity < overload) {

	   if (y_out + 2 > myend) {
	      y_end = myend - y_out + 1;

	      if (y_end < 0) {
		 y_end = 3;
	      }

	   } else {
	      y_end = 3;
	   }
       
	   if (y_out < mystr) {
              y_start = mystr - y_out;
	      y_out = mystr;

	      if (y_start < 3) {
		 rebin_pixel = rebin_pixel + 3 * y_start;
	      } else {
		 rebin_pixel = rebin_pixel + 9;
		 y_end = 3;
	      }

	   } else {
	      y_start = 0;
	   }

	   out_base = y_out * xmaxd;

	   for (yi = y_start; yi < y_end; yi = yi + 1) { 

	      rebin_int = INT_REBINNED[rebin_pixel];
	      if (rebin_int > 0) {

		 x_out = x_base;
		 if (x_out >= mxstr && x_out <= mxend) {

		    out_pixel = out_base + x_out;
		 
		    MDATA[out_pixel] = MDATA[out_pixel] +
		      (float) rebin_int * intensity;
		 }

	      }

	      rebin_pixel = rebin_pixel + 1;
	      rebin_int = INT_REBINNED[rebin_pixel];
	      if (rebin_int > 0) {

		 x_out = x_base + 1;
		 if (x_out >= mxstr && x_out <= mxend) {

		    out_pixel = out_base + x_out;

		    MDATA[out_pixel] = MDATA[out_pixel] +
		      (float) rebin_int * intensity;
		 }

	      }

	      rebin_pixel = rebin_pixel + 1;
	      rebin_int = INT_REBINNED[rebin_pixel];
	      if (rebin_int > 0) {

		 x_out = x_base + 2;

		 if (x_out >= mxstr && x_out <= mxend) {

		    out_pixel = out_base + x_out;

		    MDATA[out_pixel] = MDATA[out_pixel] +
		      (float) rebin_int * intensity;
		 }

	      }

	    /* Increment re-bin fraction element */
	      rebin_pixel = rebin_pixel + 1;

	    /* Increment Y output pixel */
	      y_out = y_out + 1;
              out_base = out_base + xmaxd;

	   } /* yi */ 

	   if (y_end < 3) {
	      rebin_pixel = rebin_pixel + (3 - y_end) * 3;
	   }

	 /* Increment array element pointers */
	   in_pixel = in_pixel + 1;
	   in_lut = in_lut + 1;

	} else {

         /* Overloaded pixel */
	   for (yi = 0; yi < 3; yi = yi + 1) { 

	      for (xi = 0; xi < 3; xi = xi + 1) { 
		 x_out = x_base + xi; 
	         if (x_out >= mxstr && x_out <= mxend &&
                   y_out >= mystr && y_out <= myend) {

		    rebin_int = INT_REBINNED[rebin_pixel];
		    if (rebin_int > 0) {
		       MDATA[out_pixel] = MDATA[out_pixel] + 
		         intensity * 255.0;
                    }
                 }
                 rebin_pixel = rebin_pixel + 1;
              }
              y_out = y_out + 1;
           }
        }
     }
  }

/***   End of Function F2D_CORR_FASTC */
}







!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_lsqfit2d.inc *
!  *                  *
!  ********************
 
!+ FIT2D: Least Squares Fitting FIT2D data passing Common
 
!  Description:
!    Passes extra control variables through common to
!    'F2D_FUNFIT2D'. This include file makes that safer.
!  Keywords:
!    FIT2D.Fitting.Common, Fitting.Common.FIT2D
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    14-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    01-Dec-1996: V0.5 Change 2-D polynomial to Chebyshev form (Hammersley)
!    16-Nov-1996: V0.4 Separated from old "f2d_fit.inc" (Hammersley)
!    27-Apr-1996: V0.3 Add commented out code for 64-bit systems (Hammersley)
!    04-Feb-1996: V0.2 Use to allow "EXCHANGE" to swap pointers to arrays in 
!      subroutines (Hammersley)
!    18-Mar-1989: V0.1 Original (Hammersley)
!  Global Constants:
!    Global Variables:
 
!  Model variables
     Integer :: f2d_num_features ! Number of features in model
     Integer :: f2d_x_order ! Order of 2-D polynomial in X-direction
     Integer :: f2d_y_order ! Order of 2-D polynomial in Y-direction
     Real :: f2d_xmin_poly ! X-coordinate of minimum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Real :: f2d_ymin_poly ! Y-coordinate of minimum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Real :: f2d_xmax_poly ! X-coordinate of maximum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)
     Real :: f2d_ymax_poly ! Y-coordinate of maximum extent of Chebyshev 
!      interval (scaled to -1.0: +1.0)

     Common / F2D_LSQINC / f2d_num_features, f2d_x_order, f2d_y_order, &
       f2d_xmin_poly, f2d_ymin_poly, f2d_xmax_poly, f2d_ymax_poly
 
!  End of f2d_lsqfit2d.inc
!********1*********2*********3*********4*********5*********6*********7**
 
 

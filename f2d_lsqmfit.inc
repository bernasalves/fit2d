!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * expg_f2d_lsqmfit.inc *
!  *                       *
!  *************************
 
!+ FIT2D: Least Squares Fitting MFIT data passing Common
 
!  Description:
!    Passes extra control variables through common to
!    'EXPG_F2D_FUNMFIT'. This include file makes that safer.
!  Keywords:
!    FIT2D.MFIT.Fitting.Common, Fitting.Common.MFIT.FIT2D
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    04-Dec-1996: V0.1 Original, based on 'expg_f2d_lsqmfit.inc'
!    (HAMMERSLEY)
!  Global Constants:
!    Global Variables:
 
!  Model variables
     Integer :: f2d_num_1dfeatures ! Number of features in model
     Integer :: f2d_1dorder ! Order of 1-D polynomial
     Real :: f2d_min_poly ! Coordinate of minimum extent of
!    Chebyshev interval (scaled to -1.0:+1.0)
     Real :: f2d_max_poly ! Coordinate of maximum extent of
!    Chebyshev interval (scaled to -1.0:+1.0)
     Real :: f2d_x_pixel_size ! Size of a pixel in the current data
!    in the X-direction (metres)
     Real :: f2d_y_pixel_size ! Size of a pixel in the current data
!    in the Y-direction (metres)
     Common / EXPG_F2D_LSQMFITINC / f2d_num_1dfeatures, f2d_1dorder, &
       f2d_min_poly, f2d_max_poly, f2d_x_pixel_size, f2d_y_pixel_size
 
!  End of expg_f2d_lsqmfit.inc
!********1*********2*********3*********4*********5*********6*********7**
 
 

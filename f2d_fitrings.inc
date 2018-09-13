!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_fitrings.inc *
!  *                  *
!  ********************
 
!+ FIT2D: Powder RING FIT data passing Common
 
!  Description:
!    It is necessary to pass some variable through common. This
!    include file makes that safer.
!  Keywords:
!    Fit.Common
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Sep-2014: V0.6 Add polar coordinate arrays (Hammersley)
!    09-Jul-1997: V0.5 Add array to store azimuth of data points (Hammersley)
!    13-Jun-1996: V0.4 Add array to store D-spacings (Hammersley)
!    17-Feb-1995: V0.3 Add intensity arrays (for use in weighted fitting) 
!      (Hammersley)
!    27-Jan-1995: V0.2 Change names of variables (Hammersley)
!    26-Jan-1995: V0.1 Original (Hammersley)
!  Global Constants:
     Integer, Parameter :: F2d_max_rcoordinates = 360 ! First dimension
!      size of ring coordinate arrays
     Integer, Parameter :: F2d_max_rings = 15 ! Second dimension size of arrays
!    Global Variables:
!    Global Arrays:
     Integer :: F2D_NUM_RCOORDINATES(F2d_max_rings) ! Number of
!      coordinates for each ring
     Real :: F2D_D_SPACINGS(F2d_max_rings) ! Passes D-spacings to
!      fitting routine
     Real :: F2D_RINTENSITIES (F2d_max_rcoordinates, F2d_max_rings)
!      Integrated intensity of powder ring sections or
!      averaged (3 pixel) intensity of profile at centre of
!      mass position for each coordinate of rings (the intensity is
!      intensity above estimated background)
     Real :: F2D_X_POLAR(F2d_max_rcoordinates, F2d_max_rings)
!      X-coordinates of different powder rings in polar coordinates (pixels)
     Real :: F2D_X_RCOORDINATES(F2d_max_rcoordinates, F2d_max_rings)
!      X-coordinates of different powder rings (metres)
     Real :: F2D_Y_POLAR(F2d_max_rcoordinates, F2d_max_rings)
!      X-coordinates of different powder rings in polar coordinates (angle)
     Real :: F2D_Y_RCOORDINATES(F2d_max_rcoordinates, F2d_max_rings)
!      Y-coordinates of different powder rings (metres)
     Real :: F2D_AZIMUTHS (F2d_max_rcoordinates, F2d_max_rings)
!      Azimuth in radians of each data point
     Real :: F2D_RADIA( F2d_max_rings) ! The radius of the powder
!      rings in metres
!    Common:
     Common / F2D_COMRINGS / F2D_NUM_RCOORDINATES, F2D_X_POLAR, &
       F2D_X_RCOORDINATES, F2D_Y_POLAR, F2D_Y_RCOORDINATES, F2D_RINTENSITIES, &
       F2D_D_SPACINGS, F2D_AZIMUTHS, F2D_RADIA
 
!  End of f2d_fitrings.inc
!********1*********2*********3*********4*********5*********6*********7*********8

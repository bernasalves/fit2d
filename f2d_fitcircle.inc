!********1*********2*********3*********4*********5*********6*********7**
 
!  *********************
!  *                   *
!  * f2d_fitcircle.inc *
!  *                   *
!  *********************
 
!+ FIT2D: CIRCLE FIT data passing Common
 
!  Description:
!    Because "E04FCF" is used to minimise the fit function it is
!    necessary to pass some variable through common. This include file
!    makes that safer.
!  Keywords:
!    Fit.E04FCF.Common
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    17-Aug-1994: V0.1 Original (Hammersley)
!    26-Jan-1995: V0.2 Change variable names (Hammersley)
!  Global Constants:
     Integer, Parameter :: F2d_max_coordinates = 720 ! Dimension size of
!      coordinate arrays
!    Global Variables:
!    Global Arrays:
     Real :: F2D_X_COORDINATES(F2d_max_coordinates) ! X-coordinates of peaks
     Real :: F2D_Y_COORDINATES(F2d_max_coordinates) ! Y-coordinates of peaks
!    Common:
     Common / F2D_COMCIRCLE / F2D_X_COORDINATES, F2D_Y_COORDINATES
 
!  End of f2d_fitcircle.inc
!********1*********2*********3*********4*********5*********6*********7**

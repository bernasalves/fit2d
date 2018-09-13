!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_rmap_cal_angles.f90 *
!  *                         *
!  ***************************
 
!+ F2D_RMAP_CAL_ANGLES: Reciprocal MAP input UB MATRIX
     Subroutine F2D_RMAP_CAL_ANGLES (x_coordinate, y_coordinate, experiment, &
       two_theta, phi, chi, omega, status)
!  Description:
!    Calculate equivelent diffractometer angles from peak position.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    14-Oct-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: x_coordinate ! Entered X-coordinate of click point
     Real, Intent(IN) :: y_coordinate ! Entered Y-coordinate of click point
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
     Real, Intent(INOUT) :: two_theta ! Two theta angle (degrees) of detector
     Real, Intent(INOUT) :: phi ! Phi angle (degrees) of diffractometer
     Real, Intent(INOUT) :: chi ! Chi angle (degrees) of diffractometer
     Real, Intent(INOUT) :: omega ! Omega angle (degrees) of diffractometer
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: d ! Distance from sample to detector on X-component point
     Real :: gamma ! Angle horizontally
     Real :: nu ! Angle vertically
     Real :: xwc ! X-component of vector on detector (metres)
     Real :: ywc ! Y-component of vector on detector (metres)
!  Local Arrays:
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_ANGLES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Distance components from centre
        xwc = (x_coordinate - experiment%x_beam) * experiment%x_pixel_size
        ywc = (y_coordinate - experiment%y_beam) * experiment%y_pixel_size
 
!     Find pixel gamma (vertical) and nu (horizontal)
        d = Sqrt(xwc**2 + experiment%detector_distance**2)
        gamma = Atan2(xwc, experiment%detector_distance)
        nu = Atan2(ywc, d)
 
     End If
 
     End Subroutine F2D_RMAP_CAL_ANGLES
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

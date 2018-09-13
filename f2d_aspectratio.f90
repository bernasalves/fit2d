!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_aspectratio.f90 *
!  *                     *
!  ***********************
 
!+ F2D_ASPECTRATIO - FIT2D: control ASPECT RATIO of images
     Subroutine F2D_ASPECTRATIO (status)
!  Description:
!  Keywords:
!    Aspectratio.Options
!  Method:
!    Simple call to "IO_INPL".
!  Deficiencies:
!  Bugs:
!    None Known
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Jan-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Logical :: correct_aspect ! .True., if images are to be displayed
!    automatically in the correct aspect ratio
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User messages
!    Internal References:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ASPECTRATIO ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire current aspect ratio control option
        Call GS_INQ_IASPECT (correct_aspect, status)
 
!     User choice of aspect ratio
        MESSAGE(1) = 'Enter  "YES"  if  you want image display ' // &
          'with automatic correct aspect'
        MESSAGE(2) = 'ratios i.e. the pixels are square. Enter ' // &
          '"NO" to use all the available'
        MESSAGE(3) = 'display region.  This  may  result in ' // &
          'non-square pixels,  but  may  be'
        MESSAGE(4) = 'preferable for very non-square images.'
        Call IO_INPL (.True., 0, 1, .True., &
          'AUTOMATIC CORRECT ASPECT RATIO IMAGE DISPLAY', 4, MESSAGE, 1, &
          'Enter "YES" on "NO"', correct_aspect, status)
 
!     Set aspect ratio control option
        Call GS_SET_IASPECT (correct_aspect, status)
 
     End If
 
     End Subroutine F2D_ASPECTRATIO
!********1*********2*********3*********4*********5*********6*********7*********8
 

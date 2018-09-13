!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_maskcolour.f90 *
!  *                    *
!  **********************
 
!+ F2D_MASKCOLOUR - FIT2D: set MASK COLOUR
     Subroutine F2D_MASKCOLOUR (status)
!  Description:
!  Keywords:
!    Mask.Colour
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    05-Mar-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: mask_colour ! Colour for drawing masked-off elements
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKCOLOUR ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find out current colour to draw masked off pixels
        Call GS_INQ_MASKCOLOUR (mask_colour, status)
 
!     Input user values for colour for masked-off elements
        Call GS_INP_COLOUR ('COLOUR FOR MASKED OFF ELEMENTS', .True., &
          mask_colour, status)
 
!     Set colour for masked off pixels
        Call GS_SET_MASKCOLOUR (mask_colour, status)
 
     End If
 
     End Subroutine F2D_MASKCOLOUR
!********1*********2*********3*********4*********5*********6*********7*********8
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_postscript.f90 *
!  *                    *
!  **********************
 
!+ F2D_POSTSCRIPT - FIT2D: control POSTSCRIPT output options
     Subroutine F2D_POSTSCRIPT (status)
!  Description:
!  Keywords:
!    PostScript.Options
!  Method:
!  Deficiencies:
!  Bugs:
!    None Known
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
     Logical :: colour ! .True., if PostScript output is to be in colour
     Integer :: unused1 ! Dummy variable for "GS_INQ_POSTSCRIPT"
     Integer :: unused2 ! Dummy variable for "GS_INQ_POSTSCRIPT"
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POSTSCRIPT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User warning of future changes
        Call IO_WRITE ('WARNING: The user input may change for this option', &
          status)
 
!     Inquire current PostScript control options
        Call GS_INQ_POSTSCRIPT (colour, unused1, unused2, status)
 
!     Toggle colour output
        colour = .Not. colour
 
!     Produce user message
        If (colour) Then
           Call IO_WRITE ('INFO: Colour PostScript output will be produced', &
             status)
        Else
           Call IO_WRITE ('INFO: Black and white PostScript ' // &
             'output will be produced', status)
        End If
 
!     Set PostScript options
        Call GS_SET_POSTSCRIPT (colour, unused1, unused2, status)
 
     End If
 
     End Subroutine F2D_POSTSCRIPT
!********1*********2*********3*********4*********5*********6*********7*********8
 

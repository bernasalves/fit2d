!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_resize.f90 *
!  *                *
!  ******************
 
!+ GS_RESIZE - FIT2D: RESIZE window event
     Subroutine GS_RESIZE (status)
!  Description:
!    Reset output areas for graphics
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Mar-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine GS_RESIZE ' // Version)
        Return
     End If

!  Set default regions for graphical drawing areas
        Call GS_DEF_MENUREGION (status)
        Call GS_DEF_MESSAGE (status)
        Call GS_DEF_GUIREGION (status)
        Call GS_DEF_GPP (status)
 
     End Subroutine GS_RESIZE
!********1*********2*********3*********4*********5*********6*********7*********8

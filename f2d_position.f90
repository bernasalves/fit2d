!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_position.f90 *
!  *                  *
!  ********************
 
!+ F2D_POSITION - FIT 2-D graph page POSITION by graphical input
     Subroutine F2D_POSITION (status)
!  Description:
!    1. Inputs two coordinates and calculates new graph page position
!  Keywords:
!    Graph~Page~Position.Graphical~Input, Graphical~Input.Graph~Page~Position
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Jan-1996: V0.3 Input prompt region (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    19-Sep-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Real :: x1 ! First X-page coordinate
     Real :: x2 ! Second X-page coordinate
     Real :: xmaxgpp ! X-coordinate of maximum graph page position
     Real :: xmingpp ! X-coordinate of minimum graph page position
     Real :: xmax_prompt ! X-maximum of prompt output region
     Real :: xmin_prompt ! X-minimum of prompt output region
     Real :: ymax_prompt ! Y-maximum of prompt output region
     Real :: ymin_prompt ! Y-minimum of prompt output region
     Real :: y1 ! First Y-page coordinate
     Real :: y2 ! Second Y-page coordinate
     Real :: ymaxgpp ! Y-coordinate of maximum graph page position
     Real :: ymingpp ! Y-coordinate of minimum graph page position
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POSITION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Find out GUI prompt area
        Call GS_INQ_GUIREGION (xmin_prompt, ymin_prompt, xmax_prompt, &
          ymax_prompt, status)
 
        Call GS_INP_PC ( xmin_prompt, ymin_prompt, xmax_prompt, ymax_prompt, &
          'CLICK ON A CORNER OF REQUIRED DISPLAY REGION', 1, &
          'CLICK ON A CORNER OF DISPLAY REGION', x1, y1, status)
 
        Call GS_INP_PC ( xmin_prompt, ymin_prompt, xmax_prompt, ymax_prompt, &
          'CLICK ON OPPOSITE CORNER OF REQUIRED DISPLAY REGION', 1, &
          'CLICK ON OPPOSITE CORNER OF DISPLAY REGION', x2, y2, status)
 
!     Find lower and upper limits of required GPP
        xmingpp = Min(x1, x2)
        ymingpp = Min(y1, y2)
        xmaxgpp = Max(x1, x2)
        ymaxgpp = Max(y1, y2)
 
!     Set new graph page position
        Call GS_SET_GPP (xmingpp, ymingpp, xmaxgpp, ymaxgpp, status)
 
     End If
 
     End Subroutine F2D_POSITION
!********1*********2*********3*********4*********5*********6*********7*********8

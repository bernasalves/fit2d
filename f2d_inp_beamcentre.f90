!********1*********2*********3*********4*********5*********6*********7**
 
!  **************************
!  *                        *
!  * f2d_inp_beamcentre.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_BEAMCENTRE -  INPut X/Y pixel coordinate of BEAM CENTRE
!    on detector
     Subroutine F2D_INP_BEAMCENTRE (gui, x_beam, y_beam, status)
!  Description:
!    User input of X/Y pixel coordinate of beam centre on detector.
!  Keywords:
!    Beam~Centre, Centre.Beam
!  Method:
!    Uses "IO_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    02-Feb-1996: V0.2 Graphical user interface option (Hammersley)
!    24-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
!  Import/Export:
     Real, Intent(INOUT) :: x_beam ! X-pixel coordinate of beam on detector
     Real, Intent(INOUT) :: y_beam ! Y-pixel coordinate of beam on detector
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_BEAMCENTRE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Beam centre
        If (gui) Then
 
           Call GS_INPR (.False., 0.0, 0.0, .True., &
             'X-PIXEL COORDINATE OF BEAM CENTRE', 1, &
             'Enter X-pixel coordinate of beam centre', 1, &
             'Must be valid real number', x_beam, status)
           Call GS_INPR (.False., 0.0, 0.0, .True., &
             'Y-PIXEL COORDINATE OF BEAM CENTRE', 1, &
             'Enter Y-pixel coordinate of beam centre', 1, &
             'Must be valid real number', y_beam, status)
 
        Else
 
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'X-PIXEL COORDINATE OF BEAM CENTRE', 1, &
             'Enter X-pixel coordinate of beam centre', 1, &
             'Must be valid real number', x_beam, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'Y-PIXEL COORDINATE OF BEAM CENTRE', 1, &
             'Enter Y-pixel coordinate of beam centre', 1, &
             'Must be valid real number', y_beam, status)
 
        End If
 
     End If
 
!  End of Subroutine F2D_INP_BEAMCENTRE
     End
!********1*********2*********3*********4*********5*********6*********7**
 
 
 

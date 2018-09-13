!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_draw_cake_cmd.f90 *
!  *                       *
!  *************************
 
!+ F2D_DRAW_CAKE_CMD: DRAW CAKE CoMmanD
     Subroutine F2D_DRAW_CAKE_CMD (EXPERIMENT, status)
!  Description:
!    Draws outline of cake based on values found in the internal store
!  Keywords:
!    Cake.Draw.Command, Draw.Cake.Command
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    14-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    26-Jul-2004: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! GS constants
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: retstat ! Return status variable
     Real :: start_azimuth ! Angle of azimuth of start of region in radians
     Real :: end_azimuth ! Angle of azimuth of end of region in radians
     Real :: inner_limit ! Inner radius in metres
     Real :: outer_limit ! Outer radius in metres
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_CAKE_CMD ' // Version)
        Return
     Else
 
!     Get current values from internal memory
        Call IO_INQ_RKEYVALUE ('CAKE_START_AZIMUTH', start_azimuth, retstat, &
          status)
        Call IO_INQ_RKEYVALUE ('CAKE_END_AZIMUTH', end_azimuth, retstat, &
          status)
        Call IO_INQ_RKEYVALUE ('CAKE_INNER_LIMIT', inner_limit, retstat, &
          status)
        Call IO_INQ_RKEYVALUE ('CAKE_OUTER_LIMIT', outer_limit, retstat, &
          status)
 
!     Draw current cake
        Call F2D_DRAW_CAKE (.False., EXPERIMENT, &
          start_azimuth, end_azimuth, inner_limit, outer_limit, status)
 
     End If
 
     End Subroutine F2D_DRAW_CAKE_CMD
!********1*********2*********3*********4*********5*********6*********7**

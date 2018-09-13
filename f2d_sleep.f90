!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_sleep.f90 *
!  *               *
!  *****************
 
!+ F2D_SLEEP - Fit 2-D: SLEEP for an input number of seconds
     Subroutine F2D_SLEEP (status)
!  Description:
!    Prompts user for number of seconds to sleep, and then pauses. (Useful for 
!    demonstration macros.)
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    29-Aug-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real, Save :: seconds = 1.0 ! Number of seconds to sleep
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SLEEP ' // Version)
 
     Else
 
!     Input value to add
        Call IO_INPR (.True., 0.0, 1.0e3, .True., 'LENGTH OF PAUSE (seconds)', &
          1, 'Enter number of seconds to pause', 1, &
          'Enter a value within the given range', seconds, status)
 
        If (seconds .Gt. 0.0) Then
 
!        Go to sleep for required time
           Call IO_SLEEP (seconds, status)
 
        End If
 
     End If
 
     End Subroutine F2D_SLEEP
!********1*********2*********3*********4*********5*********6*********7*********8
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_stop_watch.f90 *
!  *                    *
!  **********************
 
!+ F2D_STOP_WATCH - FIT 2-D STOP WATCH
     Subroutine F2D_STOP_WATCH (status)
!  Description:
!    Interactive stop watch
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Nov-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: count1 ! System counts at start of stop watch
     Integer :: count2 ! System counts at end of stop watch
     Integer :: count_max ! Maximum system count value
     Integer :: count_rate ! System counts per second
     Character(Len = 80) :: message ! User messages
     Integer :: minutes ! Number of minutes
     Real :: seconds ! Number of seconds
!  Local Arrays:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_STOP_WATCH'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_STOP_WATCH ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8

!  Output prompt
     Call IO_WRITENA ('PRESS RETURN TO START STOP WATCH:', status)
     Read (*, *)

!  Get start time
     Call SYSTEM_CLOCK (count1, count_rate, count_max)

     Call IO_WRITENA ('PRESS RETURN TO STOP STOP WATCH:', status)
     Read (*, *)

!  Get stop time
     Call SYSTEM_CLOCK (count2, count_rate, count_max)

!  Time difference 
     seconds = Real(count2 - count1) / Real(count_rate)
     minutes = Int(seconds) / 60
     seconds = seconds - minutes * 60

     Write (message, &
       '(''Elapse time = '', i4, '' Minutes '', f6.3, '' Seconds'')') &
       minutes, seconds
     Call IO_WRITE (message, status)

     End Subroutine F2D_STOP_WATCH
!********1*********2*********3*********4*********5*********6*********7*********8

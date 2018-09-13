!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_scan.f90 *
!  *              *
!  ****************
 
!+ F2D_SCAN - Fit 2-D: SCAN
     Subroutine F2D_SCAN (output_graphics, status)
!  Description:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Sep-1997: V0.2 Correct spelling errors (Hammersley)
!    23-Jun-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
     Include 'f2d_comm.inc'
!  Import:
     Logical, Intent(IN) :: output_graphics ! .True., if in graphics mode
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: system ! Name of system
     Character(Len = 80) :: user_name ! Name of user
     Integer :: format
     Integer :: ok ! Return status
!      0 = O.K.
!      1 = Environment variable not defined
!      2 = Bad value
     Integer :: user_id ! Identifier number of user
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(24) ! User messages
!  External Functions:
     Character(Len = 80), External :: Io_nodename
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_SCAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SCAN ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before F2D_SCANF'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Input condition
        Call F2D_SCANF (7, 5, ok, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After F2D_SCANF'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (ok .Eq. 0) Then
 
           Return
 
        Else
 
!        Environment variable not defined
           MESSAGE(1) = 'FIT2D is protected by a "software ' // &
             'key",  which is presently not defined.'
           MESSAGE(2) = 'If you  have registered as  a  FIT2D ' // &
             'user and signed the  "Conditions of'
           MESSAGE(3) = 'Use" papers, you should have been ' // &
             'issued with a "key". This "key" should'
           MESSAGE(4) = 'be defined with the environment ' // &
             'variable "FIT2D_KEY" (the quotes are not'
           MESSAGE(5) = 'part of the environment variable ' // &
             'name). The "key" is a 30 character long'
           MESSAGE(6) = 'combination of letters and numbers ' // &
             '(all in upper case). '
           MESSAGE(7) = ' '
           MESSAGE(8) = 'With the t-shell (or c-shell) you can ' // &
             'define the "key" using the'
           MESSAGE(9) = '"setenv" command. This is best put in ' // &
             'your ".cshrc" file. e.g. if the'
           MESSAGE(10) = 'key value is "VX2JVHPK7ZAFSQ2RBH3E7Q' // &
             'HF6K2" (not including'
           MESSAGE(11) = ' quote marks), you should include the line'
           MESSAGE(12) = ' '
           MESSAGE(13) = 'setenv FIT2D_KEY VX2JVHPK7ZAFSQ2RBH3E7QHF6K2'
           MESSAGE(14) = ' '
           MESSAGE(15) = 'If you are not registered you can ' // &
             'send an e-mail to:'
           MESSAGE(16) = ' '
           MESSAGE(17) = 'hammersley@esrf.eu'
           MESSAGE(18) = ' '
           MESSAGE(19) = 'for a registration form.'
           MESSAGE(20) = ' '
           MESSAGE(21) = 'P.S. Please, please, please, do NOT ' // &
             'try to tamper with the security.'
           MESSAGE(22) = 'It may be relatively simple to ' // &
             'by-pass this section and enter the'
           MESSAGE(23) = 'program, BUT then much more subtle ' // &
             'errors will be introduced in the'
           MESSAGE(24) = 'processing.'
 
           If (output_graphics) Then
 
!           Find out window format
              Call GS_INQ_WINDOWFORMAT (format, status)
 
!           Call GS_SET_MESSAGESTYLE (Gs_red,
!           :             Gs_blue, Gs_black,
!           :             Gs_font_standard, Gs_white,
!           :             .False.,
!           :             0.02, 1.0, 0.0, 1.2,
!           :             status)
 
              Call GS_MESSAGE (24, 24, MESSAGE, status)
 
!           Close graphics system
              Call GS_CLOSE_GRAPHICS (status)
 
           Else
 
              Call IO_TEXT (24, MESSAGE, status)
 
           End If
 
!        Find out name of system
           system = Io_nodename (status)
 
!        User
           Call IO_LOGINNAME (user_name, user_id, status)
 
!        Output necessary information to the user
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('INFO: "The software key" is ' // &
             'produced from the following information', status)
           Call IO_WRITE ('      which you may want to note:', status)
           Call IO_WRITE (' ', status)
           Write (MESSAGE(1), '(''      System name = '', a)') Trim(system)
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''      User name = '', a)') Trim(user_name)
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''      User ID = '', i10)') user_id
           Call IO_WRITE (MESSAGE(1), status)
           Call IO_WRITE (' ', status)
 
           If (ok .Eq. 1) Then
              Call IO_WRITE ('ERROR: FIT2D exiting ' // &
                'because FIT2D_KEY is not defined.', status)
           Else
              Call IO_WRITE ('ERROR: FIT2D exiting ' // &
                'because the "software key" is not correct.', status)
           End If
 
           Stop
 
        End If
 
     End If
 
     End Subroutine F2D_SCAN
!********1*********2*********3*********4*********5*********6*********7*********8
 

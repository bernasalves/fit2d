!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_open_log.f90 *
!  *                  *
!  ********************
 
!+ F2D_OPEN_LOG: FIT2D OPEN LOG file
     Subroutine F2D_OPEN_LOG (status)
!  Description:
!    Choose file to open to contain log of session.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    22-Jan-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 256), Save :: log_file = 'fit2d.log' ! File to contain log
     Integer :: retstat ! Return status for file:
!      0 = Good status
!      1 = Cancel was chosen (no file)
!      2 = Bad status (no file)
     Logical :: file_ok ! .True., if the file is to be used
!  Local Arrays:
     Character(Len = 80) :: INFO(3) ! User info
     Character(Len = 80) :: MESSAGE(4) ! User messages
     Character(Len = 80) :: PROMPT(2) ! User prompts
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OPEN_LOG ' // Version)
        Return
     Else
 
!     Name of log file
        PROMPT(1) = 'SAVE LOG IN: ' // log_file
        MESSAGE(1) = log_file
        MESSAGE(2) = 'is the default file for saving the log.'
        MESSAGE(3) = 'If this file is appropriate enter "YES". If you want '
        MESSAGE(4) = 'to use a different file, enter "NO".'
        Call GS_INPL (.True., 0, 1, .True., PROMPT(1), 4, MESSAGE, 1, &
          'Enter "YES" or "NO"', file_ok, status)
 
        If (.Not. file_ok) Then
 
!        Use file selection tool to obtain file name from user
           PROMPT(1) = 'SELECT OUTPUT FILE FOR'
           PROMPT(2) = 'SAVING THE LOG RECORD'
           INFO(1) = 'Use the file selection tool to select a directory'
           INFO(2) = 'and an output file to contain the log record.'
           INFO(3) = 'Normally such a file ends with the extension ".log".'
           Call GS_FILESELECTION (2, 2, PROMPT, 3, 3, INFO, 3, .False., &
             retstat, log_file, status)
 
!        Check return status
           If (retstat .Ne. 0) Then
              Return
           End If
 
        End If
 
!     Open log output file
        Call IO_OPEN_LOGFILE (.False., log_file, status)
 
        If (status .Ne. St_goodvalue) Then
 
           Call ST_DEF_SYSTEM (status)
 
           Call GS_FWARNING (1, 1, 'PROBLEM OPENING FILE FOR THE LOG', status)
 
        End If
 
     End If
 
     End Subroutine F2D_OPEN_LOG
!********1*********2*********3*********4*********5*********6*********7*********8
 

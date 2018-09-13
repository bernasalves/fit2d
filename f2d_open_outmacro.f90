!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_open_outmacro.f90 *
!  *                       *
!  *************************
 
!+ F2D_OPEN_OUTMACRO: FIT2D OPEN OUTput macro file
     Subroutine F2D_OPEN_OUTMACRO (outmacro_file, status)
!  Description:
!    Choose file to open to contain macro.
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
     Character(Len = *), Intent(INOUT) :: outmacro_file ! Name of output macro 
!      file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
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
        Call ST_SAVE ('Subroutine F2D_OPEN_OUTMACRO ' // Version)
        Return
     Else
 
!     Name of output file
        PROMPT(1) = 'STORE MACRO IN: ' // outmacro_file
        MESSAGE(1) = outmacro_file
        MESSAGE(2) = 'is the default file for storing the macro.'
        MESSAGE(3) = 'If this file is appropriate enter "YES". If you want '
        MESSAGE(4) = 'to use a different file, enter "NO".'
        Call GS_INPL (.True., 0, 1, .True., PROMPT(1), 4, MESSAGE, 1, &
          'Enter "YES" or "NO"', file_ok, status)
 
        If (.Not. file_ok) Then
 
!        Use file selection tool to obtain file name from user
           PROMPT(1) = 'SELECT OUTPUT FILE FOR'
           PROMPT(2) = 'STORING THE MACRO'
           INFO(1) = 'Use the file selection tool to select a directory'
           INFO(2) = 'and an output file to contain the macro.'
           INFO(3) = 'Normally such a file ends with the extension ".mac".'
           Call GS_FILESELECTION (2, 2, PROMPT, 3, 3, INFO, 3, .False., &
             retstat, outmacro_file, status)
 
!        Check return status
           If (retstat .Ne. 0) Then
              Return
           End If
 
        End If
 
!     Open macro output file
        Call IO_OPEN_OUTMACRO (.True., .False., outmacro_file, status)
 
        If (status .Ne. St_goodvalue) Then
 
!        Reset status
           Call ST_DEF_SYSTEM (status)
 
           Call GS_FWARNING (1, 1, 'PROBLEM OPENING FILE FOR THE MACRO', status)
 
        Else
 
        End If
 
     End If
 
     End Subroutine F2D_OPEN_OUTMACRO
!********1*********2*********3*********4*********5*********6*********7*********8
 

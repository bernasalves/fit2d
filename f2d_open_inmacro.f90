!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_open_inmacro.f90 *
!  *                      *
!  ************************
 
!+ F2D_OPEN_INMACRO: FIT2D OPEN INput macro file formats
     Subroutine F2D_OPEN_INMACRO (inmacro_file, status)
!  Description:
!    Choose file to open to run macro.
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
     Character(Len = *), Intent(INOUT) :: inmacro_file ! Input macro file
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
!  Local Arrays:
     Character(Len = 80) :: INFO(3) ! User info
     Character(Len = 80) :: PROMPT(2) ! User prompts
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OPEN_INMACRO ' // Version)
        Return
     Else
 
!     Use file selection tool to obtain file name from user
        PROMPT(1) = 'SELECT MACRO FILE '
        PROMPT(2) = 'TO RUN'
        INFO(1) = 'Use the file selection tool to select a directory'
        INFO(2) = 'and a macro file to run.'
        INFO(3) = 'Normally macro files end with the extension ".mac".'
        Call GS_FILESELECTION (2, 2, PROMPT, 3, 3, INFO, 1, .False., retstat, &
          inmacro_file, status)
 
!     Check return status
        If (retstat .Ne. 0) Then
           Return
        End If
 
!     Open macro input file
        Call IO_OPEN_INMACRO (.False., inmacro_file, status)
 
        If (status .Ne. St_goodvalue) Then
 
           Call GS_FWARNING (1, 1, 'PROBLEM OPENING MACRO FILE', status)
 
           Call ST_DEF_SYSTEM (status)
 
        End If
 
     End If
 
     End Subroutine F2D_OPEN_INMACRO
!********1*********2*********3*********4*********5*********6*********7*********8
 

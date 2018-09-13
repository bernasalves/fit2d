!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_filesequence.f90 *
!  *                      *
!  ************************
 
!+ F2D_FILESEQUENCE: deduce FILE SEQUENCE
     Subroutine F2D_FILESEQUENCE (status)
!  Description:
!    Inputs name of start and end file and deduces file sequence components
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Aug-1998: V0.3 Replace "SYMBOL" calls with "VARIABLE" calls (Hammersley)
!    11-Aug-1998: V0.2 Changes to "IO_SET_SYMBOL" (Hammersley)
!    08-Oct-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 256), Save :: end_file = 'fit2d_001.f2d'
!      Name of last file in sequence
     Character(Len = 256) :: extension ! File name extension
     Character(Len = 256) :: postfix ! Fixed end of file names
     Character(Len = 256) :: prefix ! Fixed start of file names
     Character(Len = 256), Save :: start_file = 'fit2d_999.f2d'
!      Name of first file in sequence
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: end_value ! Value at end of sequence
     Integer :: increment ! Step value in sequence
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer :: num_characters ! Number of characters in numerical part
     Integer :: retstat ! Return status 0 = good
     Integer :: start_value ! Value at start of sequence
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Logical :: variable_characters ! .True., if the number of characters
!      in the numerical part of the names changes
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FILESEQUENCE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Name of first file
        Call IO_INPC (.True., 'ENTER NAME OF STARTING FILE', 1, &
          'Enter file name', 1, 'Enter text', 1, start_file, status)
 
!     Name of last file
        Call IO_INPC (.True., 'ENTER NAME OF END FILE', 1, 'Enter file name', &
          1, 'Enter text', 1, end_file, status)
 
!     Deduce components of file sequence
        Call IO_FILESEQUENCE (start_file, end_file, retstat, start_value, &
          end_value, increment, prefix, variable_characters, num_characters, &
          postfix, extension, status)
 
        If (retstat .Ne. 0) Then
 
           Call IO_WRITE ('ERROR: Could not deduce required file sequence', &
             status)
        Else
 
!        Define standard internal variables
           Call IO_SET_VARIABLE ('##PREFIX', 's', int_value, log_value, &
             real_value, Len_trim(prefix), prefix, retstat, status)
           Call IO_SET_VARIABLE ('##START', 'i', start_value, log_value, &
             real_value, len_string, string_value, retstat, status)
           Call IO_SET_VARIABLE ('##END', 'i', end_value, log_value, &
             real_value, len_string, string_value, retstat, status)
           Call IO_SET_VARIABLE ('##STEP', 'i', increment, log_value, &
             real_value, len_string, string_value, retstat, status)
           Call IO_SET_VARIABLE ('##VARIABLE', 'l', int_value, &
             variable_characters, real_value, len_string, string_value, &
             retstat, status)
           Call IO_SET_VARIABLE ('##NUM_CHARS', 'i', num_characters, &
             log_value, real_value, len_string, string_value, retstat, status)
           Call IO_SET_VARIABLE ('##POSTFIX', 's', int_value, log_value, &
             real_value, Len_trim(postfix), postfix, retstat, status)
           Call IO_SET_VARIABLE ('##EXTENSION', 's', int_value, log_value, &
             real_value, Len_trim(extension), extension, retstat, status)
 
        End If
 
     End If
 
     End Subroutine F2D_FILESEQUENCE
!********1*********2*********3*********4*********5*********6*********7*********8

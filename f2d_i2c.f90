!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************
!  *             *
!  * f2d_i2c.f90 *
!  *             *
!  ***************
 
!+ F2D_I2C: Integer 2 Character conversion
     Subroutine F2D_I2C (status)
!  Description:
!    Inputs an integer and converts it to a string with or without preceding 
!    blanks, and defines a symbol.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Aug-1998: V0.4 Replace "SYMBOL" calls with "VARIABLE" calls (Hammersley)
!    11-Aug-1998: V0.3 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Oct-1996: V0.2 Output blank if fixed length and zero characters
!      (Hammersley)
!    09-Oct-1996: V0.1 Original (Hammersley)
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
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 20) :: cvalue ! Integer in character form
     Character(Len = 32), Save :: variable = '#CVALUE' ! Name of variable to
!      define
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer, Save :: num_char = 3 ! Number of characters in output
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room in symbol store
!      2 = No more room for character strings
     Integer, Save :: value = 1 ! Input integer value
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Logical, Save :: variable_length = .False. ! .True., if the output can
!      be of variable length
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
!  External Functions:
     Character(Len = 20), External :: Io_itoc ! Convert an integer to a string
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_I2C ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Enter integer value
        Call IO_INPI (.False., 0, 0, .True., 'ENTER INTEGER', 1, &
          'Enter integer value to convert', 1, 'Enter a valid integer', value, &
          status)
 
!     Enter if character output should be of variable length
        Call IO_INPL (.True., 0, 1, .True., 'VARIABLE LENGTH OUTPUT', 1, &
          'Enter whether the output should be of variable length', 1, &
          'Enter a valid integer', variable_length, status)
 
        Call IO_INPI (.False., 0, 20, .True., &
          'ENTER NUMBER OF OUTPUT CHARACTERS', 1, &
          'Enter number of characters for output', 1, 'Enter a valid integer', &
          num_char, status)
 
!     Input variable from user
        Call IO_INPTOKEN (.True., 'ENTER VARIABLE NAME', 1, &
          'Enter name of variable to define', 1, 'Enter string', 101, &
          variable, status)
 
!     Convert integer to a token
        cvalue = Io_itoc (value)
 
        If (.Not. variable_length) Then
 
           Do While (Len_trim(cvalue) .Lt. num_char)
              cvalue = '0' // cvalue
           End Do
 
           If (num_char .Eq. 0) Then
              cvalue = ' '
           End If
 
        End If
 
!     Set symbol
        Call IO_SET_VARIABLE (variable, 's', int_value, log_value, real_value, &
          Len_trim(cvalue), cvalue, retstat, status)
 
     End If
 
     End Subroutine F2D_I2C
!********1*********2*********3*********4*********5*********6*********7*********8

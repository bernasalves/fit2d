!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_listvariables.f90 *
!  *                       *
!  *************************
 
!+ F2D_LISTVARIABLES - FIT2D: LIST current VARIABLES and values
     Subroutine F2D_LISTVARIABLES (status)
!  Description:
!    Output table of current program variables and values
!  Keywords:
!    List.Variables
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    09-Oct-1998: V0.6 Add output of length of defined strings (Hammersley)
!    21-Aug-1998: V0.5 Use "IO_INQ_VARIABLE" instead of "IO_INQ_SYMBOL" 
!      (Hammersley)
!    17-Aug-1998: V0.4 Changes to "IO_INQ_SYMBOL" (Hammersley)
!    11-Aug-1998: V0.3 Output data type of variables (Hammersley)
!    08-Oct-1996: V0.2 Cope with blank variables (Hammersley)
!    06-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! status return variable.
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Character(Len = 6) :: clen_string ! Character representation of the
!      length of a string
     Character(Len = 163) :: message ! Used to output line with variable and
!      value
     Character(Len = 32) :: variable ! Variable name
     Character(Len = 1) :: type ! Data type of variable
!      i = Integer value
!      l = Logical (boolean) value
!      r = Real (floating point) value
!      s = Character string
!      u = Unknown
     Character(Len = 132) :: string_value ! Variable string value
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer :: num_variables ! Number of defined variables in database
     Integer :: retstat ! Return status variable
     Integer :: var ! Loop variable for variables
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
!  External Functions:
     Character(Len = 20), External :: Io_itoc ! Convert integer to a string
     Character(Len = 20), External :: Io_ltoc ! Convert logical to a string
     Character(Len = 20), External :: Io_rtoc ! Convert real to a string
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LISTVARIABLES ' // Version)
        Return
     End If
 
!  Find out number of defined variables
     Call IO_INQ_VARNUM (0, num_variables, retstat, variable, type, int_value, &
       log_value, real_value, len_string, string_value, status)
 
     If (num_variables .Le. 0) Then
 
!     No variables are defined
        Call IO_WRITE ('INFO: No program variables are ' // &
          'presently defined', status)
 
     Else
 
        Call IO_WRITE (' ', status)
        Call IO_WRITE ( 'INFO: Variable Names / Data Types / Variable Values', &
          status)
 
!     List variables
        Do var = 1, num_variables
 
!        Find name and value of variable
           Call IO_INQ_VARNUM (var, num_variables, retstat, variable, type, &
             int_value, log_value, real_value, len_string, string_value, &
             status)
 
           If (type .Eq. 'i') Then
              string_value = Io_itoc(int_value)
           Else If (type .Eq. 'l') Then
              string_value = Io_ltoc(log_value)
           Else If (type .Eq. 'r') Then
              string_value = Io_rtoc(real_value)
           End If
 
!        Create output line
           If (type .Eq. 's') Then
 
              If (len_string .Eq. 0) Then
                 message = '      ' // Trim(variable) // ' s(0) = '
              Else
                 clen_string = Io_itoc (len_string)
                 message = '      ' // Trim(variable) // ' s(' // &
                   Trim(clen_string) // ') = ' // string_value(1: len_string)
              End If
 
           Else
 
              message = '      ' // Trim(variable) // ' ' // type &
                // ' = ' // Trim(string_value)
 
           End If
 
!        Output line
           Call IO_WRITE (message, status)
 
        End Do
 
     End If
 
     End Subroutine F2D_LISTVARIABLES
!********1*********2*********3*********4*********5*********6*********7**
 
 

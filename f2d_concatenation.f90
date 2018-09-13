!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_concatenation.f90 *
!  *                       *
!  *************************
 
!+ F2D_CONCATENATION: CONCATENATION of two strings
     Subroutine F2D_CONCATENATION (status)
!  Description:
!    Inputs two strings, concatenates them,  and outputs them to a symbol.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    08-Oct-1998: V0.4 Use "IO_INPS" instead of "IO_INPC" to allow significant 
!    blanks at the start and end of strings to be concatenated (Hammersley)
!    19-Aug-1998: V0.3 Use "IO_SET_VARIABLE" instead of "IO_SET_SYMBOL" 
!      (Hammersley)
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
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 256) :: concat ! Concatenation
     Character(Len = 256), Save :: string1 = 'fit2d.' ! First string
     Character(Len = 256), Save :: string2 = 'f2d' ! Second string
     Character(Len = 32), Save :: variable = '#FILE_OUT' ! Name of variable
!      to define
     Integer :: int_value ! Any integer value to store
     Integer :: len_string1 ! Number of characters in first string
     Integer :: len_string2 ! Number of characters in second string
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room in symbol store
!      2 = No more room for character strings
     Logical :: log_value ! Any logical value to store
     Real :: real_value ! Any floating point real value to store
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CONCATENATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Enter first string
        Call IO_INPS (.True., 'ENTER FIRST STRING', 1, 'Enter characters', 1, &
          'Enter text', 1, len_string1, string1, status)
 
!     Enter second string
        Call IO_INPS (.True., 'ENTER SECOND STRING', 1, 'Enter characters', 1, &
          'Enter text', 1, len_string2, string2, status)
 
!     Concatenate strings
        If (len_string1 .Gt. 0 .And. len_string2 .Gt. 0) Then
           concat = string1(1: len_string1) // string2(1: len_string2)
        Else If (len_string1 .Gt. 0) Then
           concat = string1(1: len_string1)
        Else If (len_string2 .Gt. 0) Then
           concat = string2(1: len_string2)
        Else
           concat = ' '
        End If
 
!     Output result
        Call IO_WRITE ('INFO: Concatenation:', status)
        Call IO_WRITE ('      ' // concat, status)
 
!     Input variable name from user
        Call IO_INPTOKEN (.True., 'ENTER NAME OF VARIABLE', 1, &
          'Enter name of variable to define', 1, 'Enter string', 101, &
          variable, status)
 
!     Define variable
        Call IO_SET_VARIABLE (variable, 's', int_value, log_value, real_value, &
          len_string1 + len_string2, concat, retstat, status)
 
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Problem defining variable: ' // variable, &
             status)
        End If
 
     End If
 
     End Subroutine F2D_CONCATENATION
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 

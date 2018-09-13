!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_undefine.f90 *
!  *                  *
!  ********************
 
!+ F2D_UNDEFINE - FIT2D: UN-DEFINE program variable
     Subroutine F2D_UNDEFINE (status)
!  Description:
!    Input variable name from user and un-set it.
!  Keywords:
!    Un-define.Variable, Variable.Un-define
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Aug-1998: V0.3 Use "IO_UNSET_VARIABLE" instead of "IO_UNSET_SYMBOL" 
!      (Hammersley)
!    17-Aug-1998: V0.2 Changes to "IO_INQ_SYMBOL" (Hammersley)
!    25-Oct-1994: V0.1 Original, based on "F2D_SYMBOL" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 80) :: variable ! Variable name
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Character(Len = 1) :: type ! Data type of variable:
!      i = Integer value
!      l = Logical (boolean) value
!      r = Real (floating point) value
!      s = Character string
!      u = Unknown
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer, Save :: num_var = 1 ! Variable for variable to present as a 
!      default
     Integer :: num_variables ! Number of defined variables in database
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room in symbol store
!      2 = No more room for character strings
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
!    External References:
     Integer, External :: Io_var_number ! Number of variable within translation
!      tables
!    Local data:
!    Saved variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_UNDEFINE ' // Version)
        Return
     End If
 
!  Find out number of defined variables
     Call IO_INQ_VARNUM (0, num_variables, retstat, variable, type, int_value, &
       log_value, real_value, len_string, string_value, status)
 
!  If no variables have been defined return with warning message present next 
!  defined variable as a default
     If (num_variables .Le. 0) Then
        Call IO_WRITE ('WARNING: No variables are presently ' // &
          'defined so you cannot "UN-DEFINE" any', status)
     Else
 
        If (num_var .Gt. num_variables) Then
           num_var = 1
        End If
 
!     Recover name of variable as a default value
        Call IO_INQ_VARNUM (num_var, num_variables, retstat, variable, type, &
          int_value, log_value, real_value, len_string, string_value, status)
  
!     Input variable from user
        Call IO_INPTOKEN (.True., 'ENTER VARIABLE NAME', 1, &
          'Enter name of variable to un-define', 1, 'Enter string', 101, &
          variable, status)
 
!     Find out if the variable is known
        num_var = Io_var_number (variable, status)
 
        If (num_var .Gt. 0) Then
 
!        Un-define variable
           Call IO_UNSET_VARIABLE (variable, retstat, status)
 
        Else
 
!        Variable unknown, issue warning message
           Call IO_WRITE ('WARNING: Requested variable is not defined', status)
 
        End If
 
     End If
 
     End Subroutine F2D_UNDEFINE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

!********1*********2*********3*********4*********5*********6*********7**
 
!  ************************
!  *                      *
!  * f2d_inp_datatype.f90 *
!  *                      *
!  ************************
 
!+ F2D_INP_DATATYPE - FIT2D: input variable DATA TYPE
     Subroutine F2D_INP_DATATYPE (default, data_type, status)
!  Description:
!    Input data type of a program variable
!  Keywords:
!    Data~Type.Variable.Input, Variable.Data~Type.Input,
!    Input.Variable.Data~Type
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    21-Nov-1998: V0.1 Original, based on "F2D_VARIABLE" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: default ! .True., if a default value is to be given
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: data_type ! Data type for variable:
!      i = Integer value
!      l = Logical (boolean) value
!      r = Real (floating point) value
!      s = Character string
!      u = Unknown
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
     Integer, Parameter :: Max_types = 8 ! Dimension of "TYPES" array
!  Local Variables:
     Character(Len = 80) :: input ! User input
!  Local Arrays:
     Character(Len = 80), Save :: MENUTXT(Max_types) ! User menu information
     Character(Len = 30), Save :: TYPES(Max_types) ! Available data types
!      for variables
!  Local Data:
     Data MENUTXT / 'BOOLEAN VALUE: (Same as "LOGICAL (BOOLEAN) VALUE")', &
       'CHARACTER STRING VALUE: Alpha-numeric character string value', &
       'FLOATING POINT (REAL) VALUE: Floating point value', &
       'INTEGER VALUE: Store as an integer value', &
       'LOGICAL (BOOLEAN) VALUE: Logical "true" or "false" value', &
       'REAL VALUE: (Same as "FLOATING POINT (REAL) VALUE")', &
       'STRING VALUE: (Same as "CHARACTER STRING VALUE")', 'INVALID (STRING)' &
       /
     Data TYPES / 'BOOLEAN VALUE', 'CHARACTER STRING VALUE', &
       'FLOATING POINT (REAL) VALUE', 'INTEGER VALUE', &
       'LOGICAL (BOOLEAN) VALUE', 'REAL VALUE', 'STRING VALUE', &
       'INVALID (STRING)' /
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7--
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_DATATYPE ' // Version)
        Return
     End If
 
!  Set default value if required
     If (default) Then
 
        If (data_type .Eq. 'i') Then
           input = 'INTEGER VALUE'
        Else If (data_type .Eq. 'l') Then
           input = 'LOGICAL (BOOLEAN) VALUE'
        Else If (data_type .Eq. 'r') Then
           input = 'FLOATING POINT (REAL) VALUE'
        Else If (data_type .Eq. 's') Then
           input = 'CHARACTER STRING VALUE'
        Else If (data_type .Eq. 'u') Then
           input = 'CHARACTER STRING VALUE'
        End If
 
     End If
 
!  Data type of variable
     Call IO_MENU (default, 'ENTER DATA TYPE OF VARIABLE', 7, MENUTXT, 1, &
       'Choice one of the available data types', Max_types, 7, TYPES, input, &
       status)
 
!  Set required data type
     If (input .Eq. 'BOOLEAN VALUE') Then
        data_type = 'l'
     Else If (input .Eq. 'CHARACTER STRING VALUE') Then
        data_type = 's'
     Else If (input .Eq. 'FLOATING POINT (REAL) VALUE') Then
        data_type = 'r'
     Else If (input .Eq. 'INTEGER VALUE') Then
        data_type = 'i'
     Else If (input .Eq. 'LOGICAL (BOOLEAN) VALUE') Then
        data_type = 'l'
     Else If (input .Eq. 'REAL VALUE') Then
        data_type = 'r'
     Else If (input .Eq. 'STRING VALUE') Then
        data_type = 's'
     End If
 
     End Subroutine F2D_INP_DATATYPE
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 
 
 
 
 

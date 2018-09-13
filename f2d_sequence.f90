!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_sequence.f90 *
!  *                  *
!  ********************
 
!+ F2D_SEQUENCE: SEQUENCE of automatic processing
     Subroutine F2D_SEQUENCE (inmacro_file, status)
!  Description:
!    Sets up a sequence loop in the main menu
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Nov-1998: V0.10 Correct definition of variables. User definition of 
!      variable data type (Hammersley)
!    09-Sep-1998: V0.9 Replace "IO_INPC" by "IO_INPS" to allow for significant 
!      blanks in base names and extensions (Hammersley)
!    21-Aug-1998: V0.8 Replace "SYMBOL" calls with "VARIABLE" calls (Hammersley)
!    11-Aug-1998: V0.7 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Dec-1996: V0.6 Avoid open strings crossing lines (Hammersley)
!    29-May-1995: V0.5 Increase length of a symbol value to 132 characters 
!      (Hammersley)
!    10-Jan-1995: V0.4 Rename "MAXIMUM VALUE" to "END LIMIT VALUE" (Hammersley)
!    05-Jan-1995: V0.3 Add start, and step size of loop variable (Hammersley)
!    04-Jan-1995: V0.2 Use "io_sequence" in I/O data-base to define whether in
!      sequence mode or not (Hammersley)
!    20-Dec-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io_db.inc'
!  Import:
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: inmacro_file ! File name of input
!      macro file
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_variables = 20 ! Dimension for variables
!      definition arrays
!  Local Variables:
     Character(Len = 132) :: macro_line ! Input line from macro file
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Character(Len = 132) :: variable_value ! Value of program variable
     Integer, Save :: count ! Loop counter
     Integer, Save :: end_count = 10 ! Value of last loop counter
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer, Save :: num_variables = 2 ! Number of defined dynamic variables
     Integer :: retstat ! "IO_OPEN_ASCIIFILE" return variable
     Integer, Save :: start_count = 1 ! Value of first loop counter
     Integer, Save :: step_count = 1 ! Step size of change in counter value
!      between each loop
     Integer :: variable ! Loop variable for variables
     Logical :: convert ! .True., if a string converted to a numeric value 
!      successfully
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Logical :: loop_ok ! .True., if the loop defining parameters are
!      O.K. (not an infinite loop)
     Logical :: valid_macro ! .True., if a valid macro file has been defined
     Real :: real_value ! Dummy value for "IO_SET_VARIABLE"
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(11) ! User messages
     Character(Len = 80), Save :: BASE(Max_variables) ! Base part of
!      variable values
     Character(Len = 80), Save :: EXTENSION(Max_variables)
!      Last part of variable values
     Character(Len = 1), Save :: DATA_TYPES(Max_variables)
!      Data Type of variables:
!        i = Integer value
!        l = Logical (boolean) value
!        r = Real (floating point) value
!        s = Character string
!        u = Unknown
     Character(Len = 80), Save :: VARIABLES(Max_variables)
!      Names of variables
     Integer, Save :: LEN_BASE(Max_variables) ! Number of defined
!      characters for base of variable names
     Integer, Save :: LEN_EXTENSION(Max_variables) ! Number of defined
!      characters for extension part of variable names
     Integer, Save :: LENGTH(Max_variables) ! Number of characters in the
!      variable part of variables
     Logical, Save :: ARITHMETIC(Max_variables) ! .True., if arithmetic
!      operations are to be applied to the loop count value prior to
!      being used for the variable part of the name
     Logical, Save :: FIXED(Max_variables) ! .True., if the length of any
!      variable part of the variable values is to be fixed, padded out
!      with zeros if necessary
     Real, Save :: CONSTANTS(Max_variables) ! Constants to be applied to
!      the loop count value if arithmetic is to be applied)
     Real, Save :: MULTIPLIERS(Max_variables) ! Multipliers to be applied
!      to the loop count value if arithmetic is to be applied)
!  External Functions:
     Character(Len = 20), External :: Io_itoc ! Convert integer to character
!      string
!  Local Data:
     Data ARITHMETIC / .False., .False., .False., .False., .False., .False., &
       .False., .False., .False., .False., .False., .False., .False., .False., &
       .False., .False., .False., .False., .False., .False. /
     Data BASE / 'lys1_', 'lys1_', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', &
       ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '/
     Data CONSTANTS / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
     Data DATA_TYPES / 's', 's', 'i', 's', 's', 's', 's', 's', 's', 's', 's', &
       's', 's', 's', 's', 's', 's', 's', 's', 's' /
     Data EXTENSION / '.pmi', '.cor', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', &
       ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '/
     Data FIXED / .True.,  .True.,  .True.,  .True.,  .True., .True.,  .True., &
        .True.,  .True.,  .True., .True.,  .True.,  .True.,  .True.,  .True., &
       .True.,  .True.,  .True.,  .True.,  .True. /
     Data LEN_BASE / 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0 /
     Data LEN_EXTENSION / 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0 /
     Data LENGTH / 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 &
       /
     Data MULTIPLIERS / 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
       1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /
     Data VARIABLES / '#IN', '#OUT', '#VARIABLE', '#IN_2', '#OUT_2','#VAR_2', &
       '#IN_3', '#OUT_3', '#VAR_3', '#IN_4', '#OUT_4', '#VAR_4', '#IN_5', &
       '#OUT_5', '#VAR_5', '#IN_6', '#OUT_6', '#VAR_6', '#IN_7', '#OUT_7' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SEQUENCE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     If not in sequence mode ask for the name of the macro file and the number
!     of times to run it
        If (.Not. io_sequence) Then
 
!        Check that a macro is not presently being written
           If (io_output_macro) Then
 
              Call IO_WRITE ('WARNING: The "SEQUENCE" ' // &
                'command cannot be used within a macro. (It is', status)
              Call IO_WRITE ('         used to repeately ' // &
                'run a defined macro.)', status)
              Return
           End If
 
!        Only allow finite loops to be defined
           loop_ok = .False.
           Do While (.Not. loop_ok)
 
!           Loop control information
              MESSAGE(1) = 'The  sequence is defined by an ' // &
                'integer  "loop"  variable.  This  variable is'
              MESSAGE(2) = 'set with an initial value, an ' // &
                'end limit value, and a increment step for each'
              MESSAGE(3) = 'loop.  This works in the same ' // &
                'fashion as a Fortran "DO" loop. e.g. To simply'
              MESSAGE(4) = 'loop 10 times,  the initial value ' // &
                'can be set to 1,  the maximum  value to 10'
              MESSAGE(5) = 'and the increment step to 1.'
              MESSAGE(6) = '   The value of the counter at ' // &
                'each loop  through  the  sequence may be used'
              MESSAGE(7) = 'to define  varying  definitions of' // &
                ' variables  values  e.g. file names, and the'
              MESSAGE(8) = 'value of the counter can be used ' // &
                'within the called macro. The variable ##COUNT'
              MESSAGE(9) = 'is set to contain the current  ' // &
                'loop count value.  Thus,  it may be useful to'
              MESSAGE(10) = 'define  loops  starting  from  ' // &
                'values  other than 1,  and with  non-unity or'
              MESSAGE(11) = 'negative increment steps.'
 
!           Loop counter initial value
              Call IO_INPI (.False., 0, 0, .True., 'LOOP COUNT START VALUE', &
                11, MESSAGE, 1, 'Enter a valid integer', start_count, status)
              count = start_count
 
!           Loop counter maximum value
              Call IO_INPI (.False., 0, 0, .True., &
                'LOOP COUNT END LIMIT VALUE (INCLUSIVE)', 11, MESSAGE, 1, &
                'Enter a valid integer', end_count, status)
 
!           Loop counter increment step
              Call IO_INPI (.False., 0, 0, .True., &
                'LOOP COUNT INCREMENT STEP', 11, MESSAGE, 1, &
                'Enter a valid integer', step_count, status)
 
!           Check for user escape
              If (status .Eq. St_escapevalue) Then
                 Return
              End If
 
!           See if the entered values define a valid loop
              loop_ok = (end_count .Ge. start_count .And. step_count .Gt. 0) &
                .Or. (start_count .Eq. end_count) .Or. (start_count .Ge. &
                end_count .And. step_count .Lt. 0)
 
!           Error message if loop is not O.K.
              If (.Not. loop_ok) Then
 
                 Call IO_WRITE ('WARNING: The entered ' // &
                   'values do not define a valid finite ' // 'loop, please', &
                   status)
                 Call IO_WRITE ('         re-enter ' // &
                   'valid parameter values', status)
                 Call IO_WRITE ('         (You can enter ' // &
                   'user escape (\\) to exit this command.)', status)
 
              End If
 
           End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Check that input macro file exists and is valid
           valid_macro = .False.
           Do While (.Not. valid_macro)
 
!           Enter name of macro file
              Call IO_INPC (.True., 'INPUT MACRO FILE NAME', 1, &
                'Enter Name of a file containing a macro', 1, &
                'Unacceptable input', 11, inmacro_file, status)
 
!           Try to open macro file
              Call IO_OPEN_ASCIIFILE (.False., 'READ', Io_unit_inmacro, &
                inmacro_file, retstat, status)
 
              If (retstat .Eq. 0) Then
 
!              Input first line of file and check that the file is a
!              valid macro file
                 Read (Io_unit_inmacro, '(a)') macro_line
 
!              Set valid if the first line is good
                 If (macro_line(1: 36) .Eq. &
                   '%!*\ BEGINNING OF EXPG_IO MACRO FILE' .Or. &
                   macro_line(1:31) .Eq. '%!*\ BEGINNING OF IO MACRO FILE') &
                   Then
                    valid_macro = .True.
 
!                 Close file
                    Close (Io_unit_inmacro)
 
                 Else
                    Call IO_WRITE ('WARNING: The entered ' // &
                      'file is not a valid macro file', status)
                    Call IO_WRITE ('         (You can enter ' // &
                      'user escape (\\) to exit this command.)', status)
                 End If
 
              Else If (status .Eq. St_escapevalue) Then
                 Return
              Else
 
!              The file could not be opened
                 Call IO_WRITE ('WARNING: The entered file ' // &
                   'could not be opened (probably it ' // 'doesn''t exist)', &
                   status)
                 Call IO_WRITE ('         (You can enter ' // &
                   'user escape (\\) to exit this command.)', status)
              End If
 
           End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Number of program variables to automatically define each
!        iteration
           MESSAGE(1) = 'Enter the total number of programs ' // &
             'variables,  whose values you want to be'
           MESSAGE(2) = 're-defined  automatically each ' // &
             'iteration of the sequence.  (Non-changing'
           MESSAGE(3) = 'program variable values can be defined ' // &
             'before the "SEQUENCE" command using'
           MESSAGE(4) = 'the  "DEFINE VARIABLE"  command,  or  ' // &
             'may  be  defined  here as additional'
           MESSAGE(5) = 're-definable variables, but not using ' // &
             'the dynamic capabilities.)'
           Call IO_INPI (.True., 0, 20, .True., &
             'NUMBER OF VARIABLES TO DEFINE', 5, MESSAGE, 1, &
             'Enter a valid integer within the given range', num_variables, &
             status)
 
!        Input definition of each variable and its values
           Do variable = 1, num_variables
 
!           Special treatment of second variable, set defaults so that
!           its value is like the value of the first variable, but with
!           a different extension
              If (variable .Eq. 2) Then
                 BASE(2) = BASE(1)
                 LEN_BASE(2) = LEN_BASE(1)
                 FIXED(2) = FIXED(1)
                 LENGTH(2) = LENGTH(1)
              End If
 
!           Name of variable
              Call IO_INPTOKEN (.True., 'ENTER VARIABLE NAME', 1, &
                'Enter name of variable to be defined', 1, &
                'Unacceptable input', 111, VARIABLES(variable), status)
 
!           Data type of variable
              Call F2D_INP_DATATYPE (.True., DATA_TYPES(variable), status)
 
              If (DATA_TYPES(variable) .Eq. 's') Then
 
!              Base of variable values
                 Call IO_INPS (.True., 'VARIABLE VALUE BASE', 1, &
                   'Enter characters defining the unchanging ' // &
                   'base part of the variable values', 1, &
                   'Unacceptable input', 111, LEN_BASE(variable), &
                   BASE(variable), status)
 
!              Translate '##BLANK' only
                 If (BASE(variable) .Eq. '##BLANK') Then
                    BASE(variable) = ' '
                    LEN_BASE(variable) = 1
                 End If
 
!              Fixed number of character in variable part of values
                 MESSAGE(1) = 'Enter  "YES"  if any variable ' // &
                   'part of the variable values is to be of a'
                 MESSAGE(2) = 'fixed number of characters. ' // &
                   'Enter "NO" if minimum number of characters'
                 MESSAGE(3) = 'is to be automatically defined.'
                 Call IO_INPL (.True., 0, 1, .True., &
                   'VARIABLE PART FIXED LENGTH', 3, MESSAGE, 1, &
                   'Enter "YES" or "NO"', FIXED(variable), status)
 
!              Length of variable part
                 If (FIXED(variable)) Then
                    Call IO_INPI (.True., 0, 20, .True., &
                      'NUMBER OF CHARACTERS IN VARIABLE PART', 3, MESSAGE, 1, &
                      'Enter an integer within given range', LENGTH(variable), &
                      status)
 
                 End If
 
              End If
 
!           Perform arithmetic on loop count value prior to use
              MESSAGE(1) = 'The loop count value can be used ' // &
                'directly to form part of the program'
              MESSAGE(2) = 'variable value e.g. a file name, or ' // &
                'some simply arithmetic may be defined'
              MESSAGE(3) = 'to produce another integer value ' // &
                'based on the loop count value.'
              MESSAGE(4) = ' '
              MESSAGE(5) = 'Enter "NO" to use the loop count ' // &
                'value directly.'
              MESSAGE(6) = ' '
              MESSAGE(7) = 'Enter "YES" to allow a real valued ' // &
                'multiplier to be multiplied with the'
              MESSAGE(8) = 'loop count value, and a real ' // &
                'constant added. The result is then'
              MESSAGE(9) = 'converted to the nearest integer ' // &
                'and used as part of the variable value.'
              MESSAGE(10) = 'i.e. variable_value = loop * ' // &
                'multiplier + constant'
              Call IO_INPL (.True., 0, 1, .True., &
                'ARITHMETIC ON LOOP COUNT VALUE', 10, MESSAGE, 1, &
                'Enter "YES" or "NO"', ARITHMETIC(variable), status)
 
              If (ARITHMETIC(variable)) Then
 
!              Input values of multiplier and addition constant
                 MESSAGE(1) = 'The loop count value is ' // &
                   'multiplied by a real multiplier and then a real'
                 MESSAGE(2) = 'constant is added. The result ' // &
                   'is converted to the nearest integer before'
                 MESSAGE(3) = 'being used to form part of the ' // &
                   'variable value. '
                 MESSAGE(4) = ' '
                 MESSAGE(5) = 'i.e. variable_integer_value = ' // &
                   'loop_value * multiplier + constant'
                 MESSAGE(6) = ' '
                 MESSAGE(7) = 'You can enter the value of the ' // &
                   'multiplier and the constant.'
                 MESSAGE(8) = ' '
                 MESSAGE(9) = 'e.g. For a loop which goes from ' // &
                   '1 to 20, you can make a variable which'
                 MESSAGE(10) = 'counts down from 20 to 1 with: ' // &
                   'MULTIPLIER = -1.0, and CONSTANT = 21.0  '
                 Call IO_INPR (.False., 0.0, 0.0, .True., &
                   'MULTIPLIER FOR LOOP COUNT VALUE', 10, MESSAGE, 1, &
                   'Enter a valid real value', MULTIPLIERS(variable), status)
                 Call IO_INPR (.False., 0.0, 0.0, .True., &
                   'CONSTANT TO ADD TO MODIFIED LOOP COUNT VALUE', 10, &
                   MESSAGE, 1, 'Enter a valid real value', &
                   CONSTANTS(variable), status)
 
              End If
 
              If (DATA_TYPES(variable) .Eq. 's') Then
 
!              Extension of variable values
                 Call IO_INPS (.True., 'VARIABLE VALUE EXTENSION', 1, &
                   'Enter characters defining the unchanging ' // &
                   'extension part of the variable values', 1, &
                   'Unacceptable input', 111, LEN_EXTENSION(variable), &
                   EXTENSION(variable), status)
 
!              Translate "##BLANK" only
                 If (EXTENSION(variable) .Eq. '##BLANK') Then
                    EXTENSION(variable) = ' '
                    LEN_EXTENSION(variable) = 1
                 End If
 
              End If
 
           End Do
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              Return
           End If
 
!        Set sequence mode for I/O system (this is the important line)
           io_sequence = .True.
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Carry out one iteration of loop sequence if in sequence mode
        If (io_sequence) Then
 
           If ((count .Le. end_count .And. step_count .Ge. 0) .Or. (count .Ge. &
             end_count .And. step_count .Le. 0)) Then
 
!           Output loop number to user
              Write (MESSAGE(1), '(''INFO: Sequence counter value = '', i8)') &
                count
              Call IO_WRITE (' ', status)
              Call IO_WRITE (MESSAGE(1), status)
 
!           Set variable "##COUNT" to current counter value
              Call IO_SET_VARIABLE ('##COUNT', 'i', count, log_value, &
                real_value, len_string, string_value, retstat, status)
 
!           Define variable names and current values
              Do variable = 1, num_variables
 
!              Calculate variable integer part from count value
                 If (ARITHMETIC(variable)) Then
                    real_value = Real(count) * MULTIPLIERS(variable) + &
                      CONSTANTS(variable)
                    int_value = Nint(real_value)
                 Else
                    real_value = Real(count)
                    int_value = count
                 End If
 
!              Create variable value from different parts
                 Call IO_FILENAME (BASE(variable), int_value, FIXED(variable), &
                   LENGTH(variable), EXTENSION(variable), retstat, &
                   variable_value, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Setting '', a20, '' = '', a)')
!              :                VARIABLES(variable), variable_value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 If (retstat .Ne. 0) Then
 
                    Call IO_WRITE ('WARNING: The variable ' // &
                      'value could not be constructed properly', status)
                    io_sequence = .False.
                    Return
 
                 End If
 
!              Set variable name and value
                 Call IO_SET_VARIABLE (VARIABLES(variable), &
                   DATA_TYPES(variable), int_value, log_value, real_value, &
                   Len_trim(variable_value), variable_value, retstat, status)
 
              End Do
 
!           Set I/O system into input macro file state
              Call IO_OPEN_INMACRO (.False., inmacro_file, status)
 
!           Increment loop count value
              count = count + step_count
 
           Else
              io_sequence = .False.
              Call IO_WRITE ( 'INFO: *** END OF SEQUENCE ***', status)
              Call IO_WRITE (' ', status)
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_SEQUENCE
!********1*********2*********3*********4*********5*********6*********7*********8

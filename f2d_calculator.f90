!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_calculator.f90 *
!  *                    *
!  **********************
 
!+ F2D_CALCULATOR - FIT 2-D CALCULATOR sub-menu
     Subroutine F2D_CALCULATOR (status)
!  Description:
!    Reverse Polish notation calculator
!  Method:
!    Menu driven loop
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Nov-2006: V0.16 Improve documentation (Hammersley)
!    26-Feb-2004: V0.15 Don't output an empty stack (Hammersley)
!    19-Aug-1998: V0.14 Use "IO_SET_VARIABLE" instead of "IO_SET_SYMBOL" 
!      (Hammersley)
!    17-Aug-1998: V0.13 Changes to "IO_INQ_SYMBOL" (Hammersley)
!    11-Aug-1998: V0.12 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Dec-1996: V0.11 Avoid open strings crossing lines (Hammersley)
!    03-Dec-1996: V0.10 Add maximum and minimum command (Hammersley)
!    09-Oct-1996: V0.9 Correct saving of real values as symbols,
!      previously they were saved in integer form (Hammersley)
!    08-Nov-1995: V0.8 Add "MODULUS" command and "INTEGER" command (Hammersley)
!    05-Jan-1995: V0.7 Only set real part of number when defining a value of a
!      symbol (Hammersley)
!    29-Oct-1994: V0.6 Add "SYMBOL" command to store value of top of stack 
!      (Hammersley)
!    24-Jun-1994: V0.5 Convert photon energy to wavelength and vice versa
!      (Hammersley)
!    22-Jun-1994: V0.4 Add "ABSOLUTE" and "NEGATION" commands and
!      commands to store results in different registers (Hammersley)
!    20-Jun-1994: V0.3 Change order of operands for "POWER" command (Hammersley)
!    01-May-1994: V0.2 Add more commands (Hammersley)
!    30-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.16' ! Version number
     Integer, Parameter :: Max_menu = 57 ! Number of instructions in the menu
     Integer, Parameter :: Max_stack = 50 ! Dimension for stack array
     Integer, Parameter :: Max_tokens = 10 ! Dimension for tokens array
!  Local Variables:
     Character(Len = 80) :: buffer ! Buffer for user input
     Character(Len = 80) :: command ! Full command corresponding to user request
     Character(Len = 1) :: string_value ! Dummy variable for "IO_SET_VARIABLE"
     Character(Len = 80) :: variable ! Name of variable
     Character(Len = 1) :: var_type ! Data type of variable:
!      i = Integer value
!      l = Logical (boolean) value
!      r = Real (floating point) value
!      s = Character string
!      u = Unknown
     Complex, Save :: memory = (0.0, 0.0) ! Saved memory value
     Complex, Save :: register_1 = (0.0, 0.0) ! Saved register 1 value
     Complex, Save :: register_2 = (0.0, 0.0) ! Saved register 2 value
     Complex, Save :: register_3 = (0.0, 0.0) ! Saved register 3 value
     Complex, Save :: register_4 = (0.0, 0.0) ! Saved register 4 value
     Complex :: value ! Used to store complex variables
     Integer :: element ! Loop variable for elements of stack
     Integer :: int_value ! Result converted to an integer
     Integer :: item ! Loop variable for "DATA" statements items
     Integer :: len_string ! Dummy variable for "IO_SET_VARIABLE"
     Integer :: num_menu ! Number of items in a menu
     Integer, Save :: num_var = 0 ! Counter for variables to present as a
!      default
     Integer :: num_variables ! Number of defined variables in database
     Integer :: num_tokens ! Number of tokens found in a buffer
     Integer :: retstat ! Return status from IO_COMMAND
     Integer, Save :: stack_pointer = 0 ! Pointer to top of stack
     Integer :: temp_status ! Temporary version of the status variable
     Logical :: continue ! .True., until the user wants to exit the sub-menu
     Logical :: convert ! .True., if a token is converted to a real
     Logical :: log_value ! Dummy variable for "IO_SET_VARIABLE"
     Logical :: out_result ! .True., if the top element of the stack is to be 
!      output to the user
     Logical :: reset ! .True., if the status value is to be reset
     Logical :: success ! .True., if the input is treatable in token form
     Real :: imaginary_value ! Imaginary part of a complex number
     Real :: real_value ! Real part of a complex number
!  Local Arrays:
     Character(Len = 27) :: MENU(Max_menu) ! Array containing menu choices
     Character(Len = 80) :: MENUTXT(Max_menu) ! Text to explain menu choices
     Character(Len = 80) :: MESSAGE(13) ! User messages
     Character(Len = 30) :: TOKENS(Max_tokens) ! Input line separated into
!      tokens
     Complex, Save :: STACK(Max_stack) ! Stack for storing operands
!    Internal Functions:
!  External Functions:
     Character(Len = 20), External :: Io_itoc ! Convert integer to character
!      representation
     Character(Len = 20), External :: Io_rtoc ! Convert real to character
!      representation
     Integer, External :: Io_var_number ! Number of variable in tables, or 0 if
!      the variable does not exist
!  Local Data:
     Data (MENU(item), item = 1, 10) / '?', '+', '-', '*', '/', '1/X', &
       'ABSOLUTE', 'ACOSINE', 'ARCCOSINE', 'ARCSINE' /
     Data (MENU(item), item = 11, 20) / 'ARCTANGENT', 'ASINE', 'ATANGENT', &
       'ADDITION', 'CLEAR', 'COSINE', 'DIVISION', 'DUPLICATE', 'ENERGY', &
       'EXCHANGE' /
     Data (MENU(item), item = 21, 30) / 'EXIT', 'EXPONENTIAL', 'INTEGER', &
       'LN', 'LOGARITHM', 'MAXIMUM', 'MEMORY', 'MINIMUM', 'MODULUS', &
       'MULTIPLICATION' /
     Data (MENU(item), item = 31, 40) / 'NEGATION', 'PI', 'POP', 'POWER', &
       'PUSH', 'QUIT', 'R1', 'R2', 'R3', 'R4' /
     Data (MENU(item), item = 41, 50) / 'RECALL', 'RECIPROCAL', 'REGISTERS', &
       'S1', 'S2', 'S3', 'S4', 'SINE', 'SQUARE ROOT', 'SQRT' /
     Data (MENU(item), item = 51, 57) / 'STACK', 'STORE', 'SUBTRACTION', &
       'SYMBOL', 'TANGENT', 'VARIABLE', 'WAVELENGTH' /
     Data (MENUTXT(item), item = 1, 10) / &
       '?: List of available operators and commands', &
       '+: Add top two elements of stack', &
       '-: Subtract lower element of stack from top element', &
       '*: Multiple top two elements of stack', &
       '/: Divide top element of stack by lower element', &
       '1/X: Form reciprocal of top of stack', &
       'ABSOLUTE: Absolute value of top element: Sqrt(r**2 + i**2)', &
       'ACOSINE: Take Arc Cosine of top element of stack (degrees)', &
       'ARCCOSINE: Take Arc Cosine of top element of stack (degrees)', &
       'ARCSINE: Take Arc Sine of top element of stack (degrees)' /
     Data (MENUTXT(item), item = 11, 20) / &
       'ARCTANGENT: Take Arc Tangent of top element of stack (degrees)', &
       'ASINE: Take Arc Sine of top element of stack (degrees)', &
       'ATANGENT: Take Arc Tangent of top element of stack (degrees)', &
       'ADDITION: Add top two elements of stack', &
       'CLEAR: Clear stack (remove all elements)', &
       'COSINE: Take cosine of top element of stack (degrees)', &
       'DIVISION: Divide top element of stack by lower element', &
       'DUPLICATE: Copy top of stack onto stack', &
       'ENERGY: Convert wavelength (Angstroms) to photon energy (KeV)', &
       'EXCHANGE: Swap top two elements of stack' /
     Data (MENUTXT(item), item = 21, 30) / 'EXIT: Exit fit sub-menu', &
       'EXPONENTIAL: e to the power of the top element of stack', &
       'INTEGER: Truncate value to an integer', &
       'LN: Take natural logarithm  of top element', &
       'LOGARITHM: Take logarithm base 10 of top element', &
       'MAXIMUM: Maximum of top two elements of the stack', &
       'MEMORY: Output value of memory', &
       'MODULUS: Remainder when top of stack divided by lower value', &
       'MINIMUM: Minimum of top two elements of the stack', &
       'MULTIPLICATION: Multiple top two elements of stack' /
     Data (MENUTXT(item), item = 31, 40) / &
       'NEGATION: Multiple top element by -1.0', &
       'PI: Ratio of circumference to diameter of a circle on stack', &
       'POP: Remove top element from stack', &
       'POWER: Raise lower element to the power of the top element', &
       'PUSH: Duplicate top element (adds to stack)', &
       'QUIT: Exit calculator sub-menu', &
       'R1: Place value from register 1 onto stack', &
       'R2: Place value from register 2 onto stack', &
       'R3: Place value from register 3 onto stack', &
       'R4: Place value from register 4 onto stack' /
     Data (MENUTXT(item), item = 41, 50) / &
       'RECALL: Place value from memory onto stack', &
       'RECIPROCAL: 1/x, reciprocal of top element', &
       'REGISTERS: Values of current registers', &
       'S1: Store top element in register 1', &
       'S2: Store top element in register 2', &
       'S3: Store top element in register 3', &
       'S4: Store top element in register 4', &
       'SINE: Take sine of top element of stack (degrees)', &
       'SQUARE ROOT: Replace top element by its square root', &
       'SQRT: Replace top element by its square root' /
     Data (MENUTXT(item), item = 51, 57) / &
       'STACK: Shows contents of the stack', &
       'STORE: Save value of top element in memory', &
       'SUBTRACTION: Subtract lower element from top element', &
       'SYMBOL: (Same as the "VARIABLE" command)', &
       'TANGENT: Take tangent of top element of stack (degrees)', &
       'VARIABLE: Store top of stack as a named program variable', &
       'WAVELENGTH: Photon energy (keV) to wavelength (Angstroms)' /
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CALCULATOR'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CALCULATOR ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
     num_menu = Max_menu ! Number of instructions available in the menu
 
!  Start command input/action loop until EXIT requested
     continue = .True.
     Do While (continue)
 
!     Get user to select between the available menu options
        MESSAGE(1) = 'Enter a value to put it onto the stack or '// &
          'enter an operator to perform'
        MESSAGE(2) = 'an operation on existing values on the '// &
          'stack. Enter ? for list of'
        MESSAGE(3) = 'available operators and commands.'
        Call IO_INPC (.False., 'ENTER VALUE OR OPERATOR', 3, MESSAGE, 1, &
          'Enter valid text', 2, buffer, status)
 
!     Check for basic operators and convert to operator commands
        If (Len_trim(buffer) .Eq. 1) Then
 
           If (buffer(1:1) .Eq. '+') Then
              buffer = 'ADDITION'
           Else If (buffer(1:1) .Eq. '-') Then
              buffer = 'SUBTRACTION'
           Else If (buffer(1:1) .Eq. '*') Then
              buffer = 'MULTIPLICATION'
           Else If (buffer(1:1) .Eq. '/') Then
              buffer = 'DIVISION'
           End If
 
        End If
 
        If (Len_trim(buffer) .Gt. 0) Then
 
!        Separate tokens from input line
           Call IO_TOKENS (buffer, Max_tokens, success, num_tokens, TOKENS, &
             status)
 
           If (success) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''success = '', l1)') success
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert the token to a real if possible
              Call IO_TOKTR (TOKENS(1), convert, real_value, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''convert = '', l1)') convert
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (convert) Then
 
!              An operand has been entered, see if an imaginary part exists
                 If (num_tokens .Ge. 2) Then
 
                    Call IO_TOKTR (TOKENS(2), convert, imaginary_value, &
                      status)
                    If (.Not.convert) Then
                       imaginary_value = 0.0
                    End If
                 Else
                    imaginary_value = 0.0
                 End If
 
!              Add element to stack
                 value = Cmplx(real_value, imaginary_value)
                 Call F2D_PUSHSTACK (value, Max_stack, stack_pointer, STACK, &
                   status)
 
!              Set buffer to "NULL"
                 command = 'NULL'
                 buffer = 'NULL'
 
              End If
 
           End If
 
        Else
 
           buffer = 'NULL'
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Treat command if the input was not an operand
        If (buffer .Ne. 'NULL') Then
 
!        Check if input corresponds to one of the available options and
!        is not ambiguous
           Call IO_COMMAND (buffer, Max_menu, num_menu, MENU, retstat, &
             command, status)
 
           If (retstat .Eq. -1) Then
 
!           Command not found
              MESSAGE(1) = 'WARNING: Entered text does not ' // &
                'correspond to any of the available choices.'
              MESSAGE(2) = 'Please enter a command from the menu.'
              Call IO_TEXT (2, MESSAGE, status)
              command = 'NULL'
           Else If (retstat .Eq. -2) Then
 
!           Ambiguous command
              Call IO_WRITE ('WARNING: Entered command is ' // &
                'ambiguous. Please enter more letters.', status)
              command = 'NULL'
           End If
 
        Else
 
           command = 'NULL'
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     EXECUTE MENU CHOICES
 
        out_result = .False.
 
!     Call subroutine to perform requested operation.
        If (command .Eq. 'NULL') Then
 
!        Do nothing
           Continue
 
        Else If (command .Eq. '?') Then
 
!        Output list of available commands
           Call IO_TEXT (num_menu, MENUTXT, status)
           command = 'HELP'
 
        Else If (command .Eq. 'ABSOLUTE') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Calculate absolute value
              STACK(stack_pointer) = Abs(STACK(stack_pointer))
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'ARCCOSINE' .Or. command .Eq. 'ACOSINE') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Output warning message if imaginary part of number is
!           non-zero
              If (AIMAG(STACK(stack_pointer)) .Ne. 0.0) Then
                 Call IO_WRITE ('WARNING: Imaginary part '// &
                   'of number will be ignored for this operation', status)
              End If
 
!           Convert to degrees after operation
              STACK(stack_pointer) = Acos(Real(STACK(stack_pointer)))
              STACK(stack_pointer) = STACK(stack_pointer) * 180.0/Pi
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'ARCSINE' .Or. command .Eq. 'ASINE') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Output warning message if imaginary part of number is
!           non-zero
              If (AIMAG(STACK(stack_pointer)) .Ne. 0.0) Then
                 Call IO_WRITE ('WARNING: Imaginary part '// &
                   'of number will be ignored for this operation', status)
              End If
 
!           Convert to degrees after operation
              STACK(stack_pointer) = Asin(Real(STACK(stack_pointer)))
              STACK(stack_pointer) = STACK(stack_pointer) * 180.0/Pi
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'ARCTANGENT' .Or. command .Eq. 'ATANGENT') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Output warning message if imaginary part of number is
!           non-zero
              If (AIMAG(STACK(stack_pointer)) .Ne. 0.0) Then
                 Call IO_WRITE ('WARNING: Imaginary part '// &
                   'of number will be ignored for this operation', status)
              End If
 
!           Convert to degrees after operation
              STACK(stack_pointer) = Atan(Real(STACK(stack_pointer)))
              STACK(stack_pointer) = STACK(stack_pointer) * 180.0 / Pi
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'ADDITION') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = STACK(stack_pointer - 1) + &
                STACK(stack_pointer)
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'CLEAR') Then
 
!        Clear stack, set pointer to 0
           stack_pointer = 0
 
        Else If (command .Eq. 'COSINE') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Convert to radians prior to operation
              STACK(stack_pointer) = STACK(stack_pointer) * Pi / 180.0
              STACK(stack_pointer) = Cos(STACK(stack_pointer))
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'DIVISION') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = STACK(stack_pointer) / &
                STACK(stack_pointer - 1)
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'DUPLICATE') Then
 
!        Copy top of stack onto stack
           If (stack_pointer .Ge. 1) Then
              Call F2D_PUSHSTACK (STACK(stack_pointer), Max_stack, &
                stack_pointer, STACK, status)
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'ENERGY') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Convert photon wavelength in Anstroms to energy in Kev
              If (Real(STACK(stack_pointer)) .Ne. 0.0) Then
                 STACK(stack_pointer) = 12.39852 / STACK(stack_pointer)
                 out_result = .True.
              Else
                 Call IO_WRITE ( 'WARNING: Wavelength cannot be 0.0', status)
              End If
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'EXCHANGE') Then
 
!        Swap top two elements of stack
           If (stack_pointer .Ge. 2) Then
              value = STACK(stack_pointer - 1)
              STACK(stack_pointer-1) = STACK(stack_pointer)
              STACK(stack_pointer) = value
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'EXIT' .Or. command .Eq. 'QUIT') Then
 
!        EXIT: Return to main menu
           continue = .False.
 
        Else If (command .Eq. 'EXPONENTIAL') Then
 
           If (stack_pointer .Ge. 1) Then
              STACK(stack_pointer) = EXP(STACK(stack_pointer))
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'INTEGER') Then
 
           If (stack_pointer .Ge. 1) Then
              STACK(stack_pointer) = Int(STACK(stack_pointer))
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'LN') Then
 
           If (stack_pointer .Ge. 1) Then
              STACK(stack_pointer) = Log(STACK(stack_pointer))
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'LOGARITHM') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Output warning message if imaginary part of number is
!           non-zero
              If (AIMAG(STACK(stack_pointer)) .Ne. 0.0) Then
                 Call IO_WRITE ('WARNING: Imaginary part '// &
                   'of number will be ignored for this operation', status)
              End If
 
              STACK(stack_pointer) = Log10(Real(STACK(stack_pointer)))
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'MAXIMUM') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = Max(Real(STACK(stack_pointer)), &
                Real(STACK(stack_pointer - 1)))
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'MEMORY') Then
 
!        Output value of memory
           Write (message, '(''MEMORY: '', 2(1pg14.7))') memory
           Call IO_WRITE (message, status)
 
        Else If (command .Eq. 'MINIMUM') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer-1) = Min(Real(STACK(stack_pointer)), &
                Real(STACK(stack_pointer - 1)))
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'MODULUS') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = Mod(Int(STACK(stack_pointer)), &
                Int(STACK(stack_pointer - 1)))
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'MULTIPLICATION') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = STACK(stack_pointer - 1) * &
                STACK(stack_pointer)
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'NEGATION') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Calculate absolute value
              STACK(stack_pointer) = -STACK(stack_pointer)
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'PI') Then
 
!        PLace value of Pi onto top of stack
           value = (3.1415926, 0.0)
           Call F2D_PUSHSTACK (value, Max_stack, stack_pointer, STACK, status)
           out_result = .True.
 
        Else If (command .Eq. 'POP') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Remove top element from the stack
              stack_pointer = stack_pointer - 1
 
              If (stack_pointer .Ge. 1) Then
                 out_result = .True.
              End If
 
           Else
              Call IO_WRITE ('The stack is empty !', status)
           End If
 
        Else If (command .Eq. 'POWER') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer-1) = STACK(stack_pointer-1) ** &
                STACK(stack_pointer)
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'PUSH') Then
 
           If (stack_pointer .Ge. 1) Then
              Call F2D_PUSHSTACK (STACK(stack_pointer), Max_stack, &
                stack_pointer, STACK, status)
           Else
              stack_pointer = 1
              STACK(stack_pointer) = (0.0, 0.0)
           End If
 
        Else If (command .Eq. 'R1') Then
 
!        Push value from memory onto stack
           Call F2D_PUSHSTACK (register_1, Max_stack, stack_pointer, STACK, &
             status)
           out_result = .True.
 
        Else If (command .Eq. 'R2') Then
 
!        Push value from memory onto stack
           Call F2D_PUSHSTACK (register_2, Max_stack, stack_pointer, STACK, &
             status)
           out_result = .True.
 
        Else If (command .Eq. 'R3') Then
 
!        Push value from memory onto stack
           Call F2D_PUSHSTACK (register_3, Max_stack, stack_pointer, STACK, &
             status)
           out_result = .True.
 
        Else If (command .Eq. 'R4') Then
 
!        Push value from memory onto stack
           Call F2D_PUSHSTACK (register_4, Max_stack, stack_pointer, STACK, &
             status)
           out_result = .True.
 
        Else If (command .Eq. 'RECALL') Then
 
!        Push value from memory onto stack
           Call F2D_PUSHSTACK (memory, Max_stack, stack_pointer, STACK, &
             status)
           out_result = .True.
 
        Else If (command .Eq. 'RECIPROCAL' .Or. command .Eq. '1/X') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Form reciprocal
              STACK(stack_pointer) = (1.0, 0.0)/STACK(stack_pointer)
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'REGISTERS') Then
 
!        Output values of registers
           Write (message, '(''REGISTER 1: '', 2(1pg14.7))') register_1
           Call IO_WRITE (message, status)
           Write (message, '(''REGISTER 2: '', 2(1pg14.7))') register_2
           Call IO_WRITE (message, status)
           Write (message, '(''REGISTER 3: '', 2(1pg14.7))') register_3
           Call IO_WRITE (message, status)
           Write (message, '(''REGISTER 4: '', 2(1pg14.7))') register_4
           Call IO_WRITE (message, status)
 
        Else If (command .Eq. 'S1') Then
 
!        Store top of stack in register 1
           If (stack_pointer .Ge. 1) Then
 
              register_1 = STACK(stack_pointer)
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'S2') Then
 
!        Store top of stack in register 2
           If (stack_pointer .Ge. 1) Then
 
              register_2 = STACK(stack_pointer)
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'S3') Then
 
!        Store top of stack in register 3
           If (stack_pointer .Ge. 3) Then
 
              register_3 = STACK(stack_pointer)
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'S4') Then
 
!        Store top of stack in register 4
           If (stack_pointer .Ge. 4) Then
 
              register_4 = STACK(stack_pointer)
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'SINE') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Convert to radians prior to operation
              STACK(stack_pointer) = STACK(stack_pointer) * Pi/180.0
              STACK(stack_pointer) = Sin(STACK(stack_pointer))
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'SQRT' .Or. command .Eq. 'SQUARE ROOT') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Take square root of value
              STACK(stack_pointer) = Sqrt(STACK(stack_pointer))
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'STACK') Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''stack pointer = '', i6)') stack_pointer
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           If (stack_pointer .Ge. 1) Then
 
!           Output contents of the stack to the user
              Do element = 1, stack_pointer
                 Write (MESSAGE(1), '(''Element '', i2, '' = '', 2(1pg14.7))') &
                   element, STACK(element)
                 Call IO_WRITE (MESSAGE(1), status)
              End Do
 
           Else
              Call IO_WRITE ('The stack is empty', status)
           End If
 
        Else If (command .Eq. 'STORE') Then
 
!        Store top of stack in memory
           If (stack_pointer .Ge. 1) Then
 
              memory = STACK(stack_pointer)
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'SUBTRACTION') Then
 
           If (stack_pointer .Ge. 2) Then
              STACK(stack_pointer - 1) = STACK(stack_pointer) - &
                STACK(stack_pointer - 1)
              stack_pointer = stack_pointer - 1
              out_result = .True.
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'VARIABLE' .Or. command .Eq. 'SYMBOL') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Store value of top of stack as a user named variable
 
!           Find out number of defined symbols
              Call IO_INQ_VARNUM (0, num_variables, retstat, variable, &
                var_type, int_value, log_value, real_value, len_string, &
                string_value, status)
 
!           Set default value if no variables have been defined, otherwise
!           present next defined variable name as a default
              If (num_variables .Le. 0) Then
                 variable = '#VALUE'
              Else
 
                 num_var = num_var + 1
                 If (num_var .Gt. num_variables) Then
                    num_var = 1
                 End If
 
!              Recover values of symbol as default values
                 Call IO_INQ_VARNUM (num_var, num_variables, retstat, &
                   variable, var_type, int_value, log_value, real_value, &
                   len_string, string_value, status)
 
              End If
 
!           Input variable from user
              Call IO_INPTOKEN (.True., 'ENTER VARIABLE NAME', 1, &
                'Enter name of internal variable to define', 1, &
                'Enter string', 101, variable, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Real value = '', f12.5)')
!           :              Real(STACK(stack_pointer))
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert top of stack to character representation
              If (Real(STACK(stack_pointer)) .Eq. 0.0 .Or. &
                Abs((Nint(Real(STACK(stack_pointer))) - &
                Real(STACK(stack_pointer))) / Real(STACK(stack_pointer))) .Lt. &
                1.0e-5) Then
 
!              Save value in integer form
                 var_type = 'i'
                 int_value = Nint(Real(STACK(stack_pointer)))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Save in integer form'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Save in floating point form'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Save value in real form
                 var_type = 'r'
                 real_value = Real(STACK(stack_pointer))
 
              End If
 
!           Set variable
              Call IO_SET_VARIABLE (variable, var_type, int_value, log_value, &
                real_value, len_string, string_value, retstat, status)
 
!           Find number of symbol within database
              num_var = Io_var_number(variable, status)
 
           Else
              Call IO_WRITE ('WARNING: No value is defined', status)
           End If
 
        Else If (command .Eq. 'TANGENT') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Output warning message if imaginary part of number is non-zero
              If (AIMAG(STACK(stack_pointer)) .Ne. 0.0) Then
                 Call IO_WRITE ('WARNING: Imaginary part '// &
                   'of number will be ignored for this operation', status)
              End If
 
!           Convert to radians prior to operation
              STACK(stack_pointer) = STACK(stack_pointer) * Pi / 180.0
              STACK(stack_pointer) = Tan(Real(STACK(stack_pointer)))
              out_result = .True.
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else If (command .Eq. 'WAVELENGTH') Then
 
           If (stack_pointer .Ge. 1) Then
 
!           Convert photon energy in Kev to wavelength in Angstroms
              If (Real(STACK(stack_pointer)) .Ne. 0.0) Then
                 STACK(stack_pointer) = 12.39852 / STACK(stack_pointer)
                 out_result = .True.
              Else
                 Call IO_WRITE ( 'WARNING: Energy cannot be 0.0', status)
              End If
 
           Else
              Call IO_WRITE ('Not enough operands for operator', status)
           End If
 
        Else
 
!        Unknown command
           Call IO_WRITE ( &
             'WARNING: Unknown command, please enter new command', status)
 
        End If
 
!     Output result of operator
        If (out_result) Then
           Write (message, '(''RESULT: '', 2(1pg14.7))') STACK(stack_pointer)
           Call IO_WRITE (message, status)
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
 
!        Reset and continue if user escape
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else
 
!           Output user message if error
              Call ST_OUT (status)
 
!           Use choice to EXIT or reset status
              reset = .True.
              temp_status = St_goodvalue
              Call IO_INPL (.True., 0, 1, .True., 'RESET "status"', 1, &
                'YES: to reset "status" value, other exit program', 1, &
                'Enter YES or NO', reset, temp_status)
 
              If (reset) Then
                 Call ST_DEF_SYSTEM (status)
              Else
                 continue = .False.
              End If
 
           End If
 
        End If
 
     End Do
 
     End Subroutine F2D_CALCULATOR
!********1*********2*********3*********4*********5*********6*********7*********8
     Subroutine F2D_PUSHSTACK (value, max_stack, stack_pointer, STACK, status)
!  Type Definitions:
     Implicit None
!  Global Constants:
!  Include 'st_symbols.inc'
!  Import:
     Complex :: value ! Value to be added to the stack
     Integer :: max_stack ! Dimension of stack
!  Import/Export:
     Integer :: stack_pointer ! Pointer to top of stack
     Complex :: STACK(max_stack) ! Element stack
!  Status:
     Integer :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: element ! Loop variable for elements of stack
!  Local Arrays:
!  Internal Functions:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Change pointer position and set value
     If (stack_pointer .Lt. max_stack) Then
        stack_pointer = stack_pointer + 1
     Else
 
!     First push all elements down the stack and lose the bottom element
        Do element = 2, stack_pointer
           STACK(element - 1) = STACK(element)
        End Do
 
     End If
 
!  Place element onto the stack
     STACK(stack_pointer) = value
 
     End Subroutine F2D_PUSHSTACK
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

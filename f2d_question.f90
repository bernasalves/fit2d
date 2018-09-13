!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_question.f90 *
!  *                  *
!  ********************
 
!+ F2D_QUESTION - FIT 2-D set-up interactive QUESTION during a macro
     Subroutine F2D_QUESTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, status)
!  Description:
!    Inputs values to define an interactive question, which will be asked to the
!    user during an otherwise batch run macro.
!  Method:
!    Usual "IO_INP*" routines
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Feb-2001: V0.9 Investigating problem with input file from the 
!      graphical interface. "len_cvalue" was not defined (Hammersley)
!    09-Oct-1998: V0.8 Use "IO_INPS" instead of "IO_INPC" to allow input of
!      strings with significant blanks (Hammersley)
!    21-Aug-1998: V0.7 Replace "SYMBOL" calls with "VARIABLE" calls 
!      (Hammersley)
!    11-Aug-1998: V0.6 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Oct-1996: V0.5 Correct bug, so that "CANCEL" really does end the macro 
!      (Hammersley)
!    26-Aug-1996: V0.4 Treat graphical "CANCEL" input (Hammersley)
!    23-Aug-1996: V0.3 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    19-Apr-1996: V0.2 Add Graphical coordinate input option, and choice of 
!      graphical interface (Hammersley)
!    17-Apr-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'io_db.inc' ! I/O database
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_help = 100 ! Dimension for array for user help 
!      text
     Integer, Parameter :: Max_types = 7 ! Dimension for array for different
!      data types
!  Local Variables:
     Character(Len = 256), Save :: cvalue ! Input character value
     Character(Len = 20) :: data_type ! Type of data value to input
     Character(Len = 132) :: prompt ! User prompt text
     Character(Len = 32), Save :: variable = '#VALUE' ! Program variable name
     Character(Len = 32), Save :: variable2 = '#Y1' ! Program variable name
     Character(Len = 1) :: type ! Data type of value:
!      i = Integer value
!      l = Logical (boolean) value
!      r = Real (floating point) value
!      s = Character string
!      u = Unknown
     Integer :: dummy ! Dummy variable
     Integer, Save :: ilower ! Lower bound for an integer
     Integer :: item ! Loop variable for data types
     Integer, Save :: iupper ! Upper bound for an integer
     Integer, Save :: ivalue ! Input integer value
     Integer :: len_cvalue ! Number of defined characters in a string
     Integer :: line ! Line number of help text
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: num_types ! Number of different types of data value which may be
!      defined
     Integer :: retstat ! Return status 0 = good value
     Logical, Save :: bound ! .True., if the data value range is bounded
     Logical, Save :: default ! .True., if a default value is to be given
     Logical, Save :: gui = .True. ! .True., if GUI is to be used to input value
     Logical, Save :: lvalue ! Input logical value
     Logical :: macro_mode ! .True., if the input macro mode is on
     Logical :: out_macro ! .True., if the output macro mode is on
     Real, Save :: rlower ! Lower bound for a real value
     Real, Save :: rupper ! Upper bound for a real value
     Real, Save :: rvalue ! Input real value
     Real :: x_coordinate ! Input X-coordinate
     Real :: y_coordinate ! Input Y-coordinate
!  Local Arrays:
     Character(Len = 20) :: DATA_TYPES(Max_types) ! The different types of data
!      value which may be defined
     Character(Len = 80) :: HELP(Max_help) ! User help text
     Character(Len = 80) :: TYPES_TEXT(Max_types) ! Description of the different
!      types of data value which may be defined
!  Internal Functions:
!  External Functions:
!  Local Data:
     Data (DATA_TYPES(item), item = 1, 7) / 'CHARACTER STRING', &
       'GRAPHICAL COORDINATE', 'INPUT FILE', 'INTEGER VALUE', 'LOGICAL VALUE', &
       'OUTPUT FILE', 'REAL VALUE' /
     Data (TYPES_TEXT(item), item = 1, 7) / &
       'CHARACTER STRING: Input a line of text', &
       'GRAPHICAL COORDINATE: Input a coordinate by mouse click', &
       'INPUT FILE: Specify an input file name', &
       'INTEGER VALUE: Input an integer value', &
       'LOGICAL VALUE: Input a logical "YES/NO" value', &
       'OUTPUT FILE: Specify an output file name', &
       'REAL VALUE: Input a real value' /
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_QUESTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_QUESTION ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Initialise variables
     retstat = 0
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\'')')
        Write (io_unit_outmacro, &
          '(''%!*\ Definition of interactive input of a value'')')
        Write (io_unit_outmacro, '(''%!*\ Data value type'')')
     End If
 
!  Type of data value to define
     num_types = Max_types
     data_type = 'INTEGER VALUE'
     Call IO_MENU (.True., 'TYPE OF DATA VALUE TO DEFINE', Max_types, &
       TYPES_TEXT, 1, 'Enter one of available data types', Max_types, &
       num_types, DATA_TYPES, data_type, status)
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\ User prompt'')')
     End If
 
!  Enter prompt to be output to the user
     Call IO_INPC (.False., 'ENTER USER PROMPT FOR VALUE INPUT', 1, &
       'Enter text to be presented to the user', 1, &
       'Enter valid character string', 1, prompt, status)
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\ Give default'')')
     End If
 
     If (data_type .Ne. 'GRAPHICAL COORDINATE') Then
 
!     Use GUI
        Call IO_INPL (.True., 0, 1, .True., 'USE GRAPHICAL USER INTERFACE', 1, &
          '"YES": to present requests through GUI', 1, 'Enter "YES" or "NO"', &
          gui, status)
 
!     Present a default value or not
        Call IO_INPL (.True., 0, 1, .True., 'GIVE USER DEFAULT VALUE', 1, &
          '"YES": to output a default value to the user', 1, &
          'Enter "YES" or "NO"', default, status)
 
     Else
        default = .False.
     End If
 
     If (default) Then
 
        If (io_output_macro) Then
           Write (io_unit_outmacro, '(''%!*\ Default value'')')
        End If
 
        If (data_type .Eq. 'INTEGER VALUE') Then
 
!        Input integer default value
           Call IO_INPI (.False., 1, 2, .False., &
             'ENTER DEFAULT VALUE FOR INTEGER', 1, &
             'Enter the default value to be presented to the user', 1, &
             'Enter valid integer value', ivalue, status)
 
        Else If (data_type .Eq. 'REAL VALUE') Then
 
!        Input real default value
           Call IO_INPR (.False., 1.0, 2.0, .False., &
             'ENTER DEFAULT VALUE FOR REAL', 1, &
             'Enter the default value to be presented to the user', 1, &
             'Enter valid real value', rvalue, status)
 
        Else If (data_type .Eq. 'LOGICAL VALUE') Then
 
!        Input logical default value
           Call IO_INPL (.True., 0, 1, .True., &
             'ENTER DEFAULT VALUE FOR LOGICAL VALUE', 1, &
             'Enter the default value to be presented to the user', 1, &
             'Enter "YES" or "NO"', lvalue, status)
 
        Else
 
!        Input character string value
           Call IO_INPS (.False., 'ENTER CHARACTER STRING OR FILE NAME', 1, &
             'Enter default character string for string or file name', 1, &
             'Enter valid character string', 1, len_cvalue, cvalue, status)
 
        End If
 
     End If
 
     If (data_type .Eq. 'INTEGER VALUE' .Or. data_type .Eq. 'REAL VALUE') Then
 
!     Data value bound or not
        Call IO_INPL (.True., 0, 1, .True., 'RESTRICT INPUT VALUE RANGE', 1, &
          '"YES": to only allow values within a defined range', 1, &
          'Enter "YES" or "NO"', bound, status)
 
     Else
        bound = .False.
     End If
 
     If (bound) Then
 
        If (io_output_macro) Then
           Write (io_unit_outmacro, '(''%!*\ Range limits'')')
        End If
 
        If (data_type .Eq. 'INTEGER VALUE') Then
 
!        Input lower bound
           Call IO_INPI (.False., 0, 1, .False., &
             'LOWER LIMIT OF RANGE FOR INTEGER', 1, &
             'Enter the lower bound for accepted input', 1, &
             'Enter valid integer value', ilower, status)
 
!        Input upper bound
           Call IO_INPI (.True., ilower, 2147483647, .False., &
             'UPPER LIMIT OF RANGE FOR INTEGER', 1, &
             'Enter the upper bound for accepted input', 1, &
             'Enter valid integer value', iupper, status)
 
        Else If (data_type .Eq. 'REAL VALUE') Then
 
!        Input real lower bound
           Call IO_INPR (.False., 1.0, 2.0, .False., &
             'ENTER LOWER LIMIT OF INPUT RANGE FOR REAL', 1, &
             'Enter the lower bound for accepted input', 1, &
             'Enter valid real value', rlower, status)
 
!        Input real upper bound
           Call IO_INPR (.True., rlower, 1.7e38, .False., &
             'ENTER UPPER LIMIT OF INPUT RANGE FOR REAL', 1, &
             'Enter the upper bound for accepted input', 1, &
             'Enter valid real value', rupper, status)
 
        End If
 
     End If
 
!  Input user help text
     Call IO_WRITE ('INFO: Enter help text or user escape (\\) to exit', &
       status)
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\ Help text (up to 100 lines)'')')
     End If
 
     line = 0
     Do While (status .Eq. St_goodvalue .And. line .Lt. Max_help)
 
        line = line + 1
        Call IO_INPC (.False., 'TEXT', 1, 'Enter user help text', 1, &
          'Enter valid character string', 1, HELP(line), status)
 
     End Do
     line = line - 1
 
!  Re-set status
     status = St_goodvalue
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''\\'')')
        Write (io_unit_outmacro, '(''%!*\ Program variable'')')
     End If
 
     If (data_type .Eq. 'GRAPHICAL COORDINATE') Then
 
!     Input variable for X-coordinate
        Call IO_INPTOKEN (.True., 'X-COORDINATE VARIABLE NAME', 1, &
          'Enter name of variable to define', 1, 'Enter string', 101, &
          variable, status)
 
!     Input variable for Y-coordinate
        Call IO_INPTOKEN (.True., 'Y-COORDINATE VARIABLE NAME', 1, &
          'Enter name of variable to define', 1, 'Enter string', 101, &
          variable2, status)
 
     Else
 
!     Input variable from user
        Call IO_INPTOKEN (.True., 'ENTER VARIABLE NAME', 1, &
          'Enter name of variable to define', 1, 'Enter string', 101, &
          variable, status)
 
 
     End If
 
     If (io_output_macro) Then
        Write (io_unit_outmacro, '(''%!*\'')')
        Write (io_unit_outmacro, '(''%!*\ End of Definition'')')
        Write (io_unit_outmacro, '(''%!*\'')')
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Interactive input even during a macro
 
!  Store input macro status and set off
     macro_mode = io_input_macro
     io_input_macro = .False.
 
!  Store output macro mode and turn off
     out_macro = io_output_macro
     io_output_macro = .False.
 
!  Output text to highlight required input
     Call IO_WRITE (' ', status)
     Call IO_WRITE ('NOTE: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', status)
     Call IO_WRITE ('              INTERACTIVE INPUT ' // &
       'REQUIRED FOR THE MACRO', status)
     Call IO_WRITE ('      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
       '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', status)
 
!  Input interactively required value
     If (data_type .Eq. 'INTEGER VALUE') Then
 
        type = 'i'
 
!     Input an integer value
        If (gui) Then
 
           Call GS_INPI (bound, ilower, iupper, default, prompt, line, HELP, &
             1, 'Enter valid integer value', ivalue, status)
 
        Else
 
           Call IO_INPI (bound, ilower, iupper, default, prompt, line, HELP, &
             1, 'Enter valid integer value', ivalue, status)
 
        End If
 
     Else If (data_type .Eq. 'REAL VALUE') Then
 
        type = 'r'
 
!     Input a real value
        If (gui) Then
 
           Call GS_INPR (bound, rlower, rupper, default, prompt, line, HELP, &
             1, 'Enter valid real value', rvalue, status)
 
        Else
 
           Call IO_INPR (bound, rlower, rupper, default, prompt, line, HELP, &
             1, 'Enter valid real value', rvalue, status)
 
        End If
 
     Else If (data_type .Eq. 'LOGICAL VALUE') Then
 
        type = 'l'
 
!     Input a logical value
        If (gui) Then
 
           Call GS_INPL (.True., 0, 1, default, prompt, line, HELP, 1, &
             'Enter "YES" or "NO"', lvalue, status)
 
        Else
 
           Call IO_INPL (.True., 0, 1, default, prompt, line, HELP, 1, &
             'Enter "YES" or "NO"', lvalue, status)
 
        End If
 
     Else If (data_type .Eq. 'GRAPHICAL COORDINATE') Then
 
        type = 'r'
 
!     Input coordinate
        num_coordinates = 1
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, prompt, line, HELP, .False., 1, &
          num_coordinates, x_coordinate, y_coordinate, status)
 
        rvalue = x_coordinate
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Display coordinate
        Call GS_MARK (x_coordinate, y_coordinate, 1.0, status)
 
        Call GS_UPDATE (status)
 
     Else If (data_type .Eq. 'INPUT FILE' .And. gui) Then
 
        type = 's'
 
!     Select input file
        Call GS_FILESELECTION (1, 1, prompt, line, line, HELP, 1, .False., &
          retstat, cvalue, status)
        len_cvalue = Len_trim(cvalue)
 
!     Check for "CANCEL" button or a problem
        If (retstat .Ne. 0) Then
           status = St_escapevalue
        End If
 
     Else If (data_type .Eq. 'OUTPUT FILE' .And. gui) Then
 
        type = 's'
 
!     Select output file
        Call GS_FILESELECTION (1, 1, prompt, line, line, HELP, 3, .False., &
          retstat, cvalue, status)
        len_cvalue = Len_trim(cvalue)
 
!     Check for "CANCEL" button or a problem
        If (retstat .Ne. 0) Then
           status = St_escapevalue
        End If
 
     Else
 
        type = 's'
 
!     Input a character string value
        If (gui) Then
           Call GS_INPS (default, prompt, line, HELP, 1, &
             'Enter valid character string', 1, len_cvalue, cvalue, status)
        Else
           Call IO_INPS (default, prompt, line, HELP, 1, &
             'Enter valid character string', 1, len_cvalue, cvalue, status)
        End If
 
     End If
 
!  Check return status or user escape
     If (retstat .Ne. 0 .Or. status .Ne. St_goodvalue) Then
 
!     Output any error messages
        Call ST_OUT (status)
 
        If (status .Eq. St_escapevalue) Then
 
!        Re-set status
           Call ST_DEF_SYSTEM (status)
 
        End If
 
        If (macro_mode) Then
 
           Call IO_WRITE (' ', status)
           Call IO_WRITE ( 'NOTE: Macro terminated by user request.', status)
           Call IO_WRITE (' ', status)
           macro_mode = .False.
 
!        Close macro file
           Close (Io_unit_inmacro)
 
        End If
 
!     Check if sequence is running
        If (io_sequence) Then
 
           Call IO_WRITE (' ', status)
           Call IO_WRITE ( 'NOTE: Sequence terminated by user request.', &
             status)
           Call IO_WRITE (' ', status)
           io_sequence = .False.
        End If
 
        If (gui) Then
 
!        Output termination message
           Call LG_CLEAR (status)
           Call GS_FPROMPT (1, 1, 'MACRO ABORTED', status)
           Call GS_UPDATE (status)
 
        End If
 
     Else If (gui) Then
 
!     Output continuation message
        Call GS_FPROMPT (1, 1, 'CONTINUING MACRO', status)
        Call GS_UPDATE (status)
 
     End If
 
 
!**DEBUGING
!  Write (*, '(''cvalue = '', a)') cvalue
!  Write (*, '(''len_cvalue = '', i4)') len_cvalue
 
!  Set program variable
     Call IO_SET_VARIABLE (variable, type, ivalue, lvalue, rvalue, len_cvalue, &
       cvalue, retstat, status)
 
     If (data_type .Eq. 'GRAPHICAL COORDINATE') Then
 
!     Set program variable
        Call IO_SET_VARIABLE (variable2, type, ivalue, lvalue, y_coordinate, &
          len_cvalue, cvalue, retstat, status)
 
     End If
 
!  Re-set macro modes
     io_input_macro = macro_mode
     io_output_macro = out_macro
 
     End Subroutine F2D_QUESTION
!********1*********2*********3*********4*********5*********6*********7*********8
 

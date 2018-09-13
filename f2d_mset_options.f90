!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_mset_options.f90 *
!  *                      *
!  ************************
 
!+ F2D_MSET_OPTIONS - FIT2D: Multiple SET from command line OPTIONS
     Subroutine F2D_MSET_OPTIONS (fit2d_version, xmaxdat, ymaxdat, &
       data_defined, memory_exist, variance_arrays, output_graphics, gui, &
       shared_memory, memory_id, graphics_macro_mode, status)
!  Description:
!    Customises FIT2D according to input command
!    line options.
!  Keywords:
!    Options.Command~Line, Flags.Program, Program.Options
!  Method:
!    Call to lower level subroutines
!  Deficiencies:
!  Bugs:
!    None Known
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Dec-2014: V0.22 Debugging for Linux system (Hammersley)
!    14-Jan-2011: V0.21 Correct banner output options (Hammersley)
!    28-Jun-2010: V0.20 Changes for macro-mode (Hammersley)
!    17-Mar-2006: V0.19 Allow graphics window position to be set (Hammersley)
!    19-Nov-2003: V0.18 Option to set data defined (Hammersley)
!    12-Nov-2003: V0.17 Option of connecting to a shared memory segment
!      (Hammersley)
!    29-Oct-2003: V0.16 Option of re-directing input to command-line for pipe
!      sub-process control (Hammersley)
!    20-Sep-1999: V0.15 Increase maximum size of X-dimension for internal 
!      arrays (Hammersley)
!    13-Apr-1999: V0.14 Set default values of array sizes from internal 
!      data-base (Hammersley)
!    02-Sep-1997: V0.13 Automatically turn-off GUI, if "no graphics" is 
!      selected or if the graphics system fails to open the graphics output 
!      window (Hammersley)
!    14-Dec-1996: V0.12 Allow by-pass of banner (Hammersley)
!    04-Sep-1996: V0.11 Add "keyboard" option support (Hammersley)
!    21-Jan-1996: V0.10 Add fit2d version to argument list (Hammersley)
!    20-Jan-1996: V0.9 GUI option (Hammersley)
!    26-Jun-1995: V0.8 Check that X-11 window is open O.K. (Hammersley)
!    20-Jun-1995: V0.7 Change to GS graphics library (Hammersley)
!    24-May-1995: V0.6 Add more explanation of prompts (Hammersley)
!    14-May-1995: V0.5 Add default sizes for Daresbury MD scanner (Hammersley)
!    01-Mar-1995: V0.4 Option to not start graphics (Hammersley)
!    23-Feb-1995: V0.3 Call to produce graphics window program banner 
!      (Hammersley)
!    21-Jan-1995: V0.2 Set default array sizes to 512*512 elements (Hammersley)
!    07-Mar-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of FIT2D
!  Export:
     Integer, Intent(OUT) :: xmaxdat ! First dimension of program arrays
     Integer, Intent(OUT) :: ymaxdat ! Second dimension of program arrays
     Logical, Intent(OUT) :: data_defined ! .True., if the data array is
!      defined (for shared memory use)
     Logical, Intent(OUT) :: memory_exist ! .True., if memory array are to
!      be created
     Logical, Intent(OUT) :: variance_arrays ! .True., if variance arrays
!      are to be created
     Logical, Intent(OUT) :: output_graphics ! .True., if graphics output is
!      required
     Logical, Intent(OUT) :: gui ! .True., if graphical user interface
!      option is selected
     Logical, Intent(OUT) :: shared_memory ! .True., if shared memory is used
     Integer, Intent(OUT) :: memory_id ! Identifier of shared memory (if used)
     Logical, Intent(OUT) :: graphics_macro_mode ! .True., if graphics user 
!      interface is to be used in macro-mode
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.22' ! Version number
     Integer, Parameter :: Max_args = 30 ! Dimension size of "ARGUMENTS"
!  Local Variables:
     Character(Len = 20) :: token ! Token separated from input argument
     Character(Len = 20) :: x_token ! X-dimension in character form
     Character(Len = 20) :: y_token ! Y-dimension in character form
     Integer :: arg ! Loop variable for command line arguments
     Integer :: num_args ! Number of command line arguments
     Integer :: retstat ! "IO_INQ_IKEYVALUE" return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
!      3 = Problem converting character string to an integer value
     Integer :: times ! Position of multiplication sign in a character string
     Integer :: xdim ! The required X-dimension
     Integer :: xend_win ! X-pixel for end of output window
     Integer :: xstr_win ! X-pixel for start of output window
     Integer :: yend_win ! Y-pixel for end of output window
     Integer :: ystr_win ! Y-pixel for start of output window
     Integer :: ydim ! The required Y-dimension
     Logical :: ask_dimensions ! .True., if the user is to be asked
!      the required sizes for program dimensions
     Logical :: ask_variances ! .True., if the user is to be asked
!      if variance arrays are required
     Logical :: by_pass ! .True., if the banner page is to be by_passed
     Logical :: command_line ! .True., if command-line re-directed input
!      required
     Logical :: force_size ! .True., if graphics window size is to be forced
     Logical :: inmacro_open ! .True., if an input macro is open
     Logical :: convert ! .True., if a token has been successfully decoded
     Logical :: x_convert ! .True., if the token for the X-dimension has been 
!      successfully decoded
     Logical :: y_convert ! .True., if the token for the Y-dimension has been 
!      successfully decoded
!  Local Arrays:
     Character(Len = 256) :: ARGUMENTS(Max_args) ! Command line arguments
     Character(Len = 80) :: MESSAGE(7) ! User text
!  External Functions:
     Integer, External :: St_errorcode ! Return error code from status variable
     Character(Len = 80), External :: Io_upper ! Convert character strings
!      to upper case
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MSET_OPTIONS ' // Version)
     Else
 
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered f2d_mset_options'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set flags to default values
        ask_dimensions = .True.
        ask_variances = .True.
        xmaxdat = 1000
        ymaxdat = 1000
        variance_arrays = .False.
        output_graphics = .True.
        gui = .True.
        by_pass = .False.
        command_line = .False.
        shared_memory = .False.
        graphics_macro_mode = .False.

        Call IO_INQ_INMACRO (inmacro_open, status)

 
!     Read in command line arguments
        Call IO_GETARGS (Max_args, num_args, ARGUMENTS, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        Do arg = 2, Min(num_args, Max_args)
 
!        Search for program dimensions option
           If (ARGUMENTS(arg)(1: 4) .Eq. '-dim' .Or. ARGUMENTS(arg)(1: 4) .Eq. &
             '+dim' .Or. ARGUMENTS(arg)(1: 4) .Eq. '-DIM' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '+DIM') Then
 
 
!           Decode dimensions, an "x" separates the two values
 
!           Find position of first "x"
              times = Index(ARGUMENTS(arg)(5:), 'x') + 4
              If (times .Eq. 0) Then
                 times = Index(ARGUMENTS(arg)(5:), 'X') + 4
              End If
 
              If (times .Gt. 5) Then
 
!              Define symbol name and value
                 x_token = ARGUMENTS(arg)(5: times - 1)
                 y_token = ARGUMENTS(arg) (times + 1: Len_Trim(ARGUMENTS(arg)))
 
!              Convert tokens to integers
                 Call IO_TOKTI (x_token, x_convert, xdim, status)
                 Call IO_TOKTI (y_token, y_convert, ydim, status)
 
!              Provided BOTH tokens are converted O.K. Then set dimensions
                 If (x_convert .And. y_convert .And. xdim .Gt. 0 .And. &
                   ydim .Gt. 0) Then
                    ask_dimensions = .False.
                    xmaxdat = xdim
                    ymaxdat = ydim
                 Else
                    Call IO_WRITE ('WARNING: Command line option to ' // &
                      'set program array sizes could not be', status)
                    Call IO_WRITE ( '         decoded successfully', status)
                 End If
 
              End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Search for variance arrays option
           Else If (ARGUMENTS(arg)(1: 4) .Eq. '-var' .Or. ARGUMENTS(arg)(1: 4) &
             .Eq. '+var' .Or. ARGUMENTS(arg)(1: 4) .Eq. '-VAR' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '+VAR') Then
              ask_variances = .False.
              variance_arrays = .True.
 
!           Search for shared memory Identifier
           Else If (ARGUMENTS(arg)(1: 4) .Eq. '-id=' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '-ID=') Then
 
              shared_memory = .True.
 
!           Parse argument to get memory identifier
              token = ARGUMENTS(arg)(5: Len_Trim(ARGUMENTS(arg)))
              Call IO_TOKTI (token, convert, memory_id, status)
 
!           Search for define data option (for shared memory use */
           Else If (ARGUMENTS(arg)(1: 5) .Eq. '-data' .Or. &
             ARGUMENTS(arg)(1: 5) .Eq. '+data' .Or. ARGUMENTS(arg)(1: 5) .Eq. &
             '-DATA' .Or. ARGUMENTS(arg)(1: 5) .Eq. '+DATA') Then
              data_defined = .True.
 
!           Search for no graphics option
           Else If (ARGUMENTS(arg)(1: 5) .Eq. '-nogr' .Or. &
             ARGUMENTS(arg)(1: 5) .Eq. '+nogr' .Or. ARGUMENTS(arg)(1: 5) .Eq. &
             '-NOGR' .Or. ARGUMENTS(arg)(1: 5) .Eq. '+NOGR') Then
              output_graphics = .False.
              gui = .False.
 
           Else If (ARGUMENTS(arg)(1: 4) .Eq. '-gmm' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '+gmm' .Or. ARGUMENTS(arg)(1: 4) .Eq. &
             '-GMM' .Or. ARGUMENTS(arg)(1: 4) .Eq. '+GMM') Then
              output_graphics = .False.
              graphics_macro_mode = .True.
 
           Else If (ARGUMENTS(arg)(1: 9) .Eq. '-keyboard' .Or. &
             ARGUMENTS(arg)(1: 9) .Eq. '+keyboard' .Or. ARGUMENTS(arg)(1: 9) &
             .Eq. '-KEYBOARD' .Or. ARGUMENTS(arg)(1: 9) .Eq. '+KEYBOARD' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '-key' .Or. ARGUMENTS(arg)(1: 4) .Eq. &
             '+key' .Or. ARGUMENTS(arg)(1: 4) .Eq. '-KEY' .Or. &
             ARGUMENTS(arg)(1: 4) .Eq. '+KEY') Then
              gui = .False.
 
           Else If (ARGUMENTS(arg)(1: 3) .Eq. '-42') Then
              by_pass = .True.
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Information message, if the graphics system is not started
        If (output_graphics) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''f2d_mset_options: Opening Graphics'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Try to input previous window position
           Call IO_INQ_IKEYVALUE ('X_START_WINDOW', xstr_win, retstat, status)
           Call IO_INQ_IKEYVALUE ('Y_START_WINDOW', ystr_win, retstat, status)
           Call IO_INQ_IKEYVALUE ('X_END_WINDOW', xend_win, retstat, status)
           Call IO_INQ_IKEYVALUE ('Y_END_WINDOW', yend_win, retstat, status)

           
           force_size = retstat .Eq. 0

!        Initialise graphics
           Call GS_OPEN_GRAPHICS (force_size, xstr_win, ystr_win, &
             xend_win - xstr_win + 1, yend_win - ystr_win + 1, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''f2d_mset_options: After GS_OPEN_GRAPHICS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check status for failure to open graphics output window
           If (status .Eq. St_mod_lg + St_bad_x11open) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
!           Turn off graphics and GUI
              output_graphics = .False.
              gui = .False.
 
!           Output information text
              Call IO_WRITE ('WARNING: There was a problem ' // &
                'opening the graphics output window. This may be a', status)
              Call IO_WRITE ('        problem of the X-terminal ' // &
                'not accepting graphics output from the host', status)
              Call IO_WRITE ('        running FIT2D. If this is ' // &
                'the case it can be cured by using the xhost', status)
              Call IO_WRITE ('         program e.g. if ' // &
                'FIT2D is running on system ''esrf01'', then enter:', status)
              Call IO_WRITE ('         xhost +esrf01', status)
              Call IO_WRITE ('         on the local ' // &
                'machine which is being used as an X-terminal.', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('         If the problem does ' // &
                'not seem to be linked to ''xhost'' then check that', status)
              Call IO_WRITE ('         the "DISPLAY" ' // &
                'variable is properly defined. You can enter:', status)
              Call IO_WRITE ('         printenv DISPLAY', status)
              Call IO_WRITE ('         (for users of the ' // &
                '"C"-shell or "T"-shell) to check the current ', status)
              Call IO_WRITE ('         output destination.', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('         e.g. If the ' // &
                'X-terminal is called ''esrft1'' then the ' // &
                '"DISPLAY" variable', status)
              Call IO_WRITE ('         should be set to ' // &
                '''esrft1:0''. To achieve this enter:', status)
              Call IO_WRITE ('         setenv DISPLAY esrft1:0', status)
              Call IO_WRITE (' ', status)
              Call IO_WRITE ('         GRAPHICS OUTPUT and ' // &
                'the "GRAPHICS USER INTERFACE" are UNAVAILABLE.', status)
              Call IO_WRITE (' ', status)
 
           Else
 
!           Output graphics window program banner
              If ((.Not. by_pass) .And. (.Not. inmacro_open)) Then
!              If ((.Not. by_pass) .And. (.Not. gui) .And. &
!                (.Not. inmacro_open)) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Before F2D_BANNER'')')
!              Write (*, '(''inmacro_open = '', l1)') inmacro_open
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Call F2D_BANNER (fit2d_version, gui, status)

              End If
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''After F2D_BANNER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Else
 
           Call IO_WRITE ('INFO: The graphics system has ' // &
             'been turned off by a command line option', status)
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Before F2D_SCAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Enter file variables
!     Remove as ordered
!     Call F2D_SCAN (output_graphics, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''After F2D_SCAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Prompt for program dimension sizes if not in macro input mode
!     and if not already set by command line arguments
        If (.Not. inmacro_open) Then
 
           If (ask_dimensions) Then
 
              xmaxdat = 512
              ymaxdat = 512
              Call IO_INQ_IKEYVALUE ('DIM1_DATA', xmaxdat, retstat, status)
              Call IO_INQ_IKEYVALUE ('DIM2_DATA', ymaxdat, retstat, status)
 
              If (gui) Then
 
                 Call F2D_GUI_SIZES (xmaxdat, ymaxdat, memory_exist, &
                   variance_arrays, status)
 
              Else
 
!              Input sizes for program arrays
                 MESSAGE(1) = 'Here you are asked to define the ' // &
                   'size of the program arrays.  These will'
                 MESSAGE(2) = 'be used to store data "inside" ' // &
                   'FIT2D.  Normally you will want the arrays'
                 MESSAGE(3) = 'to be at least as large as the ' // &
                   'image data to be input. If the dimensions'
                 MESSAGE(4) = 'are larger this does little ' // &
                   'harm,  but is wasteful of system  resources.'
                 MESSAGE(5) = 'If the arrays are smaller then ' // &
                   'not all of  an image can be input at full'
                 MESSAGE(6) = 'resolution.  Some input options ' // &
                   'allow an image to  be re-binned on input'
                 MESSAGE(7) = 'or for a sub-region of the image ' // &
                   'to be input.'
                 Call IO_INPI (.True., 1, 1000000, .True., &
                   'X DIMENSION FOR ARRAYS', 7, MESSAGE, 1, &
                   'Must be an integer in the defined range', xmaxdat, status)
 
!              Set Y-dimension defaults for standard image sizes
                 If (xmaxdat .Eq. 1152) Then
                    ymaxdat = 1482
                 Else If (xmaxdat .Eq. 1200) Then
                    ymaxdat = 1200
                 Else If (xmaxdat .Eq. 1242) Then
                    ymaxdat = 1152
                 Else If (xmaxdat .Eq. 2048) Then
                    ymaxdat = 2464
                 Else If (xmaxdat .Eq. 2304) Then
                    ymaxdat = 2964
                 Else If (xmaxdat .Eq. 2816) Then
                    ymaxdat = 2052
                 Else
                    ymaxdat = xmaxdat
                 End If
 
                 Call IO_INPI (.True., 1, 1000000, .True., &
                   'Y DIMENSION FOR ARRAYS', 7, MESSAGE, 1, &
                   'Must be an integer in the defined range', ymaxdat, status)
 
                 If (ask_variances) Then
 
!                 Ask whether or not variance arrays are necessary
                    MESSAGE(1) = 'Arrays to hold estimates of ' // &
                      'data value  variances may be created or not.'
                    MESSAGE(2) = 'If error propagation is to be ' // &
                      'carried out these will be needed, but they'
                    MESSAGE(3) = 'double the program array ' // &
                      'requirements and  will  make  many  operations'
                    MESSAGE(4) = 'slower.  Unless you are going ' // &
                      'to need error progation  e.g. for weighted'
                    MESSAGE(5) = 'fitting, it is best to reply "NO".'
                    Call IO_INPL (.True., 0, 1, .True., &
                      'CREATE VARIANCE ARRAYS', 5, MESSAGE, 1, &
                      'You must enter "YES" or "NO"', variance_arrays, status)
 
                 End If
 
              End If
 
           End If
 
!        Save values in internal data-base
           Call IO_SET_IKEYVALUE ('DIM1_DATA', xmaxdat, retstat, status)
           Call IO_SET_IKEYVALUE ('DIM2_DATA', ymaxdat, retstat, status)
 
        End If
 
     End If
 
     End Subroutine F2D_MSET_OPTIONS
!********1*********2*********3*********4*********5*********6*********7*********8
 

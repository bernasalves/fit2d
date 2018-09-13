!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_gui_macros.f90 *
!  *                    *
!  **********************
 
!+ F2D_GUI_MACROS - FIT 2-D GUI MACROS and log file menu
     Subroutine F2D_GUI_MACROS (status)
!  Description:
!    FIT2D GUI creating and running macros, and openning and closing log file
!  Keywords:
!    File~Series.Processing, Macros.Creating, Macros.Running
!    Log~File.Creation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-May-2009: V0.5 Output messages if "OPEN LOG" pressed when a log file is
!      already open, or "CLOSE LOG" when a log file is not open (Hammersley)
!    17-Mar-2006: V0.4 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.3 Alter menu lay-out for landscape windows (Hammersley)
!    04-May-1999: V0.2 Put background message up, and rename routine
!      (Hammersley)
!    22-Jan-1998: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io_db.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_menu = 9 ! Dimension size of menu
     Integer, Parameter :: Max_message = 22 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 256), Save :: inmacro_file = 'fit2d.mac'
!      File name of file for input of macro definitions
     Character(Len = 256), Save :: outmacro_file = 'fit2d.mac'
!      File name of file for output of macro definitions
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 80) :: PROMPT(1) ! User prompt messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 9) / 'EXIT', '?', 'HELP', 'CREATE MACRO', &
       'STOP MACRO', 'RUN MACRO', 'RUN SEQUENCE', 'OPEN LOG FILE', &
       'CLOSE LOG FILE' /
     Data (MENUTXT(item), item = 1, 9) / 'EXIT: Exit FIT2D', &
       '?: This help on the menu choices', &
       'HELP: Help text on the "MACROS / LOG FILE" menu', &
       'CREATE MACRO: Start recording a macro in a named file ', &
       'STOP MACRO: Stop recording a macro', 'RUN MACRO: Run a macro once', &
       'RUN SEQUENCE: Run a macro repeatedly e.g. on a file sequence', &
       'OPEN LOG FILE: Open and starting recording a session log', &
       'CLOSE LOG FILE: Stop recording a session log' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_MACROS ' // Version)
        Return
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        update_menu = .True.
        Do While (continue)
 
           If (update_menu) Then
 
              MESSAGE(1) = ' '
              MESSAGE(2) = 'WELCOME TO THE MACRO /'
              MESSAGE(3) = ' '
              MESSAGE(4) = 'LOG FILE GUI'
              MESSAGE(5) = ' '
              MESSAGE(6) = 'Macros can be created'
              MESSAGE(7) = ' '
              MESSAGE(8) = 'with "CREATE MACRO" and'
              MESSAGE(9) = ' '
              MESSAGE(10) = '"STOP MACRO" and run'
              MESSAGE(11) = ' '
              MESSAGE(12) = 'with "RUN SEQUENCE".'
              MESSAGE(13) = ' '
              MESSAGE(14) = 'A log of the session'
              MESSAGE(15) = ' '
              MESSAGE(16) = 'can be created with '
              MESSAGE(17) = ' '
              MESSAGE(18) = '"OPEN LOG" and "CLOSE LOG"'
              MESSAGE(19) = ' '
              MESSAGE(20) = ' '
              MESSAGE(21) = ' '
              MESSAGE(22) = ' '
              Call GS_PPROMPT (Max_message, 22, MESSAGE, status)
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 20, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 3, 20, 9, status)
              End If
 
!           Re-draw menu
              PROMPT(1) = 'MACROS/LOG FILE MENU'
              Call GS_FMENU (1, 1, PROMPT, Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update menu
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''input_type, io_sequence = '', i6, l)')
!        :          input_type, io_sequence
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        If in sequence mode and "null" input returned, set
!        default command for macro sequences
           If (input_type .Eq. Gs_none .And. io_sequence) Then
              input_type = Gs_choice
              command = 'RUN SEQUENCE'
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
           If (input_type .Eq. Gs_resize) Then
 
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_menu = .False.
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_menu = .False.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_MACROSHELP (.True., status)
 
              Else If (command .Eq. 'CLOSE LOG FILE') Then
 
                 If (io_output_log) Then

!                 Close log file
                    Call IO_CLOSE_LOGFILE (status)
                 Else
                    MESSAGE(1) = 'Log file is not openned'
                    Call GS_FWARNING (1, 1, MESSAGE, status)
                 End If

              Else If (command .Eq. 'CREATE MACRO') Then
 
!              Open macro file for storing a macro
                 Call F2D_OPEN_OUTMACRO (outmacro_file, status)
 
              Else If (command .Eq. 'OPEN LOG FILE') Then
 
                 If (.Not. io_output_log) Then

!                 Open macro file for storing a macro
                    Call F2D_OPEN_LOG (status)
 
                 Else
                    MESSAGE(1) = 'A log file is already open'
                    Call GS_FWARNING (1, 1, MESSAGE, status)
                 End If

              Else If (command .Eq. 'RUN MACRO') Then
 
!              Open macro file
                 Call F2D_OPEN_INMACRO (inmacro_file, status)
 
              Else If (command .Eq. 'RUN SEQUENCE') Then
 
!              Run macro file repeatedly e.g. on a sequence of files
                 Call F2D_GUI_SEQUENCE (inmacro_file, status)
 
              Else If (command .Eq. 'STOP MACRO') Then
 
!              Close macro output file
                 Call IO_CLOSE_OUTMACRO (status)
 
!              Set default input macro file to the output macro file
                 inmacro_file = outmacro_file
 
              End If
 
           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Re-set status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_GUI_MACROS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

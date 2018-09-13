!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  *  f2d_gui.f90  *
!  *               *
!  *****************
 
!+ F2D_GUI - FIT 2-D Graphical User Interface
     Subroutine F2D_GUI (fit2d_version, gui, output_graphics, shared_memory, &
       memory_id, input_file, data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, log_file_open, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, status)
!max_vec_values, max_vectors, num_vectors, &
!       STR_VECTORS, END_VECTORS, VECTOR_TITLES, status) ! X_AXIS, Y_AXIS,
!      DATA, VARIANCES,  :    MX_AXIS, MY_AXIS, MDATA, MVARIANCES,
!  Description:
!    Top level of FIT2D GUI
!  Keywords:
!    FIT2D.GUI, GUI.FIT2D
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.36 Use result vectors data structure (Hammersley)
!    03-Nov-2014: V0.35 Add acknowledge to "EXIT" text (Hammersley)
!    15-Jan-2014: V0.34 Option for ID06 LVP data reduction (Hammersley)
!    24-Jul-2013: V0.33 Add "Pressure Calibration" interface (Hammersley)
!    21-Sep-2011: V0.32 Add "main" and "memory" (Hammersley
!    01-Jul-2010: V0.31 Don't draw menu in no graphics mode (Hammersley)
!    15-Nov-2006: V0.30 Add "SINGLE CRYSTAL" interface (Hammersley)
!    24-Apr-2006: V0.29 Changes to argument lists (Hammersley)
!    20-Mar-2006: V0.28 Add "INPUT OPTIONS" support (Hammersley)
!    14-Mar-2006: V0.27 Changes to argument lists (Hammersley)
!    10-Mar-2006: V0.26 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Feb-2006: V0.25 Changes to "F2D_GUI_SETUP" (Hammersley)
!    21-Mar-2005: V0.24 Add "RECIPROCAL SPACE MAPPING" inferface (Hammersley)
!    10-Mar-2004: V0.23 Change use of "VECTORS" array (Hammersley)
!    03-Mar-2004: V0.22 Add "SET-UP" interface (Hammersley)
!    04-May-1999: V0.21 Change names of subroutines (Hammersley)
!    29-Apr-1999: V0.20 Changes to "F2D_GUI_SAXS" (Hammersley)
!    17-Apr-1999: V0.19 Add "2-D FITTING" interface (Hammersley)
!    28-Oct-1998: V0.18 Add "VECTORS" arrays (Hammersley)
!    21-Jul-1998: V0.17 Add "TEST" interface (Hammersley)
!    31-Mar-1998: V0.16 Add "SAXS / GISAXS" interface (Hammersley)
!    22-Jan-1998: V0.15 Add "MACROS / LOG FILE" interface (Hammersley)
!    23-Dec-1997: V0.14 Add "FILE SERIES" interface (Hammersley)
!    30-May-1997: V0.13 Remove, temporarily, "DETECTOR CALIBRATION" 
!      interface (Hammersley)
!    21-Mar-1997: V0.12 Add "DETECTOR CALIBRATION" interface (Hammersley)
!    20-Feb-1997: V0.11 Transfer name of input file between interfaces 
!      (Hammersley)
!    14-Jan-1997: V0.10 Add "gui" variable, which is true if called from the 
!      GUI (Hammersley)
!    09-Dec-1996: V0.9 Temporarily remove "MFIT" interface for
!      user version (Hammersley)
!    04-Dec-1996: V0.8 Add "MFIT" interface (Hammersley)
!    21-Oct-1996: V0.7 Output "NOTE:" warning when the keyboard menu is 
!      entered (Hammersley)
!    09-Oct-1996: V0.6 Changes to input arguments and "F2D_KEYBOARD"
!      (Hammersley)
!    16-Apr-1996: V0.5 Interactive re-setting of bad status value (Hammersley)
!    12-Apr-1996: V0.4 Re-set user escape values (Hammersley)
!    11-Apr-1996: V0.3 Remove setting of false colour table (Hammersley)
!    25-Feb-1996: V0.2 Add default style the first time an interface is 
!      called (Hammersley)
!    15-Feb-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use LG_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc' ! Type definitions and constants
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of fit2d
!      being run
     Logical, Intent(IN) :: gui ! .True., if the graphical user interface is
!      being used (otherwise, this has been called through the keyboard 
!      interface)
     Logical, Intent(IN) :: output_graphics ! .True., if graphics are to be
!      output
     Logical, Intent(IN) :: shared_memory ! .True., if shared memory is to used
     Integer, Intent(IN) :: memory_id ! Identifier of shared memory (if used)
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variance_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Logical, Intent(INOUT) :: log_file_open ! .True., if a log file is open
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Real VARIANCES(xmaxdat, ymaxdat) ! The estimated variance values
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
!     Type(IMAGE_DATA), Intent(OUT) :: main ! Main data-set
!     Type(IMAGE_DATA), Intent(OUT) :: memory ! Memory data-set
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory
!      data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory
!      data in the Y-direction (metres)
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(INOUT) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(INOUT) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(INOUT) :: num_vectors ! Number of values defined in the
!    "time"-series for each vector
!     Integer, Intent(INOUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(INOUT) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!    Real VECTORS(max_vec_values, max_vectors) ! Multiple 1-D arrays of
!      vector values
!     Character(Len = *), Intent(INOUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.36' ! Version number
     Integer, Parameter :: Max_menu = 18 ! Dimension size of menu
     Integer, Parameter :: Max_message = 20 ! Dimension size of message
!  Local Variables:
     Character(Len = 40) :: command ! User choice
     Integer :: input_mode ! Mode for graphical input:
!    2 = Wait for event
!    10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
!    Integer lut_choice ! Number of LUT:
!      1 = Colour wheel
!      2 = Geographical
!      3 = Grey-scale black-white
!      4 = Inverse grey-scale
!      5 = Original (black-magenta-red-yellow-green-cyan-blue-white)
!      6 = Psychologically based colour table
!      7 = Repeating intensity colour table
!      8 = "Temperature" based colour scale
!      9 = Upside down geographical colour table
!    Integer num_levels ! Number of colour table levels for false "colour" 
!      image output
     Integer :: num_menu ! Number of choices in menu
     Integer :: temp_status ! Temporary version of "status"
     Logical :: confirm ! .True., if the user confirms an "EXIT"
     Logical :: continue ! .True., until user wants to exit
     Logical, Save :: first = .True. ! .True., the first time an interface
!      is called
     Logical :: reset ! .True., if "status" is to be reset to the good
!      status value
     Logical :: update ! .True., if the screen needs to be redrawn
     Real :: max_image ! Maximum image scaling limit (not used)
     Real :: min_image ! Minimum image scaling limit (not used)
     Real :: x_coordinate ! Input X-coordinate
     Real :: xmax_message ! The maximum X-coordinate for the message region
     Real :: xmin_message ! The minimum X-coordinate for the message region
     Real :: ymax_message ! The maximum Y-coordinate for the message region
     Real :: ymin_message ! The minimum Y-coordinate for the message region
     Real :: xmax_prompt ! The maximum X-coordinate for the prompt region
     Real :: xmin_prompt ! The minimum X-coordinate for the prompt region
     Real :: ymax_prompt ! The maximum Y-coordinate for the prompt region
     Real :: ymin_prompt ! The minimum Y-coordinate for the prompt region
     Real :: y_coordinate ! Input Y-coordinate
!  Local Arrays:
     Character(Len = 80) :: ERROR(2) ! User error messages
     Character(Len = 30), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80) :: MENU_TITLE(2) ! Menu title
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / &
       '?', &
       'HELP', &
       '2-D FITTING', &
       'FILE SERIES', &
       'ID06 LVP DATA REDUCTION', &
       'IMAGE PROCESSING (GENERAL)', &
       'KEYBOARD INTERFACE', &
       'MACROS / LOG FILE', &
       'MFIT (MULTIPLE 1-D FITTING)', &
       'ON-LINE CRYSTALLOGRAPHY', &
       'POWDER DIFFRACTION (2-D)', &
       'PRESSURE CALIBRATION', &
       'RECIPROCAL SPACE MAPPING', &
       'SAXS / GISAXS', &
       'SET-UP', &
       'SINGLE CRYSTAL', &
       'TEST', &
       'EXIT FIT2D' /
!    :  'DETECTOR CALIBRATION: Distortions: calibration and correction',
     Data (MENUTXT(item), item = 1, Max_menu) / &
       '?: This help text, describing the menu choices', &
       'HELP: Help text on this graphical menu', &
       '2-D FITTING: Versatile fitting of 2-D functions to data', &
       'FILE SERIES: Operations on a whole series of files', &
       'ID06 LVP DATA REDUCTION: Large volume press powder diffraction', &
       'IMAGE PROCESSING (GENERAL): Image I/O, display and manipulation', &
       'KEYBOARD INTERFACE: Classic FIT2D command line interface', &
       'MACROS / LOG FILE: Create and run macros, and create log files', &
       'MFIT (MULTIPLE 1-D FITTING): The functionality of MFIT program', &
       'ON-LINE CRYSTALLOGRAPHY: Live input, display, and statistics', &
       'POWDER DIFFRACTION (2-D): Tilt determination and integration', &
       'PRESSURE CALIBRATION: Compare spectra to predicted peaked positions', &
       'RECIPROCAL SPACE MAPPING: Transform series of diff. images', &
       'SAXS / GISAXS: Small Angle X-ray Scattering / Grazing Incidence', &
       'SET-UP: Control of general FIT2D set-up options', &
       'SINGLE CRYSTAL: Indexing of single/multiple xtals and more ...', &
       'TEST: Testing and simulation interface', &
       'EXIT FIT2D: Finish FIT2D session (au revoir)' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Arguments would appear to be reasonable, go ahead.
 
!     Change "EXIT MENU" text, if not in GUI
        If (.Not. gui) Then
           MENU(Max_menu) = 'EXIT MENU'
           MENUTXT(Max_menu) = 'EXIT MENU: Return to keyboard menu'
        End If
 
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        update = .True.
        num_menu = Max_menu
        continue = .True.
 
        Do While (continue)

           If (Lg_inq_open(status)) Then

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''F2D_GUI: LG open'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!           Find out GUI prompt area
              Call GS_INQ_GUIREGION (xmin_prompt, ymin_prompt, xmax_prompt, &
                ymax_prompt, status)
 
!           Find out message area
              Call GS_INQ_MESSAGE (xmin_message, ymin_message, xmax_message, &
                ymax_message, status)
 
              If (update) Then
 
!              Output welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO FIT2D GUI'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'SELECT SCIENTIFIC INTERFACE'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'FROM GIVEN CHOICES'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = '(OR CLICK ON "HELP" FOR INFORMATION)'
                 MESSAGE(9) = ' '
                 Call GS_PROMPT (.False., Max_message, 9, MESSAGE, &
                   xmin_message, ymin_message + (ymax_message - ymin_message) &
                   / 2.0, xmax_message, ymax_message, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_GUI: Before GS_MENU'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Draw menu
                 Call GS_SET_MENULAYOUT (1, 1, 20, 30, status)
                 MENU_TITLE(1) = 'FIT2D: SCIENTIFIC INTERFACES'
                 Call GS_MENU (1, 1, MENU_TITLE, Max_menu, num_menu, MENU, &
                   xmin_message, ymin_message, xmax_message, ymin_message + &
                   (ymax_message - ymin_message) / 2.0, status)
 
!              Force updating (this shouldn't be necessary, but ...) */
                 Call GS_UPDATE (status)
 
              End If
 
           End If

!        By default update
           update = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_GUI: Before GS_INP_MENUCHOICE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_GUI: command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

           If (input_type .Eq. Gs_resize) Then
 
              update = .True.
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update = .False.
 
              Else If (command .Eq. 'EXIT FIT2D' .Or. command .Eq. &
                'EXIT MENU') Then
 
                 If (gui) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''F2D_GUI: Before GS BACKGROUND'')') 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

                    Call GS_BACKGROUND (status)

!                 Output message (to cover menu)
                    MESSAGE(1) = '!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
                      '!!!!!!!!!!!!!!!!'
                    MESSAGE(2) = '!!! REQUEST TO EXIT FIT2D !!!'
                    MESSAGE(3) = '!!!  ARE YOU REALLY SURE  !!!'
                    MESSAGE(4) = '!!!!!!!!!!!!!!!!!!!!!!!!!!!' // &
                      '!!!!!!!!!!!!!!!!'
                    MESSAGE(5) = ' '
                    MESSAGE(6) = ' '
                    MESSAGE(7) = ' '
                    MESSAGE(8) = '    ACKNOWLEDGEMENTS'
                    MESSAGE(9) = 'Spline surface fitting uses ' // &
                      'FITPACK, written by Paul Dierckx'
                    MESSAGE(10) = 'CBF file I/O uses CBFLIB by Paul Ellis ' // &
                      'and Herb Bernstein.'
                    MESSAGE(11) = 'The "PRESSURE CALIBRATION" interface is ' // &
                      'based on the HPdiff' 
                    MESSAGE(12) = 'Applet by Sebastian Merkel.'
                    MESSAGE(13) = ' '
                    MESSAGE(14) = ' '
                    MESSAGE(15) = ' '
                    MESSAGE(16) = ' '
                    Call GS_PROMPT (.False., Max_message, 16, MESSAGE, &
                      xmin_message, ymin_message, xmax_message, ymax_message, &
                      status)
 
!                 Confirm exit
                    MESSAGE(1) = 'Enter "YES" to finish FIT2D session'
                    ERROR(1) = 'Click on "YES" or "NO"'
                    Call GS_INPL (.True., 0, 1, .True., 'CONFIRM EXIT', 1, &
                      MESSAGE, 1, ERROR, confirm, status)

                 Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''F2D_GUI: EXIT FIT2D no gui'')') 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!                 Output message (to cover menu)
                    MESSAGE(1) = ' '
                    MESSAGE(2) = ' '
                    MESSAGE(3) = 'CONTROL RETURNED TO'
                    MESSAGE(4) = ' '
                    MESSAGE(5) = 'KEYBOARD MENU'
                    MESSAGE(6) = ' '
                    MESSAGE(7) = ' '
                    Call GS_PROMPT (.False., Max_message, 7, MESSAGE, &
                      xmin_message, ymin_message, xmax_message, ymax_message, &
                      status)
                    Call GS_UPDATE (status)
 
                    confirm = .True.
                 End If
                 continue = .Not. confirm
                 update = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI: "EXIT FIT2D" section'')') 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Help on menu
                 Call F2D_GUIHELP (.True., status)
                 update = .True.
 
              Else If (command .Eq. '2-D FITTING') Then
 
                 Call F2D_GUI_2DFIT (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
!              Else If (command .Eq. 'DETECTOR CALIBRATION') Then
 
!              Detector calibration and correction menu
!              Call F2D_GUI_CALIBRATION (input_file,
!              :                data_defined, memory_exist, memory_defined,
!              :                variance_exist, mask_exist,
!              :                xmaxdat, ymaxdat, xnumdat, ynumdat,
!              :    X_AXIS, Y_AXIS, DATA, VARIANCES,
!              :                title, xlabel, ylabel, zlabel,
!              :                experiment%x_pixel_size, experiment%y_pixel_size,
!              :                xstrelm, ystrelm, xendelm, yendelm,
!              :                mxnumdat, mynumdat,
!              :    MX_AXIS, MY_AXIS, MDATA, MVARIANCES,
!              :                mxstrelm, mystrelm, mxendelm, myendelm,
!              :                mtitle, mxlabel, mylabel, mzlabel,
!              :                mx_pixel_size, my_pixel_size, status)
 
!              update = .True.
!              first = .False.
 
              Else If (command .Eq. 'SINGLE CRYSTAL') Then

!              Extreme crystallography interface
                 Call F2D_GUI_EXTREME_XTAL (input_options, &
                   input_file, data_defined, &
                   memory_exist, memory_defined, variance_exist, mask_exist, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, &
                   zlabel, experiment, xstrelm, ystrelm, &
                   xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'FILE SERIES') Then
 
!              File series processing interface
                 Call F2D_GUI_FS (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, results, status)
 
                 update = .True.
                 first = .False.
 
       
              Else If (command .Eq. 'ID06 LVP DATA REDUCTION') Then

                 Call F2D_GUI_ID06LVP (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.

              Else If (command .Eq. 'IMAGE PROCESSING (GENERAL)') Then
 
!              General purpose image processing interface
                 Call F2D_GUI_IP (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'KEYBOARD INTERFACE') Then
 
!              Output message in graphics window
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'NOTE: WITHIN THE KEYBOARD MENU MANY'
                 MESSAGE(3) = 'OPERATIONS PRODUCE OUTPUT IN THE MEMORY'
                 MESSAGE(4) = 'AND "EXCHANGE" MUST BE USED TO RECOVER'
                 MESSAGE(5) = 'THE RESULT. (THIS IS DONE AUTOMATICALLY'
                 MESSAGE(6) = 'WITHIN THE "GRAPHICAL USER INTERFACE").'
                 MESSAGE(7) = '"EXCHANGE" IS USUALLY THE DEFAULT'
                 MESSAGE(8) = 'COMMAND AFTER SUCH OPERATIONS. HOWEVER'
                 MESSAGE(9) = '"SIMPLE" COMMANDS PRODUCE OUTPUT IN THE'
                 MESSAGE(10) = 'CURRENT DATA ARRAY. "PLOT" OR "IMAGE"'
                 MESSAGE(11) = 'IS OFTEN THE DEFAULT COMMAND.  THE'
                 MESSAGE(12) = 'TABLES AT THE END OF THE REFERENCE MANUAL'
                 MESSAGE(13) = 'DESCRIBE THE OUTPUT OF ALL COMMANDS AND'
                 MESSAGE(14) = 'SHOULD BE USED WHEN THIS NOT CLEAR.'
                 MESSAGE(15) = ' '
                 Call GS_PROMPT (.False., 15, 15, MESSAGE, xmin_message, &
                   ymax_prompt, xmax_message, ymax_message, status)
 
!              User message
                 MESSAGE(1) = 'CONTROL TRANSFERRED TO TERMINAL WINDOW'
                 Call GS_FPROMPT (1, 1, MESSAGE, status)
 
!              Make sure that the display is updated
                 Call GS_UPDATE (status)
 
!              Classic FIT2D command line interface
                 Call F2D_KEYBOARD (fit2d_version, .True., output_graphics, &
                   shared_memory, memory_id, input_file, data_defined, &
                   memory_exist, memory_defined, variance_exist, mask_exist, &
                   log_file_open, xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, experiment, input_options, &
                   xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                   mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
                   mylabel, mzlabel, mx_pixel_size, my_pixel_size, results, &
                   status)

!                   max_vec_values, max_vectors, num_vectors, STR_VECTORS, &
!                   END_VECTORS, VECTOR_TITLES, status)
                 first = .False.
 
              Else If (command .Eq. 'MACROS / LOG FILE') Then
 
                 Call F2D_GUI_MACROS (status)
                 update = .True.
 
              Else If (command .Eq. 'MFIT (MULTIPLE 1-D FITTING)') Then
 
                 Call F2D_GUI_MFIT (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, results, status)
!max_vec_values, max_vectors, num_vectors, &
!                   STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'ON-LINE CRYSTALLOGRAPHY') Then
 
                 If (first) Then
 
!                 Set automatic "weak" diffraction peak scaing
                    Call GS_SET_IMAGESCALE (4, min_image, max_image, status)
 
                    first = .False.
 
                 End If
 
!              On-line / live update crystallographic interface
                 Call F2D_GUI_XTALLOGRAPHY (input_options, &
                   input_file, data_defined, &
                   memory_exist, memory_defined, variance_exist, mask_exist, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, &
                   zlabel, experiment, xstrelm, ystrelm, &
                   xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'POWDER DIFFRACTION (2-D)') Then
 
                 Call F2D_GUI_PD (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'PRESSURE CALIBRATION') Then
 
!              Pressure calibration interface
                 Call F2D_GUI_PRESSURECAL (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)

              Else If (command .Eq. 'RECIPROCAL SPACE MAPPING') Then
 
!                 MESSAGE(1) = 'This interface is not yet available'
!                 Call GS_FWARNING (1, 1, MESSAGE, status)

!              Input a series of diffraction images and transform
!              to a reciprocal space lattice
                 Call F2D_GUI_RECIPROCAL (input_file, data_defined, &
                   memory_exist, memory_defined, variance_exist, mask_exist, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, &
                   zlabel, experiment, input_options, xstrelm, ystrelm, &
                   xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   mx_pixel_size, my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'SAXS / GISAXS') Then
 
                 Call F2D_GUI_SAXS (input_options, &
                   input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'SET-UP') Then
 
                 Call F2D_GUI_SETUP (input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, input_options, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
                 update = .True.
                 first = .False.
 
              Else If (command .Eq. 'TEST') Then
 
                 Call F2D_GUI_TEST (input_file, data_defined, memory_exist, &
                   memory_defined, variance_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
                 update = .True.
                 first = .False.
 
              End If
 
           End If
 
!        Re-set user escape values
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
 
              temp_status = St_goodvalue
              MESSAGE(1) = 'YES: to reset "status" value, other exit program'
              ERROR(1) = 'Enter YES or NO'
              Call GS_INPL (.True., 0, 1, .True., &
                'Bad "status": RESET "status" ?', 1, MESSAGE, 1, ERROR, reset, &
                temp_status)
 
              If (reset) Then
                 Call ST_DEF_SYSTEM (status)
              Else
                 continue = .False.
              End If
 
           End If
 
        End Do
 
     End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_GUI: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_GUI
!********1*********2*********3*********4*********5*********6*********7*********8
 

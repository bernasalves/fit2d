!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_gui_setup.f90 *
!  *                   *
!  *********************
 
!+ F2D_GUI_SETUP - FIT 2-D GUI SET-UP menu
     Subroutine F2D_GUI_SETUP (input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, status)
!  Description:
!    FIT2D GUI for changing the program set-up
!  Keywords:
!    Set-Up.Interface, FIT2D.Set-up
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.15 Changes to "F2D_CLICK" (Hammersley)
!    20-Dec-2013: V0.14 Investigating error with "OPTIONS" option (Hammersley)
!    22-Jun-2006: V0.13 Set default file name for defaults file (Hammersley)
!    28-Apr-2006: V0.12 Output file name if loading data-base fails 
!      (Hammersley)
!    23-Mar-2006: V0.11 Rename "FILE INPUT GEOMETRY" option to "INPUT 
!      EXPERIMENTAL DETAILS" (Hammersley)
!    20-Mar-2006: V0.10 Add "INPUT OPTIONS" button (Hammersley)
!    17-Mar-2006: V0.9 Support for arbitrary aspect ratio windows (Hammersley)
!    10-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Feb-2006: V0.7 Add more possible items for experimental geometry input
!      (Hammersley)
!    22-Feb-2006: V0.6 Use data structure for experiemental geometry input
!      (Hammersley)
!    16-Feb-2006: V0.5 Option to input geometry from CIF/CBF file (Hammersley)
!    29-Jul-2005: V0.4 Add option to input program state from a named file 
!      (Hammersley)
!    26-Jul-2005: V0.3 Add option to output program state to a named file 
!      (Hammersley)
!    05-Mar-2004: V0.2 Add "OPTIONS" button and rename "PIXEL SIZES" to
!      "DEFAULT GEOMETRY" (Hammersley)
!    03-Mar-2004: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use MA_LIB
!     Use GS_LIB
!     Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
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
     Integer, Intent(INOUT) :: xmaxdat ! X-direction dimension for data arrays
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
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.15' ! Version number
     Integer, Parameter :: Max_menu = 9 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 256) :: file_name = 'fit2d.idb' ! User entered file
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Status return variable
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: geometry_defined ! .True.,  if geometry is defined
     Logical :: vertical_2theta ! .True., if detector rotation is vertical
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_beam ! Y-coordinate of beam centre in pixel coordinates
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 80) :: INFO(Max_message) ! User information
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  Data Structures:
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / &
       'EXIT', &
       '?', &
       'EXPERIMENTAL DETAILS: Input experiment set-up from CIF/CBF file',&
       'GEOMETRY', &
       'INPUT OPTIONS', &
       'OPTIONS', &
       'OVER-RIDE VALUES', &
       'SAVE FIT2D DEFAULTS', &
       'LOAD FIT2D DEFAULTS' /
     Data (MENUTXT(item), item = 1, Max_menu) / 'EXIT: Exit menu', &
       '?: This help on the menu choices', &
       'EXPERIMENTAL DETAILS: Input experiment set-up from CIF/CBF file',&
       'GEOMETRY: Alter diffraction geometry parameters', &
       'INPUT OPTIONS: Control input of auxiliary experimental information', &
       'OPTIONS: Image display control options menu', &
       'OVER-RIDE VALUES: Define over-riding experimental values', &
       'SAVE FIT2D DEFAULTS: Save program settings to a named file', &
       'LOAD FIT2D DEFAULTS: Load program default values from a file' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_SETUP ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_SETUP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image or 1-D plot
                 Call GS_MPLOT (.False.,  xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE FIT2D'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'SET-UP GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = ' '
                 Call GS_PPROMPT (Max_message, 6, MESSAGE, status)
 
              End If
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 15, 19, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 2, 15, 19, status)
              End If
 
!           Re-draw menu
              MESSAGE(1) = 'FIT2D SET-UP:'
              Call GS_FMENU (1, 1, MESSAGE, Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
           command = 'null'
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              If (data_defined) Then
 
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
 
              End If
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'GEOMETRY') Then
 
!              Input geometry
                 If (data_defined) Then
 
!                 Input experimental geometry and store in internal database
                    Call F2D_GEOMETRY (.True., xmaxdat, ymaxdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pDATA), %val(pXAXIS), &
                      %val(pYAXIS), title, xlabel, ylabel, zlabel, &
                      experiment, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .True.
                 update_menu = .True.

              Else If (command .Eq. 'EXPERIMENTAL DETAILS') Then

                 Call F2D_INPUT_EXPERIMENT (input_options, experiment, status)

              Else If (command .Eq. 'INPUT OPTIONS') Then

                 Call F2D_INPUT_OPTIONS (input_options, status)

              Else If (command .Eq. 'OPTIONS') Then

                 If (data_defined) Then

                    Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      experiment, variance_exist, xstrelm, ystrelm, &
                      xendelm, yendelm, print_type, status)

                 Else

                    MESSAGE(1) = 'Data must be defined for this option'
                    Call GS_FWARNING (1, 1, MESSAGE, status)

                 End If

                 update_menu = .True.
 
              Else If (command .Eq. 'OVER-RIDE VALUES') Then
 
!              Define default pixel sizes and whether to over-ride file
!              input values
                 Call F2D_DEF_GEOMETRY (experiment, status)
 
              Else If (command .Eq. 'SAVE FIT2D DEFAULTS') Then

!              Set experimental details to internal data-base
                 Call F2D_SET_EXPERIMENT (experiment, status)
 
!              Select file name
                 MESSAGE(1) = 'SELECT FILE TO CONTAIN VALUES'
                 INFO(1) = 'The file may or may not exist already'
                 Call GS_FILESELECTION (1, 1, MESSAGE, 1, 1, INFO, &
                   3, .False., retstat, file_name, status)
 
                 If (retstat .Eq. 0) Then
 
!                 Save internal data-base values to a selected file
                    Call IO_SAVE_DATABASE (file_name, retstat, status)
 
                    If (retstat .Gt. 0) Then
 
                       MESSAGE(1) = 'Problem outputting values to file'
                       Call GS_FWARNING (1, 1, MESSAGE, status)
 
                    End If
 
                 End If
 
              Else If (command .Eq. 'LOAD FIT2D DEFAULTS') Then
 
!              Select file name
                 MESSAGE(1) = 'SELECT FIT2D DEFAULT VALUES FILE'
                 INFO(1) = 'The default values file must exist'
                 Call GS_FILESELECTION (1, 1, MESSAGE, 1, 1, INFO, &
                   1, .False., retstat, file_name, status)
 
                 If (retstat .Eq. 0) Then
 
!                 Load internal data-base values from a selected file
                    Call IO_LOAD_DATABASE (file_name, retstat, status)
 
                    If (retstat .Gt. 0) Then
 
                       MESSAGE(1) = 'Problem inputting values from file:'
                       MESSAGE(2) = file_name
                       Call GS_FWARNING (2, 2, MESSAGE, status)
                    Else

!                    Get experimental details from internal data-base
                       Call F2D_INQ_EXPERIMENT (experiment, status)

                    End If
 
                 End If
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              MESSAGE(1) = 'DATA NEEDED, BUT NONE IS DEFINED'
              Call GS_FWARNING ( 1, 1, MESSAGE, status)
 
              update_menu = .True.
 
           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_GUI_SETUP
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_gui_ip.f90 *
!  *                *
!  ******************
 
!+ F2D_GUI_IP - FIT 2-D GUI Image Processing (General)
     Subroutine F2D_GUI_IP (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for general purpose image processing
!  Keywords:
!    Image~Processing, Processing~Image, Image.Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Dec-2014: V0.20 Add "MASK" option (Hammersley)
!    11-Dec-2014: V0.19 Changes to "F2D_GUI_MATHS" (Hammersley)
!    26-Nov-2014: V0.18 Changes to "F2D_CLICK" (Hammersley)
!    24-Apr-2006: V0.17 Add "INPUT_OPTIONS" structure (Hammersley)
!    14-Mar-2006: V0.16 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.15 Alter menu lay-out for landscape windows (Hammersley)
!    17-Feb-1999: V0.14 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    23-Jan-1998: V0.13 Changes to "F2D_GUI_INPUT" so that the user prompt text 
!      is set by the calling routine (Hammersley)
!    22-Jan-1998: V0.12 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    10-Mar-1997: V0.11 Changes with "F2D_GUI_ZSCALE" to cope with masked data
!      (Hammersley)
!    20-Feb-1997: V0.10 Transfer name of input file between interfaces 
!      (Hammersley)
!    25-Jan-1997: V0.9 Cater for "CANCEL" buttons (Hammersley)
!    08-Oct-1996: V0.8 Use "GS_INP_UNZOOM" to un-zoom 1-D and/or 2-D regions 
!      (Hammersley)
!    26-Sep-1996: V0.7 Add "OUTPUT" command (Hammersley)
!    03-Sep-1996: V0.6 Add "GEOMETRIC" transformation command and sub-menu 
!      (Hammersley)
!    27-Aug-1996: V0.5 Re-set menu layout just before re-drawing (Hammersley)
!    23-Aug-1996: V0.4 Changes to "F2D_REGION" (Hammersley)
!    12-Apr-1996: V0.3 Re-set user escape values (Hammersley)
!    19-Mar-1996: V0.2 Allow output and control of 1-D X/Y graphs (Hammersley)
!    25-Feb-1996: V0.1 Original, based on "F2D_GUI_XTALLOGRAPHY" (Hammersley)
!  Modules:
     Use IO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
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
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.20' ! Version number
     Integer, Parameter :: Max_menu = 18 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer :: dummy ! Dummy variable for "F2D_GUI_ZSCALE"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: window_format ! Format of workstation window:
!      Gs_A4portrait: A4 portrait orientation window
!      Gs_A4landscape: A4 landscape orientation window
!      Gs_square: Square window
!      Gs_fullscreen: Full screen window
!      Gs_arbitrary: Arbitrary aspect ratio window
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real height ! Height of window in page coordinates
     Real width ! Width of window in page coordinates
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!    position
!  Local Arrays:
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 18) / &
       'EXIT',      'DISPLAY',  'EXCHANGE',  'FILTER', &
       '?',         'FULL',     'GEOMETRIC', 'INPUT', &
       'HELP',      'OPTIONS',  'OUTPUT',    'MATHS', &
       'PRINT',     'MOVEMENT', 'UN-ZOOM',   'ZOOM IN', &
       'Z-SCALING', 'MASK' /
     Data (MENUTXT(item), item = 1, 10) / &
       'EXIT: Exit FIT2D', &
       'DISPLAY: Further graphical display possibilities', &
       'EXCHANGE: Swap current data with the "memory"', &
       'FILTER: Filtering and smoothing operations', &
       '?: This help on the menu choices', &
       'FULL: View image of full data', &
       'GEOMETRIC: Geometrical operations on data', &
       'INPUT: Input data from a file on disk', &
       'HELP: Help text on this graphical menu', &
       'OPTIONS: Further display control menu' /
     Data (MENUTXT(item), item = 11, 18) / &
       'OUTPUT: Save data in an output file', &
       'MATHS: Maths operations, with scalars or element by element', &
       'PRINT: Output current graphics to PostScript file', &
       'MOVEMENT: Easily controlled movement around an image', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Graphical region of interest definition', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'MASK: Defined masked-off regions of the image' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_IP ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_IP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_IP'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image
                 Call GS_MPLOT (.True., xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
                 print_type = 'masked_image'
 
              Else
 
!              Re-draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE GENERAL'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'IMAGE PROCESSING GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'PRESS "INPUT" TO '
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'SELECT A FILE'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_PPROMPT (Max_message, 10, MESSAGE, status)
 
              End If
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 9, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'IMAGE PROCESSING MENU', Max_menu, &
                num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_GUI_IP: Before GS_INP_MENUCHOICE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
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
                   xstrelm, ystrelm, xendelm, yendelm, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
              End If
 
           Else
 
!           Menu choice input
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
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
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'FILTER') Then
 
                 If (data_defined) Then
 
!                 Filtering and smoothing operations
                    Call F2D_GUI_FILTER ( data_defined, memory_exist, &
                      memory_defined, variances_exist, mask_exist, xmaxdat, &
                      ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, &
                      zlabel, experiment, xstrelm, ystrelm, &
                      xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
                      mzlabel, mx_pixel_size, my_pixel_size, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_IPHELP (.True., status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, ymaxdat, variances_exist, &
                   data_defined, input_file, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'GEOMETRIC') Then
 
                 If (data_defined) Then
 
!                 Interactive image display and manipulation
                    Call F2D_GUI_GEOMETRIC ( data_defined, memory_exist, &
                      memory_defined, variances_exist, mask_exist, xmaxdat, &
                      ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, &
                      zlabel, experiment, xstrelm, ystrelm, &
                      xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
                      mzlabel, mx_pixel_size, my_pixel_size, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'MASK') Then

                 If (data_defined) Then
 
                    Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                      ylabel, zlabel, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pMASK), status)
                    update_image = .False.
                 Else
                    data_warning = .True.
                 End If
                 update_menu = .True.
 
              Else If (command .Eq. 'MATHS') Then
 
                 If (data_defined) Then
 
!                 Interactive image display and manipulation
                    Call F2D_GUI_MATHS (experiment, data_defined, &
                      memory_exist, memory_defined, variances_exist, &
                      mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      mxnumdat, mynumdat, %val(pMXAXIS), &
                      %val(pMYAXIS), %val(pMDATA), %val(pMVARIANCES), &
                      mxstrelm, mystrelm, mxendelm, myendelm, &
                      mtitle, mxlabel, mylabel, &
                      mzlabel, mx_pixel_size, my_pixel_size, &
                      %val(pXAXIS), %val(pYAXIS), &
                      title, xlabel, ylabel, zlabel, &
                      xstrelm, ystrelm, xendelm, yendelm, &
                      %val(pDATA), %val(pVARIANCES), %val(pMASK), status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'MOVEMENT') Then
 
                 If (data_defined) Then
 
!                 Interactive image display and manipulation
                    Call F2D_IMAGE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pMASK), &
                      %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      variances_exist, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                      %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                      memory_defined, mx_pixel_size, my_pixel_size, &
                      print_type, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, experiment, &
                   variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variances_exist, &
                      experiment%x_pixel_size, experiment%y_pixel_size, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
                    Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                      ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 If (data_defined) Then
                    Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      variances_exist, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                      %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                      memory_defined, mx_pixel_size, my_pixel_size, &
                      print_type, status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, mask_exist, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, xlabel, ylabel, &
                      zlabel, variances_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 If (data_defined) Then
                    Call F2D_GUI_ZSCALE (.False., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), &
                      title, xlabel, ylabel, zlabel, xstrelm, ystrelm, &
                      xendelm, yendelm, experiment, .False., &
                      x_coordinate, y_coordinate, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              Call GS_FWARNING ( 1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
                status)
 
              update_menu = .True.
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
 
     End Subroutine F2D_GUI_IP
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

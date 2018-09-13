!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_gui_test.f90 *
!  *                  *
!  ********************
 
!+ F2D_GUI_TEST - FIT 2-D GUI TEST interface
     Subroutine F2D_GUI_TEST (input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for test purposes
!  Keywords:
!    Test.Interface, Interface.Test
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.7 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.6 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.4 Alter menu lay-out for landscape windows (Hammersley)
!    13-Nov-2000: V0.3 Add "TRIANGLE" command (Hammersley)
!    29-Oct-1998: V0.2 Add "POISSON" command (Hammersley)
!    21-Jul-1998: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!  Use MA_LIB
!  Use GS_LIB
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
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
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
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_menu = 6 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "masked_image"
!        "x/y graph"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / 'EXIT', '?', 'GAUSSIAN', &
       'DISPLAY', 'POISSON', 'TRIANGLE' /
     Data (MENUTXT(item), item = 1, Max_menu) / 'EXIT: Exit menu', &
       '?: This help on the menu choices', &
       'GAUSSIAN: Create 2-D Gaussian function data', &
       'DISPLAY: Further graphical display possibilities', &
       'POISSON: Add Poisson noise to the data', &
       'TRIANGLE: Define triangle region in image' /
!--------1---------2---------3---------4---------5---------6---------7--------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_TEST ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_TEST ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Inquire window format
           Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
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
                 MESSAGE(2) = 'WELCOME TO THE TEST'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'AND SIMULATION GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = ' '
                 Call GS_PPROMPT (Max_message, 6, MESSAGE, status)
 
              End If
 
           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 10, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 2, 12, 10, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, &
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
 
              Else If (command .Eq. 'GAUSSIAN') Then
 
!              Create data if not defined
                 If (.Not. data_defined) Then
 
                    data_defined = .True.
                    xnumdat = xmaxdat
                    ynumdat = ymaxdat
 
!                 Set active data region to be all the data
                    xstrelm = 1
                    ystrelm = 1
                    xendelm = xnumdat
                    yendelm = ynumdat
 
!                 Initialise data
                    Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, 0.0, %val(pDATA), status)
 
!                 Define axis values
                    Call F2D_AXES (xmaxdat, xnumdat, 0.5, 1.0, %val(pXAXIS), &
                      status)
                    Call F2D_AXES (ymaxdat, ynumdat, 0.5, 1.0, %val(pYAXIS), &
                      status)
 
!                 Define text items
                    title = 'Simulated Data'
                    xlabel = 'Columns'
                    ylabel = 'Rows'
                    zlabel = 'Intensity'
 
                 End If
 
!              Add Gaussian peak
                 Call MA_2DGAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), 1000.0, &
                   Real(xnumdat) / 2.0, Real(xnumdat) / 2.0, 75.0, 50.0, &
                   0.0, 5.0, 1, %val(pDATA), status)
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                   %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, variance_exist, &
                   experiment, xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, print_type, &
                   status)
 
              Else If (command .Eq. 'POISSON') Then
 
                 If (data_defined) Then
 
!                 Apply Poisson noise to data values
                    Call MA_2DPOISSON (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, -1.0, %val(pDATA), status)
 
                    If (variance_exist) Then
 
!                    Copy data values to variance array
                       Call MA_RCOPY (xmaxdat, ymaxdat, %val(pDATA), xstrelm, &
                         ystrelm, xendelm, yendelm, xmaxdat, ymaxdat, &
                         %val(pVARIANCES), status)
 
                    End If
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'TRIANGLE') Then
 
!              Create data if not defined
                 If (.Not. data_defined) Then
 
                    data_defined = .True.
                    xnumdat = xmaxdat
                    ynumdat = ymaxdat
 
!                 Set active data region to be all the data
                    xstrelm = 1
                    ystrelm = 1
                    xendelm = xnumdat
                    yendelm = ynumdat
 
!                 Initialise data
                    Call MA_RVALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, 0.0, %val(pDATA), status)
 
!                 Define axis values
                    Call F2D_AXES (xmaxdat, xnumdat, 0.5, 1.0, %val(pXAXIS), &
                      status)
                    Call F2D_AXES (ymaxdat, ynumdat, 0.5, 1.0, %val(pYAXIS), &
                      status)
 
!                 Define text items
                    title = 'Simulated Data'
                    xlabel = 'Columns'
                    ylabel = 'Rows'
                    zlabel = 'Intensity'
 
                 End If
 
!              Add user defined triangle area
                 Call F2D_TRIANGLE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, %val(pDATA), status)
 
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
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_GUI_TEST
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

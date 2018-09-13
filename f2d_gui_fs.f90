!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_gui_fs.f90 *
!  *                *
!  ******************
 
!+ F2D_GUI_FS - FIT 2-D GUI File Series processing
     Subroutine F2D_GUI_FS (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, results, status)
!  Description:
!    FIT2D GUI for file series processing
!  Keywords:
!    File~Series.Processing
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.14 Add results vector (Hammersley)
!    26-Nov-2014: V0.13 Changes to "F2D_CLICK" (Hammersley)
!    24-Apr-2006: V0.12 Add "INPUT_OPTIONS" structure (Hammersley)
!    17-Mar-2006: V0.11 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.10 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    18-Nov-2004: V0.9 Update welcome text (Hammersley)
!    16-Nov-2004: V0.8 Add "PROJECTIONS" command (Hammersley)
!    25-Feb-2004: V0.7 Alter menu lay-out for landscape windows (Hammersley)
!    09-Feb-2001: V0.6 Add "SUMMATION" command (Hammersley)
!    27-Oct-1998: V0.5 Add "INPUT (1-D)" command (Hammersley)
!    20-Feb-1998: V0.4 Add "INTEGRATE" command, and create mask if necessary 
!      (Hammersley)
!    26-Jan-1998: V0.3 Add "AVERAGE" command (Hammersley)
!    22-Jan-1998: V0.2 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    23-Dec-1997: V0.1 Original (Hammersley)
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
     Integer, Intent(INOUT) :: xmaxdat ! X-direction dimension for data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Y-direction dimension for data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
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
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.14' ! Version number
     Integer, Parameter :: Max_menu = 16 ! Dimension size of menu
     Integer, Parameter :: Max_message = 14 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
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
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window:
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / &
       'EXIT', &
       'AVERAGE', &
       'COMPOSITE', &
       'DISPLAY', &
       '?', &
       'INPUT (1-D)', &
       'INTEGRATE', &
       'EXCHANGE', &
       'HELP', &
       'PROJECTIONS', &
       'SUMMATION', &
       'OPTIONS', &
       'PRINT',  &
       'OUTPUT', &
       'STATISTICS', &
       'RESULTS' /
     Data (MENUTXT(item), item = 1, Max_menu) / &
       'EXIT: Exit FIT2D', &
       'AVERAGE: Create average from a series of files', &
       'COMPOSITE: Create composite image from a file series', &
       'DISPLAY: Further graphical display possibilities', &
       '?: This help on the menu choices', &
       'INPUT (1-D): Input a series of 1-D data-sets', &
       'INTEGRATE: Integrate n * 2-D regions to choice of n * 1-D scans', &
       'EXCHANGE: Swap current data with the "memory"', &
       'HELP: Help text on the "FILE SERIES" menu', &
       'PROJECTIONS: Projection regions onto 1-D "slices"', &
       'SUMMATION: Sum together a series of images from file', &
       'OPTIONS: Further display control menu', &
       'PRINT: Output current graphics a PostScript file', &
       'OUTPUT: Save data in an output file', &
       'STATISTICS: Calculate statistics for a region of all images', &
       'RESULTS: View, output, etc. results vectors'/
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_FS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_FS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
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
                 MESSAGE(2) = 'WELCOME TO THE FILE'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'SERIES PROCESSING GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = '"AVERAGE", "INTEGRATE"'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'calculate "PROJECTIONS'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = 'or create a "COMPOSITE"'
                 MESSAGE(11) = ' '
                 MESSAGE(12) = 'image from a a file series'
                 MESSAGE(13) = ' '
                 MESSAGE(14) = ' '
                 Call GS_PPROMPT (Max_message, 14, MESSAGE, status)
 
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
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
 
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
                   xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
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
 
              Else If (command .Eq. 'AVERAGE') Then
 
!              Input a series of files and average them
                 Call F2D_AVERAGE (input_options, &
                   .True., data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
                   %val(pDATA), %val(pVARIANCES), title, xlabel, ylabel, &
                   zlabel, experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, yendelm, experiment, status)
 
              Else If (command .Eq. 'COMPOSITE') Then
 
!              Input a series of files and create a composite image
                 Call F2D_COMPOSITE (input_options, &
                   data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_FSHELP (.True., status)
 
              Else If (command .Eq. 'INPUT (1-D)') Then
 
!              Input a series of files and integrate them
                 Call F2D_FSINPUT (input_options, data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
                   %val(pDATA), %val(pVARIANCES), %val(pMASK), title, xlabel, &
                   ylabel, zlabel, &
                   experiment%x_pixel_size, experiment%y_pixel_size, xstrelm, &
                   ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                   %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                   %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, myendelm, &
                   mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, experiment, status)
 
              Else If (command .Eq. 'INTEGRATE') Then
 
!              Input a series of files and integrate them
                 Call F2D_FSINTEGRATE (input_options, &
                   data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
                   %val(pDATA), %val(pVARIANCES), %val(pMASK), title, xlabel, &
                   ylabel, zlabel, experiment, xstrelm, &
                   ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                   %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                   %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, myendelm, &
                   mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
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
 
              Else If (command .Eq. 'PROJECTIONS') Then
 
!              Input a series of files and calculate projections
                 Call F2D_FSPROJECTIONS (input_options, &
                   data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
                   %val(pDATA), %val(pVARIANCES), %val(pMASK), title, xlabel, &
                   ylabel, zlabel, experiment, xstrelm, &
                   ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                   %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                   %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, myendelm, &
                   mtitle, mxlabel, mylabel, mzlabel, mx_pixel_size, &
                   my_pixel_size, status)
 
              Else If (command .Eq. 'SUMMATION') Then
 
!              Input a series of files and sum them
                 Call F2D_AVERAGE (input_options, &
                   .False., data_defined, memory_exist, &
                   memory_defined, variances_exist, mask_exist, xmaxdat, &
                   ymaxdat, xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), &
                   %val(pDATA), %val(pVARIANCES), title, xlabel, ylabel, &
                   zlabel, experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, yendelm, experiment, status)
 
              Else If (command .Eq. 'RESULTS') Then

!              Interactive viewing of results vectors
                 Call F2D_GUI_VECTORS (input_options, experiment, results, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, &
                   %val(pDATA), %val(pXAXIS), %val(pYAXIS), &
                   title, xlabel, ylabel, zlabel,  status)
 
              Else If (command .Eq. 'STATISTICS') Then

!              Calculate statistics within a defined region for every image 
!              in the series
                 Call F2D_FSSTATISTICS (input_options, data_defined, &
                   variances_exist, mask_exist, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   %val(pMASK), title, xlabel, ylabel, zlabel, &
                   experiment, xstrelm, ystrelm, xendelm, yendelm, results, &
                   status)

              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              Call GS_FWARNING (1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
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
 
     End Subroutine F2D_GUI_FS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

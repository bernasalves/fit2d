!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  *  f2d_display.f90 *
!  *                  *
!  ********************
 
!+ F2D_DISPLAY - FIT 2-D further DISPLAY possibilities for image sub-menu
     Subroutine F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, MASK, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, print_type, &
       status)
!  Description:
!    Graphical menu choice for further display for image display
!  Keywords:
!    Image~Display.Options, Options.Image~Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.24 Include "MASK", and use masking (Hammersley)
!    26-Nov-2014: V0.23 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.22 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.21 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.20 Alter menu lay-out for landscape windows (Hammersley)
!    20-May-1998: V0.19 Add "DISTANCE" command (Hammersley)
!    22-Jan-1998: V0.18 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    25-Jan-1997: V0.17 Cater for "CANCEL" buttons (Hammersley)
!    11-Nov-1996: V0.16 Re-instate "3-D LINES" option (Hammersley)
!    07-Nov-1996: V0.15 Remove "3-D LINES" option until fully tested 
!      (Hammersley)
!    24-Oct-1996: V0.14 Add "3-D LINES" option (Hammersley)
!    23-Aug-1996: V0.13 Changes to "F2D_GSTATISTICS" (Hammersley)
!    19-Mar-1996: V0.12 Allow output and control of 1-D X/Y graphs (Hammersley)
!    04-Feb-1996: V0.11 Add numbers command (Hammersley)
!    10-Nov-1995: V0.10 Change to "F2D_CLICK" (Hammersley)
!    24-Oct-1995: V0.9 Changes to "F2D_SLICE" (Hammersley)
!    27-Sep-1995: V0.8 Add "PROJECTION" command (Hammersley)
!    06-Sep-1995: V0.7 Allow the user to be able to click on an
!      image pixel and get immediate information (Hammersley)
!    19-Jul-1995: V0.6 Add Saturated pixels command (Hammersley)
!    26-Jun-1995: V0.5 Add memory parameters to argument list (Hammersley)
!    20-Jun-1995: V0.4 Change to GS graphics library (Hammersley)
!    24-Jan-1995: V0.3 Input pixel sizes from arguments (Hammersley)
!    04-Apr-1994: V0.2 Add "INTEGRATE" button to allow integration of
!      arbitrary areas (Hammersley)
!    16-Nov-1993: V0.1 Original, based on "F2D_OPTIONS" (Hammersley)
!  Modules:
     Use IO_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Character(Len = *), Intent(INOUT) :: print_type ! Type, of graphics to
!      print: supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory contains
!      data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.24' ! Version number
     Integer, Parameter :: Max_menu = 15 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: draw_command ! Type of graphics last drawn
     Integer :: dummy ! Dummy variable for argument lists
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window:
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80) :: MENU_TITLE(2) ! Title for menu
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / 'EXIT', 'ARC SLICE', &
       'CONTOUR PLOT', 'DISTANCE', '?', 'NUMBERS', 'PIXEL (X/Y)', &
       'PROJECTION', 'HELP', 'SATURATED', 'SLICE', 'STATISTICS', 'PRINT', &
       '3-D SURFACE', '3-D LINES' /
     Data (MENUTXT(item), item = 1, Max_menu) / 'EXIT: Exit from sub-menu', &
       'ARC SLICE: 1-D slice through a curved arc', &
       'CONTOUR PLOT: Data displayed with contour lines', &
       'DISTANCE: Measure distance between two points', &
       '?: Display list of commands with short description', &
       'NUMBERS: Print intensity values around a clicked coordinate', &
       'PIXEL (X/Y): Details of cursor defined coordinate', &
       'PROJECTION: Re-binning of a region onto an arbitrary line', &
       'HELP: Text explaining menu choices', &
       'SATURATED: Number of pixels equal or above a threshold value', &
       'SLICE: 1-D slice of line between two cursor input points', &
       'PRINT: Output current graphics to PostScript file', &
       'STATISTICS: Integration statistics, average, sigma etc.', &
       '3-D SURFACE: Data displayed as 3-D surface', &
       '3-D LINES: Simple 3-d line plot of data' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DISPLAY ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_DISPLAY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
        print_type = 'image'
        num_menu = Max_menu
 
!     Loop inputing menu commands until requested to stop
        draw_command = 'image'
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Draw image
              Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, MASK, &
                X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, &
                title, xlabel, ylabel, zlabel, status)
              print_type = 'image'
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 12, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 12, status)
              End If
 
!           Draw menu
              MENU_TITLE = 'DISPLAY MENU'
              Call GS_FMENU (1, 1, MENU_TITLE, Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
!        Get user to select between the available menu options
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''input_type = '', i)')
!        Write (*, '(''command = '', a)')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           If (input_type .Eq. Gs_resize) Then
 
!           Update is the default
              Continue
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'ARC SLICE') Then
 
!              Output 1-D arc slice as defined by user
                 Call F2D_ARCSLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, &
                   VARIANCES, title, xlabel, ylabel, zlabel, variances_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   mxnumdat, mynumdat, MX_AXIS, &
                   MY_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'CONTOUR PLOT') Then
 
!              Output data as a contour plot
                 If (yendelm - ystrelm .Gt. 1) Then
 
!                 PLOT IMAGE: Produce image display of data array
                    Call GS_2DCONTOUR (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                      xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                      ylabel, zlabel, status)
                    print_type = 'contour'
 
                 Else
 
!                 Plot X-Y graph
                    Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, X_AXIS, &
                      DATA(1, ystrelm), title, xlabel, zlabel, status)
                    print_type = 'x/y graph'
 
                 End If
 
!              Wait until user is ready
                 Call F2D_CONTINUE (print_type, xmaxdat, ymaxdat, X_AXIS, &
                   Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
                   variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   status)
 
              Else If (command .Eq. 'DISTANCE') Then
 
!              Input two coordinates and calculate distance
                 Call F2D_DISTANCE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, &
                   title, xlabel, ylabel, zlabel, &
                   experiment%x_pixel_size, experiment%y_pixel_size, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the DISPLAY menu
                 Call F2D_DISPLAYHELP (.True., status)
 
              Else If (command .Eq. 'NUMBERS') Then
 
!              Output pixel intensities around clicked spots
                 Call F2D_NUMBERS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, &
                   ylabel, zlabel, status)
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'PIXEL (X/Y)') Then
 
!              Output values associated with user defined coordinates
                 Call F2D_PIXELXY (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
                   xlabel, ylabel, zlabel, variances_exist, experiment, status)
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'PROJECTION') Then
 
!              Output 1-D projection of a region onto a line
                 Call F2D_PROJECTION (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
                   xlabel, ylabel, zlabel, variances_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, &
                   MVARIANCES, mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
                   mxlabel, mylabel, mzlabel, memory_defined, mx_pixel_size, &
                   my_pixel_size, status)
 
              Else If (command .Eq. 'SATURATED') Then
 
!              Number of saturated pixels
                 Call F2D_SATURATED (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, DATA, status)
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'SLICE') Then
 
!              Output 1-D slice as defined by user
                 Call F2D_SLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, VARIANCES, &
                   title, xlabel, ylabel, zlabel, variances_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   mxnumdat, mynumdat, MX_AXIS, &
                   MY_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'STATISTICS') Then
 
!              Statistics of user defined polygon region
                 Call F2D_GSTATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, &
                   title, xlabel, ylabel, zlabel, status)
 
              Else If (command .Eq. '3-D SURFACE') Then
 
                 If (yendelm-ystrelm .Gt. 1) Then
 
!                 3-D plot: Produce 3-d surface display of data array
                    Call F2D_3DSURFACE (print_type, xmaxdat, ymaxdat, DATA, &
                      X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, &
                      title, xlabel, ylabel, zlabel, status)
 
                 Else
 
!                 Plot X-Y graph
                    Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, X_AXIS, &
                      DATA(1, ystrelm), title, xlabel, zlabel, status)
 
                 End If
 
              Else If (command .Eq. '3-D LINES') Then
 
                 If (yendelm - ystrelm .Ge. 1) Then
 
!                 3-D line plot
                    Call GS_3DLINES (.True., xmaxdat, ymaxdat, DATA, X_AXIS, &
                      Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, &
                      xlabel, ylabel, zlabel, status)
                    print_type = '3-d lines'
 
                 Else
 
!                 Plot X-Y graph
                    Call GS_XYSGRAPH (xmaxdat, xstrelm, xendelm, X_AXIS, &
                      DATA(1, ystrelm), title, xlabel, zlabel, status)
 
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 Call F2D_PRINT (.True., print_type, .False., xmaxdat, &
                   ymaxdat, X_AXIS, Y_AXIS, DATA, VARIANCES, dummy, title, &
                   xlabel, ylabel, zlabel, variances_exist, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              End If
 
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
 
     End Subroutine F2D_DISPLAY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

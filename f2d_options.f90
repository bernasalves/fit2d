!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_options.f90 *
!  *                 *
!  *******************
 
!+ F2D_OPTIONS - FIT 2-D further OPTIONS for image sub-menu
     Subroutine F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, experiment, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
       print_type, status)
!  Description:
!    Graphical menu choice for further options for image display
!  Keywords:
!    Image~Display.Options, Options.Image~Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.22 Changes to "F2D_CLICK" (Hammersley)
!    20-Dec-2013: V0.21 Investigating error with the option (Hammersley)
!    23-Mar-2006: V0.20 Use "LUT_STYLE" structure (Hammersley)
!    17-Mar-2006: V0.19 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.18 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.17 Alter menu lay-out for landscape windows (Hammersley)
!    28-Mar-1998: V0.16 Add "CURVE STYLES" option (Hammersley)
!    03-Mar-1998: V0.15 Use "F2D_ROTATELUT" to rotate look-up table 
!      (Hammersley)
!    10-Mar-1997: V0.14 Changes to "F2D_GUI_ZSCALE" to cope with masked data 
!      (Hammersley)
!    25-Jan-1997: V0.13 Cater for "CANCEL" buttons (Hammersley)
!    03-Sep-1996: V0.12 Add control of LUT output (Hammersley)
!    19-Mar-1996: V0.11 Allow output and control of 1-D X/Y graphs (Hammersley)
!    31-Jan-1996: V0.10 Add "ASPECT RATIO" command (Hammersley)
!    10-Nov-1995: V0.9 Change to "F2D_CLICK" (Hammersley)
!    28-Sep-1995: V0.8 Add "GRID" option (Hammersley)
!    12-Sep-1995: V0.7 Changes to argument list of "F2D_GUI_ZSCALE" (Hammersley)
!    07-Sep-1995: V0.6 Update image, if re-sized during LUT rotation 
!      (Hammersley)
!    06-Sep-1995: V0.5 Allow the user to be able to click on an
!      image pixel and get immediate information (Hammersley)
!    20-Jun-1995: V0.4 Change to GS graphics library (Hammersley)
!    10-Dec-1993: V0.3 Option to change title and axis labels (Hammersley)
!    13-Oct-1993: V0.2 Use index 0 and 1 for button text and interior colour 
!      (Hammersley)
!    19-Sep-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'gs.inc' ! Graphics System definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Character(Len = *), Intent(INOUT) :: print_type ! Type, of graphics to
!      print: supported types are:
!        'banner'
!        'image'
!        'contour'
!        'x/y graph'
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.22' ! Version number
     Integer, Parameter :: Max_menu = 14 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Logical :: continue ! .True., until user wants to exit
     Logical :: correct_aspect ! .True., if images are to be displayed
!      automatically in the correct aspect ratio
     Logical :: current_aspect ! .True., if images were previously to
!      be displayed automatically in the correct aspect ratio
     Integer :: dummy ! Dummy variable for "F2D_GUI_ZSCALE"
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User information
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE_LUT ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 14) / 'EXIT', '?', 'COLOURS', 'CURVE STYLES', &
       'GRID', 'POSITION', 'ROTATE LUT', 'TITLE', 'X-AXIS LABEL', &
       'Y-AXIS LABEL', 'Z-AXIS LABEL', 'Z-SCALING', 'ASPECT RATIO', 'NO LUT' /
     Data (MENUTXT(item), item = 1, 14) / 'EXIT: Exit from sub-menu', &
       '?: Display list of commands with short description', &
       'COLOURS: Choice of false colour intensity tables', &
       'CURVE STYLES: Set display style for curves', &
       'GRID: User choice of horizontal and vertical grid lines', &
       'POSITION: Graphical input of graph/image limits', &
       'ROTATE LUT: GUI rotation of false colour look-up table', &
       'TITLE: Change title of graphics', &
       'X-AXIS LABEL: Change X-axis label of graphics', &
       'Y-AXIS LABEL: Change Y-axis label of graphics', &
       'Z-AXIS LABEL: Change Z-axis label (intensity) of graphics', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'ASPECT RATIO: Control automatic correct aspect ratio (or not)', &
       'NO LUT: Don''t add the look-up table to the image display' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OPTIONS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_OPTIONS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Find out style for LUT output
        Call GS_INQ_LUTSTYLE (STYLE_LUT, status)
        If (STYLE_LUT%draw) Then
           MENU(14) = 'NO LUT'
           MENUTXT(14) = 'NO LUT: Don''t add the look-up table ' // &
             'to the image display'
        Else
           MENU(14) = 'DRAW LUT'
           MENUTXT(14) = 'DRAW LUT: Add a look-up table ' // &
             'to the image display'
        End If
 
!     Loop inputing menu commands until requested to stop
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Redraw image
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
              print_type = 'image'
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 30, 12, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 30, 12, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'FIT2D: OPTIONS MENU', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        By default update
           update_image = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
!           Default is update
              Continue
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else If (input_type .Eq. Gs_choice) Then
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'NULL') Then
 
                 Continue
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'COLOURS') Then
 
!              Graphical input of colour table choice
                 Call F2D_COLOURS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
                   variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   status)
                 update_image = .False.
 
              Else If (command .Eq. 'CURVE STYLES') Then
 
                 Call GS_INP_CURVESTYLES (.True., status)
                 update_image = .True.
 
              Else If (command .Eq. 'DRAW LUT') Then
 
!              Set LUT to be drawn
                 STYLE_LUT%draw = .True.
                 Call GS_SET_LUTSTYLE (STYLE_LUT, status)
                 MENU(14) = 'NO LUT'
                 MENUTXT(14) = 'NO LUT: Don''t add the look-up ' // &
                   'table to the image display'
 
              Else If (command .Eq. 'GRID') Then
 
!              Graphical input of choice of grid lines
                 Call F2D_GUI_GRID (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                   xstrelm, ystrelm, xendelm, yendelm, experiment, status)
 
              Else If (command .Eq. 'NO LUT') Then
 
!              Set LUT not to be drawn
                 STYLE_LUT%draw = .False.
                 Call GS_SET_LUTSTYLE (STYLE_LUT, status)
                 MENU(14) = 'DRAW LUT'
                 MENUTXT(14) = 'DRAW LUT: Add the look-up ' // &
                   'table to the image display'
 
              Else If (command .Eq. 'POSITION') Then
 
!              Graphical input of graph page position
                 Call F2D_POSITION (status)
 
              Else If (command .Eq. 'ROTATE LUT') Then
 
!              User controlled rotation of colour table
                 Call F2D_ROTATELUT ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
                   zlabel, variances_exist, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
                 update_image = .False.
 
              Else If (command .Eq. 'TITLE') Then
 
!              Input new title for graphics
                 Call GS_INPC (.True., 'ENTER TITLE FOR IMAGE', 1, &
                   'Enter new title', 1, 'Enter valid characters', 1, title, &
                   status)
 
              Else If (command .Eq. 'X-AXIS LABEL') Then
 
!              Input new X-axis label for graphics
                 Call GS_INPC (.True., 'ENTER TEXT FOR X-AXIS LABEL', 1, &
                   'Enter new X-axis label', 1, 'Enter valid characters', 1, &
                   xlabel, status)
 
              Else If (command .Eq. 'Y-AXIS LABEL') Then
 
!              Input new Y-axis label for graphics
                 Call GS_INPC (.True., 'ENTER TEXT FOR Y-AXIS LABEL', 1, &
                   'Enter new Y-axis label', 1, 'Enter valid characters', 1, &
                   ylabel, status)
 
              Else If (command .Eq. 'Z-AXIS LABEL') Then
 
!              Input new Z-axis label for graphics
                 Call GS_INPC (.True., 'ENTER TEXT FOR Z-AXIS LABEL', 1, &
                   'Enter new Z-axis label', 1, 'Enter valid characters', 1, &
                   zlabel, status)
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 Call F2D_GUI_ZSCALE (.False., xmaxdat, ymaxdat, xnumdat, &
                   ynumdat, X_AXIS, Y_AXIS, DATA, dummy, title, xlabel, &
                   ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                   experiment, .False., x_coordinate, y_coordinate, status)
                 update_image = .False.
 
              Else If (command .Eq. 'ASPECT RATIO') Then
 
!              Inquire current aspect ratio control option
                 Call GS_INQ_IASPECT (current_aspect, status)
 
!              User choice of aspect ratio
                 MESSAGE(1) = 'Enter  "YES"  if  you want image ' // &
                   'display with automatic correct aspect'
                 MESSAGE(2) = 'ratios i.e. the pixels are ' // &
                   'square. Enter "NO" to use all the available'
                 MESSAGE(3) = 'display region.  This  may  ' // &
                   'result in non-square pixels,  but  may  be'
                 MESSAGE(4) = 'preferable for very non-square images.'
                 Call GS_INPL (.True., 0, 1, .True., &
                   'AUTOMATIC CORRECT ASPECT RATIO IMAGE DISPLAY', 4, MESSAGE, &
                   1, 'Enter "YES" on "NO"', correct_aspect, status)
 
                 If (correct_aspect .Neqv. current_aspect) Then
 
!                 Set aspect ratio control option
                    Call GS_SET_IASPECT (correct_aspect, status)
 
                 Else
 
!                 No change
                    update_image = .False.
 
                 End If
 
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
 
     End Subroutine F2D_OPTIONS
!********1*********2*********3*********4*********5*********6*********7*********8

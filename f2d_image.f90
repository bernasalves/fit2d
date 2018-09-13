!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_image.f90 *
!  *               *
!  *****************
 
!+ F2D_IMAGE - FIT 2-D IMAGE display (interactive)
     Subroutine F2D_IMAGE (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, MASK, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, print_type, &
       status)
!  Description:
!    1. Outputs ROI as pixel image
!    2. The user can change the image with an interactive graphical
!    menu
!  Keywords:
!    Image~Display, Display~Image
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.25 Include "MASK" (Hammersley)
!    26-Nov-2014: V0.24 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.23 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.22 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.21 Alter menu lay-out for landscape windows (Hammersley)
!    22-Jan-1998: V0.20 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    08-Oct-1996: V0.19 Use "GS_INP_UNZOOM" to un-zoom 1-D and/or 2-D regions 
!      (Hammersley)
!    23-Aug-1996: V0.18 Changes to "F2D_REGION" (Hammersley)
!    19-Mar-1996: V0.17 Allow output and control of 1-D X/Y graphs (Hammersley)
!    01-Nov-1995: V0.16 Output message at end of interactive session 
!      (Hammersley)
!    26-Oct-1995: V0.15 Add spy-glass output to "ZOOM-IN" option (Hammersley)
!    12-Sep-1995: V0.14 Obtain information on GUI prompt position (Hammersley)
!    06-Sep-1995: V0.13 Allow the user to be able to click on an
!      image pixel and get immediate information (Hammersley)
!    31-Aug-1995: V0.12 Force image update at exit of routine (Hammersley)
!    26-Jun-1995: V0.11 Memory is now passed through to "f2d_display" 
!      (Hammersley)
!    20-Jun-1995: V0.10 Change to GS graphics library (Hammersley)
!    24-Jan-1995: V0.9 Input pixel sizes from arguments (Hammersley)
!    19-Oct-1994: V0.8 Change "BIGGER" command to "UN-ZOOM" (Hammersley)
!    31-Mar-1994: V0.7 Change "G-REGION" command to "ZOOM IN" (Hammersley)
!    08-Dec-1993: V0.6 Testing further display options (Hammersley)
!    16-Nov-1993: V0.5 Add further display options menu (Hammersley)
!    15-Nov-1993: V0.4 Add print option (Hammersley)
!    13-Oct-1993: V0.3 Use index 0 and 1 for button text and
!      interior colour (Hammersley)
!    19-Sep-1993: V0.2 Faster movement, more movement choice, and
!      further options (Hammersley)
!    20-Aug-1993: V0.1 Original (Hammersley)
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
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
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
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
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
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory contains data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
     Character(Len = *), Intent(OUT) :: print_type ! Type, of graphics to
!      print: supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.25' ! Version number
     Integer, Parameter :: Max_menu = 25 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: dummy ! Dummy variable for arguments lists
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: xnumelm ! Number of X-elements in ROI
     Integer :: ynumelm ! Number of Y-elements in ROI
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window:
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 10), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80) :: MENU_TITLE(2) ! Menu title
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(2) ! User message
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / 'EXIT', '?', 'FAR-LEFT', 'UN-ZOOM', &
       'FULL', 'L-TOP', 'L-UP', 'LEFT', 'L-DOWN', 'L-BOTTOM' /
     Data (MENU(item), item = 11, 20) / 'TOP', 'UP', 'CENTRE', 'DOWN', &
       'BOTTOM', 'R-TOP', 'R-UP', 'RIGHT', 'R-DOWN', 'R-BOTTOM' /
 
     Data (MENU(item), item = 21, 25) / 'OPTIONS', 'ZOOM IN', 'FAR-RIGHT', &
       'DISPLAY', 'PRINT' /
 
     Data (MENUTXT(item), item = 1, 10) / 'EXIT: Exit from sub-menu', &
       '?: Display list of commands with short description', &
       'FAR-LEFT: Move to far left of data', &
       'UN-ZOOM: Make display region bigger', &
       'FULL: Make display region cover all defined data', &
       'L-TOP: Move to far left/top of data', 'L-UP: Move left and up', &
       'LEFT: Move to the left', 'L-DOWN: Move left and down', &
       'L-BOTTOM: Move to far left/bottom of data' /
     Data (MENUTXT(item), item = 11, 20) / &
       'TOP: Move vertically to top of data', 'UP: Move up', &
       'CENTRE: Move to centre of data', 'DOWN: Move down', &
       'BOTTOM: Move vertically to bottom of data', &
       'R-TOP: Move to far right/top of data', 'R-UP: Move right and up', &
       'RIGHT: Move right and up', 'R-DOWN: Move right and down', &
       'R-BOTTOM: Move to far right/bottom of data' /
     Data (MENUTXT(item), item = 21, 25) / &
       'OPTIONS: Further display control menu', &
       'ZOOM IN: Graphical region definition', &
       'FAR-RIGHT: Move horizontally to right edge of data', &
       'DISPLAY: Further graphical display possibilities', &
       'PRINT: Output current graphics to PostScript file' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_IMAGE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_IMAGE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm.Gt.xmaxdat .Or. xendelm.Lt.xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm.Gt.ymaxdat .Or. yendelm.Lt.ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_IMAGE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Set menu layout style
        num_menu = Max_menu
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
 
        update_image = .True.
        update_menu = .True.
        continue = .True.
        Do While (continue)
 
!        Change graphics output if required
           If (update_image) Then
 
!           Redraw image
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
              print_type = 'image'
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Make sure that the menu-layout is correct
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 5, 12, 9, status)
              End If
 
!           Draw menu
              MENU_TITLE(1) = 'not used'
              Call GS_FMENU (1, 0, MENU_TITLE, Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)

!        By default update
           update_image = .True.
           update_menu = .True.
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity or
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
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
 
              Else If (command .Eq. 'FAR-LEFT') Then
 
!              Look far-left
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = 1
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'L-TOP') Then
 
!              Look far-left/top
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = 1
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = ynumdat
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'L-UP') Then
 
!              Look left and up
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = Max(1, xstrelm - xnumelm / 2)
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = Min(ynumdat, yendelm + ynumelm / 2)
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'LEFT') Then
 
!              Look left
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = Max(1, xstrelm - xnumelm / 2)
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
 
              Else If (command .Eq. 'L-DOWN') Then
 
!              Look left and down
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = Max(1, xstrelm - xnumelm / 2)
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = Max(1, ystrelm - ynumelm / 2)
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'L-BOTTOM') Then
 
!              Look far-left/bottom
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = 1
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = 1
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'TOP') Then
 
!              Move vertically to top edge of data
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = ynumdat
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'UP') Then
 
!              Look up
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = Min(ynumdat, yendelm + ynumelm / 2)
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'CENTRE') Then
 
!              Move to centre of data
                 xnumelm = xendelm - xstrelm + 1
                 xstrelm = Max(1, xnumdat / 2 - xnumelm / 2)
                 xendelm = Min(xnumdat, xstrelm + xnumelm)
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = Max(1, ynumdat / 2 - ynumelm / 2)
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'DOWN') Then
 
!              Look down
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = Max(1, ystrelm - ynumelm / 2)
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'BOTTOM') Then
 
!              Move vertically to lower edge
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = 1
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'R-TOP') Then
 
!              Move to far right/top
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = xnumdat
                 xstrelm = Max(1, xendelm - xnumelm + 1)
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = ynumdat
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'R-UP') Then
 
!              Look right and up
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = Min(xnumdat, xendelm + xnumelm / 2)
                 xstrelm = Max(1, xendelm - xnumelm + 1)
                 ynumelm = yendelm - ystrelm + 1
                 yendelm = Min(ynumdat, yendelm + ynumelm / 2)
                 ystrelm = Max(1, yendelm - ynumelm + 1)
 
              Else If (command .Eq. 'RIGHT') Then
 
!              Look right
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = Min(xnumdat, xendelm + xnumelm / 2)
                 xstrelm = Max(1, xendelm - xnumelm + 1)
 
              Else If (command .Eq. 'R-DOWN') Then
 
!              Look right and down
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = Min(xnumdat, xendelm + xnumelm / 2)
                 xstrelm = Max(1, xendelm - xnumelm + 1)
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = Max(1, ystrelm - ynumelm / 2)
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'R-BOTTOM') Then
 
!              Move to far-right/bottom
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = xnumdat
                 xstrelm = Max(1, xendelm - xnumelm + 1)
                 ynumelm = yendelm - ystrelm + 1
                 ystrelm = 1
                 yendelm = Min(ynumdat, ystrelm + ynumelm)
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
                   experiment, variances_exist, xstrelm, &
                   ystrelm, xendelm, yendelm, print_type, status)
 
!              Re-set menu layout style
                 Call GS_SET_MENULAYOUT (1, 5, 12, 9, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, status)
 
              Else If (command .Eq. 'FAR-RIGHT') Then
 
!              Move horizontally to far-right
                 xnumelm = xendelm - xstrelm + 1
                 xendelm = xnumdat
                 xstrelm = Max(1, xendelm - xnumelm + 1)
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, MASK, DATA, VARIANCES, &
                   title, xlabel, ylabel, zlabel, &
                   variances_exist, experiment, xstrelm, &
                   ystrelm, xendelm, yendelm, mxnumdat, mynumdat, MX_AXIS, &
                   MY_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, print_type, &
                   status)
 
!              Re-set menu layout style
                 Call GS_SET_MENULAYOUT (1, 5, 12, 9, status)
 
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
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Before updating of graphics'')')
!        Write (*, '(''update_image = '', l1)') update_image
!        Write (*, '(''update_menu = '', l1)') update_menu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
        If (.Not. gui) Then
 
!        User message
           MESSAGE(1) = 'CONTROL RETURNED TO TERMINAL WINDOW'
           Call GS_FPROMPT (1, 1, MESSAGE, status)
           Call GS_UPDATE (status)
        End If
 
     End If
 
     End Subroutine F2D_IMAGE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

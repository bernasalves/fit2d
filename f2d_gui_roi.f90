!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_gui_roi.f90 *
!  *                 *
!  *******************
 
!+ F2D_GUI_ROI - FIT 2-D GUI Region Of Interest menu
     Subroutine F2D_GUI_ROI (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, experiment, xstrelm, ystrelm, xendelm, &
       yendelm, print_type, status)
!  Description:
!  Keywords:
!    ROI, Image~Display.ROI, Select.ROI, Region~Of~Interest
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.3 Changes to "F2D_CLICK" (Hammersley)
!    13-Mar-2006: V0.2 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    23-Jan-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
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
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated variance 
!      values
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
     Character(Len = *), Intent(OUT) :: print_type ! Type, of graphics to
!      print. Supported types are:
!        "banner"
!        "image"
!        "contour"
!         "x/y graph"
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_menu = 6 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: window_format ! Format of graphics window:
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 10), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 6) / 'EXIT', '?', 'HELP', 'UN-ZOOM', 'FULL', &
       'ZOOM IN' /
 
     Data (MENUTXT(item), item = 1, 6) / 'EXIT: Exit from sub-menu', &
       '?: Display list of commands with short description', &
       'HELP: Help text on menu and available choices', &
       'UN-ZOOM: Make display region bigger', &
       'FULL: Make display region cover all defined data', &
       'ZOOM IN: Graphical region definition' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_GUI_ROI'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_ROI ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_ROI ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Loop inputting menu commands until requested to stop
 
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Create image plot of data
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
              print_type = 'image'
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu

!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 20, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (Gs_horizontal, 2, 20, 9, status)
              End If
 
!           Draw menu
              Call GS_FMENU ( 1, 1, 'FIT2D: REGION OF INTEREST MENU', &
                Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity or
              Call F2D_CLICK (0, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
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
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the OI menu
                 Call F2D_GUI_ROIHELP (.True., status)
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, status)
 
              End If
 
           End If
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_GUI_ROI
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

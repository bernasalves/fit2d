!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_1dmask.f90 *
!  *                *
!  ******************
 
!+ F2D_1DMASK - FIT 2-D 1-D data MASK definition
     Subroutine F2D_1DMASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
!  Description:
!    Graphical menu for masking/unmasking operations
!  Keywords:
!    Mask.1-D.Input, 1-D~Mask.Definition, Define.1-D~Mask,
!    Input.Mask.1-D
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P  Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.5 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.4 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.2 Alter menu lay-out for landscape windows (Hammersley)
!    04-Feb-1997: V0.1 Original, (Hammersley)
!  Modules:
     Use IO_LIB
     Use MA_LIB
     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to display
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
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
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be fitted, .True. = masked / bad data point, 
!      not to be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_menu = 8 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window:
!      Gs_A4portrait: A4 portrait orientation window
!      Gs_A4landscape: A4 landscape orientation window
!      Gs_square: Square window
!      Gs_fullscreen: Full screen window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 16), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: PROMPT(1) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 8) / 'EXIT', 'CLEAR MASK', 'ZOOM IN', '?', &
       'UN-ZOOM', 'MASK REGION', 'HELP', 'UN-MASK REGION' /
     Data (MENUTXT(item), item = 1, 8) / 'EXIT: Exit from sub-menu', &
       'CLEAR MASK: Un-mask all elements', &
       'ZOOM IN: Graphical region definition', &
       '?: Display list of commands with short description', &
       'UN-ZOOM: Make region displayed bigger', &
       'MASK REGION: Define masked off rectangular region', &
       'HELP: Explanation of masking menu', &
       'UN-MASK REGION: Un-mask rectangular region' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DMASK ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_1DMASK ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!     Loop inputing menu commands until requested to stop
        num_menu = Max_menu
        continue = .True.
        update_image = .Not. gui
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Draw image plot of data
              Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
           End If
 
           If (update_menu) Then
 
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 14, 17, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 14, 17, status)
              End If
 
!           Draw menu
              PROMPT(1) = 'FIT: MASK MENU'
              Call GS_FMENU (1, 0, PROMPT, Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update
           update_image = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
!           Default is to update
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
 
              Else If (command .Eq. 'CLEAR MASK') Then
 
!              Set all mask elements to un-masked
                 Call MA_L1VALUE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, .False., MASK, status)
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text
                 Call F2D_1DMASKHELP (.True., status)
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'MASK REGION') Then
 
!              Enter coordinates to mask off a rectangular region
                 Call F2D_1DMASKREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   X_AXIS, Y_AXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, &
                   .True., title, xlabel, ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'UN-MASK REGION') Then
 
!              Enter coordinates to un-mask a rectangular region
                 Call F2D_1DMASKREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   X_AXIS, Y_AXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, &
                   .False., title, xlabel, ylabel, zlabel, MASK, status)

 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, status)
 
              Else
                 update_image = .False.
                 update_menu = .False.
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
 
        If (.Not. gui) Then
 
!        User message
           PROMPT(1) = 'CONTROL RETURNED TO TERMINAL WINDOW'
           Call GS_FPROMPT (1, 1, PROMPT, status)
 
!        Force output
           Call GS_UPDATE (status)
 
        End If
 
     End If
 
     End Subroutine F2D_1DMASK
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

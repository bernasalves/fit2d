!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_gui_grid.f90 *
!  *                  *
!  ********************
 
!+ F2D_GUI_GRID: (Graphical User Interface) GRID requirements
     Subroutine F2D_GUI_GRID (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, xendelm, &
       yendelm, experiment, status)
!  Description:
!    User choice of grid lines: horizontal and vertical, and thick
!    and fine.
!  Keywords:
!    Grid~Lines, Lines.Grid
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.10 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.9 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.8 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.7 Alter menu lay-out for landscape windows (Hammersley)
!    03-Jun-2003: V0.6 Tidy-up code (Hammersley)
!    25-Jan-1997: V0.5 Cater for "CANCEL" buttons (Hammersley)
!    10-Apr-1996: V0.4 Change order of buttons, so that "EXIT" is first command 
!      (Hammersley)
!    19-Mar-1996: V0.3 Allow output and control of 1-D X/Y graphs (Hammersley)
!    10-Nov-1995: V0.2 Change to "F2D_CLICK" (Hammersley)
!    28-Sep-1995: V0.1 Original, based on "F2D_GUI_ZSCALE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of elements defined in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of elements defined in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_menu = 8 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: hcoarse_grid ! .True., if a horizontal coarse grid is required
     Logical :: hfine_grid ! .True., if a horizontal finee grid is required
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical :: vcoarse_grid ! .True., if a vertical coarse grid is required
     Logical :: vfine_grid ! .True., if a vertical finee grid is required
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
     Character(Len = 80) :: MESSAGE(7) ! Output user text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 8) / 'EXIT', '?', 'HELP', 'UPDATE', &
       'HORIZONTAL COARSE', 'VERTICAL COARSE', 'HORIZONTAL FINE', &
       'VERTICAL FINE' /
     Data (MENUTXT(item), item = 1, 8) / 'EXIT: Exit from this sub-menu', &
       '?: Display list of commands with short description', &
       'HELP: Further help information on grid options', &
       'UPDATE: Re-draw graphics with current grid requirements', &
       'HORIZONTAL COARSE: Add horizontal coarse grid lines', &
       'VERTICAL COARSE: Add vertical coarse grid lines', &
       'HORIZONTAL FINE: Add horizontal fine grid lines', &
       'VERTICAL FINE: Add vertical fine grid lines' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_GRID ' // Version)
     Else
 
!     Inquire current grid requirements
        Call GS_INQ_GRID (hcoarse_grid, vcoarse_grid, hfine_grid, vfine_grid, &
          status)
 
!     Change menu buttons accordingly
        num_menu = Max_menu
        If (hcoarse_grid) Then
           MENU(5) = 'NO HORIZONTAL COARSE'
           MENUTXT(5) = 'NO HORIZONTAL COARSE: Remove horizontal ' // &
             'coarse grid lines'
        End If
 
        If (vcoarse_grid) Then
           MENU(6) = 'NO VERTICAL COARSE'
           MENUTXT(6) = 'NO VERTICAL COARSE: Remove vertical ' // &
             'coarse grid lines'
        End If
 
        If (hfine_grid) Then
           MENU(7) = 'NO HORIZONTAL FINE'
           MENUTXT(7) = 'NO HORIZONTAL FINE: Remove horizontal ' // &
             'fine grid lines'
        End If
 
        If (vfine_grid) Then
           MENU(8) = 'NO VERTICAL FINE'
           MENUTXT(8) = 'NO VERTICAL FINE: Remove vertical ' // &
             'fine grid lines'
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
 
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Set current grid requirements
              Call GS_SET_GRID (hcoarse_grid, vcoarse_grid, hfine_grid, &
                vfine_grid, status)
 
!           Redraw image
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 15, 22, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 2, 15, 22, status)
              End If
 
!           Draw menu
              Call GS_FMENU (1, 1, 'GRID OPTIONS', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default no update
           update_image = .False.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 Continue
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 update_image = .True.
 
              Else If (command .Eq. 'HELP') Then
 
!              Display help text
                 MESSAGE(1) = 'The grid option allows the ' // &
                   'addition or removal of grid lines'
                 MESSAGE(2) = 'overlaid on the graphics. The ' // &
                   '"coarse" grid lines will appear'
                 MESSAGE(3) = 'where large ticks appear on the ' // &
                   'axes, and the "fine" grid lines'
                 MESSAGE(4) = 'will appear where the small tick ' // &
                   'marks appear on the axes. The'
                 MESSAGE(5) = 'style (colour, width, line type) ' // &
                   'of each of the grid line'
                 MESSAGE(6) = 'categories may be changed using ' // &
                   'the main keyboard menu'
                 MESSAGE(7) = '"SET GRID STYLE" command.'
                 Call GS_MESSAGE (7, 7, MESSAGE, status)
                 update_image = .True.
 
              Else If (command .Eq. 'HORIZONTAL COARSE') Then
 
                 MENU(5) = 'NO HORIZONTAL COARSE'
                 MENUTXT(5) = 'NO HORIZONTAL COARSE: Remove ' // &
                   'horizontal coarse grid lines'
                 hcoarse_grid = .True.
 
              Else If (command .Eq. 'NO HORIZONTAL COARSE') Then
 
                 MENU(5) = 'HORIZONTAL COARSE'
                 MENUTXT(5) = 'HORIZONTAL COARSE: Add ' // &
                   'horizontal coarse grid lines'
                 hcoarse_grid = .False.
 
              Else If (command .Eq. 'VERTICAL COARSE') Then
 
                 MENU(6) = 'NO VERTICAL COARSE'
                 MENUTXT(6) = 'NO VERTICAL COARSE: Remove ' // &
                   'vertical coarse grid lines'
                 vcoarse_grid = .True.
 
              Else If (command .Eq. 'NO VERTICAL COARSE') Then
 
                 MENU(6) = 'VERTICAL COARSE'
                 MENUTXT(6) = 'VERTICAL COARSE: Add ' // &
                   'vertical coarse grid lines'
                 vcoarse_grid = .False.
 
              Else If (command .Eq. 'HORIZONTAL FINE') Then
 
                 MENU(7) = 'NO HORIZONTAL FINE'
                 MENUTXT(7) = 'NO HORIZONTAL FINE: Remove ' // &
                   'horizontal fine grid lines'
                 hfine_grid = .True.
 
              Else If (command .Eq. 'NO HORIZONTAL FINE') Then
 
                 MENU(7) = 'HORIZONTAL FINE'
                 MENUTXT(7) = 'HORIZONTAL FINE: Add ' // &
                   'horizontal fine grid lines'
                 hfine_grid = .False.
 
              Else If (command .Eq. 'VERTICAL FINE') Then
 
                 MENU(8) = 'NO VERTICAL FINE'
                 MENUTXT(8) = 'NO VERTICAL FINE: Remove ' // &
                   'vertical fine grid lines'
                 vfine_grid = .True.
 
              Else If (command .Eq. 'NO VERTICAL FINE') Then
 
                 MENU(8) = 'VERTICAL FINE'
                 MENUTXT(8) = 'VERTICAL FINE: Add ' // &
                   'vertical fine grid lines'
                 vfine_grid = .False.
 
              Else If (command .Eq. 'UPDATE') Then
 
                 update_image = .True.
 
              End If
 
!           Check status
              If (status .Eq. St_escapevalue) Then
 
!              Reset status system
                 Call ST_DEF_SYSTEM (status)
 
              Else If (status .Ne. St_goodvalue) Then
                 continue = .False.
              End If
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        End Do
 
!     Set current grid requirements
        Call GS_SET_GRID (hcoarse_grid, vcoarse_grid, hfine_grid, vfine_grid, &
          status)
 
     End If
 
     End Subroutine F2D_GUI_GRID
!********1*********2*********3*********4*********5*********6*********7*********8
 

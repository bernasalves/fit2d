!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_rotatelut.f90 *
!  *                   *
!  *********************
 
!+ F2D_ROTATELUT - FIT 2-D ROTATE Look-Up Table
     Subroutine F2D_ROTATELUT (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, zlabel, &
       variances_exist, xstrelm, ystrelm, xendelm, yendelm, status)
!  Description:
!    Graphical menu choice for rotating the look-up table
!  Keywords:
!    Colour~Table.Rotate, Rotate.Colour~Table, LUT.Rotate
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Mar-2006: V0.3 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.2 Alter menu lay-out for landscape windows (Hammersley)
!    03-Mar-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
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
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_menu = 10 ! Dimension size of menu
!  Local Variables:
     Character(Len = 7) :: choice ! Chosen menu command
     Integer :: bit_planes ! Number of bit planes in colour map
     Integer :: colour_mapping ! Type of colour mapping:
!      1 = 'PseudoColor': Variable colour map
!      2 = 'TrueColor': Fixed colour map
     Integer :: input_type ! Type of graphical input
!    Integer min_index ! Minimum index to be used for look-up table
!    for image display
!    Integer max_index ! Maximum index to be used for look-up table
!    for image display
     Integer :: lut_position ! Memory of number of rotated levels
     Integer :: num_menu ! Number of choices in menu
     Integer :: rotation ! Number of levels to rotate
     Logical :: continue ! .True., until user wants to exit
     Logical :: static_colours ! .True., if the display colour map is static
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 7), Save :: MENU(Max_menu) ! The menu choices
!  External Functions:
!  Local Data:
     Data MENU / 'EXIT', '-100', '- 50', '- 10', '-  1', 'DEFAULT', '+  1', &
       '+ 10', '+ 50', '+100' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ROTATELUT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ROTATELUT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!     Inquire colour mapping method
        Call LG_INQ_COLOURMAPPING (colour_mapping, bit_planes, status)
 
        static_colours = colour_mapping .Ne. 1
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
 
!     By default update menu
        update_image = .False.
        update_menu = .True.
 
        continue = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Redraw image
              Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
                ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                status)
 
              update_image = .False.
              update_menu = .True.
           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 19, 7, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, num_menu, 19, 7, &
                   status)
              End If
 
!           Draw graphical menu of control choices
              Call GS_FMENU (1, 1, 'LEVELS TO ROTATE:', Max_menu, num_menu, &
                MENU, status)
 
              update_menu = .False.
 
           End If
 
!        Get user to select between the available menu options
           Call GS_INP_MENUCHOICE (max_menu, 5, MENU, 2, input_type, choice, &
             x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (choice .Eq. 'EXIT') Then
                 continue = .False.
                 rotation = 0
              Else If (choice .Eq. '-100') Then
                 rotation = -100
              Else If (choice .Eq. '- 50') Then
                 rotation = -50
              Else If (choice .Eq. '- 10') Then
                 rotation = -10
              Else If (choice .Eq. '-  1') Then
                 rotation = -1
              Else If (choice .Eq. 'DEFAULT') Then
                 rotation = -lut_position
              Else If (choice .Eq. '+  1') Then
                 rotation = 1
              Else If (choice .Eq. '+ 10') Then
                 rotation = 10
              Else If (choice .Eq. '+ 50') Then
                 rotation = 50
              Else If (choice .Eq. '+100') Then
                 rotation = 100
              End If
 
              If (rotation .Ne. 0) Then
 
                 lut_position = lut_position + rotation
 
!              Rotated LUT
                 Call LG_ROTATELUT (rotation, status)
              End If
 
              update_image = static_colours
 
           End If
 
!        Check status
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Reset colour translation table
        Call GS_CAL_COLOURS (status)
 
     End If
 
     End Subroutine F2D_ROTATELUT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

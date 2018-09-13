!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_continue.f90 *
!  *                  *
!  ********************
 
!+ F2D_CONTINUE - FIT 2-D CONTINUE when prompted
     Subroutine F2D_CONTINUE (print_type, xmaxdat, ymaxdat, X_AXIS, Y_AXIS, &
       DATA, VARIANCES, title, xlabel, ylabel, zlabel, variances_exist, &
       xstrelm, ystrelm, xendelm, yendelm, status)
!  Description:
!    Graphical menu to "PRINT" or "CONTINUE", used to make program wait until 
!    user graphical prompt
!  Keywords:
!    Wait, Graphical~Input.Wait
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    17-Mar-2006: V0.6 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.5 Alter menu lay-out for landscape windows (Hammersley)
!    30-Jun-1998: V0.4 Correction to call for "F2D_PRINT" (Hammersley)
!    26-Jun-1995: V0.3 Add proper printing (Hammersley)
!    20-Jun-1995: V0.2 Change to GS graphics library (Hammersley)
!    08-Dec-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Character(Len = *), Intent(IN) :: print_type ! Type, of graphics to print:
!      supported types are:
!       "banner"
!       "image"
!       "contour"
!       "x/y graph"
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated variances
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
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
     Integer, Parameter :: Max_menu = 3 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_menu ! .True., if the menu needs to be re-draw
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!    position
!  Local Arrays:
     Logical*1 :: DUMMY(1, 1) ! Dummy array for call to "F2D_PRINT"
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 3) / 'EXIT', '?', 'PRINT' /
     Data (MENUTXT(item), item = 1, 3) / &
       'EXIT: Exit from current display and continue', &
       '?: Display list of commands with short description', &
       'PRINT: Output current graphics to PostScript file' /
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CONTINUE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Loop inputing menu commands until requested to stop
 
        continue = .True.
        update_menu = .True.
        Do While (continue)
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 17, 8, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 17, 8, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 1, 'DISPLAY WAIT MENU', Max_menu, num_menu, &
                MENU, status)
 
              update_menu = .False.
 
           End If
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 1, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
                 Continue
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 update_menu = .True.
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 Call F2D_PRINT (.True., print_type, .False., xmaxdat, &
                   ymaxdat, X_AXIS, Y_AXIS, DATA, VARIANCES, DUMMY, title, &
                   xlabel, ylabel, zlabel, variances_exist, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
                 update_menu = .True.
 
              End If
 
           End If
 
!        Check status
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_CONTINUE
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_banner.f90 *
!  *                *
!  ******************
 
!+ F2D_BANNER - FIT2D: Output graphical banner
     Subroutine F2D_BANNER (fit2d_version, gui, status)
!  Description:
!    Creates fit2d graphical banner, and input O.K. or not, if in
!    GUI mode.
!  Keywords:
!    Banner.Graphical
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-May-1998: V0.8 Correct call to "F2D_TRAILER" (Hammersley)
!    05-Jan-1997: V0.7 Change text for GUI, for "conditions of use" (Hammersley)
!    01-Apr-1996: V0.6 Add "CONDITIONS" and "HELP" buttons, and use "menu" 
!      structure to draw buttons (Hammersley)
!    30-Jan-1996: V0.5 Use DDDR instead of DDR (Hammersley)
!    21-Jan-1996: V0.4 Drawing performed by "f2d_draw_banner", control of 
!      graphical input (Hammersley)
!    16-Jan-1996: V0.3 Parameterise for variable aspect ratio screen 
!      (Hammersley)
!    20-Jun-1995: V0.2 Change to GS graphics library (Hammersley)
!    23-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return variables
     Include 'gs_constants.inc' ! Graphics colours
!  Import:
     Character(Len = *), Intent(IN) :: fit2d_version ! Version of FIT2D
     Logical, Intent(IN) :: gui ! .True., if the graphical user interface is
!      active
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_menu = 4 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: update ! .True., if the graphics needs to be redrawn
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: xmaxdddr ! X-maximum of Data Display Region
     Real :: xmaxgpp ! X-maximum of Graph Page Position
     Real :: xmindddr ! X-minimum of Data Display Region
     Real :: xmingpp ! X-minimum of Graph Page Position
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: ymaxdddr ! Y-maximum of Data Display Region
     Real :: ymaxgpp ! Y-maximum of Graph Page Position
     Real :: ymindddr ! Y-minimum of Data Display Region
     Real :: ymingpp ! Y-minimum of Graph Page Position
!  Local Arrays:
     Character(Len = 15) :: MENU(Max_menu) ! Available menu commands
!    Internal References:
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 4) / 'CONDITIONS', 'I ACCEPT', 'HELP', &
       'DON''T ACCEPT' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_BANNER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_BANNER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Initialise variables
        num_menu = 4
 
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''After F2D_DRAW_BANNER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Graphical input if in GUI mode
        If (gui) Then
 
!        Update workstation
!           Call GS_UPDATE (status)

           continue = .True.
           update = .True.
           Do While (continue)
 
              If (update) Then
 
!              Re-draw banner
                 Call F2D_DRAW_BANNER (fit2d_version, gui, status)
 
              End If
 
!           By default no updating
              update = .False.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''F2D_BANNER: Before GS_INP_MENUCHOICE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!           Get user to select between the available menu options
              command = 'null'
              Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
                command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!             Write (*, '(''input_type, x_coordinate, y_coordinate = '', &
!               i5, 2f6.3)') input_type, x_coordinate, y_coordinate
!             Write (*, '(''Command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (input_type .Eq. Gs_resize) Then
 
                 update = .True.
 
              Else
 
!              Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!              Carry out menu choices
                 If (command .Eq. 'null') Then
 
                    update = .False.
 
                 Else If (command .Eq. 'CONDITIONS') Then
 
!                 Output conditions text
                    Call F2D_CONDITIONSHELP (.True., status)
                    update = .True.
 
                 Else If (command .Eq. 'I ACCEPT') Then
 
                    continue = .False.
 
                 Else If (command .Eq. 'HELP') Then
 
!                 Output help text
                    Call F2D_BANNERHELP (.True., status)
                    update = .True.
 
                 Else If (command .Eq. 'DON''T ACCEPT') Then
 
!                 Close graphics and EXIT program
                    Call GS_CLOSE_GRAPHICS (status)
 
!                 Output trailer
                    Call F2D_TRAILER (fit2d_version, status)
 
!                 Exit program
                    Stop
 
                 End If
 
              End If
 
!           Check that the status is good
              If (status .Ne. St_goodvalue) Then
 
                 Call IO_WRITE ('WARNING: Problem in banner ' // &
                   'widget, bad internal error status', status)
                 Call ST_OUT (status)
                 continue = .False.
 
              End If
 
           End Do
 
        End If
 
!     Set current page transform
        Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
        Call GS_INQ_GPP (xmingpp, ymingpp, xmaxgpp, ymaxgpp, status)
        Call LG_DATAWINDOW (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
        Call LG_VIEWPORT (xmingpp, ymingpp, xmaxgpp, ymaxgpp, status)
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_BANNER: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_BANNER
!********1*********2*********3*********4*********5*********6*********7*********8
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_gui_geometric.f90 *
!  *                       *
!  *************************
 
!+ F2D_GUI_GEOMETRIC - FIT 2-D GUI GEOMETRIC operations
     Subroutine F2D_GUI_GEOMETRIC (data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for geometric image transformations
!  Keywords:
!    Image~Processing, Processing~Image, Image.Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Jan-2015: V0.14 Include "MASK" in "FLIP" command (Hammersley)
!    12-Dec-2014: V0.13 Add "CYCLIC SHIFT" command (Hammersley)
!    26-Nov-2014: V0.12 Changes to "F2D_CLICK" (Hammersley)
!    23-Apr-2014: V0.11 Changes to "F2D_REBIN" (Hammersley)
!    17-Mar-2006: V0.10 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.8 Alter menu lay-out for landscape windows (Hammersley)
!    16-Apr-1999: V0.7 Add "RE-BIN" command (Hammersley)
!    29-Nov-1998: V0.6 Add "REFLECT" command (Hammersley)
!    25-Nov-1998: V0.5 Add "TRANSFORM" command (Hammersley)
!    12-Mar-1998: V0.4 Add "EXTEND" command to allow defined data
!      region to be increased in size (Hammersley)
!    04-Sep-1997: V0.3 Only exchange "memory" and current data arrays after 
!      successful tranposition within the "TRANSPOSE" command (Hammersley)
!    25-Jan-1997: V0.2 Cater for "CANCEL" buttons (Hammersley)
!    03-Sep-1996: V0.1 Original, based on "F2D_GUI_MATHS" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if data exists within
!      the program
     Logical, Intent(INOUT) :: memory_exist ! .True. if memory array exists
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory
!      contains data
     Logical, Intent(INOUT) :: variance_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(INOUT) :: xmaxdat ! Dimension size in X-direction for
!      data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Dimension size in Y-direction for
!      data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
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
     Integer, Intent(OUT) :: mxstrelm ! Start X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.13' ! Version number
     Integer, Parameter :: Max_menu = 13 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Logical :: continue ! .True., until user wants to exit
     Logical, Save :: flip_lr = .True. ! .True., if the image is to be
!      flipped left/right
     Logical, Save :: flip_tb = .True. ! .True., if the image is to be
!      flipped top/bottom
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong ROI and array shape
     Logical :: exchange ! .True., if the current data array and "memory" need 
!      to be swapped
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 13) / &
       'EXIT',        'EXCHANGE', 'EXTEND',    'FLIP', &
       '?',           'FULL',     'REFLECT',   'TRANSPOSE', &
       'HELP',        'RE-BIN',   'TRANSFORM', 'ZOOM IN', &
       'CYCLIC SHIFT' /
     Data (MENUTXT(item), item = 1, 13) / &
       'EXIT: Exit FIT2D', &
       'EXCHANGE: Swap current data with the "memory"', &
       'EXTEND: Make the defined data region larger', &
       'FLIP: Top to bottom, and/or left to right element exchange', &
       '?: This help on the menu choices', &
       'FULL: View image of full data', &
       'REFLECT: Mirror Reflection about line defined by 2 points', &
       'TRANSPOSE: Active data region element tranposition', &
       'HELP: Help text on this graphical menu', &
       'RE-BIN: Re-bin pixels by integer or non-integer factors', &
       'TRANSFORM: Rotate about a fixed point and translate', &
       'ZOOM IN: Define smaller graphical display region', &
       'CYCLIC SHIFT: Cyclically shift data by one or more rows' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_GEOMETRIC ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_GEOMETRIC ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_GEOMETRIC: Before GS_PLOT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

                 Call GS_MPLOT (mask_exist, xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_GEOMETRIC: After GS_PLOT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else
 
!              Re-draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE GENERAL'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'IMAGE PROCESSING GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'GEOMETRIC SUB-MENU'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = ' '
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_PPROMPT (Max_message, 10, MESSAGE, status)
 
              End If
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 9, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'FIT2D: GEOMETRY MENU', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           exchange = .False.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
           If (input_type .Eq. Gs_resize) Then
 
              Continue
 
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
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'CYCLIC SHIFT') Then

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_GEOMETRIC: Before F2D_CYCLICSHIFT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Shift rows cyclically by a user input value
                 Call F2D_CYCLICSHIFT (xmaxdat, ymaxdat, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                   ylabel, zlabel, variance_exist, retstat, memory_defined, &
                   %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), %val(pMASK), &
                   %val(pMVARIANCES), mxnumdat, mynumdat, mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   status)
                 exchange = retstat .Eq. 0
 
!              Set memory pixel sizes
                 mx_pixel_size = experiment%x_pixel_size
                 my_pixel_size = experiment%y_pixel_size
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Display list of available commands
                 MESSAGE(1) = '     Welcome to the FIT2D  ' // &
                   'Geometrical operations menu.'
                 MESSAGE(2) = ' '
                 MESSAGE(3) = 'The commands here allow you to ' // &
                   'perform geometric operations.'
                 Call GS_MESSAGE (3, 3, MESSAGE, status)
 
              Else If (command .Eq. 'EXTEND') Then
 
!              Extend currently defined data region of image
                 Call F2D_EXTEND (.True., xmaxdat, ymaxdat, variance_exist, &
                   xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
                   %val(pVARIANCES), status)
 
              Else If (command .Eq. 'FLIP') Then
 
!              Horizontal flip ?
                 Call GS_INPL (.True., 0, 1, .True., 'FLIP LEFT / RIGHT', 1, &
                   'Click "YES" to reverse the elements horizontally', &
                   1, '"YES" or "NO"', flip_lr, status)
 
!              Vertical flip ?
                 Call GS_INPL (.True., 0, 1, .True., 'FLIP TOP / BOTTOM', 1, &
                   'Click "YES" to reverse the elements vertically', 1, &
                   '"YES" or "NO"', flip_tb, status)
 
                 If (flip_lr) Then
 
!                 Flip data in horizontal
                    Call MA_RFLIP (.True., xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, %val(pDATA), status)
                    Call MA_L1FLIP (.True., xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pMASK), status)
 
                    If (variance_exist) Then
 
!                    Flip variances in horizontal
                       Call MA_RFLIP (.True., xmaxdat, ymaxdat, xstrelm, &
                         ystrelm, xendelm, yendelm, %val(pVARIANCES), status)
 
                    End If
 
                 End If
 
                 If (flip_tb) Then
 
!                 Flip data in vertical
                    Call MA_RFLIP (.False., xmaxdat, ymaxdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pDATA), status)
                    Call MA_L1FLIP (.False., xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pMASK), status)
 
                    If (variance_exist) Then
 
!                    Flip variances in vertical
                       Call MA_RFLIP (.False., xmaxdat, ymaxdat, xstrelm, &
                         ystrelm, xendelm, yendelm, %val(pVARIANCES), status)
 
                    End If
 
                 End If
 
              Else If (command .Eq. 'EXCHANGE') Then
 
                 exchange = .True.
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'RE-BIN') Then
 
!              Re-bin image
                 Call F2D_REBIN (.True., .False., &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xmaxdat, ymaxdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK),&
                   %val(pVARIANCES), xstrelm, ystrelm, xendelm, yendelm, &
                   variance_exist, retstat, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, &
                   mxendelm, myendelm, mxnumdat, mynumdat, mx_pixel_size, &
                   my_pixel_size, status)
 
!              Set memory labels
                 mtitle = title
                 mxlabel = xlabel
                 mylabel = ylabel
                 mzlabel = zlabel
 
                 If (retstat .Eq. 0) Then
!                 Write (*, '(''memory_defined'')')
                    memory_defined = .True.
                    exchange = .True.
                 End If
 
              Else If (command .Eq. 'REFLECT') Then
 
!              Reflect image
                 Call F2D_REFLECT (.True., xmaxdat, ymaxdat, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                   ylabel, zlabel, variance_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   retstat, memory_defined, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxnumdat, mynumdat, &
                   mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
                   mylabel, mzlabel, status)
                 exchange = retstat .Eq. 0
 
!              Set memory pixel sizes
                 mx_pixel_size = experiment%x_pixel_size
                 my_pixel_size = experiment%y_pixel_size
 
              Else If (command .Eq. 'TRANSFORM') Then
 
!              Rotate image
                 Call F2D_TRANSFORM (.True., xmaxdat, ymaxdat, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                   ylabel, zlabel, variance_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   retstat, memory_defined, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxnumdat, mynumdat, &
                   mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, &
                   mylabel, mzlabel, status)
                 exchange = retstat .Eq. 0
 
!              Set memory pixel sizes
                 mx_pixel_size = experiment%x_pixel_size
                 my_pixel_size = experiment%y_pixel_size
 
              Else If (command .Eq. 'TRANSPOSE') Then
 
!              Transpose image
                 Call F2D_TRANSPOSE (.True., xmaxdat, ymaxdat, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                   ylabel, zlabel, variance_exist, retstat, memory_defined, &
                   %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), %val(pMASK), &
                   %val(pMVARIANCES), mxnumdat, mynumdat, mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   status)
                 exchange = retstat .Eq. 0
 
!              Set memory pixel sizes
                 mx_pixel_size = experiment%x_pixel_size
                 my_pixel_size = experiment%y_pixel_size
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                   ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, status)
 
              End If
 
           End If
 
           If (exchange) Then
 
!           Swap current data with "memory"
              Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                xlabel, ylabel, zlabel, variance_exist, data_defined, &
                experiment%x_pixel_size, experiment%y_pixel_size, &
                xstrelm, ystrelm, xendelm, &
                yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_defined, &
                mx_pixel_size, my_pixel_size, status)
 
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
 
     End Subroutine F2D_GUI_GEOMETRIC
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

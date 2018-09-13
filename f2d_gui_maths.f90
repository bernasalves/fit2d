!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_gui_maths.f90 *
!  *                   *
!  *********************
 
!+ F2D_GUI_MATHS - FIT 2-D GUI MATHematical operationS
     Subroutine F2D_GUI_MATHS (experiment, &
       data_defined, memory_exist, memory_defined, &
       variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, mx_pixel_size, my_pixel_size, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, VARIANCES, MASK, status)
!  Description:
!    FIT2D GUI for general purpose image processing
!  Keywords:
!    Image~Processing, Processing~Image, Image.Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.10 Include "MASK", and use masking for all operations
!      (Hammersley)
!    26-Nov-2014: V0.9 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.8 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.6 Alter menu lay-out for landscape windows (Hammersley)
!    28-Oct-1998: V0.5 Add "SCALED SUB" command (Hammersley)
!    25-Jan-1997: V0.4 Cater for "CANCEL" buttons (Hammersley)
!    04-Sep-1996: V0.3 Add "LOG(10)", "X^(n)", and "THRESHOLD" commands 
!      (Hammersley)
!    19-Mar-1996: V0.2 Allow output and control of 1-D X/Y graphs (Hammersley)
!    25-Feb-1996: V0.1 Original, based on "F2D_GUI_IP" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: data_defined ! .True., if data exists
     Logical, Intent(IN) :: memory_exist ! .True. if memory array exists
     Logical, Intent(IN) :: memory_defined ! .True. if the memory contains data
     Logical, Intent(IN) :: variance_exist ! .True., if a data variances exist 
     Logical, Intent(IN) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of elements defined in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of elements defined in Y-direction
     Integer, Intent(IN) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: MX_AXIS(xmaxdat) ! 2nd array X-axis coordinates
     Real, Intent(IN) :: MY_AXIS(ymaxdat) ! 2nd array Y-axis coordinates
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat) ! Second data array 
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat) ! 2nd data variances 
     Integer, Intent(IN) :: mxendelm ! End X-element of memory data region
     Integer, Intent(IN) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(IN) :: myendelm ! End Y-element of memory data region
     Integer, Intent(IN) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(IN) :: mtitle ! Title label for data
     Character(Len = *), Intent(IN) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: mzlabel ! Z-axis label for data
     Real, Intent(IN) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(IN) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_menu = 16 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: warning ! Type of warning:
!      0 = No warning, everything fine
!      1 = Data not defined
!      2 = Memory not defined at all
!      3 = Memory not defined throughout whole of ROI
!      4 = Memory arrays do not exist
!      5 = No graphics
!      6 = Variance arrays do not exist
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real, Save :: addition_constant = 1.0 ! Constant to add to ROI
     Real, Save :: division_constant = 0.5 ! Constant to divide ROI
     Real, Save :: multiple_constant = 2.0 ! Constant to multiple ROI
     Real, Save :: power = -1.0 ! Power to raise elements
     Real :: value ! Storage for a real value
     Integer :: window_format ! Format of graphics window:
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
     Data (MENU(item), item = 1, 16) / &
       'EXIT', &
       '?', &
       'HELP', &
       'SCALAR +', &
       'SCALAR /', &
       'SCALAR *', &
       'ADD', &
       'DIVIDE', &
       'LOG(10)', &
       'MULTIPLY', &
       'NORMALISE', &
       'STATISTICS', &
       'SUBTRACT', &
       'THRESHOLD', &
       'X^(n)', &
       'SCALED SUB' /
     Data (MENUTXT(item), item = 1, 16) / 'EXIT: Exit FIT2D', &
       '?: This help on the menu choices', &
       'HELP: Help text on this grphical menu', &
       'SCALAR +: Add a constant to all region of interest elements', &
       'SCALAR /: Divide all ROI elements by a constant', &
       'SCALAR *: Multiply all ROI elements by a constant', &
       'ADD: Add memory data to current image throughout ROI', &
       'DIVIDE: Divide current image by memory data throughout ROI', &
       'LOG(10): Take logarithm, base 10, of data throughout ROI', &
       'MULTIPLY: Multiple image by memory data throughout ROI', &
       'NORMALISE: Normalise ROI; divide by maximum value within ROI', &
       'STATISTICS: Integration statistics, average, sigma etc.', &
       'SUBTRACT: Subtract memory data from current image in ROI', &
       'THRESHOLD: Set minimum and/or maximum values in ROI', &
       'X^(n): Raise elements to a required power', &
       'SCALED SUB: Interactive adjustment of scaled subtraction' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_MATHS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_MATHS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        update_image = .False.
        update_menu = .True.
        num_menu = Max_menu
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
!           Redraw image
              Call GS_MPLOT (mask_exist, xmaxdat, ymaxdat, DATA, MASK, &
                X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, &
                title, xlabel, ylabel, zlabel, status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 9, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_vertical, 4, 12, 9, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'FIT2D: MAIN MENU', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           warning = 0
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              If (data_defined) Then
                 Call F2D_CLICK (0, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, &
                   zlabel, x_coordinate, y_coordinate, experiment, &
                   update_image, update_menu, status)
              End If
 
           Else
 
!           Menu choice input
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''command = '', a)') command
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
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
 
!              Display list of available commands
                 MESSAGE(1) = '     Welcome to the FIT2D  ' // &
                   'Mathematical operations menu.'
                 MESSAGE(2) = ' '
                 MESSAGE(3) = 'The commands here allow you to ' // &
                   'perform maths operations.'
                 Call GS_MESSAGE (3, 3, MESSAGE, status)
 
              Else If (command .Eq. 'SCALAR +') Then
 
!              Add scalar to ROI
 
!              Input value of scalar
                 Call GS_INPR (.False., 0.0, 0.0, .True., 'ADDITION CONSTANT', &
                   1, 'Enter value to add to region of interest', 1, &
                   'Enter real number', addition_constant, status)
 
                 If (addition_constant .Ne. 0.0) Then
 
!                 Add constant
!                    Call MA_RCADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                      xendelm, yendelm, addition_constant, DATA, status)
                    Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                      DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                      DATA(xstrelm: xendelm, ystrelm: yendelm) + &
                      addition_constant

                 End If
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'SCALAR /') Then
 
!              Divide ROI by a scalar
 
!              Input value of scalar
                 Call GS_INPR (.False., 0.0, 0.0, .True., 'DIVISION CONSTANT', &
                   1, 'Enter value to divide region of interest', 1, &
                   'Enter real number', division_constant, status)
 
                 If (division_constant .Eq. 0.0) Then
 
                    Call GS_FWARNING ( 1, 1, 'NOT POSSIBLE TO DIVIDE BY ZERO', &
                      status)
 
                 Else If (division_constant .Ne. 1.0) Then
 
!                 Multiply by inverse of constant
                    value = 1.0 / division_constant
!                    Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                      xendelm, yendelm, value, DATA, status)

                    Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                      DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                      DATA(xstrelm: xendelm, ystrelm: yendelm) * value
 
                    If (variance_exist) Then

!                       Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                         xendelm, yendelm, value**2, VARIANCES, status)

                       Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                         VARIANCES(xstrelm: xendelm, ystrelm: yendelm) = &
                         VARIANCES(xstrelm: xendelm, ystrelm: yendelm) * &
                         value**2

                    End If
 
                 End If
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'SCALAR *') Then
 
!              Multiply ROI by a scalar
 
!              Input value of scalar
                 Call GS_INPR (.False., 0.0, 0.0, .True., &
                   'MULTIPLICATION CONSTANT', 1, &
                   'Enter value to multiple region of interest', 1, &
                   'Enter real number', multiple_constant, status)
 
!              Multiply by constant
!                 Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
!                   yendelm, multiple_constant, DATA, status)

                 Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                   DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                   DATA(xstrelm: xendelm, ystrelm: yendelm) * &
                   multiple_constant
 
                 If (variance_exist) Then
!                    Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                      xendelm, yendelm, multiple_constant**2, VARIANCES, &
!                      status)

                    Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                      VARIANCES(xstrelm: xendelm, ystrelm: yendelm) = &
                      VARIANCES(xstrelm: xendelm, ystrelm: yendelm) * &
                      multiple_constant**2

                 End If
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'SCALED SUB') Then
 
                 If (memory_defined) Then
 
                    If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                      Then
 
!                    Interactive menu to subtract scaled "background"
!                    image (memory) from main array
                       Call F2D_GUI_SCALEDSUB (mask_exist, experiment, &
                         xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
                         X_AXIS, Y_AXIS, MDATA, MASK, &
                         title, xlabel, ylabel, zlabel, DATA, status)
 
                    Else
                       warning = 3
                    End If
 
                 Else
                    warning = 2
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'ADD') Then
 
!              Add two images element by element
                 If (memory_defined) Then
 
!                 Add memory to current data in region of interest 
                    If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                      Then
 
                       Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) + &
                         MDATA(xstrelm: xendelm, ystrelm: yendelm)

!                       Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                         xendelm, yendelm, MDATA, xmaxdat, ymaxdat, DATA, &
!                         status)
 
                       If (variance_exist) Then
 
                          Where &
                            (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                            VARIANCES(xstrelm: xendelm, ystrelm: yendelm) = &
                            VARIANCES(xstrelm: xendelm, ystrelm: yendelm) + &
                          MVARIANCES(xstrelm: xendelm, ystrelm: yendelm)

!                          Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                            xendelm, yendelm, MVARIANCES, xmaxdat, ymaxdat, &
!                            VARIANCES, status)
 
                       End If
                       update_image = .True.
 
                    Else
                       warning = 3
                    End If
 
                 Else
                    warning = 2
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'DIVIDE') Then
 
!              Divide two images element by element
                 If (memory_defined) Then
 
!                 Divide current data by memory in region of interest 
                    If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                      Then
 
                       Call F2D_DIVIDE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                         xendelm, yendelm, variance_exist, MDATA, MVARIANCES, &
                         MASK, DATA, VARIANCES, status)
                       update_image = .True.
 
                    Else
                       warning = 3
                    End If
 
                 Else
                    warning = 2
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'LOG(10)') Then
 
!              Take Log(10) of elements
                 Call F2D_LOGARITHM (.True., xmaxdat, ymaxdat, xstrelm, &
                   ystrelm, xendelm, yendelm, variance_exist, MASK, &
                   DATA, VARIANCES, status)
 
              Else If (command .Eq. 'MULTIPLY') Then
 
!              Multiply two images element by element
                 If (memory_defined) Then
 
!                 Multiply current data and memory in region of interest 
                    If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                      Then
 
!                    Variance calculation must take place before multiplication
                       If (variance_exist) Then
 
                          Call MA_MRVARMULT (xmaxdat, ymaxdat, xstrelm, &
                            ystrelm, xendelm, yendelm, MDATA, MVARIANCES, &
                            xmaxdat, ymaxdat, MASK, DATA, VARIANCES, status)
 
                       End If
 
!                       Call MA_RMULTIPLY (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                         xendelm, yendelm, MDATA, xmaxdat, ymaxdat, DATA, &
!                         status)
                       Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) * &
                         MDATA(xstrelm: xendelm, ystrelm: yendelm)
 
                       update_image = .True.
 
                    Else
                       warning = 3
                    End If
 
                 Else
                    warning = 2
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'NORMALISE') Then
 
!              Divide ROI by maximum value
                 Call F2D_NORMALISE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, variance_exist, MASK, DATA, VARIANCES, &
                   status)
 
              Else If (command .Eq. 'STATISTICS') Then
 
!              Statistics of user defined polygon region
                 Call F2D_GSTATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, &
                   xlabel, ylabel, zlabel, status)
 
              Else If (command .Eq. 'SUBTRACT') Then
 
!              Subtract memory from current data element by element
                 If (memory_defined) Then
 
!                 Subtract memory from current data in region of interest 
                    If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                      Then
 
                       Where (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) = &
                         DATA(xstrelm: xendelm, ystrelm: yendelm) - &
                         MDATA(xstrelm: xendelm, ystrelm: yendelm)

!                       Call MA_RSUBTRACT (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                         xendelm, yendelm, MDATA, xmaxdat, ymaxdat, DATA, &
!                         status)
 
                       If (variance_exist) Then
 
                          Where &
                            (.Not. MASK(xstrelm: xendelm, ystrelm: yendelm)) &
                            VARIANCES(xstrelm: xendelm, ystrelm: yendelm) = &
                            VARIANCES(xstrelm: xendelm, ystrelm: yendelm) + &
                            MVARIANCES(xstrelm: xendelm, ystrelm: yendelm)
                          
!                          Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
!                            xendelm, yendelm, MVARIANCES, xmaxdat, ymaxdat, &
!                            VARIANCES, status)
 
                       End If
                       update_image = .True.
 
                    Else
                       warning = 3
                    End If
 
                 Else
                    warning = 2
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'THRESHOLD') Then
 
!              Threshold data with user input values
                 Call F2D_THRESHOLD (.True., xmaxdat, ymaxdat, xstrelm, &
                   ystrelm, xendelm, yendelm, MASK, DATA, status)
 
              Else If (command .Eq. 'X^(n)') Then
 
!              Raise elements to required power
                 Call F2D_POWER (.True., xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, variance_exist, MASK, DATA, VARIANCES, &
                   status)
 
              End If
 
           End If
 
!        Output warning message if required
           If (warning .Eq. 2) Then
 
              Call GS_FWARNING (1, 1, 'MEMORY NOT DEFINED', status)
 
              update_menu = .True.
 
           Else If (warning .Eq. 3) Then
 
              Call GS_FWARNING (1, 1, &
                'MEMORY DEFINED, BUT NOT IN WHOLE OF ROI', status)
 
              update_menu = .True.

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
 
     End Subroutine F2D_GUI_MATHS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

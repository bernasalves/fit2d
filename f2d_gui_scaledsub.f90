!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_gui_scaledsub.f90 *
!  *                       *
!  *************************
 
!+ F2D_GUI_SCALEDSUB - FIT 2-D GUI SCALED SUBtraction
     Subroutine F2D_GUI_SCALEDSUB (mask_data, experiment, xmaxdat, &
       ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, MDATA, &
       MASK, title, xlabel, ylabel, zlabel, DATA, status)
!  Description:
!    GUI for interactive adjustment of scaled factor applied to memory array 
!    prior to subtraction.
!  Keywords:
!    Subtraction.Scaled, Scaled.Subtraction
!  Method:
!    User menu allows interactive adjustment of scaling factor applied to 
!    memory prior to subtracting from the main array.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.9 Include "MASK", and use masking (Hammersley)
!    26-Nov-2014: V0.8 Changes to "F2D_CLICK" (Hammersley)
!    25-Apr-2006: V0.7 Use Fortran-90 dynamically allocated arrays (Hammersley)
!    17-Mar-2006: V0.6 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.4 Alter menu lay-out for landscape windows (Hammersley)
!    23-Feb-1999: V0.3 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.2 Change to use IO internal database routines (Hammersley)
!    28-Oct-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if data is to be masked
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat) ! Memory data
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array, the output
!      elements are "DATA(x, y) - scale_factor *  MDATA(x, y)"
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Max_menu = 12 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 40) :: menu_title ! Title for menu
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
!      3 = Problem converting character string to a real value
     Integer stat ! Status return variable for "Allocate"
     Integer x ! Loop variable for X-direction
     Integer :: xnumelm ! Number of elements in X-direction of ROI
     Integer y ! Loop variable for Y-direction
     Integer :: ynumelm ! Number of elements in Y-direction of ROI
     Logical :: change_data ! .True., if the data should be updated
     Logical :: continue ! .True., until user wants to exit
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real, Save :: scale_factor = 1.0 ! Scale factor to apply to memory prior to
!      subtraction
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Logical :: DUMMY(1, 1) ! Unused dummy argument
     Real, Allocatable :: RESULT(:, :) ! Dynamic array for the result of the 
!      scaled subtraction
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 12) / 'O.K.', 'CANCEL', 'FACTOR', &
       'Z-SCALING', '?', 'INCREASE 1%', 'INCREASE 10%', 'INCREASE 50%', &
       'HELP', 'DECREASE 1%', 'DECREASE 9%', 'DECREASE 33%' /
     Data (MENUTXT(item), item = 1, 12) / &
       'O.K.: Apply current scaled subtraction to data', &
       'CANCEL: Cncel operation with no subtraction', &
       'FACTOR: Enter new scale factor manually', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       '?: This help text on the available menu choices', &
       'INCREASE 1%: Increase scale factor applied to subtracted image', &
       'INCREASE 10%: Increase scale factor applied to subtracted image', &
       'INCREASE 50%: Increase scale factor applied to subtracted image', &
       'HELP: Help text on using this graphical menu', &
       'DECREASE 1%: Decrease scale factor applied to subtracted image', &
       'DECREASE 9%: Decrease scale factor applied to subtracted image', &
       'DECREASE 33%: Decrease scale factor applied to subtracted image' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_SCALEDSUB ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_SCALEDSUB ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Try to recover scale factor value from internal data-base
        Call IO_INQ_RKEYVALUE ('MATHS_SUB_SCALE_FACTOR', scale_factor, &
          retstat, status)
 
!     Size of temporary result array
        xnumelm = xendelm - xstrelm + 1
        ynumelm = yendelm - ystrelm + 1
 
!     Allocate memory for temporay array
        Allocate (RESULT(xnumelm, ynumelm), Stat = stat)
        If (stat .Ne. 0) Then
           Call GS_FWARNING (1, 1, 'Failed to allocate temporary memory', &
             status)
           Return
        End If
 
!     Input initial scale factor
        MESSAGE(1) = 'Enter the scale factor to be applied to the data in the'
        MESSAGE(2) = &
          'memory prior to the result being subtracted from the data'
        MESSAGE(3) = 'in the current (displayed) image'
        Call GS_INPR (.True., -1.7e38, 1.7e38, .True., &
          'SCALE FACTOR APPLIED TO MEMORY PRIOR TO SUBTRACTION', 3, MESSAGE, &
          1, 'Enter value within given range', scale_factor, status)
 
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        change_data = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
!           Subtraction scaled memory values from main data
              Do y = ystrelm, yendelm
 
                 Do x = xstrelm, xendelm
                    RESULT(x - xstrelm + 1, y - ystrelm + 1) = &
                      DATA(x, y) - scale_factor * MDATA(x, y)
                 End Do
                 
              End Do
 
!           Redraw image
              Call GS_MPLOT (mask_data, xnumelm, ynumelm, RESULT, MASK, &
                X_AXIS(xstrelm), Y_AXIS(ystrelm), 1, 1, xnumelm, ynumelm, &
                title, xlabel, ylabel, zlabel, status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Create Menu title
              Write (menu_title, '(''SCALING = '', g14.7)') scale_factor
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 24, 14, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 24, 14, status)
              End If
 
!           Draw menu
              Call GS_FMENU (1, 1, menu_title, Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
 
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
              Call F2D_CLICK (1, xnumelm, ynumelm, 1, 1, xnumelm, ynumelm, &
                X_AXIS(xstrelm), Y_AXIS(ystrelm), RESULT, title, &
                xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                experiment, update_image, update_menu, status)
 
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
 
              Else If (command .Eq. 'O.K.') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Menu help text
                 Call F2D_GUI_SCALEDSUBHELP (.True., status)
 
              Else If (command .Eq. 'CANCEL') Then
 
!              No change to data
                 continue = .False.
                 change_data = .False.
 
              Else If (command .Eq. 'FACTOR') Then
 
!              Enter new scaling factor manually
                 Call GS_INPR (.True., -1.7e38, 1.7e38, .True., &
                   'SCALE FACTOR APPLIED TO MEMORY PRIOR TO ' // &
                   'SUBTRACTION', 3, MESSAGE, 1, &
                   'Enter value within given range', scale_factor, status)
 
                 update_image = .True.
                 update_menu = .True.
 
              Else If (command .Eq. 'INCREASE 1%') Then
 
!              Increase scale factor applied to memory by 1%
                 scale_factor = scale_factor * 1.01
 
              Else If (command .Eq. 'INCREASE 10%') Then
 
!              Increase scale factor applied to memory by 10%
                 scale_factor = scale_factor * 1.1
 
              Else If (command .Eq. 'INCREASE 50%') Then
 
!              Increase scale factor applied to memory by 50%
                 scale_factor = scale_factor * 1.5
 
              Else If (command .Eq. 'DECREASE 1%') Then
 
!              Decrease scale factor applied to memory by 1%
                 scale_factor = scale_factor / 1.01
 
              Else If (command .Eq. 'DECREASE 9%') Then
 
!              Decrease scale factor applied to memory by 9%
                 scale_factor = scale_factor / 1.1
 
              Else If (command .Eq. 'DECREASE 33%') Then
 
!              Decrease scale factor applied to memory by 50%
                 scale_factor = scale_factor / 1.5
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 Call F2D_GUI_ZSCALE (mask_data, xnumelm, ynumelm, xnumelm, &
                   ynumelm, X_AXIS(xstrelm), Y_AXIS(ystrelm), RESULT, &
                   MASK, title, xlabel, ylabel, zlabel, 1, 1, xnumelm, &
                   ynumelm, experiment, &
                   .False., x_coordinate, y_coordinate, status)
 
                 update_image = .False.
                 update_menu = .True.
 
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
 
!     Update data with result unless "CANCEL" has been pressed
        If (change_data) Then
 
           Call MA_MRMOVE (xnumelm, ynumelm, RESULT, 1, 1, xnumelm, &
             ynumelm, xmaxdat, ymaxdat, xstrelm, ystrelm, MASK, DATA, status)
 
!        Store scale factor in internal data-base
           Call IO_SET_RKEYVALUE ('MATHS_SUB_SCALE_FACTOR', scale_factor, &
             retstat, status)
 
        End If
 
!     Free allocated memory
        Deallocate (RESULT)
 
     End If
 
     End Subroutine F2D_GUI_SCALEDSUB
!********1*********2*********3*********4*********5*********6*********7*********8

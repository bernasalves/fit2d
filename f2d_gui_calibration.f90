!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_gui_calibration.f90 *
!  *                         *
!  ***************************
 
!+ F2D_GUI_CALIBRATION - FIT 2-D GUI detector CALIBRATION
     Subroutine F2D_GUI_CALIBRATION (input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, x_pixel_size, y_pixel_size, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for detector distortion calibration and correction
!  Keywords:
!    Image~Processing, Processing~Image, Image.Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.4 Use Fortran allocate (Hammersley)
!    17-Feb-1999: V0.3 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    23-Jan-1998: V0.2 Changes to "F2D_GUI_INPUT" so that the user prompt text 
!      is set by the calling routine (Hammersley)
!    21-Mar-1997: V0.1 Original, based on "F2D_GUI_IP" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: input_file ! Name of current data
!      input file
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
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
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
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
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
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: Max_menu = 12 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!       "banner"
!       "image"
!       "contour"
!       "x/y graph"
     Integer :: dummy ! Dummy variable for "F2D_GUI_ZSCALE"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Return status variable from "F2D_TREATGRID":
!       0 = Good status
!      -1 = Peak search failed
     Integer :: stat ! Status return variable for "Allocate"
     Integer, Save :: xmax_peaks = 400 ! X-dimension of 2-D arrays to hold
!      grid peak positions
     Integer :: xnum_peaks ! Maximum size of found peak grid in the X-direction
     Integer, Save :: ymax_peaks = 400 ! Y-dimension of 2-D arrays to hold
!      grid peak positions
     Integer :: ynum_peaks ! Maximum size of found peak grid in the Y-direction
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the user tries 
!      an operation which requires data
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real, Save :: detect_ratio = 0.3 ! Ratio of "intensity value found for a 
!      peak divided by adjacent peak intensity values, above which a new "peak"
!      is accepted (avoids noise being classed as peaks)
     Real, Save :: initial_max_distance = 3.7 ! This is the maximum distance in
!      pixels that a peak position may be accepted as corresponding to a search
!      peak position in the initial search for a good starting position
     Real, Save :: search_max_distance = 5.9 ! This is the maximum distance
!      in pixels that a peak position may be accepted as corresponding
!      to a search peak position in the general grid search
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: xmax_message ! The maximum X-coordinate for the message region
     Real :: xmin_message ! The minimum X-coordinate for the message region
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: ymax_message ! The maximum Y-coordinate for the message region
     Real :: ymin_message ! The minimum Y-coordinate for the message region
!  Local Arrays:
     Character(Len = 12), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Real, Allocatable :: X_2DPEAKS(:, :) ! Stores the X-coordinates of grid 
!      mask peak centres
     Real, Allocatable :: Y_2DPEAKS(:, :) ! Stores the Y-coordinates of grid 
!      mask peak centres
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 12) / 'EXIT', '?', 'HELP', 'ZOOM-IN', &
       'UN-ZOOM', 'FULL', 'INPUT', 'OUTPUT', 'Z-SCALING', 'SET-UP', &
       'TREAT GRID', 'CORRECTION' /
     Data (MENUTXT(item), item = 1, 12) / &
       'EXIT: Exit FIT2D', &
       '?: List of the menu choices and short explanation', &
       'HELP: Help text on this grphical menu', &
       'ZOOM IN: Graphical region definition', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'FULL: View image of full data', &
       'INPUT: Input data from a file on disk', &
       'OUTPUT: Save data in an output file', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'SET-UP: Modification of control parameters', &
       'TREAT GRID: Calibration spatial distortion using a grid image', &
       'CORRECTION: Correct image for detector distortions' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_CALIBRATION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_CALIBRATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Get dynamic memory for peak coordinate arrays
        Allocate (X_2DPEAKS(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_ma + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GUI_CALIBRATION ' // Version)
           Return
        End If
        Allocate (Y_2DPEAKS(xmax_peaks, ymax_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_ma + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GUI_CALIBRATION ' // Version)
           Return
        End If
 
!     Find out message area
        Call GS_INQ_MESSAGE (xmin_message, ymin_message, xmax_message, &
          ymax_message, status)
 
        If (.Not. mask_exist) Then
 
!        Need to allocate memory for the mask
           Call IO_MALLOC (xmaxdat * ymaxdat, pMASK, status)
 
           If (status .Eq. St_goodvalue) Then
 
!           Initialise mask to all good elements
              Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, &
                .False., %val(pMASK), status)
              mask_exist = .True.
 
           Else
              Return
           End If
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image or 1-D plot
                 Call GS_MPLOT (.True., xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
                 print_type = 'image'
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE DETECTOR'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'DISTORTION CALIBRATION'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'AND CORRECTION INTERFACE'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = '(USE "INPUT" FOR FILE DATA)'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_PROMPT (.False., Max_message, 10, MESSAGE, &
                   xmin_message, ymin_message, xmax_message, ymax_message, &
                   status)
 
              End If
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Set menu layout style
              num_menu = Max_menu
              Call GS_SET_MENULAYOUT (1, 4, 12, 11, status)
 
!           Re-draw menu
              Call GS_FMENU (1, 0, ' ', Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
 
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
 
                 Call F2D_CLICK (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   x_pixel_size, y_pixel_size, update_image, update_menu, &
                   status)
 
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
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variance_exist, data_defined, &
                   x_pixel_size, y_pixel_size, xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_CALIHELP (.True., status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT (for correction, etc.)'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   x_pixel_size, y_pixel_size, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
 
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variance_exist, x_pixel_size, &
                      y_pixel_size, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'TREAT GRID') Then
 
!              Use an image of a calibration grid to deduce the
!              spatial distortion
                 If (data_defined) Then
 
                    Call F2D_TREATGRID (initial_max_distance, &
                      search_max_distance, detect_ratio, xmaxdat, ymaxdat, &
                      xnumdat, ynumdat, x_pixel_size, y_pixel_size, title, &
                      xlabel, ylabel, zlabel, xmax_peaks, ymax_peaks, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
                      xstrelm, ystrelm, xendelm, yendelm, retstat, &
                      %val(pMDATA), xnum_peaks, ynum_peaks, X_2DPEAKS, &
                      Y_2DPEAKS, status)
                    update_image = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
 
                    Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                      ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 If (data_defined) Then
 
                    Call F2D_GUI_ZSCALE (.False., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), dummy, &
                      title, xlabel, ylabel, zlabel, xstrelm, ystrelm, &
                      xendelm, yendelm, x_pixel_size, y_pixel_size, .False., &
                      x_coordinate, y_coordinate, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              Call GS_FWARNING ( 1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
                status)
 
              update_menu = .True.
 
           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Re-set status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Free dynamic memory for peak coordinate arrays
        Deallocate (X_2DPEAKS)
        Deallocate (Y_2DPEAKS)

     End If
 
     End Subroutine F2D_GUI_CALIBRATION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

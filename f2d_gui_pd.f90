!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_gui_pd.f90 *
!  *                *
!  ******************
 
!+ F2D_GUI_PD - FIT 2-D GUI Powder Diffraction (2-D)
     Subroutine F2D_GUI_PD (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for powder diffraction on 2-D detectors
!  Keywords:
!    Powder~Diffraction.Interface, Interface.Powder~Diffraction
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.34 Changes to "F2D_CLICK" (Hammersley)
!    24-Apr-2006: V0.33 Add "input_options" structure (Hammersley)
!    17-Mar-2006: V0.32 Support for arbitrary aspect ratio windows (Hammersley)
!    10-Mar-2006: V0.31 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.30 Alter menu lay-out for landscape windows (Hammersley)
!    23-Feb-1999: V0.29 All data-base saving and recovering routines
!      renamed (Hammersley)
!    17-Feb-1999: V0.28 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    15-Dec-1998: V0.27 Change to use IO internal database routines (Hammersley)
!    27-Oct-1998: V0.26 Don't set default geometry (Hammersley)
!    20-May-1998: V0.25 Remember tilt angles between calls to FIT2D (Hammersley)
!    21-Apr-1998: V0.24 Set geometry to turn on D-spacings output from clicking 
!      (Hammersley)
!    03-Mar-1998: V0.23 Save and try to recover sample to detector distance 
!      from the internal data-store (Hammersley)
!    28-Jan-1998: V0.22 Option of Q-space re-binning (Hammersley)
!    23-Jan-1998: V0.21 Changes to "F2D_GUI_INPUT" so that the
!      user prompt text is set by the calling routine (Hammersley)
!    22-Jan-1998: V0.20 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    04-Sep-1997: V0.19 After the "INTEGRATE" command set the X-Y graph to 
!      be fully auto-scaling and make sure that a line is used to draw the 
!      curve, if neither a line nor markers are set (Hammersley)
!    08-Jul-1997: V0.18 Add "polarisation" argument to
!      "F2D_CALIBRANT" (Hammersley)
!    21-Mar-1997: V0.17 Add "UN-ZOOM" command (Hammersley)
!    24-Feb-1997: V0.16 Rename "F2D_SILICON" to "F2D_CALIBRANT" (Hammersley)
!    20-Feb-1997: V0.15 Transfer name of input file between interfaces  
!      (Hammersley)
!    25-Jan-1997: V0.14 Cater for "CANCEL" buttons (Hammersley)
!    13-Dec-1996: V0.13 Remember requirement for masking inbetween
!      calls to routine (Hammersley)
!    04-Dec-1996: V0.12 Add "DISPLAY" and "OPTIONS" buttons (Hammersley)
!    02-Oct-1996: V0.11 Save geometry (Hammersley)
!    27-Sep-1996: V0.10 Correct beam centre units to metres prior
!      to call to "F2D_BEAMCENTRE" (Hammersley)
!    26-Sep-1996: V0.9 Don't allow "OUTPUT" unless data is defined (Hammersley)
!    23-Aug-1996: V0.8 Add "CALIBRANT" option, and save and try use wavelength 
!      from the internal data-store (Hammersley)
!    12-Apr-1996: V0.7 Re-set user escape values (Hammersley)
!    10-Apr-1996: V0.6 Add "BEAM CENTRE" command to "POWDER DIFFRACTION" menu 
!      (Hammersley)
!    29-Mar-1996: V0.5 Add "Z-SCALING" command to menu "Hammersley)
!    20-Mar-1996: V0.4 Add print command to menu (Hammersley)
!    08-Mar-1996: V0.3 Try to ensure that transformed data is not masked 
!      (Hammersley)
!    26-Feb-1996: V0.2 Add spatial distortion correction option (Hammersley)
!    15-Feb-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!  Use MA_LIB
!  Use GS_LIB
!  Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Type(INPUT_OPTIONS_STRUCTURE), Intent(IN) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
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
     Character(Len = 5), Parameter :: Version = 'V0.34' ! Version number
     Integer, Parameter :: Max_menu = 20 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "masked_image"
!        "x/y graph"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer, Save :: lorentz_geometry = 1 ! The Lorentz correction is dependent
!      on the experiment geometry:
!      0 = None
!       1 = Partial correction for an ideal powder to the
!       equivalent of a 2-theta scan i.e. detector at equal distance
     Integer :: method_used ! The input method that was used to
!      specify the beam centre:
!        0 = None, (existing centre was O.K.)
!        1 = Keyboard,            only "x_beam, y_beam" are defined
!        2 = Single cursor point,   "      "       "     "     "
!        3 = Symmetric points,      "      "       "     "     "
!        4 = Points on circle,  "radius1" and "radial_error" are defined
!        5 = Points on ellipse, "radius1", radius2","angle" and
!            "radial_error" are defined
!        6 = Fitted 2-D Gaussian
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! "IO_INQ_*KEYVALUE" return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
     Logical :: temp_data_defined ! .True., if data has been input
     Logical :: continue ! .True., until user wants to exit
     Logical, Save :: correct_polarisation = .True. ! .True., if the
!      polarisation correction is to be applied
     Integer, Save :: data_type = 1 ! Type of image data:
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: draw_line ! .True., if a line is to be drawn through
!      the data-points
     Logical :: draw_markers ! .True., if markers are to be drawn at
!      each data-point
     Logical :: draw_errorboxes ! .True., if error-boxes are to be
!      drawn at each data-point (only if errors are defined)
     Logical :: geometry_defined ! .True.,  if geometry is defined
     Logical, Save :: mask_data ! .True., if the current data is to be masked
     Logical, Save :: mask_memory ! .True., if the memory data is to be masked
     Integer, Save :: memory_data_type ! Type of image data:
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: angle ! Angle of "radius1" of ellipse to X-axis (anti-clockwise 
!      radians)
     Real, Save :: detector_gain = 1.0 ! The gain of the detector i.e. the
!      intensity values divided by the gain should produce counts
     Real, Save :: detector_rotation ! Angle of rotation from ideal detector
!      X-axis (laboratory Y-axis) TO X-axis of actual detector i.e.
!      scanned film, or image plate (radians)
     Real, Save :: polarisation = 0.99 ! Polarisation, defined as
!      (I_h - I_v) / (I_h + I_v), where horizontal should normally
!      correspond to the X-direction of the image
     Real :: radial_error ! Estimated average error (radially) in
!      coordinate positions if defined (metres)
     Real :: radius1 ! Radius of circle or first radius of ellipse if
!      defined (metres)
     Real :: radius2 ! Second radius of ellipse if defined (metres)
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / 'EXIT', 'BEAM CENTRE', 'FULL', &
       'OUTPUT', '?', 'CAKE', 'INPUT', 'TILT', 'HELP', 'CORRECTION' /
     Data (MENU(item), item = 11, 20) / 'INTEGRATE', 'Z-SCALING', 'PRINT', &
       'EXCHANGE', 'MASK', 'ZOOM IN', 'CALIBRANT', 'DISPLAY', 'OPTIONS', &
       'UN-ZOOM' /
     Data (MENUTXT(item), item = 1, 10) / &
       'EXIT: Exit menu', &
       'BEAM CENTRE: Determine beam centre by a choice of methods', &
       'FULL: View image of full data', 'OUTPUT: Save data in an output file', &
       '?: This help on the menu choices', &
       'CAKE: Versatile multiple 2-theta scans integration', &
       'INPUT: Input data from a file on disk', &
       'TILT: Fit tilt and beam centre to powder rings', &
       'HELP: Help text on this graphical menu', &
       'CORRECTION: Spatial distortion correction' /
     Data (MENUTXT(item), item = 11, 20) / &
       'INTEGRATE: 2-D to 1-D 2-theta integration', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'PRINT: Output current graphics to PostScript file', &
       'EXCHANGE: Swap current data with the "memory"', &
       'MASK: Defined masked-off regions of the image', &
       'ZOOM IN: Define smaller graphical display region', &
       'CALIBRANT: Refine wavelength, distance, tilt, etc from powder', &
       'DISPLAY: Further graphical display possibilities', &
       'OPTIONS: Graphics display control menu', &
       'UN-ZOOM: Zoom out to see more of the data' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_PD ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_PD ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_PD'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Arguments would appear to be reasonable, go ahead.
        mask_data = .True.
        mask_memory = .False.
 
!     Loop inputting menu commands until requested to stop
        num_menu = Max_menu
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image or 1-D plot
                 Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
                 If (mask_data) Then
                    print_type = 'masked_image'
                 Else
                    print_type = 'image'
                 End If
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE POWDER'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'DIFFRACTION GUI'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'PRESS "INPUT" TO '
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'SELECT A DATA FILE'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_PPROMPT (Max_message, 10, MESSAGE, status)
 
              End If
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 10, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 10, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, MESSAGE, Max_menu, num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           data_warning = .False.
           command = 'null'
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
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
 
                 Call F2D_CLICK (data_type, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
 
              End If
 
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
 
              Else If (command .Eq. 'BEAM CENTRE') Then
 
!              Determine beam centre
                 If (data_defined) Then
 
                    Call F2D_BEAMCENTRE (.True., .True., xmaxdat, ymaxdat, &
                      %val(pDATA), %val(pMASK), %val(pXAXIS), %val(pYAXIS), &
                      xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                      ylabel, zlabel, .True., method_used, experiment, &
                      radius1, radius2, angle, radial_error, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'CAKE') Then
 
                 If (data_defined) Then
 
                    Call F2D_CAKE (.True., xmaxdat, ymaxdat, data_defined, &
                      title, xlabel, ylabel, zlabel, variance_exist, xnumdat, &
                      ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
                      experiment, lorentz_geometry, data_type, &
                      memory_defined, mask_data, mask_memory, mtitle, mxlabel, &
                      mylabel, mzlabel, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mx_pixel_size, &
                      my_pixel_size, memory_data_type, status)
                    update_image = .False.

                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'CALIBRANT') Then
 
                 If (data_defined) Then
 
!                 Fit beam centre and tilt angles
                    Call F2D_CALIBRANT (.True., xmaxdat, ymaxdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
                      %val(pDATA), %val(pMASK), title, xlabel, ylabel, zlabel, &
                      experiment, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'CORRECTION') Then
 
                 If (data_defined .And. memory_exist) Then
 
!                 Correct data for spatial distortion
                    Call F2D_GUI_CORRECTION (input_options, &
                      data_defined, memory_exist, &
                      memory_defined, variance_exist, xmaxdat, ymaxdat, title, &
                      xlabel, ylabel, zlabel, xnumdat, ynumdat, xstrelm, &
                      ystrelm, xendelm, yendelm, &
                      experiment%x_pixel_size, experiment%y_pixel_size, &
                      mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
                      mxstrelm, mystrelm, mxendelm, myendelm, mx_pixel_size, &
                      my_pixel_size, experiment, status)
 
                 Else If (data_defined) Then
 
                    MESSAGE(1) = 'MEMORY DOES NOT EXIST'
                    Call GS_FWARNING (1, 1, MESSAGE, status)
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                   %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, variance_exist, &
                   experiment, xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, print_type, &
                   status)
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variance_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
!              Swap masked drawing variables
                 Call IO_LSWAP (mask_data, mask_memory, status)
                 Call IO_ISWAP (data_type, memory_data_type, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_PDHELP (.True., status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Set data temporary undefined
                 temp_data_defined = .False.

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_PD: "INPUT"'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT OF IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, temp_data_defined, input_file, &
                   xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 data_type = 1

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_PD: After FIO_GUI_INPUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!              On input correction
                 If (temp_data_defined .And. memory_exist) Then
 
!                 Correct data for spatial distortion
                    Call F2D_GUI_CORRECTION (input_options, &
                      data_defined, memory_exist, &
                      memory_defined, variance_exist, xmaxdat, ymaxdat, title, &
                      xlabel, ylabel, zlabel, xnumdat, ynumdat, xstrelm, &
                      ystrelm, xendelm, yendelm, &
                      experiment%x_pixel_size, experiment%y_pixel_size, &
                      mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
                      mxstrelm, mystrelm, mxendelm, myendelm, mx_pixel_size, &
                      my_pixel_size, experiment, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''F2D_GUI_PD: After FIO_GUI_CORRECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG


                 End If
 
                 data_defined = data_defined .Or. temp_data_defined
 
              Else If (command .Eq. 'INTEGRATE') Then
 
                 If (data_defined) Then
 
                    Call F2D_INTEGRATE (input_file, xmaxdat, ymaxdat, &
                      data_defined, title, &
                      xlabel, ylabel, zlabel, variance_exist, xnumdat, & 
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), xstrelm, ystrelm, xendelm, yendelm, &
                      experiment, lorentz_geometry, &
                      memory_defined, mtitle, mxlabel, mylabel, mzlabel, &
                      mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
                      %val(pMDATA), mxstrelm, mystrelm, mxendelm, myendelm, &
                      mx_pixel_size, my_pixel_size, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''F2D_GUI_PD: After F2D_INTEGRATE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!                 Turn-on fully automatic scaling for X-Y graph
                    Call GS_SET_AUTODDR ( .True., .True., .True., .True., &
                      status)
 
!                 Make sure that either a line will be drawn to
!                 show the graph or markers
                    Call GS_INQ_CURVESTYLE (1, draw_line, draw_markers, &
                      draw_errorboxes, status)
                    If ((.Not. draw_line) .And. (.Not. draw_markers)) Then
                       Call GS_SET_CURVESTYLE (1, .True., draw_markers, &
                         draw_errorboxes, status)
                    End If
 
!                 Swap masked drawing variables
                    Call IO_LSWAP (mask_data, mask_memory, status)

                    memory_data_type = data_type
                    data_type = 2

                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, experiment, &
                   variance_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variance_exist, &
                      experiment%x_pixel_size, experiment%y_pixel_size, status)
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'MASK') Then
 
                 If (data_defined) Then
 
                    Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                      ylabel, zlabel, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pMASK), status)
                    update_image = .False.
                    mask_data = .True.
                 Else
                    data_warning = .True.
                 End If
                 update_menu = .True.
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, mask_data, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, xlabel, ylabel, &
                      zlabel, variance_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'TILT') Then
 
!              Output current graphics to file, prompting for file name if 
!              necessary
                 If (data_defined) Then
 
                    Call F2D_TILTCENTRE (.True., xmaxdat, ymaxdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
                      %val(pDATA), %val(pMASK), title, xlabel, ylabel, zlabel, &
                      experiment, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
 
                    Call F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                      ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 If (data_defined) Then
 
                    Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), title, xlabel, ylabel, zlabel, xstrelm, &
                      ystrelm, xendelm, yendelm, &
                      experiment, .False., x_coordinate, y_coordinate, status)
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              MESSAGE(1) = 'DATA NEEDED, BUT NONE IS DEFINED'
              Call GS_FWARNING (1, 1, MESSAGE, status)
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
 
     End Subroutine F2D_GUI_PD
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 
 
 

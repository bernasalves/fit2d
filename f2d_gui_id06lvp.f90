!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_gui_id06lvp.f90 *
!  *                     *
!  ***********************
 
!+ F2D_GUI_ID06LVP - FIT 2-D GUI ID 06 Large Volume Press data reduction
     Subroutine F2D_GUI_ID06LVP (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for powder diffraction on ID 06 1-D detector
!  Keywords:
!    Powder~Diffraction.Interface, Interface.Powder~Diffraction
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.11 Changes to "F2D_GUI_MATHS" (Hammersley)
!    26-Nov-2014: V0.10 Change "F2D_CLICK" so that it accounts for integrated 
!      data. Add "MATHS" button (Hammersley)
!    28-Jul-2014: V0.9 Temporary disable (Hammersley)
!    17-Jun-2014: V0.8 Try using two lines in "CALIBRATE" (Hammersley)
!    22-May-2014: V0.7 Add command to convert to Cartesian (Hammersley)
!    23-Apr-2014: V0.6 Add "RE-BIN" command (Hammersley)
!    19-Mar-2014: V0.5 Remove pointless options (Hammersley)
!    04-Mar-2014: V0.4 Remove "INTEGRATE" option (Hammersley)
!    24-Jan-2014: V0.3 Add "INPUT RAW" and "INPUT" options (Hammersley)
!    16-Jan-2014: V0.2 Add code (Hammersley)
!    15-Jan-2014: V0.1 Original, based on "F2D_GUI_PD" (Hammersley)
!  Modules:
!     Use IO_LIB
!     Use MA_LIB
!     Use GS_LIB
!     Use FIO_LIB
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
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
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
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
     Integer, Parameter :: Max_menu = 20 ! Dimension size of menu
     Integer, Parameter :: Max_message = 12 ! Dimension size of message
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
     Integer, Save :: data_type ! Type of image data:
!      0 = Unknown (No D-spacings)
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
     Real, Save :: azimuth_end = 2.0 * Pi ! Azimuth at start of full input data 
!      region (radians)
     Real, Save :: azimuth_start = 0.0 ! Azimuth at start of full input data 
!      region (radians)
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
     Data (MENU(item), item = 1, 10) / &
       'EXIT', &
       'FULL', &
       'OUTPUT', &
       '?', &
       'CAKE', &
       'INPUT', &
       'INPUT RAW', &
       'HELP', &
       'Z-SCALING', &
       'PRINT' /
     Data (MENU(item), item = 11, 20) / &
       'EXCHANGE', &
       'MASK', &
       'ZOOM IN', &
       'CALIBRANT', &
       'DISPLAY', &
       'OPTIONS', &
       'RE-BIN', &
       'UN-ZOOM', &
       'CARTESIAN', &
       'MATHS' /
     Data (MENUTXT(item), item = 1, 10) / &
       'EXIT: Exit menu', &
       'FULL: View image of full data', 'OUTPUT: Save data in an output file', &
       '?: This help on the menu choices', &
       'CAKE: Versatile multiple 2-theta scans integration', &
       'INPUT: Input data from a file on disk', &
       'INPUT RAW: Input two raw images and dark current to form composite', &
       'HELP: Help text on this graphical menu', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'PRINT: Output current graphics to PostScript file' /
     Data (MENUTXT(item), item = 11, 20) / &
       'EXCHANGE: Swap current data with the "memory"', &
       'MASK: Defined masked-off regions of the image', &
       'ZOOM IN: Define smaller graphical display region', &
       'CALIBRANT: Refine wavelength, distance, tilt, etc from powder', &
       'DISPLAY: Further graphical display possibilities', &
       'OPTIONS: Graphics display control menu', &
       'RE-BIN: Re-bin pixels by integer or non-integer factors', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'CARTESIAN: Convert polar data to Cartesian image', &
       'MATHS: Maths operations, with scalars or element by element' /
   
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_ID06LVP ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_ID06LVP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_ID06LVP'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Call GS_BACKGROUND (status)
!        Call GS_FWARNING (1, 1, 'TEMPORARILY DISABLED', status)
!        Return
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Arguments would appear to be reasonable, go ahead.
        mask_data = .True.
        mask_memory = .False.
 
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
                 MESSAGE(2) = 'WELCOME TO THE ID 06'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'LARGE VOLUME CELL POWDER'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'DIFFRACTION GUI'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'PRESS "INPUT" TO '
                 MESSAGE(9) = ' '
                 MESSAGE(10) = 'SELECT DATA FILES'
                 MESSAGE(11) = ' '
                 MESSAGE(12) = ' '
                 Call GS_PPROMPT (Max_message, 12, MESSAGE, status)
 
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
                   xstrelm, ystrelm, xendelm, yendelm, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
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
 
              Else If (command .Eq. 'CAKE') Then
 
                 If (data_defined) Then
 
                    Call F2D_ID06_CAKE (.True., xmaxdat, ymaxdat, &
                      azimuth_start, azimuth_end, data_defined,&
                      title, xlabel, ylabel, zlabel, variances_exist, xnumdat, &
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
                    Call F2D_ID06_CALIBRANT3 (.True., xmaxdat, ymaxdat, &
                      xnumdat, ynumdat, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
                      %val(pDATA), %val(pMASK), title, xlabel, ylabel, zlabel, &
                      azimuth_start, azimuth_end, experiment, status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'CARTESIAN') Then
 
!              Convert polar data to cartesian image
                 Call F2D_ID06_CARTESIAN (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, %val(pDATA), experiment, &
                   azimuth_start, azimuth_end, mxnumdat, mynumdat, &
                   mxstrelm, mystrelm, mxendelm, myendelm, %val(pMDATA), &
                   memory_defined, mx_pixel_size, my_pixel_size, status)

!              Define axis data
                 Call F2D_AXES (xmaxdat, mxnumdat, 0.5, 1.0, %val(pMXAXIS), &
                   status)
                 Call F2D_AXES (ymaxdat, mynumdat, 0.5, 1.0, %val(pMYAXIS), &
                   status)
                 mtitle = title
                 mxlabel = xlabel
                 mylabel = ylabel
                 mzlabel = zlabel
                    
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
!              Swap masked drawing variables
                 Call IO_LSWAP (mask_data, mask_memory, status)
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                   %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, variances_exist, &
                   experiment, xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, &
                   mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, print_type, &
                   status)
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
!              Swap masked drawing variables
                 Call IO_LSWAP (mask_data, mask_memory, status)

!              Swap data type variables
                 Call IO_ISWAP (data_type, memory_data_type, status)
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
!                 Call F2D_GUI_ID06LVPHELP (.True., status)
! ??????????????????????????????????????????????????????????????
! ???????????????????  USE PD HELP for the moment ??????????????
                 Call F2D_GUI_PDHELP (.True., status)
! ??????????????????????????????????????????????????????????????
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, ymaxdat, variances_exist, &
                   data_defined, input_file, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 data_type = 3

                 If (data_defined) Then

!                 User input of data start and end azimuths
                    Call F2D_INP_ID06_AZIMUTHS (azimuth_start, azimuth_end, &
                      status)

                 End If

              Else If (command .Eq. 'INPUT RAW') Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_ID06LVP: "INPUT RAW"'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!              Input two images, dark current subtracted, and from composite
                 Call F2D_ID06_INPUT (input_options, xmaxdat, ymaxdat, &
                   variances_exist, data_defined, input_file, &
                   xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 data_type = 3

                 If (data_defined) Then

!                 User input of data start and end azimuths
                    Call F2D_INP_ID06_AZIMUTHS (azimuth_start, azimuth_end, &
                      status)

                 End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''F2D_GUI_ID06LVP: After F2D_ID06_INPUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, xlabel, ylabel, zlabel, experiment, &
                   variances_exist, xstrelm, ystrelm, xendelm, yendelm, &
                   print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      xlabel, ylabel, zlabel, variances_exist, &
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
 
              Else If (command .Eq. 'MATHS') Then

                 If (data_defined) Then
 
!                 Interactive image display and manipulation
                    Call F2D_GUI_MATHS (experiment, data_defined, &
                      memory_exist, memory_defined, variances_exist, &
                      mask_exist, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      mxnumdat, mynumdat, %val(pMXAXIS), &
                      %val(pMYAXIS), %val(pMDATA), %val(pMVARIANCES), &
                      mxstrelm, mystrelm, mxendelm, myendelm, &
                      mtitle, mxlabel, mylabel, &
                      mzlabel, mx_pixel_size, my_pixel_size, &
                      %val(pXAXIS), %val(pYAXIS), &
                      title, xlabel, ylabel, zlabel, &
                      xstrelm, ystrelm, xendelm, yendelm, &
                      %val(pDATA), %val(pVARIANCES), %val(pMASK), status)
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, mask_data, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, xlabel, ylabel, &
                      zlabel, variances_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'RE-BIN') Then
 
!              Re-bin image
                 Call F2D_REBIN (.True., mask_exist, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xmaxdat, ymaxdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pMASK), &
                   %val(pVARIANCES), xstrelm, ystrelm, xendelm, yendelm, &
                   variances_exist, retstat, %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), mxstrelm, mystrelm, &
                   mxendelm, myendelm, mxnumdat, mynumdat, mx_pixel_size, &
                   my_pixel_size, status)
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_defined,&
                   mx_pixel_size, my_pixel_size, status)
 
!              Set memory labels
                 mtitle = title
                 mxlabel = xlabel
                 mylabel = ylabel
                 mzlabel = zlabel
 
                 If (retstat .Eq. 0) Then
!                 Write (*, '(''memory_defined'')')
                    memory_defined = .True.
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
 
     End Subroutine F2D_GUI_ID06LVP
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 
 
 

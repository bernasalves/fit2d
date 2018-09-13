!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_gui_saxs.f90 *
!  *                  *
!  ********************
 
!+ F2D_GUI_SAXS - FIT 2-D GUI SAXS / GISAXS
     Subroutine F2D_GUI_SAXS (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for Small Angle X-ray Scattering and Grazing Incidence
!    Small Angle X-ray Scattering on 2-D detectors
!  Keywords:
!    Small~Angle~X-ray~Scattering.Interface, Interface.SAXS,
!    Interface.Small~Angle~X-ray~Scattering, SAXS,Interface,
!    Grazing~Incidence.Small~Angle~X-ray~Scattering, GISAXS.Interface
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.19 Changes to "F2D_GUI_MATHS" (Hammersley)
!    27-Nov-2014: V0.18 Changes to "F2D_CAKE" (Hammersley)
!    26-Nov-2014: V0.17 Changes to "F2D_CLICK" (Hammersley)
!    05-Feb-2010: V0.16 Changes to "F2D_GUI_MATHS" subroutine to correct "MEMORY
!      NOT DEFINED" error (Hammersley)
!    24-Apr-2006: V0.15 Add "input_options" structure (Hammersley)
!    17-Mar-2006: V0.14 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.13 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.12 Alter menu lay-out for landscape windows (Hammersley)
!    20-Apr-2001: V0.11 Add "REMEMBER ZOOM" command to menu (Hammersley)
!    09-Feb-2001: V0.10 Add "MATHS" command to menu. Remove "under
!      development" warning message (Hammersley)
!    29-Apr-1999: V0.9 Add "SPHERICAL MOD" command to perform
!      Abs(Sin(theta)) correction (Hammersley)
!    23-Feb-1999: V0.8 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    17-Feb-1999: V0.7 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    15-Dec-1998: V0.6 Change to use IO internal database routines (Hammersley)
!    27-Oct-1998: V0.5 Don't set default geometry (Hammersley)
!    25-May-1998: V0.4 Add "1-D TRANSFORMS" command (Hammersley)
!    20-May-1998: V0.3 Save tilt angles between calls to FIT2D (Hammersley)
!    17-Apr-1998: V0.2 Add warning message (Hammersley)
!    31-Mar-1998: V0.1 Original based on "F2D_GUI_PD" (Hammersley)
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
     Character(Len = 5), Parameter :: Version = 'V0.19' ! Version number
     Integer, Parameter :: Max_menu = 23 ! Dimension size of menu
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
     Integer, Save :: lorentz_geometry = 1 ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the
!            equivalent of a 2-theta scan i.e. detector at equal distance
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
!       2 = Key not found for retrieval, or element doesn't exist
     Logical :: temp_data_defined ! .True., if data has been input
     Logical :: continue ! .True., until user wants to exit
     Integer, Save :: data_type ! Type of data
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
     Logical, Save :: mask_data ! .True., if the current data is to be masked
     Logical, Save :: mask_memory ! .True., if the memory data is to be masked
     Integer :: memory_data_type ! Type of data
!      0 = Unknown (No D-spacings)
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
     Logical :: remember_roi ! .True., if the region of interest is to be
!      remembered between input images
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: angle ! Angle of "radius1" of ellipse to X-axis
!      (anti-clockwise radians)
     Real, Save :: detector_gain = 1.0 ! The gain of the detector i.e. the
!      intensity values divided by the gain should produce counts
     Real :: radial_error ! Estimated average error (radially) in
!      coordinate positions if defined (metres)
     Real :: radius1 ! Radius of circle or first radius of ellipse if
!      defined (metres)
     Real :: radius2 ! Second radius of ellipse if defined (metres)
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
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / 'EXIT', 'BEAM CENTRE', 'FULL', &
       'OUTPUT', '?', 'CAKE', 'INPUT', 'NORMALISE', 'HELP', 'PROJECTION' /
     Data (MENU(item), item = 11, 20) / 'INTEGRATE', 'Z-SCALING', 'PRINT', &
       'EXCHANGE', 'MASK', 'ZOOM IN', 'DISPLAY', 'OPTIONS', '1-D TRANSFORMS', &
       'UN-ZOOM' /
     Data (MENU(item), item = 21, 23) / 'SPHERICAL MOD', 'MATHS', &
       'REMEMBER ROI' /
     Data (MENUTXT(item), item = 1, 10) / 'EXIT: Exit menu', &
       'BEAM CENTRE: Determine beam centre by a choice of methods', &
       'FULL: View image of full data', 'OUTPUT: Save data in an output file', &
       '?: This help on the menu choices', &
       'CAKE: Versatile multiple 2-theta scans integration', &
       'INPUT: Input data from a file on disk', &
       'NORMALISE: Apply intensity normalisation corrections', &
       'HELP: Help text on this graphical menu', &
       'PROJECTION: Integrate rectangular region to 1-D scan' /
     Data (MENUTXT(item), item = 11, 20) / &
       'INTEGRATE: 2-D to 1-D 2-theta integration', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'PRINT: Output current graphics to PostScript file', &
       'EXCHANGE: Swap current data with the "memory"', &
       'MASK: Defined masked-off regions of the image', &
       'ZOOM IN: Define smaller graphical display region', &
       'DISPLAY: Further graphical display possibilities', &
       'OPTIONS: Graphics display control menu', &
       '1-D TRANSFORMS: Choice of conversion of intensity and Q scales', &
       'UN-ZOOM: Zoom out to see more of the data' /
     Data (MENUTXT(item), item = 21, 23) / &
       'SPHERICAL MOD: I'' = I * ABS(SIN(angle)) starting from Meridian', &
       'MATHS: Maths operations, with scalars or element by element', &
       'REMEMBER ROI: Remember region of interest between input images' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_SAXS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_SAXS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
        mask_data = .True.
        mask_memory = .False.
 
!     Input remember zoom from internal data-base
        Call IO_INQ_LKEYVALUE ('REMEMBER_ROI', remember_roi, retstat, status)
 
        If (remember_roi) Then
           MENU(23) = 'FORGET ROI'
           MENUTXT(23) = 'FORGET ROI: Don''t remember region ' // &
             'of interest between input images'
        Else
           MENU(23) = 'REMEMBER ROI'
           MENUTXT(23) = 'REMEMBER ROI: Remember region of ' // &
             'interest between input images'
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
                 MESSAGE(2) = 'WELCOME TO THE SAXS /'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = '    GISAXS GUI'
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
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 15, 14, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 15, 14, status)
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
 
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
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
                      ylabel, zlabel, .True., &
                      method_used, experiment, radius1, radius2, angle, &
                      radial_error, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'CAKE') Then
 
                 If (data_defined) Then
                    Call F2D_CAKE (.True., xmaxdat, ymaxdat, data_defined, &
                      title, xlabel, ylabel, zlabel, variances_exist, xnumdat, &
                      ynumdat, xstrelm, ystrelm, xendelm, yendelm, experiment, &
                      lorentz_geometry, data_type, &
                      memory_defined, mask_data, mask_memory, mtitle, mxlabel, &
                      mylabel, mzlabel, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mx_pixel_size, &
                      my_pixel_size, memory_data_type, status)
                    update_image = .False.
                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
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
 
              Else If (command .Eq. 'FORGET ROI') Then
 
                 remember_roi = .False.
                 MENU(23) = 'REMEMBER ROI'
                 MENUTXT(23) = 'REMEMBER ROI: Remember region of ' // &
                   'interest between input images'
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_SAXSHELP (.True., status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Set data temporary undefined
                 temp_data_defined = .False.
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT OF IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, &
                   ymaxdat, variances_exist, temp_data_defined, input_file, &
                   xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
 
!              Change ROI if required or necessary
                 If ((.Not. remember_roi) .Or. xendelm .Gt. xnumdat .Or. &
                   yendelm .Gt. ynumdat) Then
                    xstrelm = 1
                    ystrelm = 1
                    xendelm = xnumdat
                    yendelm = ynumdat
                 End If
 
                 data_defined = data_defined .Or. temp_data_defined
 
              Else If (command .Eq. 'INTEGRATE') Then
 
                 If (data_defined) Then
 
                    Call F2D_INTEGRATE (input_file, xmaxdat, ymaxdat, &
                      data_defined, title, &
                      xlabel, ylabel, zlabel, variances_exist, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), xstrelm, ystrelm, xendelm, yendelm, &
                      experiment, lorentz_geometry, &
                      memory_defined, mtitle, mxlabel, mylabel, mzlabel, &
                      mxnumdat, mynumdat, %val(pMXAXIS), %val(pMYAXIS), &
                      %val(pMDATA), mxstrelm, mystrelm, mxendelm, myendelm, &
                      mx_pixel_size, my_pixel_size, status)
 
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
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
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
 
              Else If (command .Eq. 'NORMALISE') Then
 
                 Call GS_FWARNING (1, 1, 'NOT YET IMPLEMENTED', status)
 
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
 
              Else If (command .Eq. 'PROJECTION') Then
 
                 If (data_defined) Then
 
                    Call F2D_GUI_PROJECTION (mask_data, xmaxdat, ymaxdat, &
                      title, xlabel, ylabel, zlabel, variances_exist, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), xstrelm, ystrelm, xendelm, yendelm, &
                      experiment, &
                      memory_defined, mtitle, mxlabel, mylabel, mzlabel, &
                      mxnumdat, mynumdat, %val(pMXAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mx_pixel_size, my_pixel_size, status)
 
!                 Exchange current data and memory if successful
                    If (memory_defined) Then
 
                       Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                         title, xlabel, ylabel, zlabel, variances_exist, &
                         data_defined, &
                         experiment%x_pixel_size, experiment%y_pixel_size, &
                         xstrelm, &
                         ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                         mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
                         mxlabel, mylabel, mzlabel, memory_defined, &
                         mx_pixel_size, my_pixel_size, status)
 
                    End If
 
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
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'REMEMBER ROI') Then
 
                 remember_roi = .True.
                 MENU(23) = 'FORGET ROI'
                 MENUTXT(23) = 'FORGET ROI: Don''t remember ' // &
                   'region of interest between input images'
 
              Else If (command .Eq. 'SPHERICAL MOD') Then
 
                 If (data_defined) Then
 
                    Call F2D_SPHERICAL_MOD (xmaxdat, ymaxdat, title, xlabel, &
                      ylabel, zlabel, xnumdat, ynumdat, %val(pXAXIS), &
                      %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, &
                      experiment%x_beam, experiment%y_beam, %val(pDATA), status)
 
                    update_image = .True.
                    update_menu = .True.
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. '1-D TRANSFORMS') Then
 
                 If (data_defined) Then
 
                    Call F2D_1DTRANSFORMS (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                      xendelm, yendelm, %val(pXAXIS), %val(pYAXIS), &
                      %val(pDATA), status)
                    update_image = .True.
                    update_menu = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''Transformation completed'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Before GS_INP_UNZOOM'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Returned from UNZOOM'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two
!              points
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
 
                    Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), title, xlabel, ylabel, zlabel, xstrelm, &
                      ystrelm, xendelm, yendelm, experiment, &
                      .False., x_coordinate, y_coordinate, status)
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
 
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
 
     End Subroutine F2D_GUI_SAXS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

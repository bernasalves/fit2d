!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_gui_pressurecal.f90 *
!  *                         *
!  ***************************
 
!+ F2D_GUI_PRESSURECAL - FIT 2-D GUI PRESSURE CALibration)
     Subroutine F2D_GUI_PRESSURECAL (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for pressure calibration
!  Keywords:
!    Pressure.Calibration.Interface, Interface.Pressure.Calibration
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.11 Changes to "F2D_CLICK" (Hammersley)
!    15-Apr-2014: V0.10 Make "pressure" double precision (Hammersley)
!    11-Apr-2014: V0.9 Add "BASELINE +" and "BASELINE -" commands
!      (Hammersley)
!    10-Apr-2014: V0.8 Change increase pressure for very low pressures
!      (Hammersley)
!    11-Oct-2013: V0.7 Add "REMOVE CALIBRANT" command (Hammersley)
!    26-Sep-2013: V0.6 Protect "EXCHANGE" command for 2-D data (Hammersley)
!    20-Sep-2013: V0.5 Add "WAVELENGTH" option (Hammersley)
!    18-Sep-2013: V0.4 First user version (very primitive) (Hammersley)
!    09-Sep-2013: V0.3 Add "CALIBRANTS" array (Hammersley)
!    02-Aug-2013: V0.2 Add pressure input (Hammersley)
!    25-Jul-2013: V0.1 Original, based on "F2D_GUI_PD" (Hammersley)
!  Modules:
!     Use IO_LIB
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
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
     Integer, Parameter :: Max_calibrants = 10 ! Dimension size of "CALIBRANTS"
     Integer, Parameter :: Max_menu = 16 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
!  Local Variables:
     Real, Save :: baseline = 0.0 ! Baseline for drawing peak positions
     Integer :: calibrant ! Loop variable for calibrants
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
     Integer, Save :: num_calibrants = 0 ! Number of defined calibrants
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! "IO_INQ_*KEYVALUE" return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
     Logical :: temp_data_defined ! .True., if data has been input
     Logical :: continue ! .True., until user wants to exit
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
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: angle ! Angle of "radius1" of ellipse to X-axis (anti-clockwise 
!      radians)
     Double Precision, Save :: pressure = 1.0 ! Pressure on samples in GPa
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
     Type(EXPERIMENTAL_DETAILS), Save :: CALIBRANTS(Max_calibrants) ! Array of
!      calibrant crystals for pressure calibrantion (see "io.inc")
     Character(Len = 10), Save :: CALIBRANT_NAMES(Max_calibrants) ! Name of 
!      calibrants
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 15) / &
       'EXIT', &
       'ADD CALIBRANT', &
       'REMOVE CALIBRANT', &
       'WAVELENGTH', &
       '?', &
       'SET PRESSURE', &
       'PRESSURE +', &
       'PRESSURE -', &
       'HELP', &
       'INPUT', &
       'UN-ZOOM', &
       'ZOOM IN', &
       'EXCHANGE', &
       'BASELINE +', &
       'BASELINE -' /
     Data (MENUTXT(item), item = 1, 15) / &
       'EXIT: Exit menu', &
       'ADD CALIBRANT: Input calibrant from JCPDS file', &
       'REMOVE CALIBRANT: Remove last entered calibrant', &
       'WAVELENGTH: Set experiment wavelength', &
       '?: This help on the menu choices', &
       'SET PRESSURE: Input value for pressure in GPa', &
       'PRESSURE +: Increase pressure by 5%', &
       'PRESSURE -: Decrease pressure inversely', &
       'HELP: Help text on this graphical menu', &
       'INPUT: Input spectrum from a file', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Define smaller graphical display region', &
       'EXCHANGE: Swap current data with the "memory"', &
       'BASELINE +: Increase baseline for drawing peak positions by 5%', &
       'BASELINE -: Decrease baseline for drawing peak positions by 5%' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_PRESSURECAL ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_PRESSURECAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_PRESSURECAL'')')
!        Call GS_FWARNING (1, 1, 'COMING SOON, BUT PRESENTLY NOT AVAILABLE!', &
!          status)
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
!     Check that the data is a 1-D spectrum
        If (yendelm - ystrelm .Gt. 1) Then

           Call GS_FWARNING (1, 1, 'ONLY AVAILABLE FOR SINGLE SPECTRA', status)
           Return

        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        num_menu = 15
        input_mode = 2
        continue = .True.
        update_image = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw 1-D spectra and calibrant positions
                 Call F2D_PLOT_PRES (mask_data, xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, &
                   experiment, Max_calibrants, num_calibrants, &
                   CALIBRANT_NAMES, CALIBRANTS, pressure, baseline, status)
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE PRESSURE'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'CALIBRATION AND PHASE'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'IDENTIFICATION GUI'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = '(Inspired by S. Merkel''s HPDiff Applet) '
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
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 14, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 14, status)
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
 
                 Call F2D_CLICK (2, xmaxdat, ymaxdat, &
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
 
              Else If (command .Eq. 'ADD CALIBRANT') Then

                 If (num_calibrants .Lt. Max_calibrants) Then

                    num_calibrants = num_calibrants + 1

!                 Input calibrant from JCPDS file
                    Call FIO_IN_JCPDS (CALIBRANT_NAMES(num_calibrants), &
                      CALIBRANTS(num_calibrants), status)

!                 Check for triclinic lattice
                    If (CALIBRANTS(num_calibrants)%symmetry .Eq. Io_triclinic) &
                      Then

                       num_calibrants = num_calibrants - 1
                       Call GS_FWARNING (1, 1, &
                         'TRICLINIC lattice not implemented', status)

                    End If

                 End If

              Else If (command .Eq. 'BASELINE +') Then

                 baseline = Min(1.0, baseline + 0.05)

              Else If (command .Eq. 'BASELINE -') Then

                 baseline = Max(0.0, baseline - 0.05)

              Else If (command .Eq. 'SET PRESSURE') Then

!              Set pressure
                 Call F2D_INP_PRESSURE (pressure, status)

              Else If (command .Eq. 'PRESSURE +') Then

                 If (pressure .Ge. 0.00099d0) Then
                    pressure = pressure * 1.05d0
                 Else
                    pressure = 0.001d0
                 End If

              Else If (command .Eq. 'PRESSURE -') Then

                 pressure = pressure / 1.05d0

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
 
!              Check that the data is a 1-D spectrum
                 If (yendelm - ystrelm .Gt. 1) Then
                    
                    Call GS_FWARNING (1, 1, &
                      'ONLY AVAILABLE FOR SINGLE SPECTRA', status)
                    Return

                 End If

              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
!                 Call F2D_GUI_PRESSURECALHELP (.True., status)

              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT OF IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, &
                   xnumdat, ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
!              Check that the data is a 1-D spectrum
                 If (yendelm - ystrelm .Gt. 1) Then
                    
                    Call GS_FWARNING (1, 1, &
                      'ONLY AVAILABLE FOR SINGLE SPECTRA', status)
                    Return

                 End If

              Else If (command .Eq. 'REMOVE CALIBRANT') Then

                 If (num_calibrants .Gt. 0) Then
                    num_calibrants = num_calibrants - 1
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
 
              Else If (command .Eq. 'WAVELENGTH') Then

!              Input wavelength from user
                 Call F2D_INP_WAVELENGTH (.True., experiment%wavelength, status)
                 experiment%wavelength_set = .True.

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
 
     End Subroutine F2D_GUI_PRESSURECAL
!********1*********2*********3*********4*********5*********6*********7*********8

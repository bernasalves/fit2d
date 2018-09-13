!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_gui_reciprocal.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_RECIPROCAL - FIT 2-D GUI RECIPROCAL space mapping interface
     Subroutine F2D_GUI_RECIPROCAL (input_file, data_defined, memory_exist, &
       memory_defined, variances_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, xlabel, ylabel, zlabel, experiment, input_options, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    FIT2D GUI for reciprocal space mapping from an input series of
!    diffraction images
!  Keywords:
!    File~Series.Processing
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.30 Changes to "F2D_DISPLAY" (Hammersley)
!    08-Dec-2014: V0.29 Correctly use "stat2" (Hammersley)
!    26-Nov-2014: V0.28 Changes to "F2D_CLICK" (Hammersley)
!    13-Nov-2006: V0.27 Add volume output command (Hammersley)
!    25-Sep-2006: V0.26 Try to input auxilliary CIF file when inputting
!      (Hammersley)
!    22-Jun-2006: V0.25 Add "thickness" variable for slices (Hammersley)
!    27-Apr-2006: V0.24 Add "MASK" option and masking in transform (Hammersley)
!    24-Apr-2006: V0.23 Changes to argument lists (Hammersley)
!    18-Apr-2006: V0.22 Changes to "F2D_RMAP_CAL_COORDINATE" (Hammersley)
!    30-Mar-2006: V0.21 Correct input of diffractometer angles (Hammersley)
!    20-Mar-2006: V0.20 Add "INPUT OPTIONS" support (Hammersley)
!    17-Mar-2006: V0.19 Support for arbitrary aspect ratio windows (Hammersley)
!    10-Mar-2006: V0.18 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    03-Nov-2005: V0.17 Option to mirror images (Hammersley)
!    27-Oct-2005: V0.16 Change the way detector horizontal or vertical
!      position is handled for coordinate input (Hammersley)
!    25-Oct-2005: V0.15 Change the way detector horizontal or vertical
!      position is handled (Hammersley)
!    17-Oct-2005: V0.14 Output information from clicking on image (Hammersley)
!    13-Oct-2005: V0.13 Add "INPUT" command (Hammersley)
!    12-Oct-2005: V0.12 Add command to change UB matrix (Hammersley)
!    05-Oct-2005: V0.11 Changes to "F2D_RMAP_INUBMATRIX" (Hammersley)
!    29-Sep-2005: V0.10 Changes to "F2D_RMAP_ORIENTATION" (Hammersley)
!    09-Jun-2005: V0.9 Set 2-D section axes to "step" units (Hammersley)
!    13-May-2005: V0.8 Add "C5C1" etc. in call to "F2D_RMAP_ORIENTATION"
!      and "F2D_RMAP_MAPSERIES" (Hammersley)
!    12-May-2005: V0.7 Add "ZOOM IN" and "UN-ZOOM" controls (Hammersley)
!    11-May-2005: V0.6 Include "C2C1" etc. in call to "F2D_RMAP_ORIENTATION"
!      (Hammersley)
!    21-Apr-2005: V0.5 Transfer coordinates to transformation (Hammersley)
!    20-Apr-2005: V0.4 Implementing mapping (Hammersley)
!    12-Apr-2005: V0.3 Initialise map (Hammersley)
!    04-Apr-2005: V0.2 Add under development warning message (Hammersley)
!    21-Mar-2005: V0.1 Original (Hammersley)
!  Modules:
!     Use IO_LIB
!     Use MA_LIB
!     Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
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
     Logical, Intent(INOUT) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(INOUT) :: mask_exist ! .True., if the mask array exists
     Integer, Intent(INOUT) :: xmaxdat ! X-direction dimension for data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Y-direction dimension for data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
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
     Type(INPUT_OPTIONS_STRUCTURE), Intent(INOUT) :: input_options ! Control of 
!      input of auxiliary experimental information (see "io.inc")
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
!      the data values
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Real, Intent(OUT) :: mx_pixel_size ! Memory X-direction pixel size (metres)
     Real, Intent(OUT) :: my_pixel_size ! Memory Y-direction pixel size (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.30' ! Version number
     Integer, Parameter :: Max_menu = 18 ! Dimension size of menu
     Integer, Parameter :: Max_message = 18 ! Dimension size of message
!  Local Variables:
     Character(Len = 256) :: aux_file ! File name for auxilliary input
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Return status variable
     Integer stat1 ! Allocate return status variable
     Integer stat2 ! Allocate return status variable
     Integer, Save :: xmaxmap = 256 ! First dimension of reciprocal map
     Integer, Save :: xnummap = 256 ! Number of pixels in 1st dimension of map
     Integer, Save :: ymaxmap = 256 ! Second dimension of reciprocal map
     Integer, Save :: ynummap = 256 ! Number of pixels in 2nd dimension of map
     Integer, Save :: zmaxmap = 16 ! Third dimension of reciprocal map
     Integer, Save :: znummap = 5 ! Number of pixels in 3rd dimension of map
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: destory_map ! .True., if the reciprocal map is to be destoried
     Logical :: geometry_defined ! .True., if experiment geometry is defined
     Logical, Save :: map_allocated = .False. ! .True., if the map is allocated
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical :: use_auxiliary_file = .True. ! .True., if experimental details
!      are to be input from an auxiliary file
     Integer :: window_format ! Format of graphics window
     Real :: c3c1s ! Difference vector C3 - C1
     Real :: c5c1s ! Square length of difference vector C5 - C1
     Real :: height ! Height of window in page coordinates 
     Real :: step ! Step between sections
     Real :: thickness ! Thickness of a single section
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate position
!  Local Arrays:
     Character(Len = 80) :: ERROR(1) ! Error messages
     Character(Len = 16), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 80) :: PROMPT(2) ! User prompt
     Real :: C1(3) ! Starting coordinate of first section
     Real :: C2C1(3) ! Difference vector C2 - C1
     Real :: c2c1s ! Difference vector C2 - C1
     Real :: C3C1(3) ! Difference vector C3 - C1
     Real :: C5(3) ! Difference vector between sections
     Real :: C5C1(3) ! Difference vector C5 - C1
     Real :: CN(3) ! Normalised difference vector
     Real, Save :: DIFFERENCE(3) ! HKL difference between planes
     Real, Save :: LL(3) ! HKL of lower left-hand corner of volume to map
     Real, Save :: LR(3) ! HKL of lower right-hand corner of volume to map
     Real, Allocatable, Save :: MAP(:, :, :) ! Reciprocal map array
     Real, Allocatable, Save :: NORMALISE(:, :, :) ! Normalisation array
     Real, Save :: UP(3) ! HKL of upper boundary point of first section of map
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, Max_menu) / &
       'EXIT', &
       'DISPLAY', &
       'EXCHANGE', &
       '?', &
       'HELP', &
       'INITIALISE MAP', &
       'INPUT', &
       'OPTIONS', &
       'MAP IMAGE SERIES', &
       'MASK', &
       'PRINT', &
       'OUTPUT', &
       'SAVE VOLUME', &
       'TRANSFER SECTION', &
       'UB MATRIX', &
       'UN-ZOOM', &
       'ZOOM IN', &
       'Z-SCALING' /
     Data (MENUTXT(item), item = 1, Max_menu) / &
       'EXIT: Exit FIT2D', &
       'DISPLAY: Further graphical display possibilities', &
       '?: This help on the menu choices', &
       'HELP: Help text on the "FILE SERIES" menu', &
       'INITIALISE MAP: Reset map arrays to zero', &
       'INPUT: Input data from a file on disk', &
       'EXCHANGE: Swap current data with the "memory"', &
       'MAP IMAGE SERIES: Input and map a series of diffraction images', &
       'MASK: Defined masked-off regions of the image', &
       'OPTIONS: Further display control menu', &
       'PRINT: Output current graphics a PostScript file', &
       'OUTPUT: Save data in an output file', &
       'SAVE VOLUME: Output volume to binary file', &
       'TRANSFER SECTION: Transfer plane of map to data array', &
       'UB MATRIX: Re-enter or calculate UB (orientation) matrix', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Graphical region of interest definition', &
       'Z-SCALING: Automatic or user control of intensity display range' /
     Data DIFFERENCE / 0.0, 0.0, 0.5 /
     Data LL / 0.0, 0.0, 0.0 /
     Data LR / 2.0, 0.0, 0.0 /
     Data UP / 0.0, 2.0, 0.0 /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_RECIPROCAL ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_RECIPROCAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
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
!     Under development warning
        Call GS_BACKGROUND (status)
        Call GS_FWARNING (1, 1, 'This interface is under ' // &
          'development, incomplete and likely to change', status)
 
        If (.Not. map_allocated) Then
 
!        Enter size of map to produce and try to allocate memory
           Call F2D_RMAP_SIZE (xmaxmap, ymaxmap, zmaxmap, retstat, status)
 
           If (retstat .Ne. 0) Then
              Return
           End If

!         Allocate arrays
           Allocate (MAP(xmaxmap, ymaxmap, zmaxmap), Stat = stat1)
           Allocate (NORMALISE(xmaxmap, ymaxmap, zmaxmap), Stat = stat2)

           If (stat1 .Ne. 0 .Or. stat2 .Ne. 0) Then
 
!           User warning message
              Call GS_FWARNING (1, 1, 'Failed to allocate ' // &
                'memory for map. You must reduce the sizes', status)
              Return

           Else
              map_allocated = .True.
           End If
 
!        Initialise map
           Call F2D_RMAP_INITIALISE (xmaxmap, ymaxmap, zmaxmap, MAP, &
             NORMALISE, status)

!        Input experimental details from a CIF/CBF file
           Call GS_BACKGROUND (status)
           MESSAGE(1) = 'Enter "TRUE" if experimental detail such as geometry'
           MESSAGE(2) = 'is to be input from a CIF or CBF file'
           ERROR(1) = 'Select "TRUE" of "FALSE" only'
           Call GS_INPL (.True., 0, 1, .True., &
             'USE CIF/CBF TO INPUT EXPERIMENTAL DETAILS', 2, MESSAGE, &
             1, ERROR, use_auxiliary_file, status)

           If (use_auxiliary_file) Then
              Call F2D_INPUT_EXPERIMENT (input_options, experiment, status)
           End If

           If (.Not. use_auxiliary_file .Or. &
             .Not. (input_options%input_pixel_sizes .And. &
             input_options%input_detector_distance .And. &
             input_options%input_wavelength .And. &
             input_options%input_beam_centre)) Then

!           Define diffraction geometry
              Call F2D_GUI_EXPERIMENT (.False., experiment, status)

           End If

           If (.Not. use_auxiliary_file .Or. &
             .Not. input_options%input_ub_matrix) Then

!           Define UB matrix and inverse
              Call F2D_RMAP_INUBMATRIX (input_options, &
                xmaxdat, ymaxdat, xnumdat, ynumdat, &
                %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                xlabel, ylabel, zlabel, experiment, retstat, status)

           End If

           If (retstat .Ne. 0) Then
              Return
           End If
 
!        Define orientation of map planes
           Call F2D_RMAP_ORIENTATION (xmaxmap, ymaxmap, zmaxmap, &
             experiment, xnummap, ynummap, znummap, &
             LL, LR, UP, DIFFERENCE, C1, C5, CN, C2C1, &
             c2c1s, C3C1, c3c1s, C5C1, c5c1s, step, thickness, status)
 
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
 
!              Redraw image
                 Call GS_MPLOT (.True., xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
                 print_type = 'masked_image'
 
              Else
 
!              Re-draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE RECIPROCAL'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'SPACE MAPPING INTERFACE'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'The commands allow a'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'file series of diffraction'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = 'images to be transformed to'
                 MESSAGE(11) = ' '
                 MESSAGE(12) = 'build up a reciprocal space'
                 MESSAGE(13) = ' '
                 MESSAGE(14) = 'lattice.'
                 MESSAGE(15) = ' '
                 MESSAGE(16) = ' '
                 MESSAGE(17) = ' '
                 Call GS_PPROMPT (Max_message, 17, MESSAGE, status)
 
              End If
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!            Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              num_menu = Max_menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 16, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 16, status)
              End If
 
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, &
                status)
 
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
 
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                   xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                   experiment, update_image, update_menu, status)
 
!              Calculate angles relative to beam centre etc.
                 Call F2D_RMAP_CAL_COORDINATE (x_coordinate, y_coordinate, &
                  experiment, status)
 
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
                   xstrelm, ystrelm, xendelm, yendelm, &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_RECIPROCALHELP (.True., status)
 
              Else If (command .Eq. 'INITIALISE MAP') Then
 
!              Set map to zero
                 Call F2D_RMAP_INITIALISE (xmaxmap, ymaxmap, zmaxmap, &
                   MAP, NORMALISE, status)
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT IMAGE DATA'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (2, 2, PROMPT, 0, &
                   input_options, xmaxdat, ymaxdat, &
                   variances_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 update_image = .True.
                 update_menu = .True.
 
!               Try to input diffractometer angles from auxiliary files .cif
                 Call IO_FILEEXTENSION (input_file, 'cif', retstat, aux_file, &
                   status)
                 Call FIO_IN_AUXILIARY (input_options, aux_file, retstat, &
                   experiment, status)

!              Input diffractometer angles corresponding to image
                 Call F2D_RMAP_ANGLES (experiment, status)
 
              Else If (command .Eq. 'MAP IMAGE SERIES') Then
 
!              Input and map a series of images
                 Call F2D_RMAP_MAPSERIES (xmaxmap, ymaxmap, zmaxmap, xnummap, &
                   ynummap, znummap, C1, C2C1, c2c1s, C3C1, c3c1s, C5, C5C1, &
                   c5c1s, CN, step, thickness, xmaxdat, ymaxdat, xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, input_options, &
                   experiment, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), %val(pMXAXIS), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), %val(pMASK), title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   mask_exist, MAP, NORMALISE, status)
 
              Else If (command .Eq. 'MASK') Then

                 If (data_defined) Then
 
                    Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                      ylabel, zlabel, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pMASK), status)
                    update_image = .False.
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
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    Call F2D_PRINT (.True., print_type, mask_exist, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, xlabel, ylabel, &
                      zlabel, variances_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'SAVE VOLUME') Then

!              Output volume to output file
                 Call F2D_RMAP_OUT_VOLUME (input_file, &
                   xmaxmap, ymaxmap, zmaxmap, &
                   xnummap, ynummap, znummap, MAP, NORMALISE, status)

              Else If (command .Eq. 'TRANSFER SECTION') Then
 
!              Transfer section of map to data array and display
                 Call F2D_RMAP_TRANSFER (LL, LR, UP, DIFFERENCE, xmaxmap, &
                   ymaxmap, zmaxmap, xnummap, ynummap, znummap, MAP, &
                   NORMALISE, step, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), title, xlabel, ylabel, zlabel, &
                   status)
 
                 update_image = .True.
 
              Else If (command .Eq. 'UB MATRIX') Then
 
!              Re-define UB matrix and inverse
                 Call F2D_RMAP_INUBMATRIX (input_options, &
                   xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   title, xlabel, ylabel, zlabel, &
                   experiment, retstat, status)

!              Re-define orientation of map planes
                 Call F2D_RMAP_ORIENTATION (xmaxmap, ymaxmap, zmaxmap, &
                   experiment, xnummap, ynummap, znummap, LL, LR, UP, &
                   DIFFERENCE, C1, C5, CN, C2C1, c2c1s, C3C1, &
                   c3c1s, C5C1, c5c1s, step, thickness, status)
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
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
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), title, xlabel, ylabel, zlabel, &
                      xstrelm, ystrelm, xendelm, yendelm, experiment, &
                      .False., x_coordinate, y_coordinate, status)
 
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              End If
 
           End If
 
!        Output warning message if required
           If (data_warning) Then
 
              Call GS_FWARNING (1, 1, 'DATA NEEDED, BUT NONE IS DEFINED', &
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire if map arrays are to be destroyed
        Call GS_INPL (.True., 0, 1, .True., 'DESTROY RECIPROCAL MAP ARRAYS', &
          1, '"YES" to save memory, "NO" to preserve reciprocal map', 1, &
          'Enter "YES" or "NO"', destory_map, status)
 
        If (destory_map) Then
 
           Deallocate (MAP)
           deallocate (NORMALISE)
           map_allocated = .False.
 
        End If
 
     End If
 
     End Subroutine F2D_GUI_RECIPROCAL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_gui_mfit.f90 *
!  *                  *
!  ********************
 
!+ F2D_GUI_MFIT - FIT 2-D GUI MFIT (multiple 1-D fitting)
     Subroutine F2D_GUI_MFIT (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, x_label, y_label, z_label, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mx_label, my_label, mz_label, &
       mx_pixel_size, my_pixel_size, results, status)
!max_vec_values, max_vectors, num_vectors, &
!       STR_VECTORS, END_VECTORS, VECTOR_TITLES, status)
!  Description:
!    FIT2D GUI for "MFIT" (multiple 1-D fitting)
!  Keywords:
!    MFIT.Interface, Interface.MFIT, Fitting.1-D, 1-D.Fitting
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.29 Use result vectors data structure (Hammersley)
!    08-Dec-2014: V0.28 Changes to "F2D_GUI_VECTORS" (Hammersley)
!    26-Nov-2014: V0.27 Changes to "F2D_CLICK" (Hammersley)
!    24-Apr-2006: V0.26 Add "input_options" structure (Hammersley)
!    17-Mar-2006: V0.25 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.24 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    14-Feb-2005: V0.23 Initialise "num_coordinates" before call to
!      "GS_INPS_FCOORDINATES" (Hammersley)
!    10-Mar-2004: V0.22 Change use of "VECTORS" array (Hammersley)
!    25-Feb-2004: V0.21 Alter menu lay-out for landscape windows (Hammersley)
!    11-Oct-2001: V0.20 Change calls to "GS_INPS_FCOORDINATES" so that
!      the number of coordinates input is returned to "num_coordinates".
!      This is not needed, but previously this was the constant 1, and
!      the Windows compiler correctly doesn't like overwriting constants.
!      (Hammersley)
!    23-Feb-1999: V0.19 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    17-Feb-1999: V0.18 "F2D_GUI_INPUT" replaced by "FIO_GUI_INPUT" (Hammersley)
!    15-Dec-1998: V0.17 Change to use IO internal database routines (Hammersley)
!    30-Oct-1998: V0.16 Allow user to specific starting row for multiple row 
!      fits, and make fit go in both directions from this initialisation 
!      (Hammersley)
!    29-Oct-1998: V0.15 Add "VECTORS" command (Hammersley)
!    28-Oct-1998: V0.14 Add support for "VECTORS" array (Hammersley)
!    20-Feb-1998: V0.13 Change "WARNING" text as the "POWDER
!      DIFFRACTION" interface now produces the 2-theta output in the
!      X-direction, and azimuthal in the Y-direction (Hammersley)
!    23-Jan-1998: V0.12 Changes to "F2D_GUI_INPUT" so that the
!      user prompt text is set by the calling routine (Hammersley)
!    22-Jan-1998: V0.11 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    04-Sep-1997: V0.10 Only exchange "memory" and current data arrays after 
!      successful tranposition within the "TRANSPOSE"command (Hammersley)
!    01-Aug-1997: V0.9 Add "TRANSPOSE" command to interface, and
!      output information message (Hammersley)
!    10-Mar-1997: V0.8 Changes to "F2D_GUI_ZSCALE" to cope
!      with masked data (Hammersley)
!    20-Feb-1997: V0.7 Transfer name of input file between interfaces 
!      (Hammersley)
!    25-Jan-1997: V0.6 Cater for "CANCEL" buttons (Hammersley)
!    15-Jan-1997: V0.5 Remember range of optimised data (Hammersley)
!    17-Dec-1996: V0.4 Enable display frequency support (Hammersley)
!    10-Dec-1996: V0.2 Allow printing of "OPTIMISE" graphical output 
!      (Hammersley)
!    04-Dec-1996: V0.1 Heavily based on "MFIT.FOR": original 16-Oct-1987, 
!       and "FITMENU.FOR": orginal 10-Mar-1988 (Hammersley)
!  Modules:
!  Use IO_LIB
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
     Include 'f2d_lsqmfit.inc' ! MFIT model control variables
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
     Character(Len = *), Intent(INOUT) :: x_label ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: y_label ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: z_label ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
!    Real MVARIANCES(xmaxdat, ymaxdat) ! Array containing variances in
!    the data values
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mx_label ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: my_label ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mz_label ! Z-axis label for data
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(INOUT) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(INOUT) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
!     Integer, Intent(INOUT) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Integer, Intent(INOUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(INOUT) :: END_VECTORS(max_vectors) ! End defined
!      elements for "VECTORS"
!    Real VECTORS(max_vec_values, max_vectors) ! Multiple 1-D arrays of
!    vector values
!     Character(Len = *), Intent(INOUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.29' ! Version number
     Integer, Parameter :: Max_menu = 20 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
     Integer, Parameter :: Max_parameters = 200 ! Maximum number of fitting
!      parameters
     Integer, Parameter :: Max_prompt = 2 ! Dimension of file selection
!      prompt text
     Integer, Parameter :: Max_results = 100 ! Dimension for maximum number
!      of results deduced from the fitted parameters which can be stored
     Integer, Parameter :: Max_text = 250 ! Dimension size of text array
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Character(Len = 20) :: print_type ! Type, of graphics to print:
!      supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer, Save :: display_frequency = 5 ! Frequency with which to
!      display fitted model results
     Integer, Save :: end_optimised ! First bin in ROI when "OPTIMISE"
!      was used
     Integer, Save :: fitting_info = 0 ! Level of user information output:
!      0: no information
!      >= 1" output iteration numbers
!      >= 2: output sum of squared residuals
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_coordinates ! Number of coordinates returned
     Integer :: num_iterations ! Number of iterations
     Integer :: num_menu ! Number of choices in menu
     Integer, Save :: num_parameters ! Number of parameters (variable and
!      fixed) in fitting model function
     Integer, Save :: num_results ! The number of results stored by the
!      subroutine
     Integer, Save :: num_text ! Number of lines of defined text
     Integer :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong ROI and array shape
     Integer, Save :: str_optimised ! First bin in ROI when "OPTIMISE" was used
     Integer :: y_start ! Row to be used for initialisation
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
     Logical :: file_open ! .True., if graphics output file open
     Logical, Save :: first = .True. ! .True., the first time the routine
!      is called
     Logical :: mask_data ! .True., if the current data is to be masked
     Logical :: mask_memory ! .True., if the memory data is to be masked
     Logical, Save :: model_evolution = .True. ! .True., if the fitting
!      model parameters "evolve" during a multiple row fit, or always start 
!      from the initialisation values
     Logical, Save :: params_exist = .False. ! .True. if fitting parameters
!      exist
     Logical, Save :: results_exist = .False. ! .True. if fit results exist
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical, Save :: weighted_fit = .False. ! .True., if weighted fitting
!      is required
     Real :: alpha ! Scaling factor for required accuracy
     Real :: chisqr ! Goodness of fit value
     Real, Save :: itsperpar = 11.5 ! Number of iterations per parameter
!      required to  minimise function
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: y_pixel ! Y-pixel position of graphical input
!  Local Arrays:
     Character(Len = 20), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
     Character(Len = 10), Save :: PARNAMES(Max_parameters)
!      A description of each fit parameter
     Character(Len = 70) :: PROMPT(Max_prompt) ! User prompt text
     Character(Len = 10), Save :: RESNAMES(Max_results) ! A description of
!      each result from the fit parameters
     Character(Len = 80), Save :: TEXT(Max_text) ! Text for user output e.g.
!      results
     Integer, Save :: PARAM_INFO(Max_parameters) ! Information on each of
!      the parameters of the fit. Describing to which feature it belongs,
!      the type of feature and the number of the parameter within the
!      feature, and whether the parameter is constrained or free to
!      be varied.
!
!    The digits of each element are defined as follows :
!
!    ......XX - Number of feature (1:99)
!
!    ....XX.. - Type of feature     1 = Standard polynomial curve
!    2 = Gaussian
!    3 = Lorentzian
!    4 = Voigtian
!    5 = Chebyshev form polynomial
!    6 = 2-D Gaussian
!    7 = 2-D polar Gaussian
!    8 = 2-D polar Gaussian,but with
!    no centre of coordinate system
!    9 = 2-D polynomial (normal)
!    10 = 2-D twin polar Gaussian
!    11 = 2-D twin polar Gaussian (no centre)
!
!    ..XX.... - Number of parameter within the feature
!    For a polynomial: 1 = zero order term
!    2 = first order term
!    etc.
!
!    For a Gaussian:   1 = Central position
!    2 = Maximum height (intensity)
!    3 = Standard deviation (width)
!
!    For a Lorentzian: 1 = Central position
!    2 = Maximum height (intensity)
!    3 = Full-width at half maximum
!
!    For a Voigtian :  1 = Centre position
!    2 = Maximum height (intensity)
!    3 = Gaussian sigma
!    4 = Lorentzian FWHM
!
!    For a 2-D Gaussian    1 = X-centre position
!    2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Sigma 1st axis
!    5 = Sigma 2nd axis
!    6 = rotation (radians anticlockwise from X-axis to 1st axis)
!
!    For a 2-D polar Gaussian 1 = X-centre (polar)
!    2 = Y-centre (polar)
!    3 = X-centre position
!    4 = Y-centre position
!    5 = Maximum height (intensity)
!    6 = Radial sigma
!    7 = Angular sigma (radians)
!
!    For a 2-D polar Gaussian 1 = X-centre position
!    without centre          2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!
!    For a 2-D twin polar Gaussian 1 = X-centre (polar)
!    2 = Y-centre (polar)
!    3 = X-centre position
!    4 = Y-centre position
!    5 = Maximum height (intensity)
!    6 = Radial sigma
!    7 = Angular sigma (radians)
!    8 = Twin peak maximum
!
!    For a 2-D twin polar Gaussian 1 = X-centre position
!    without centre                2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!    6 = Twin peak maximum
!
!    .X...... - Nature of parameter (Unused at present)
!
!    X....... - Transform type, for changing from world units to
!    pixels and vice versa
!    0 = No transform required
!    1 = X-axis Simple scaling
!    2 = X-axis Scaling and shift
!    3 = Y-axis simple scaling
!    4 = Y-axis scaling and shift
!    5 = Angle (internally radians)
!    6 = Divide by 2 (Cheb. Poly. 0)
!
     Logical, Save :: CONSTRAINTS(Max_parameters) ! Constraints array, .True.
!      if a model parameter is fixed and not to be fitted
     Real :: COVARIANCE(Max_parameters, Max_parameters) ! The estimated
!      fractional variance-covariance matrix
     Real, Save :: PAR_BASE(Max_parameters) ! The initial fit model parameters
     Real, Save :: PARAMS(Max_parameters) ! The current fit model parameters
     Real, Save :: RESULT_ERR(Max_results) ! Array to contain errors in
!      calculated results
     Real, Save :: FRESULTS(Max_results) ! Array to contain results
!      calculated from the fit parameters for each frame
     Real :: PARAM_ERR(Max_parameters) ! Array to contain errors in the fit
     Real, Save :: SCALE_FACTORS(Max_parameters) ! Scale factors array for
!      the parameter
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / &
       'EXIT', &
       'FULL', &
       'OUTPUT', '?', &
       'CONSTRAINTS', &
       'INITIALISE', &
       'INPUT', &
       'HELP', &
       'Z-SCALING', &
       'PRINT' /
     Data (MENU(item), item = 11, 20) / &
       'EXCHANGE', &
       'MASK', &
       'ZOOM IN', &
       'DISPLAY', &
       'OPTIONS', &
       'OPTIMISE', &
       'RESULTS', &
       'SET-UP', &
       'TRANSPOSE', &
       'VECTORS' /
     Data (MENUTXT(item), item = 1, 10) / 'EXIT: Exit menu', &
       'FULL: View image of full data', 'OUTPUT: Save data in an output file', &
       '?: This help on the menu choices', &
       'CONSTRAINTS: Constrain or set the fit parameters', &
       'INITIALISE: Enter fitting model; peaks and other functions', &
       'INPUT: Input data from a file on disk', &
       'HELP: Help text on this graphical menu', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'PRINT: Output current graphics to PostScript file' /
     Data (MENUTXT(item), item = 11, 20) / &
       'EXCHANGE: Swap current data with the "memory"', &
       'MASK: Defined masked-off regions of the image', &
       'ZOOM IN: Define smaller graphical display region', &
       'DISPLAY: Further graphical display possibilities', &
       'OPTIONS: Graphics display control menu', &
       'OPTIMISE: Optimise fit model to the data values', &
       'RESULTS: Results of model fitting', &
       'SET-UP: Alter set-up of fitting control variables', &
       'TRANSPOSE: Active data region element tranposition', &
       'VECTORS: Interactive viewing of results vectors' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_MFIT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_MFIT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_GUI_MFIT'')')
!        Write (*, '(''results%max_values = '', i8)') results%max_values

!     Test VECTORS assignment
!        results%VECTORS(1, 1) = 5.0
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Try to get default values from internal data-base
        Call IO_INQ_RKEYVALUE ('MFIT_ITERATIONS', itsperpar, retstat, status)
 
!**TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY
!     "MFIT_ITERATIONS" was accidently stored as an integer. This
!     code is designed to re-set errors caused by this
        If (itsperpar .Lt. 0.9 .Or. itsperpar .Gt. 10000) Then
           itsperpar = 11.5
        End If
!**TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY***TEMPORARY
 
        Call IO_INQ_IKEYVALUE ('MFIT_DIS_FREQUENCY', display_frequency, &
          retstat, status)
        Call IO_INQ_LKEYVALUE ('MFIT_MODEL_EVOLUTION', model_evolution, &
          retstat, status)
 
!     Arguments would appear to be reasonable, go ahead.
        mask_data = .True.
        mask_memory = .False.
 
        If (first) Then
           first = .False.
 
!        Output information message
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('NOTE: This menu is designed to ' // &
             'work on data stored in horizontal', status)
           Call IO_WRITE ('      rows. The "POWDER ' // &
             'DIFFRACTION" menu NOW PRODUCES 2-THETA,', status)
           Call IO_WRITE ('      RADIAL, Q-SPACE DATA IN ' // &
             'THE X-DIRECTION. Azimuthal data', status)
           Call IO_WRITE ('      is now in the ' // &
             'Y-direction. If fitting azimuthal data the', status)
           Call IO_WRITE ('      "TRANSPOSE" command ' // &
             'should be used to convert the data to', status)
           Call IO_WRITE ('      row order.', status)
           Call IO_WRITE (' ', status)
 
        End If
 
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
        print_type = 'image'
        Do While (continue)
 
!        Draw or re-draw display
           If (update_image) Then
 
              If (data_defined) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Before re-draw'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 If (print_type .Eq. 'optimise') Then
 
!                 Special output
                    Call F2D_MFITDISPLAY (experiment%x_pixel_size, &
                      variance_exist, &
                      xmaxdat, ymaxdat, %val(pXAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), %val(pMDATA), xstrelm, &
                      xendelm, ystrelm, str_optimised, end_optimised, title, &
                      x_label, z_label, max_parameters, num_parameters, &
                      f2d_num_1dfeatures, PARAM_INFO, PARAMS, f2d_1dorder, &
                      f2d_min_poly, f2d_max_poly, max_text, num_text, TEXT, &
                      status)
                 Else
 
!                 Redraw image or 1-D plot
                    Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, %val(pDATA), &
                      %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, &
                      ystrelm, xendelm, yendelm, title, x_label, y_label, &
                      z_label, status)
                    print_type = 'image'
                 End If
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE MFIT'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = '(MULTIPLE 1-D FITTING)'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'GUI PRESS "INPUT" TO '
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
                 Call GS_SET_MENULAYOUT (gs_vertical, 4, 12, 10, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, &
                status)
 
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
                   x_label, y_label, z_label, x_coordinate, y_coordinate, &
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
 
              Else If (command .Eq. 'CONSTRAINTS') Then
 
                 If (params_exist) Then
 
!                 Allow constraint of fit parameters
                    Call F2D_1DCONSTRAINTS (Max_parameters, num_parameters, &
                      PARNAMES, CONSTRAINTS, PARAMS, status)
                 Else
 
!                 No parameters have been defined
                    Call IO_WRITE ('WARNING: No parameters; ' // &
                      'parameters must be input using "INITIALISE"', status)
                 End If
 
              Else If (command .Eq. 'DISPLAY') Then
 
!              Further display options menu
                 If (data_defined) Then
                    Call F2D_DISPLAY (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pMASK), %val(pDATA), &
                      %val(pVARIANCES), title, x_label, y_label, z_label, &
                      variance_exist, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
                      %val(pMXAXIS), %val(pMYAXIS), %val(pMDATA), &
                      %val(pMVARIANCES), mxstrelm, mystrelm, mxendelm, &
                      myendelm, mtitle, mx_label, my_label, mz_label, &
                      memory_defined, mx_pixel_size, my_pixel_size, &
                      print_type, status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   x_label, y_label, z_label, variance_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mx_label, my_label, mz_label, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
!              Swap masked drawing variables
                 Call IO_LSWAP (mask_data, mask_memory, status)
 
                 print_type = 'image'
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 Call F2D_FULL (xnumdat, ynumdat, xstrelm, ystrelm, xendelm, &
                   yendelm, status)
 
              Else If (command .Eq. 'HELP') Then
 
!              Output help text for the PD menu
                 Call F2D_GUI_MFITHELP (.True., status)
 
              Else If (command .Eq. 'INITIALISE') Then
 
!              Initialise fitting model
                 If (data_defined) Then
 
                    If (yendelm .Gt. ystrelm) Then
 
!                    Click on row to use for fit model initialisation
                       MESSAGE(1) = 'Choose row to be used to ' // &
                         'enter the initial fitting'
                       MESSAGE(2) = 'model parameters, by clicking on it.'
                       num_coordinates = 1
                       Call GS_INPS_FCOORDINATES (mask_data, .True., xmaxdat, &
                         ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
                         %val(pDATA), %val(pMASK), %val(pXAXIS), %val(pYAXIS), &
                         title, x_label, y_label, z_label, 'CLICK ON ROW ' // &
                         'TO USE FOR MODEL INITIALISATION', 2, MESSAGE, &
                         .False., 1, num_coordinates, x_coordinate, &
                         y_coordinate, status)
 
                       Call MA_DC2PIXC (ymaxdat, yendelm, %val(pYAXIS), &
                         y_coordinate, y_pixel, status)
                       y_start = Int(y_pixel) + 1
                       y_start = Min(Max(ystrelm, y_start), yendelm)
 
                    Else
                       y_start = ystrelm
                    End If
 
                    Call F2D_1DINITIALISE (experiment%x_pixel_size, &
                      xmaxdat, ymaxdat, &
                      xnumdat, %val(pXAXIS), %val(pDATA), title, x_label, &
                      z_label, max_parameters, max_results, xstrelm, xendelm, &
                      y_start, params_exist, %val(pMASK), f2d_1dorder, &
                      f2d_min_poly, f2d_max_poly, f2d_num_1dfeatures, &
                      num_parameters, PAR_BASE, PARAM_INFO, CONSTRAINTS, &
                      PARNAMES, SCALE_FACTORS, num_results, RESNAMES, &
                      memory_defined, mxnumdat, mxstrelm, mxendelm, mtitle, &
                      mx_label, mz_label, %val(pMXAXIS), %val(pMDATA), status)
 
!                 Set vectors undefined
                    results%num_vectors = &
                      Min(results%max_vectors, num_parameters + num_results) 
                    Do item = 1, results%num_vectors
                       results%STARTS(item) = 1
                       results%ENDS(item) = 0
                    End Do

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''Returned from F2D_1DINITIALISE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Set Y-direction variables
                    mynumdat = 1
                    mystrelm = 1
                    myendelm = 1
                    Call IO_RSET (0.5, %val(pMYAXIS), status)
 
                    print_type = 'image'
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'INPUT') Then
 
!              Input data
                 PROMPT(1) = 'SELECT FILE TO INPUT (for fitting)'
                 PROMPT(2) = '(click on "HELP" for list of formats)'
                 Call FIO_GUI_INPUT (Max_prompt, 2, PROMPT, 0, &
                   input_options, xmaxdat, &
                   ymaxdat, variance_exist, data_defined, input_file, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pVARIANCES), title, x_label, y_label, z_label, &
                   experiment, status)
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
                 print_type = ' '
 
              Else If (command .Eq. 'OPTIONS') Then
 
!              Further options menu
                 Call F2D_OPTIONS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), %val(pVARIANCES), &
                   title, x_label, y_label, z_label, experiment, &
                   variance_exist, xstrelm, ystrelm, xendelm, &
                   yendelm, print_type, status)
 
                 update_image = .False.
                 update_menu = .True.
                 print_type = 'image'
 
              Else If (command .Eq. 'OPTIMISE') Then
 
                 If (params_exist .And. data_defined) Then
 
!                 Optimise fit model
                    Call F2D_MFITOPTIMISE ( & ! max_vec_values, max_vectors, &
                      experiment%x_pixel_size, variance_exist, &
                      xmaxdat, ymaxdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), xstrelm, xendelm, &
                      ystrelm, yendelm, y_start, title, x_label, z_label, &
                      alpha, fitting_info, weighted_fit, itsperpar, &
                      display_frequency, model_evolution, Max_parameters, &
                      num_parameters, f2d_num_1dfeatures, Max_results, &
                      Max_text, num_results, PARNAMES, PARAM_INFO, &
                      CONSTRAINTS, SCALE_FACTORS, PAR_BASE, PARAMS, PARAM_ERR, &
                      results_exist, FRESULTS, RESULT_ERR, RESNAMES, &
                      num_iterations, chisqr, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, %val(pMXAXIS), &
                      %val(pMYAXIS), %val(pMDATA), COVARIANCE, num_text, TEXT, &
                      print_type, results, status)
!num_vectors, STR_VECTORS, END_VECTORS, &
!                      %val(pVECTORS), VECTOR_TITLES, status)

!                 Store optimised region
                    str_optimised = mxstrelm
                    end_optimised = mxendelm
 
                    mtitle = 'Fitted Model'
                    update_image = .False.
                    update_menu = .True.
 
                 Else If (data_defined) Then
 
!                 Output warning message
                    Call GS_FWARNING (1, 1, 'NO FIT MODEL: USE "INITIALSE"', &
                      status)
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'OUTPUT') Then
 
                 If (data_defined) Then
                    Call FIO_GUI_OUTPUT (input_file, xmaxdat, ymaxdat, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pXAXIS), &
                      %val(pYAXIS), %val(pDATA), %val(pVARIANCES), title, &
                      x_label, y_label, z_label, variance_exist, &
                      experiment%x_pixel_size, experiment%y_pixel_size, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'MASK') Then
 
                 If (data_defined) Then
                    Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, x_label, &
                      y_label, z_label, experiment, xstrelm, &
                      ystrelm, xendelm, yendelm, %val(pMASK), status)
                    update_image = .False.
                    mask_data = .True.
                 Else
                    data_warning = .True.
                 End If
                 update_menu = .True.
                 print_type = 'image'
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 If (data_defined) Then
 
                    If (print_type .Eq. 'optimise') Then
 
!                    Special output
 
!                    Deactive terminal workstation and activate
!                    hardcopy workstation
                       Call GS_ON_PRINT (.True., file_open, status)
 
                       If (file_open) Then
 
                          Call F2D_MFITDISPLAY (experiment%x_pixel_size, &
                            variance_exist, &
                            xmaxdat, ymaxdat, %val(pXAXIS), %val(pDATA), &
                            %val(pVARIANCES), %val(pMASK), %val(pMDATA), &
                            xstrelm, xendelm, ystrelm, str_optimised, &
                            end_optimised, title, x_label, z_label, &
                            max_parameters, num_parameters, &
                            f2d_num_1dfeatures, PARAM_INFO, PARAMS, &
                            f2d_1dorder, f2d_min_poly, f2d_max_poly, max_text, &
                            num_text, TEXT, status)
 
!                       Deactive hardcopy workstation and
!                       activate terminal workstation
                          Call GS_OFF_PRINT (status)
 
!                       Close graphics file immediately (only
!                       one page per file)
                          Call GS_CLOSE_PS (status)
 
!                       Output user message
                          Call GS_FPROMPT ( 1, 1, 'FINISHED WRITING FILE', &
                            status)
 
!                       Force output
                          Call GS_UPDATE (status)
 
                          Call IO_WRITE ('INFO: Finished ' // &
                            'writing graphics file', status)
 
                       End If
 
                    Else
 
                       Call F2D_PRINT (.True., print_type, mask_exist, &
                         xmaxdat, ymaxdat, %val(pXAXIS), %val(pYAXIS), &
                         %val(pDATA), %val(pVARIANCES), %val(pMASK), title, &
                         x_label, y_label, z_label, variance_exist, xstrelm, &
                         ystrelm, xendelm, yendelm, status)
 
                    End If
                    update_image = .False.
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'RESULTS') Then
 
                 If (results_exist) Then
 
!                 Output to screen
                    Call GS_MESSAGE (Max_text, num_text, TEXT, status)
 
                 Else
 
!                 Output warning message
                    Call GS_FWARNING (1, 1, &
                      'NO RESULTS DEFINED: USE "OPTIMISE"', status)
 
                 End If
 
              Else If (command .Eq. 'SET-UP') Then
 
!              User control of fitting control variables
                 Call F2D_MFITSETUP (variance_exist, alpha, fitting_info, &
                   itsperpar, display_frequency, weighted_fit, &
                   model_evolution, status)
 
              Else If (command .Eq. 'TRANSPOSE') Then
 
!              Transpose image
                 Call F2D_TRANSPOSE (.True., xmaxdat, ymaxdat, %val(pXAXIS), &
                   %val(pYAXIS), %val(pDATA), %val(pVARIANCES), xnumdat, &
                   ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
                   x_label, y_label, z_label, variance_exist, retstat, &
                   memory_defined, %val(pMXAXIS), %val(pMASK), %val(pMYAXIS), &
                   %val(pMDATA), %val(pMVARIANCES), &
                   mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, myendelm, &
                   mtitle, mx_label, my_label, mz_label, status)
 
                 If (retstat .Eq. 0) Then
 
!                 Swap current data with "memory"
                    Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      title, x_label, y_label, z_label, variance_exist, &
                      data_defined, &
                      experiment%x_pixel_size, experiment%y_pixel_size, &
                      xstrelm, &
                      ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mtitle, mx_label, &
                      my_label, mz_label, memory_defined, mx_pixel_size, &
                      my_pixel_size, status)
 
                 End If
 
              Else If (command .Eq. 'VECTORS') Then
 
!              Interactive viewing of results vectors
                 Call F2D_GUI_VECTORS (input_options, experiment, results, &
!                   max_vec_values, max_vectors, &
!                   num_vectors, STR_VECTORS, END_VECTORS, %val(pVECTORS), &
!                   VECTOR_TITLES, 
                   xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, &
                   %val(pDATA), %val(pXAXIS), %val(pYAXIS), &
                   title, x_label, y_label, z_label,  status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
 
                    Call F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, x_label, &
                      y_label, z_label, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''Returned from ZOOM IN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Else
                    data_warning = .True.
                 End If
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 If (data_defined) Then
 
                    Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                      ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pMASK), title, x_label, y_label, z_label, xstrelm, &
                      ystrelm, xendelm, yendelm, experiment, &
                      .False., x_coordinate, y_coordinate, status)
                    print_type = 'image'
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .False.
 
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
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Save values in internal data-base
        Call IO_SET_RKEYVALUE ('MFIT_ITERATIONS', itsperpar, retstat, status)
        Call IO_SET_IKEYVALUE ('MFIT_DIS_FREQUENCY', display_frequency, &
          retstat, status)
        Call IO_SET_LKEYVALUE ('MFIT_MODEL_EVOLUTION', model_evolution, &
          retstat, status)
 
     End If
 
     End Subroutine F2D_GUI_MFIT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

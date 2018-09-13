!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_gui_2dfit.f90 *
!  *                   *
!  *********************
 
!+ F2D_GUI_2DFIT - FIT 2-D GUI 2-D function FITting
     Subroutine F2D_GUI_2DFIT (input_options, &
       input_file, data_defined, memory_exist, &
       memory_defined, variance_exist, mask_exist, xmaxdat, ymaxdat, xnumdat, &
       ynumdat, title, x_label, y_label, z_label, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mx_label, my_label, mz_label, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    GUI for 2-D fitting
!  Keywords:
!    Fitting~2-D.Interface, Interface.2-D~Interface, Fitting.2-D,
!    2-D~Fitting.Interface
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.6 Changes to "F2D_CLICK" (Hammersley)
!    24-Apr-2006: V0.5 Add "input_options" structure (Hammersley)
!    17-Mar-2006: V0.4 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.2 Alter menu lay-out for landscape windows (Hammersley)
!    17-Apr-1999: V0.1 Original based on "F2D_GUI_MFIT" (Hammersley)
!  Modules:
!     Use IO_LIB
!     Use MA_LIB
!     Use GS_LIB
!    Use FIO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
     Include 'f2d_lsqfit2d.inc' ! FIT2D model control variables
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
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory
!      data in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory
!      data in the Y-direction (metres)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
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
!    supported types are:
!      'banner'
!      'image'
!      'contour'
!      'x/y graph'
     Integer, Save :: display_frequency = 5 ! Frequency with which to
!      display fitted model results
     Integer, Save :: fitting_info = 0 ! Level of user information output:
!      0: no information
!      >= 1" output iteration numbers
!      >= 2: output sum of squared residuals
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_iterations ! Number of iterations
     Integer :: num_menu ! Number of choices in menu
     Integer, Save :: num_parameters ! Number of parameters (variable and
!      fixed) in fitting model function
     Integer, Save :: num_results ! The number of results stored by the
!      subroutine
     Integer, Save :: num_text ! Number of lines of defined text
     Integer :: par ! Loop variable for parameters
     Integer :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong
!          ROI and array shape
     Logical :: continue ! .True., until user wants to exit
     Logical :: data_warning ! .True., if there is no data, and the
!      user tries an operation which requires data
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
!      required to minimise function
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real, Save :: x_beam = 1000.0 ! X-coordinate of beam centre in pixel
!      coordinates
     Real :: x_coordinate ! Graphical input X-coordinate
     Real, Save :: y_beam = 1000.0 ! Y-coordinate of beam centre in pixel
!      coordinates
     Real :: y_coordinate ! Graphical input Y-coordinate
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
!    11 = 2-D twin polar Gaussian (no
!    centre)
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
!    6 = rotation (radians
!    anticlockwise from X-axis to
!    1st axis)
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
     Logical, Save :: CONSTRAINTS(Max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real :: COVARIANCE(Max_parameters, Max_parameters) ! The estimated
!      fractional variance-covariance matrix
     Real, Save :: PAR_BASE(Max_parameters) ! The initial fit model parameters
     Real, Save :: PARAMS(Max_parameters) ! The current fit model parameters
     Real, Save :: RESULT_ERR(Max_results) ! Array to contain errors in
!      calculated results
     Real, Save :: RESULTS(Max_results) ! Array to contain results
!      calculated from the fit parameters for each frame
     Real :: PARAM_ERR(Max_parameters) ! Array to contain errors in the fit
     Real, Save :: SCALE_FACTORS(Max_parameters) ! Scale factors array for
!      the parameter
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / 'EXIT', 'INPUT', 'OUTPUT', 'ZOOM IN', &
       '?', 'MASK', 'EXCHANGE', 'FULL', 'HELP', 'INITIALISE' /
     Data (MENU(item), item = 11, 20) / 'CONSTRAINTS', 'Z-SCALING', 'PRINT', &
       'OPTIMISE', 'SET-UP', 'DISPLAY', 'OPTIONS', 'RESULTS', 'SUBTRACT', &
       'ADD' /
     Data (MENUTXT(item), item = 1, 10) / 'EXIT: Exit menu', &
       'INPUT: Input data from a file on disk', &
       'OUTPUT: Save data in an output file', &
       'ZOOM IN: Define smaller graphical display region', &
       '?: This help on the menu choices', &
       'MASK: Defined masked-off regions of the image', &
       'EXCHANGE: Swap current data with the "memory"', &
       'FULL: View image of full data', &
       'HELP: Help text on this graphical menu', &
       'INITIALISE: Enter fitting model; peaks and other functions' /
     Data (MENUTXT(item), item = 11, 20) / &
       'CONSTRAINTS: Constrain or set the fit parameters', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'PRINT: Output current graphics to PostScript file', &
       'OPTIMISE: Optimise fit model to the data values', &
       'SET-UP: Alter set-up of fitting control variables', &
       'DISPLAY: Further graphical display possibilities', &
       'OPTIONS: Graphics display control menu', &
       'RESULTS: Results of model fitting', &
       'SUBTRACT: Subtract the "memory" (create residuals)', &
       'ADD: Add the "memory" to the data' /
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_2DFIT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GUI_2DFIT ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Try to get default values from internal data-base
        Call IO_INQ_RKEYVALUE ('2DFIT_ITERATIONS', itsperpar, retstat, status)
 
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
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
 
!              Redraw image or 1-D plot
                 Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, x_label, y_label, z_label, status)
                 print_type = 'image'
 
              Else
 
!              Draw welcome message
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'WELCOME TO THE 2-D'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'FITTING INTERFACE'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'PRESS "INPUT" TO'
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
 
!           Re-draw menu
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 10, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 10, status)
              End If
 
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
 
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
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
                 Call F2D_GUI_2DFITHELP (.True., status)
 
              Else If (command .Eq. 'INITIALISE') Then
 
!              Initialise fitting model
                 If (data_defined) Then
 
                    Call F2D_GUI_PARAMETERS (.True., experiment, &
                      xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                      x_label, y_label, z_label, max_parameters, max_results, &
                      xstrelm, ystrelm, xendelm, yendelm, %val(pMASK), &
                      f2d_x_order, f2d_y_order, f2d_xmin_poly, f2d_ymin_poly, &
                      f2d_xmax_poly, f2d_ymax_poly, f2d_num_features, &
                      num_parameters, PARAMS, PARAM_INFO, CONSTRAINTS, &
                      PARNAMES, SCALE_FACTORS, num_results, RESNAMES, &
                      %val(pMDATA), status)
                    params_exist = num_parameters .Gt. 0
 
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
                    Call F2D_LSQFIT2D (experiment, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), xstrelm, ystrelm, &
                      xendelm, yendelm, alpha, weighted_fit, itsperpar, &
                      max_parameters, num_parameters, max_results, &
                      num_results, PARAM_INFO, CONSTRAINTS, SCALE_FACTORS, &
                      PARAMS, PARAM_ERR, RESULTS, RESULT_ERR, RESNAMES, &
                      num_iterations, chisqr, %val(pMDATA), COVARIANCE, &
                      status)
 
!                 Create text array of results of fit
                    Write (TEXT(1), '(''PARAMETERS OF FIT'')')
                    Write (TEXT(2), '('' '')')
                    num_text = 2
 
                    Write (TEXT(num_text + 1), '('' '')')
                    Write (TEXT(num_text + 2), &
                      '(''No Quantity  Value      Error'')')
                    Write (TEXT(num_text + 3),'('' '')')
                    num_text = num_text + 3
 
!                 Add parameter values to text
                    Do par = 1, num_parameters
                       num_text = num_text + 1
                       Write (TEXT(num_text), &
                         '(a10, '' '', 1pe12.5, 1x, 1pe9.2)') PARNAMES(par), &
                         PARAMS(par), PARAM_ERR(par)
                    End Do
 
!                 Add results to text
                    Do par = 1, num_results
                       num_text = num_text + 1
                       Write (TEXT(num_text),'(a10,'' '',1pe10.3)') &
                         RESNAMES(par), RESULTS(par)
                    End Do
 
                    mxnumdat = xendelm
                    mynumdat = yendelm
                    mxstrelm = xstrelm
                    mystrelm = ystrelm
                    mxendelm = xendelm
                    myendelm = yendelm
                    mtitle = 'FIT MODEL'
                    mx_label = x_label
                    my_label = y_label
                    mz_label = z_label
                    Call MA_RCOPY (xmaxdat, 1, %val(pXAXIS), 1, 1, xnumdat, 1, &
                      xmaxdat, 1, %val(pMXAXIS), status)
                    Call MA_RCOPY (ymaxdat, 1, %val(pYAXIS), 1, 1, ynumdat, 1, &
                      ymaxdat, 1, %val(pMYAXIS), status)
 
                    memory_defined = .True.
                    results_exist = .True.
                    update_image = .False.
                    update_menu = .True.
 
                    Call IO_WRITE (' ', status)
                    Call IO_WRITE ('INFO: The fit model is ' // &
                      'stored in the "memory", use "EXCHANGE"', status)
                    Call IO_WRITE ('      to view the fit model.', status)
 
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
 
                    Call F2D_PRINT (.True., print_type, mask_exist, xmaxdat, &
                      ymaxdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                      %val(pVARIANCES), %val(pMASK), title, x_label, y_label, &
                      z_label, variance_exist, xstrelm, ystrelm, xendelm, &
                      yendelm, status)
 
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
                      'NO RESULTS DEFINE: USE "OPTIMISE"', status)
 
                 End If
 
              Else If (command .Eq. 'SET-UP') Then
 
!              User control of fitting control variables
                 Call F2D_MFITSETUP (variance_exist, alpha, fitting_info, &
                   itsperpar, display_frequency, weighted_fit, &
                   model_evolution, status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 If (data_defined) Then
                    Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, x_label, &
                      y_label, z_label, xstrelm, ystrelm, xendelm, yendelm, &
                      status)
 
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
 
              Else If (command .Eq. 'SUBTRACT') Then
 
!              Subtract memory to create residuals (?)
                 If (data_defined) Then
 
                    If (memory_defined) Then
 
!                    Subtract memory from current data in active data
!                    region
                       If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                         Then
 
                          Call MA_RSUBTRACT (xmaxdat, ymaxdat, xstrelm, &
                            ystrelm, xendelm, yendelm, %val(pMDATA), xmaxdat, &
                            ymaxdat, %val(pDATA), status)
 
                          If (variance_exist) Then
 
                             Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                               xendelm, yendelm, %val(pMVARIANCES), xmaxdat, &
                               ymaxdat, %val(pVARIANCES), status)
 
                          End If
                          update_image = .True.
 
                       Else
                          Call GS_FWARNING (1, 1, 'MEMORY ' // &
                            'DEFINED, BUT NOT IN WHOLE OF ROI', status)
                       End If
 
                    Else
                       Call GS_FWARNING (1, 1, 'MEMORY NOT DEFINED', status)
                    End If
 
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .True.
 
              Else If (command .Eq. 'ADD') Then
 
!              Add memory to residuals (?)
                 If (data_defined) Then
 
                    If (memory_defined) Then
 
!                    Add memory from current data in active data
!                    region
                       If (xendelm .Le. mxnumdat .And. yendelm .Le. mynumdat) &
                         Then
 
                          Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                            xendelm, yendelm, %val(pMDATA), xmaxdat, ymaxdat, &
                            %val(pDATA), status)
 
                          If (variance_exist) Then
 
                             Call MA_RADD (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                               xendelm, yendelm, %val(pMVARIANCES), xmaxdat, &
                               ymaxdat, %val(pVARIANCES), status)
 
                          End If
                          update_image = .True.
 
                       Else
                          Call GS_FWARNING (1, 1, 'MEMORY ' // &
                            'DEFINED, BUT NOT IN WHOLE OF ROI', status)
                       End If
 
                    Else
                       Call GS_FWARNING (1, 1, 'MEMORY NOT DEFINED', status)
                    End If
 
                    update_menu = .True.
 
                 Else
                    data_warning = .True.
                 End If
 
                 update_image = .True.
 
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
        Call IO_SET_RKEYVALUE ('2DFIT_ITERATIONS', itsperpar, retstat, status)
        Call IO_SET_RKEYVALUE ('X_BEAM_CENTRE', x_beam, retstat, status)
        Call IO_SET_RKEYVALUE ('Y_BEAM_CENTRE', y_beam, retstat, status)
 
     End If
 
     End Subroutine F2D_GUI_2DFIT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

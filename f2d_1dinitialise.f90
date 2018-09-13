!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_1dinitialise.f90 *
!  *                      *
!  ************************
 
!+ F2D_1DINITIALISE - FIT2D 1-D INITIALISE'ation of a 1-D fitting model
     Subroutine F2D_1DINITIALISE (experiment, xmaxdat, ymaxdat, numdat, AXIS, &
       DATA, title, xlabel, ylabel, max_parameters, max_results, strelm, &
       endelm, y_value, params_exist, MASK, poly_order, min_poly, max_poly, &
       num_features, num_parameters, PARAMS, PARAMS_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, num_results, RESNAMES, memory_defined, &
       mnumdat, mstrelm, mendelm, mtitle, mxlabel, mylabel, MAXIS, RESIDUALS, &
       status)
!  Description:
!    Allows the user to input features to define a 1-D fitting
!    model. The residuals from the 1-D initial model subtracted
!    "DATA" are returned in "RESIDUALS".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Dec-2014: V0.17 Use Fortran allocate instead of "IO_MALLOC" (Hammersley)
!    26-Nov-2014: V0.16 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.15 Support for arbitrary aspect ratio windows (Hammersley)
!    13-Mar-2006: V0.14 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.13 Alter menu lay-out for landscape windows (Hammersley)
!    02-Jun-2003: V0.12 Tidy up code (Hammersley)
!    04-Sep-1997: V0.11 Exit with warning message if the data is
!      only one column wide (Hammersley)
!    07-Feb-1997: V0.10 Change "TRIGONOMETRIC" to "SINUSOIDAL" (Hammersley)
!      number of coordinates (Hammersley)
!    05-Feb-1997: V0.9 Treat "CANCEL" requests (Hammersley)
!    04-Feb-1997: V0.8 Correct text for "HWHM" on Voigtian functions
!      (Hammersley)
!    03-Feb-1997: V0.7 Change scaling of polynomial values (Hammersley)
!    30-Jan-1997: V0.6 Add exponential decay and sinusodial functions 
!      (Hammersley)
!    15-Jan-1997: V0.5 Add "ZOOM-IN", "UN-ZOOM", and "HELP" commands
!      (Hammersley)
!    16-Dec-1996: V0.4 Correct calls to "GS_INPS_FCOORDINATES":
!      replace "1" with variable "num_coordinates" for the returned
!    15-Dec-1996: V0.3 Define results names, and allow initialisation
!      on any of the Y-rows (Hammersley)
!    14-Dec-1996: V0.2 Display initialisation peak (Hammersley)
!    04-Dec-1996: V0.1 Original, based on "f2d_guiparameters" (Hammersley)
!  Modules:
     Use IO_LIB
!  Use MA_LIB
!  Use LG_LIB
!  Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension size for data arrays
     Integer, Intent(IN) :: ymaxdat ! First dimension size for data arrays
     Integer, Intent(IN) :: numdat ! The end of the defined data region
     Real, Intent(IN) :: AXIS(xmaxdat) ! Axis data
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Integer, Intent(IN) :: max_parameters ! Dimension of parameter arrays
     Integer, Intent(IN) :: max_results ! Dimension size of results array
!  Import/Export:
     Integer, Intent(INOUT) :: strelm ! Element number for the start of the ROI
     Integer, Intent(INOUT) :: endelm ! Element number for the end of the ROI
     Integer, Intent(INOUT) :: y_value ! The Y-row used for the initialisation
     Logical, Intent(INOUT) :: params_exist ! .True. if fitting parameters
!      exist
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
!  Export:
     Integer, Intent(OUT) :: poly_order ! Order of background polynomial
     Real, Intent(OUT) :: min_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: max_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Integer, Intent(OUT) :: num_features ! Number of features in fit model
     Integer, Intent(OUT) :: num_parameters ! Number of parameters in fit model
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The initial values for
!      the fit parameters
     Integer, Intent(OUT) :: PARAMS_INFO(max_parameters) ! Information on
!      each of the parameters of the fit describing, to which feature it
!      belongs, the type of feature and the number of the parameter
!      within the feature.
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
!    12 = Pseudo-Voigt
!    13 = Exponential decay
!    14 = Sinusoidal function
!    20 = 2-D Chebyshev polynomial
!    50 = "Row-line"
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
!    8 = 2nd peak maximum
!
!    For a 2-D twin polar Gaussian 1 = X-centre position
!    without centre                2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!    6 = 2nd peak maximum
!
!    For an Exponential: 1 = Zero (central) position
!    Decay Function      2 = Zero (maximum) intensity
!    3 = Half-life or decay (width)
!
!    For an sinusoidal:    1 = Zero (maximum) position
!    Function              2 = Amplitude (half peak to peak)
!    3 = Period
!
!    For a "Row" line :       1 = Lowest order (constrained)
!    2 = Highest order (constrained)
!    3 = X-centre (polar symmetry)
!    4 = Y-centre (polar symmetry)
!    5 = X-centre (row line)
!    6 = Y-centre (row line)
!    7 = Angle of row line (radians)
!    8 = Peak to peak spacing
!    9 = Distance from centre of row
!    line to zero order peak
!    10 = Ratio of reflected row to
!    primary row-line peaks
!    11 = Radial peak width (sigma)
!    12 = Angular peak width (sigma)
!    13 = Lowest order peak maximum
!    intensity
!    14 = Lowest+1 order peak maximum
!    intensity
!    15 = Lowest+2 order peak maximum
!    intensity
!    16 =  etc.
!    13+No. of orders = Intensities
!
!    .X...... - Nature of parameter 0 = Unconstrained
!    1 = Constrained to a fixed value
!
!    X....... - Transform type, for changing from world units to
!    pixels and vice versa
!    0 = No transform required
!    1 = X-axis Simple scaling
!    2 = X-axis Scaling and shift
!    3 = Y-axis simple scaling
!    4 = Y-axis scaling and shift
!    5 = Angle (internally radians)
!    6 = Divide by 2 (Chebyshev poly 0)
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters) ! .True., if a
!      model parameter is fixed
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
!      Name for each of the parameters
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters) ! The typical
!      expected changes in the parameter values
     Integer, Intent(OUT) :: num_results ! Number of results, named and to
!      be calculated
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
!      Name for each of the results
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory is
!      defined
     Integer, Intent(OUT) :: mnumdat ! Number of defined elements in "RESIDUALS"
     Integer, Intent(OUT) :: mstrelm ! The element number for the start of
!      the residuals
     Integer, Intent(OUT) :: mendelm ! The element number for the end of the
!      residuals
     Character(Len = *), Intent(OUT) :: mtitle ! Title for residuals
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for residuals
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for residuals
     Real, Intent(OUT) :: MAXIS(xmaxdat) ! The axis values for the residuals
     Real, Intent(OUT) :: RESIDUALS(xmaxdat, ymaxdat) ! The initial user
!      defined fit residuals
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.17' ! Version number
     Integer, Parameter :: Maxcoeffs = 100 ! Dimension of 1-D polynomial
!      fit coefficients array
     Integer, Parameter :: Maxpeak = 30 ! Dimension size of peak
!      information arrays, maximum number of peaks which may be specified
     Integer, Parameter :: Max_menu = 11 ! Dimension size of commands array
!  Local Variables:
     Character(Len = 20) :: command ! Choice entered by user
     Integer :: dummy ! Dummy call argument
     Integer :: end ! End pixel for calculating profile
     Integer :: feature ! Loop variable for features
     Integer :: input_type ! Type of graphical input
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: num_exponential ! Number of exponential decay functions
!      in the fit model
     Integer :: num_gauss ! Counter for number of Gaussian peaks in
!      the fit model
     Integer :: num_lor ! Counter for number of Lorentzian peaks in
!      the fit model
     Integer :: num_menu ! Number of commands in menu
     Integer :: num_sinusoidal ! Number of sinusoidal functions
!      in the fit model
     Integer :: num_voigt ! Counter for number of Voigtian peaks in
!      the fit model
     Integer :: order ! Loop variable for polynomial orders
     Integer :: order_init ! Polynomial order for initial fit
     Integer :: par ! Loop variable for parameters
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = Bad status, not enough independent data points for
!          required maximum order
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: unused ! Unsed variable for "F2D_ZOOMIN"
     Integer :: x ! Loop variable for X-direction
     Integer :: start ! Starting pixel for calculating profile
     Logical :: continue ! .True., until end of input session
     Logical :: polynomial_defined ! .True., if the fit model contains
!      a polynomial
     Logical :: update_image ! .True., if image needs to be re-drawn
     Logical :: update_menu ! .True., if menu needs to be re-drawn
     Real :: amplitude ! Half peak to peak of sinusoidal function
     Real :: half_life ! Half-life of exponential decay function
     Real :: hwhm ! Half-width at half-height of a Lorentzian
     Real :: peak_half ! Half height intensity of peak
     Real :: peak_max ! Maximum intensity of peak
     Real :: period ! Period of sinusoidal function
     Real :: rms_r ! RMS residual of polynomial fit
     Real :: sigma ! Standard deviation of a Gaussian peak
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_centre ! X-pixel coordinate of the centre of a peak
     Real :: x_coordinate ! Input X-coordinate
     Real :: x_half ! X-pixel coordinate of the half height position
!      of a peak
     Real :: xmax_dddr ! The maximum X-coordinate of the displayed data
!      display region
     Real :: xmin_dddr ! The minimum X-coordinate of the displayed data
!      display region
     Real :: y_coordinate ! Input Y-coordinate
     Real :: ymax_dddr ! The maximum Y-coordinate of the displayed data
!      display region
     Real :: ymin_dddr ! The minimum Y-coordinate of the displayed data
!      display region
!  Local Arrays:
     Real :: COEFFICIENTS(Maxcoeffs) ! Polynomial background coefficients in 
!      Chebyshev form
     Real :: OFFSETS(Maxpeak) ! Offset value for peak initialisation
     Character(Len = 20), Save :: MENU(Max_menu) ! Commands choices for
!      graphical menu
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu text
     Character(Len = 80) :: MESSAGE(3) ! User messages
     Real, Allocatable  :: PEAKS(:) ! Peak profiles for graphical display
!  External Functions:
!  Local Data:
     Data MENU / 'EXIT', '?', 'HELP', 'EXP. DECAY', 'GAUSSIAN', 'LORENTZIAN', &
       'POLYNOMIAL', 'SINUSOIDAL', 'VOIGTIAN', 'UN-ZOOM', 'ZOOM IN' /
     Data MENUTXT / 'EXIT: No more peaks to define, exit model defining menu', &
       '?: This help text', 'HELP: Advice on defining an initial fit model', &
       'EXP. DECAY: Add exponential decay function to fit model', &
       'GAUSSIAN: Add Gaussian peak function to fit model', &
       'LORENTZIAN: Add Lorentzian peak function to fit model', &
       'POLYNOMIAL: Add "background" Chebyshev polynomial to fit model', &
       'SINUSOIDAL: Add cosine/sine function to fit model', &
       'VOIGTIAN: Add (true) Voigtain peak function to fit model', &
       'UN-ZOOM: Define larger graphical display region (and ROI)', &
       'ZOOM IN: Define smaller graphical display region (and ROI)' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_1DINITIALISE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DINITIALISE ' // Version)
        Return
     End If
 
!  Check subroutine arguments for validity
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (strelm .Le. 0 .Or. strelm .Gt. endelm .Or. endelm .Gt. xmaxdat) &
       Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_1DINITIALISE ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_1DINITIALISE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Inquire window format
     Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!  Check that the data is more than one column wide
     If (endelm .Le. strelm) Then
 
        MESSAGE(1) = 'The data is only 1 column wide !'
        MESSAGE(2) = 'If the data is a vertical column, use'
        MESSAGE(3) = '"TRANSPOSE" to create a 1-D row'
        Call GS_FWARNING (3, 3, MESSAGE, status)
 
        Return
 
     End If
 
!  Output advice note to user
     Call IO_WRITE (' ', status)
     Call IO_WRITE ('NOTE: Unlike the program MFIT you ' // &
       'need to click on "POLYNOMIAL"', status)
     Call IO_WRITE ('      for a background polynomial, ' // &
       'including a zero order polynomial,', status)
     Call IO_WRITE ('      to be included in the fit model', status)
     Call IO_WRITE (' ', status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Copy data to residuals
     Call MA_RCOPY (xmaxdat, ymaxdat, DATA, 1, y_value, numdat, y_value, &
       xmaxdat, ymaxdat, RESIDUALS, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Initialise variables
     num_menu = Max_menu
     polynomial_defined = .False.
     num_features = 0
     num_parameters = 0
     num_exponential = 0
     num_gauss = 0
     num_lor = 0
     num_sinusoidal = 0
     num_voigt = 0
     num_results = 0
 
!  Allocate memory for peak display
!     Call IO_MALLOC (numdat * 4, pPEAK, status)
     Allocate (PEAKS(numdat), Stat = stat)

     If (stat .Ne. 0) Then
        status = St_mod_ma + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_1DINITIALISE ' // Version)
        Return
     End If
 
!  Loop inputing menu commands or peak parameters until requested to stop
     polynomial_defined = .False.
 
     continue = .True.
     update_image = .True.
     Do While (continue)
 
        If (update_image) Then
 
!        Output image
           Call GS_MPLOT (.True., xmaxdat, 1, DATA(1, y_value), MASK(1, &
             y_value), AXIS, dummy, strelm, 1, endelm, 1, title, xlabel, ' ', &
             ylabel, status)
 
!        Find out displayed data region
           Call GS_INQ_DDDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, &
             status)
 
!        Set clip region
           Call LG_CLIPWINDOW (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, &
             status)
 
!        Turn on clipping
           Call LG_CLIP (.True., status)
 
!        Set line style solid, blue, and standard width
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
 
!        Create features and draw on top of graph
           Do feature = 1, num_features
 
!           Initialise work array
              Call MA_RVALUE (xmaxdat, 1, 1, 1, endelm, 1, OFFSETS(feature), &
                PEAKS, status)
 
!           Calculate feature
              Call F2D_1DFEATURE (experiment%x_pixel_size, feature, &
                max_parameters, PARAMS, &
                PARAMS_INFO, num_parameters, xmaxdat, 1, endelm, poly_order, &
                min_poly, max_poly, AXIS, PEAKS, status)
 
!           Draw feature on top of the data
              Call LG_POLYLINE (endelm, AXIS, PEAKS, status)
 
           End Do
 
!        Turn off clipping
           Call LG_CLIP (.False., status)
 
           update_menu = .True.
 
        End If
 
        If (update_menu) Then
 
!        Re-set menu layout style to horizontal
           If (width / height .Gt. 1.2) Then
              Call GS_SET_MENULAYOUT (gs_vertical, 1, 20, 10, status)
           Else
              Call GS_SET_MENULAYOUT (gs_vertical, 3, 20, 10, status)
           End If
 
!        Re-draw menu
           Call GS_FMENU (1, 1, 'ENTER MODEL FEATURE', Max_menu, num_menu, &
             MENU, status)
 
        End If
 
!     By default no update
        update_image = .False.
        update_menu = .False.
 
!     Get user to select between the available menu options
        Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
          command, x_coordinate, y_coordinate, status)
 
        If (input_type .Eq. Gs_resize) Then
 
           update_image = .True.
           update_menu = .True.
 
        Else If (input_type .Eq. Gs_coordinate) Then
 
!        Output the coordinate position and intensity
           Call F2D_CLICK (1, xmaxdat, 1, strelm, 1, endelm, 1, AXIS, dummy, &
             DATA(1, y_value), title, xlabel, ylabel, 'N.A.', x_coordinate, &
             y_coordinate, experiment, update_image, update_menu, status)
 
        Else If (input_type .Eq. Gs_choice) Then
 
!        Carry out menu choices
           If (command .Eq. 'EXIT') Then
              continue = .False.
              update_image = .False.
              update_menu = .False.
 
           Else If (command .Eq. '?') Then
 
!           Display list of available commands
              Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
              update_image = .True.
              update_menu = .True.
 
           Else If (command .Eq. 'HELP') Then
 
!           Output help text
              Call F2D_1DINITHELP (.True., status)
 
              update_image = .True.
 
           Else If (command .Eq. 'UN-ZOOM') Then
 
!           Set ROI to cover all data
              Call F2D_FULL (numdat, 1, strelm, dummy, endelm, dummy, status)
 
              update_image = .True.
 
           Else If (command .Eq. 'ZOOM IN') Then
 
!           Allow user to define new data region by clicking on two
!           points
              unused = 1
              Call F2D_ZOOMIN (xmaxdat, 1, numdat, 1, AXIS, dummy, DATA, &
                title, xlabel, ' ', ylabel, strelm, unused, endelm, unused, &
                status)
              update_image = .True.
 
           Else If (command .Eq. 'POLYNOMIAL') Then
 
!           Check that the polynomial is not already defined
              If (polynomial_defined) Then
 
                 Call GS_FWARNING (1, 1, 'The polynomial is already defined', &
                   status)
 
              Else
 
!              Enter order of background polynomial
                 poly_order = 0
                 Call GS_INPI (.True., 0, Maxcoeffs - 1, .True., &
                   'POLYNOMIAL ORDER', 1, 'Order of polynomial function', 1, &
                   'Must be within specified bounds', poly_order, status)
 
!              Enter order of initialisation polynomial (all other
!              coefficients will be set to zero)
                 order_init = poly_order
                 Call GS_INPI (.True., 0, poly_order, .True., &
                   'INITIALISATION POLYNOMIAL ORDER', 1, &
                   'Order of initialisation polynomial function ', 1, &
                   'Must be within specified bounds', order_init, status)
 
!              Set all polynomial coefficients to zero
                 Call MA_RVALUE (Maxcoeffs, 1, 1, 1, poly_order + 1, 1, 0.0, &
                   COEFFICIENTS, status)
 
!              Fit initialisation polynomial
                 Call MA_1DCHEBYSHEV (.False., .True., 0, endelm - strelm + 1, &
                   endelm - strelm + 1, AXIS(strelm), RESIDUALS(strelm, &
                   y_value), dummy, Maxcoeffs, order_init, min_poly, max_poly, &
                   retstat, COEFFICIENTS, rms_r, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Do x = 1, order_init + 1
!              Write (*, '(''COEFFICIENT = '', g14.7)')
!              :                   COEFFICIENTS(x)
!              End Do
!              Write (*, '(''rms_r = '', g14.7)') rms_r
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 If (retstat .Ne. 0) Then
 
                    polynomial_defined = .False.
 
!                 Output warning
                    Call GS_FWARNING (1, 1, &
                      'Failed to fit polynomial coefficients', status)
 
                 Else
 
                    polynomial_defined = .True.
 
                    num_features = num_features + 1
                    OFFSETS(num_features) = 0.0
 
!                 Temporarily negate all the coefficients so that
!                 the polynomial values are subtracted from the data
!                 instead of being added
                    Call MA_RCMULT (poly_order + 1, 1, 1, 1, poly_order + 1, &
                      1, -1.0, COEFFICIENTS, status)
 
!                 Subtract polynomial from residuals
                    Call MA_CAL_POLYNOMIAL (.True., min_poly, max_poly, &
                      Maxcoeffs, poly_order, COEFFICIENTS, endelm - strelm + &
                      1, endelm - strelm + 1, AXIS(strelm), RESIDUALS(strelm, &
                      y_value), status)
 
!                 Negate coefficient values again to undo previous
!                 negation
                    Call MA_RCMULT (poly_order + 1, 1, 1, 1, poly_order + 1, &
                      1, -1.0, COEFFICIENTS, status)
 
!                 Add parameters to fit model
                    Do order = 1, poly_order + 1
                       num_parameters = num_parameters + 1
                       PARAMS(num_parameters) = COEFFICIENTS(order)
                       If (order .Eq. 1) Then
                          PARAMS_INFO(num_parameters) = 60000000 + order * &
                            10000 + 500 + num_features
                       Else
                          PARAMS_INFO(num_parameters) = order * 10000 + 500 + &
                            num_features
                       End If
                       Write (PARNAMES(num_parameters), '(''POL   '', i2)') &
                         order - 1
                       SCALE_FACTORS(num_parameters) = &
                         Max(PARAMS(num_parameters) / 50.0, 0.1)
                    End Do
 
!                 Calculate values of polynomial
                    Call MA_CAL_POLYNOMIAL (.False., min_poly, max_poly, &
                      Maxcoeffs, poly_order, COEFFICIENTS, endelm - strelm + &
                      1, endelm - strelm + 1, AXIS(strelm), PEAKS, &
                      status)
 
!                 Turn on clipping
                    Call LG_CLIP (.True., status)
 
!                 Set line style solid, blue, and standard width
                    Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
                    Call LG_POLYLINE (endelm - strelm + 1, AXIS(strelm), &
                      PEAKS, status)
 
!                 Turn off clipping
                    Call LG_CLIP (.False., status)
 
                 End If
 
              End If
              update_menu = .True.
 
           Else If (command .Eq. 'GAUSSIAN' .Or. command .Eq. 'LORENTZIAN' &
             .Or. command .Eq. 'VOIGTIAN') Then
 
              update_menu = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Before inps peak top'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Input top of peak
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, 'CLICK ON PEAK TOP', 1, &
                'Place cursor on centre and top', .False., 1, num_coordinates, &
                x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''after inps peak top'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_centre, &
                status)
 
              peak_max = y_coordinate
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''Before half height'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Input half height
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, 'CLICK ON PEAK AT HALF HEIGHT', 1, &
                'Place cursor on edge of peak at half its height', .False., 1, &
                num_coordinates, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''After half height'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_half, &
                status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''After MA_DC2PIXC'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              peak_half = y_coordinate
 
!           Set parameters
              num_features = num_features + 1
 
!           Peak position
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = x_centre
              SCALE_FACTORS(num_parameters) = 1.0
 
              If (command .Eq. 'GAUSSIAN') Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Before GAU POS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Create description of parameter value
                 num_gauss = num_gauss + 1
                 PARAMS_INFO(num_parameters) = 20000000 + 10000 + 200 + &
                   num_features
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''immediately before GAU POS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Write (PARNAMES(num_parameters), '(''GAU'', i2, '' POS'')') &
                   num_gauss
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''after GAU POS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 num_results = num_results + 1
                 Write (RESNAMES(num_results), '(''GAU'', i2, '' INT'')') &
                   num_gauss
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''after GAU INT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else If (command .Eq. 'LORENTZIAN') Then
 
!              Create description of parameter value
                 num_lor = num_lor + 1
                 PARAMS_INFO(num_parameters) = 20000000 + 10000 + 300 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''LOR'', i2, '' POS'')') &
                   num_lor
                 num_results = num_results + 1
                 Write (RESNAMES(num_results), '(''LOR'', i2, '' INT'')') &
                   num_lor
 
              Else If (command .Eq. 'VOIGTIAN') Then
 
!              Create description of parameter value
                 num_voigt = num_voigt + 1
                 PARAMS_INFO(num_parameters) = 20000000 + 10000 + 400 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''VOI'', i2, '' POS'')') &
                   num_voigt
                 num_results = num_results + 1
                 Write (RESNAMES(num_results), '(''VOI'', i2, '' INT'')') &
                   num_voigt
                 num_results = num_results + 1
                 Write (RESNAMES(num_results), '(''VOI'', i2, ''FWHM'')') &
                   num_voigt
 
              End If
 
!           Peak intensity
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = 2.0 * (peak_max - peak_half)
              SCALE_FACTORS(num_parameters) = &
                Sqrt(Abs(PARAMS(num_parameters)))
 
              If (command .Eq. 'GAUSSIAN') Then
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 20000 + 200 + num_features
                 Write (PARNAMES(num_parameters), '(''GAU'', i2, '' MAX'')') &
                   num_gauss
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''after GAU MAX'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else If (command .Eq. 'LORENTZIAN') Then
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 20000 + 300 + num_features
                 Write (PARNAMES(num_parameters), '(''LOR'', i2, '' MAX'')') &
                   num_lor
 
              Else If (command .Eq. 'VOIGTIAN') Then
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 20000 + 400 + num_features
                 Write (PARNAMES(num_parameters), '(''VOI'', i2, '' MAX'')') &
                   num_voigt
 
              End If
 
!           Standard Deviation or Half width
              num_parameters = num_parameters + 1
              SCALE_FACTORS(num_parameters) = 0.5
 
              If (command .Eq. 'GAUSSIAN') Then
 
                 sigma = Abs(x_centre - x_half) * 2.0 / 2.35482
                 PARAMS(num_parameters) = sigma
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 10000000 + 30000 + 200 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''GAU'', i2, ''  SD'')') &
                   num_gauss
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''after GAU SD'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              Else If (command .Eq. 'LORENTZIAN') Then
 
                 hwhm = Abs(x_centre - x_half)
                 PARAMS(num_parameters) = hwhm
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 10000000 + 30000 + 300 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''LOR'', i2, ''HWHM'')') &
                   num_lor
 
              Else If (command .Eq. 'VOIGTIAN') Then
 
!              Half to Gaussian and half to Lorentzian component
                 sigma = Abs(x_centre - x_half) / 2.35482
                 PARAMS(num_parameters) = sigma
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 10000000 + 30000 + 400 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''VOI'', i2, ''  SD'')') &
                   num_voigt
 
                 num_parameters = num_parameters + 1
                 hwhm = Abs(x_centre - x_half) / 2.0
                 PARAMS(num_parameters) = hwhm
 
!              Create description of parameter value
                 PARAMS_INFO(num_parameters) = 10000000 + 40000 + 400 + &
                   num_features
                 Write (PARNAMES(num_parameters), '(''VOI'', i2, ''HWHM'')') &
                   num_voigt
                 SCALE_FACTORS(num_parameters) = 0.5
 
              End If
 
!           Calculate offset value, for peak above background
              OFFSETS(num_features) = peak_max - (2.0 * (peak_max - &
                peak_half))
 
!           Turn on clipping
              Call LG_CLIP (.True., status)
 
!           Draw peak and subtract peak from residuals
              If (command .Eq. 'GAUSSIAN') Then
 
                 Call MA_1DGAUSSIAN (xmaxdat, strelm, endelm, -(2.0 * &
                   (peak_max - peak_half)), x_centre, sigma, 5.0, 1, &
                   RESIDUALS(1, y_value), status)
 
!              Calculate range of peak
                 start = Int(x_centre - 3.0 * sigma) + 1
                 start = Max(strelm, start)
                 end = Int(x_centre + 3.0 * sigma) + 1
                 end = Min(endelm, end)
 
!              Set offset value
                 Call MA_RVALUE (end - start + 1, 1, 1, 1, end - start + 1, 1, &
                   OFFSETS(num_features), PEAKS, status)
 
!              Add Gaussian
                 Call MA_1DGAUSSIAN (end - start + 1, 1, end - start + 1, 2.0 &
                   * (peak_max - peak_half), x_centre - Real(start-1), sigma, &
                   4.0, 1, PEAKS, status)
 
!              Set line style solid, blue, and standard width
                 Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
                 Call LG_POLYLINE (end - start + 1, AXIS(start), PEAKS, &
                   status)
 
              Else If (command .Eq. 'LORENTZIAN') Then
 
                 Call MA_1DLORENTZIAN (xmaxdat, strelm, endelm, -(2.0 * &
                   (peak_max - peak_half)), x_centre, hwhm, 1, RESIDUALS(1, &
                   y_value), status)
 
!              Calculate range of peak
                 start = Int(x_centre - 4.0 * hwhm) + 1
                 start = Max(strelm, start)
                 end = Int(x_centre + 4.0 * hwhm) + 1
                 end = Min(endelm, end)
 
!              Set offset value
                 Call MA_RVALUE (end - start + 1, 1, 1, 1, end - start + 1, 1, &
                   OFFSETS(num_features), PEAKS, status)
 
!              Add Lorentzian
                 Call MA_1DLORENTZIAN (end - start + 1, 1, end - start + 1, &
                   2.0 * (peak_max - peak_half), x_centre - Real(start - 1), &
                   hwhm, 1, PEAKS, status)
 
!              Set line style solid, blue, and standard width
                 Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
                 Call LG_POLYLINE (end - start + 1, AXIS(start), PEAKS, &
                   status)
 
              Else If (command .Eq. 'VOIGTIAN') Then
 
                 Call MA_1DVOIGTIAN (xmaxdat, strelm, endelm, -(2.0 * &
                   (peak_max - peak_half)), x_centre, sigma, hwhm, 0, &
                   RESIDUALS(1, y_value), status)
 
!              Calculate range of peak
                 start = Int(x_centre - 6.0 * sigma) + 1
                 start = Max(strelm, start)
                 end = Int(x_centre + 6.0 * sigma) + 1
                 end = Min(endelm, end)
 
!              Set offset value
                 Call MA_RVALUE (end - start + 1, 1, 1, 1, end - start + 1, 1, &
                   OFFSETS(num_features), PEAKS, status)
 
!              Add Voigtian
                 Call MA_1DVOIGTIAN (end - start + 1, 1, end - start + 1, 2.0 &
                   * (peak_max - peak_half), x_centre - Real(start - 1), &
                   sigma, hwhm, 0, PEAKS, status)
 
!              Set line style solid, blue, and standard width
                 Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
                 Call LG_POLYLINE (end - start + 1, AXIS(start), PEAKS, &
                   status)
 
              End If
 
!           Turn off clipping
              Call LG_CLIP (.False., status)
 
           Else If (command .Eq. 'EXP. DECAY') Then
 
              update_menu = .True.
 
!           Input top of start of decay
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, &
                'CLICK ON MAXIMUM AND START OF DECAY', 1, &
                'Place cursor on decay start and maximum', .False., 1, &
                num_coordinates, x_coordinate, y_coordinate, status)
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_centre, &
                status)
 
              peak_max = y_coordinate
 
!           Input position at half height
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, &
                'CLICK ON DECAY SLOPE AT HALF HEIGHT', 1, &
                'Place cursor on decay slope at half height', .False., 1, &
                num_coordinates, x_coordinate, y_coordinate, status)
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_half, &
                status)
 
              peak_half = y_coordinate
 
!           Set parameters
              num_features = num_features + 1
 
!           Peak position
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = x_centre
              SCALE_FACTORS(num_parameters) = 1.0
 
!           Create description of parameter value
              num_exponential = num_exponential + 1
              PARAMS_INFO(num_parameters) = 20000000 + 10000 + 1300 + &
                num_features
 
              Write (PARNAMES(num_parameters), '(''EXP'', i2, '' POS'')') &
                num_exponential
 
!           Peak intensity
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = 2.0 * (peak_max - peak_half)
              SCALE_FACTORS(num_parameters) = &
                Sqrt(Abs(PARAMS(num_parameters)))
 
!           Create description of parameter value
              PARAMS_INFO(num_parameters) = 20000 + 1300 + num_features
              Write (PARNAMES(num_parameters), '(''EXP'', i2, '' MAX'')') &
                num_exponential
 
!           Half-life
              num_parameters = num_parameters + 1
              SCALE_FACTORS(num_parameters) = 1.0
              half_life =  Abs(x_half - x_centre)
              PARAMS(num_parameters) = half_life
 
!           Create description of parameter value
              PARAMS_INFO(num_parameters) = 10000000 + 30000 + 1300 + &
                num_features
              Write (PARNAMES(num_parameters), '(''EXP'', i2, '' H-L'')') &
                num_exponential
 
!           Calculate offset value, for peak above background
              OFFSETS(num_features) = peak_max - (2.0 * (peak_max - &
                peak_half))
 
!           Turn on clipping
              Call LG_CLIP (.True., status)
 
!           Draw peak and subtract peak from residuals
              Call MA_1DEXPDECAY (xmaxdat, strelm, endelm, -(2.0 * (peak_max - &
                peak_half)), x_centre, half_life, 0, RESIDUALS(1, y_value), &
                status)
 
!           Calculate range of peak
              start = Int(x_centre) + 1
              start = Max(strelm, start)
              end = endelm
 
!           Set offset value
              Call MA_RVALUE (end - start + 1, 1, 1, 1, end - start + 1, 1, &
                OFFSETS(num_features), PEAKS, status)
 
!           Add exponential decay function
              Call MA_1DEXPDECAY (end - start + 1, 1, end - start + 1, 2.0 * &
                (peak_max - peak_half), x_centre - Real(start - 1), half_life, &
                0, PEAKS, status)
 
!           Set line style solid, blue, and standard width
              Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
              Call LG_POLYLINE (end - start + 1, AXIS(start), PEAKS, &
                status)
 
!           Turn off clipping
              Call LG_CLIP (.False., status)
 
           Else If (command .Eq. 'SINUSOIDAL') Then
 
              update_menu = .True.
 
!           Input top of cosine/sin function
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, &
                'CLICK ON MAXIMUM OF SINUSOIDAL FUNCTION', 1, &
                'Place cursor on maximum of sinusoidal function', .False., 1, &
                num_coordinates, x_coordinate, y_coordinate, status)
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_centre, &
                status)
 
              peak_max = y_coordinate
 
!           Click on adjacent minimum
              num_coordinates = 1
              Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, 1, strelm, &
                1, endelm, 1, DATA(1, y_value), MASK(1, y_value), AXIS, dummy, &
                title, xlabel, ' ', ylabel, 'CLICK ON ADJACENT MINIMUM', 1, &
                'Place cursor on adjacent minimum', .False., 1, &
                num_coordinates, x_coordinate, y_coordinate, status)
 
!           Convert to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, endelm, AXIS, x_coordinate, x_half, &
                status)
 
              peak_half = y_coordinate
 
!           Set parameters
              num_features = num_features + 1
 
!           Peak position
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = x_centre
              SCALE_FACTORS(num_parameters) = 1.0
 
!           Create description of parameter value
              num_sinusoidal = num_sinusoidal + 1
              PARAMS_INFO(num_parameters) = 20000000 + 10000 + 1400 + &
                num_features
 
              Write (PARNAMES(num_parameters), '(''TRI'', i2, '' POS'')') &
                num_sinusoidal
 
!           Amplitude
              num_parameters = num_parameters + 1
              amplitude = (peak_max - peak_half) / 2.0
              PARAMS(num_parameters) = amplitude
              SCALE_FACTORS(num_parameters) = &
                Sqrt(Abs(PARAMS(num_parameters)))
 
!           Create description of parameter value
              PARAMS_INFO(num_parameters) = 20000 + 1400 + num_features
              Write (PARNAMES(num_parameters), '(''TRI'', i2, '' AMP'')') &
                num_sinusoidal
 
!           Period
              num_parameters = num_parameters + 1
              SCALE_FACTORS(num_parameters) = 1.0
              period =  Abs(x_half - x_centre) * 2.0
              PARAMS(num_parameters) = period
 
!           Create description of parameter value
              PARAMS_INFO(num_parameters) = 10000000 + 30000 + 1400 + &
                num_features
              Write (PARNAMES(num_parameters), '(''TRI'', i2, '' PER'')') &
                num_sinusoidal
 
!           Calculate offset value, for peak above background
              OFFSETS(num_features) = peak_max - amplitude
 
!           Turn on clipping
              Call LG_CLIP (.True., status)
 
!           Draw function and subtract from residuals
              Call MA_1DSINUSOIDAL (xmaxdat, strelm, endelm, -amplitude, &
                x_centre, period, 0, RESIDUALS(1, y_value), status)
 
!           Calculate range of peak
              start = strelm
              end = endelm
 
!           Set offset value
              Call MA_RVALUE (end - start + 1, 1, 1, 1, end - start + 1, 1, &
                OFFSETS(num_features), PEAKS, status)
 
!           Add sinusoidal function
              Call MA_1DSINUSOIDAL (end - start + 1, 1, end - start + 1, &
                amplitude, x_centre - Real(start-1), period, 0, PEAKS, &
                status)
 
!           Set line style solid, blue, and standard width
              Call GS_LINESTYLE (Lg_solid, 1.0, Gs_blue, status)
              Call LG_POLYLINE (end - start + 1, AXIS(start), PEAKS, &
                status)
 
!           Turn off clipping
              Call LG_CLIP (.False., status)
 
           End If
 
        End If
 
!     Check status
        If (status .Eq. St_escapevalue) Then
 
!        Reset status system
           Call ST_DEF_SYSTEM (status)
 
        Else If (status .Ne. St_goodvalue) Then
           continue = .False.
        End If
 
     End Do
 
!  Free memory
!     Call IO_FREE (pPEAK, status)
     Deallocate (PEAKS)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Set model parameters to be defined
     params_exist = .True.
 
!  Set all model parameters to be unconstrained
     Do par = 1, num_parameters
        CONSTRAINTS(num_parameters) = .False.
     End Do
 
!  Define "RESIDUALS" array auxiliary parameters
     memory_defined = .True.
     mstrelm = strelm
     mendelm = endelm
     mnumdat = endelm
     mtitle = 'RESIDUALS OF INTITIAL FIT MODEL'
     mxlabel = xlabel
     mylabel = ylabel
 
!  Transfer axis values
     Do x = 1, endelm
        MAXIS(x) = AXIS(x)
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Do par = 1, num_parameters
!  Write (*, '(a, g14.7)') PARNAMES(par), PARAMS(par)
!  End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_1DINITIALISE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

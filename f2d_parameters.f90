!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_parameters.f90 *
!  *                    *
!  **********************
 
!+ F2D_PARAMETERS - FIT2D PARAMETERS input
     Subroutine F2D_PARAMETERS (experiment, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, DATA, VARIANCES, &
       title, xlabel, ylabel, zlabel, max_parameters, max_results, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, PARAMS, PARAM_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, RESNAMES, num_features, num_parameters, &
       num_results, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, parexist, weighted_fit, alpha, itsperpar, evolve, disfreq, &
       fastdis, haltcrit, haltval, MODEL, status)
!  Description:
!    Inputs parameters by one of several means controlled by a menu
!  Method:
!    Menu options allow selection of various forms of input for the
!    parameters
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    17-Apr-1999: V0.8 Extra argument for "F2D_GUI_PARAMETERS" (Hammersley)
!    25-Nov-1998: V0.7 Add beam centre arguments and use as the default for 
!      the symmetry centre (Hammersley)
!    01-Dec-1996: V0.6 Change 2-D fitting polynomial to a Chebyshev form, 
!      resulting in argument list changes (Hammersley)
!    16-Nov-1996: V0.5 Separate parameter constraints to a logical
!    array "CONSTRAINTS" (Hammersley)
!    03-Feb-1996: V0.4 User text when control passes to graphics window 
!      (Hammersley)
!    28-Feb-1995: V0.3 Make mask 1 byte elements (Hammersley)
!    24-Jan-1995: V0.2 Input pixel sizes from argument list (Hammersley)
!    25-Jan-1993: V0.1 Original, based on FIT2PAR (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Integer, Intent(IN) :: max_parameters ! The dimension size of 'PARAMS',
!      etc.
     Integer, Intent(IN) :: max_results ! The dimension size of 'RESNAMES'
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! Starting X-element of region to be
!      fitted
     Integer, Intent(INOUT) :: ystrelm ! Starting Y-element of region to be
!      fitted
     Integer, Intent(INOUT) :: xendelm ! End X-element of region to be
!      fitted
     Integer, Intent(INOUT) :: yendelm ! End Y-element of region to be
!      fitted
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
!  Export:
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The returned values for
!      the fit parameters
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters) ! Information on
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
!    8 = Twin peak maximum
!
!    For a 2-D twin polar Gaussian 1 = X-centre position
!    without centre                2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!    6 = Twin peak maximum
!
!    .X...... - Nature of parameter: Now unused:
!    Previously 0 = Unconstrained
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
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters) ! .True., if a
!      model parameter is fixed
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
!      A description of each of the parameters
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters) ! The typical change
!      sizes expected for the parameter values
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
!      A description of each of the results
     Integer, Intent(OUT) :: num_features ! Number of "features" in fit
     Integer, Intent(OUT) :: num_parameters ! Total of number of parameters
!      in fit
     Integer, Intent(OUT) :: num_results ! Number of results
     Integer, Intent(OUT) :: x_order ! X-order of 2-D background polynomial
     Integer, Intent(OUT) :: y_order ! Y-order of 2-D background polynomial
     Real, Intent(OUT) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymax_poly ! Y-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Logical, Intent(OUT) :: parexist ! .True., if parameters have been defined
     Logical, Intent(OUT) :: weighted_fit ! .True. if weighting is to be applied
     Real, Intent(OUT) :: alpha ! Accuracy level
     Real, Intent(OUT) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
     Logical, Intent(OUT) :: evolve ! .True. if the fit model is allowed to
!      evolve
     Integer, Intent(OUT) :: disfreq ! Frequency of graphical output during
!      fitting
     Logical, Intent(OUT) :: fastdis ! .True. if minimal text to be added to
!      diagrams
     Integer, Intent(OUT) :: haltcrit ! Criterion used to prematurely halt
!      the fitting
     Real, Intent(OUT) :: haltval ! Value for the halt criterion
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat) ! Contains input fit
!      model, corresponding to initial user input parameters
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Integer, Parameter :: Max_menu = 2 ! Maximum number of commands in the
!      menu
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
     Character(Len = 80) :: command ! Input command from the user
     Integer :: num_menu ! Number of choices in menu
!  Local Arrays:
     Character(Len = 20) :: MENU(Max_menu) ! Menu choices
     Character(Len = 80) :: MENUTXT(Max_menu) ! Text explaining menu choices
!  External Functions:
!  Local Data:
     Data MENU / 'FILE', 'GRAPHICALLY' /
     Data MENUTXT / 'FILE: Input from a file', &
       'GRAPHICALLY: Input through interactive graphics' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PARAMETERS ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Le. 0 .Or. max_parameters .Le. 0 .Or. max_results .Le. 0) &
       Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
 
!     Store position of error
        Call ST_SAVE ('Subroutine F2D_PARAMETERS ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Initialise variables
     command = 'GRAPHICALLY'
     num_menu = Max_menu
 
!  Get user to select between available menu options
     Call IO_MENU (.True., 'Input method', Max_menu, MENUTXT, 1, &
       'Enter one of the available choices', Max_menu, num_menu, MENU, &
       command, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  EXECUTE MENU CHOICES
 
     If (command .Eq. 'FILE') Then
 
        Call F2D_INP_PARAMETERS (xmaxdat, ymaxdat, max_parameters, &
          max_results, xnumdat, ynumdat, MASK, PARAMS, PARAM_INFO, &
          CONSTRAINTS, PARNAMES, SCALE_FACTORS, RESNAMES, num_features, &
          num_parameters, num_results, x_order, y_order, xmin_poly, ymin_poly, &
          xmax_poly, ymax_poly, weighted_fit, alpha, itsperpar, evolve, &
          disfreq, fastdis, haltcrit, haltval, parexist, status)
 
     Else If (command .Eq. 'GRAPHICALLY') Then
 
!     Input fit parameters graphically giving first frame to be
!     fitted as graphical output
        Call F2D_GUI_PARAMETERS (.False., experiment, &
          xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, &
          DATA, title, xlabel, ylabel, zlabel, max_parameters, max_results, &
          xstrelm, ystrelm, xendelm, yendelm, MASK, x_order, y_order, &
          xmin_poly, ymin_poly, xmax_poly, ymax_poly, num_features, &
          num_parameters, PARAMS, PARAM_INFO, CONSTRAINTS, PARNAMES, &
          SCALE_FACTORS, num_results, RESNAMES, MODEL, status)
 
        parexist = .True.
 
     End If
 
     End Subroutine F2D_PARAMETERS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

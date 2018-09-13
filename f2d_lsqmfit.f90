!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_lsqmfit.f90 *
!  *                 *
!  *******************
 
!+ F2D_LSQMFIT - Least SQuares MFIT optimisation
     Subroutine F2D_LSQMFIT ( & ! max_vec_values, max_vectors, 
       y_row, x_pixel_size, &
       variances_exist, maxdat, AXIS, DATA, VARIANCES, MASK, strelm, endelm, &
       title, x_label, y_label, alpha, fitting_info, weighted_fit, itsperpar, &
       display, max_parameters, num_parameters, num_1dfeatures, max_results, &
       max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PARAMS, PARERRORS, results_exist, FRESULTS, RESERRORS, &
       RESNAMES, num_fun_calls, chisqr, MODEL, COVARIANCE, num_text, TEXT, &
       print_type, WEIGHTS, results, status)
!VECTORS, status)
!  Description:
!    Fits 2-D data by least squares fitting with geometric functions
!    using "MA_MODELFIT" to refine the parameters.
!    "PARDES(max_parameters)" contains the information to determine what
!    each parameter represents.
!    N.B. The X-coordinate parameters must all be in pixels
!    coordinates.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Dec-2014: V0.18 Protect "results" array against too many parameters
!      (Hammersley)
!    09-Dec-2014: V0.17 Use result vectors data structure (Hammersley)
!    15-Mar-2006: V0.16 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    02-Dec-1998: V0.15 Add output of R-factor (Hammersley)
!    30-Oct-1998: V0.14 Improve calculation of Voigtian FWHM (Hammersley)
!    29-Oct-1998: V0.13 Save results in "VECTORS" array (Hammersley)
!    23-Oct-1998: V0.12 Calculate "Voigtian" integrated intensity
!      using Lorentzian formula if SD sigma is zero (Hammersley)
!    21-Oct-1998: V0.11 Checking why Voigtian HWHM values can still
!      appear negative, and why results seem not to be updated
!      immediately (Hammersley)
!    02-Jul-1998: V0.10 Set Voigtian sigma or half width values to
!      zero if set negative (Hammersley)
!    04-Feb-1997: V0.9 Correct Voigtian FWHM result for scaling (Hammersley)
!    16-Jan-1997: V0.8 Output RMS mis-fit (Hammersley)
!    15-Jan-1997: V0.7 Correct peak positions for zoomed-in data (Hammersley)
!    10-Jan-1997: V0.6 Take into axis scaling when outputing results text 
!      (Hammersley)
!    17-Dec-1996: V0.5 Enable display frequency support (Hammersley)
!    15-Dec-1996: V0.4 Support weighted fitting (Hammersley)
!    13-Dec-1996: V0.3 Changes for "F2D_MFITDISPLAY" (Hammersley)
!    10-Dec-1996: V0.2 Define results text (Hammersley)
!    05-Dec-1996: V0.1 Original, based on "F2D_LSQFIT2D" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_lsqmfit.inc' ! Used to pass extra information to fitting 
!      minimisation objective function
!  Import:
!     Integer, Intent(IN) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
     Integer, Intent(IN) :: y_row ! Number of row being fitted
     Real, Intent(IN) :: x_pixel_size ! Size of a raw pixel in the
!      X-direction (metres)
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(IN) :: maxdat ! First dimension of data arrays
     Real, Intent(IN) :: AXIS(maxdat) ! Axis data values
     Real, Intent(IN) :: DATA(maxdat) ! Array containing data to be fitted
     Real, Intent(IN) :: VARIANCES(maxdat) ! Array containing variances in
!      the  data values
     Logical*1, Intent(IN) :: MASK(maxdat) ! Data mask, .True., indicates
!    masked data point (i.e. point not considered in fitting)
     Integer, Intent(IN) :: strelm ! Starting element of region to be fitted
     Integer, Intent(IN) :: endelm ! End element of region to be fitted
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: x_label ! X-axis label for plot
     Character(Len = *), Intent(IN) :: y_label ! Y-axis label for plot
     Real, Intent(IN) :: alpha ! Accuracy required for fit in average sigma
!      per data point (this does not mean that this accuracy will
!      necessarily be reached)
     Integer, Intent(IN) :: fitting_info ! Level of user information output:
!      0: no information
!      >= 1" output iteration numbers
!      >= 2: output sum of squared residuals
     Logical, Intent(IN) :: weighted_fit ! .True., if weighted fitting is
!      required
     Real, Intent(IN) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
     Logical, Intent(IN) :: display ! .True., if the fitted results are to
!      be displayed graphically
     Integer, Intent(IN) :: max_parameters ! Dimension size of "PARAMS",
!      etc.
     Integer, Intent(IN) :: num_parameters ! Total number of variable and
!      constrained parameters in the fit
     Integer, Intent(IN) :: num_1dfeatures ! Number of features in 1-D model
     Integer, Intent(IN) :: max_results ! Dimension size of "RESULTS",
!      "RESERRORS"
     Integer, Intent(IN) :: max_text ! Dimension size of "TEXT"
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      A description of each fit parameter
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
!      of the parameters of the fit. Describing to which feature it belongs,
!    the type of feature and the number of the parameter within the
!    feature, and whether the parameter is constrained or free to
!    be varied.
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
!    12 = Pseudo-Voigt
!    13 = Exponential decay
!    20 = 2-D Chebyshev polynomial
!    50 = "Row-line":
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
!
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters) ! Scale factors array
!      for the parameter
!  Import/Export:
     Real, Intent(INOUT) :: PARAMS(max_parameters) ! The fit model parameters
!  Export:
     Integer, Intent(OUT) :: num_results ! Number of results calculated
     Real, Intent(INOUT) :: PARERRORS(max_parameters) ! Estimated errors for
!      each fitted parameter
     Logical, Intent(OUT) :: results_exist ! .True. if fit results exist
     Real, Intent(OUT) :: FRESULTS(max_results) ! Results calculated from the
!      fit parameters, i.e. integration intensity
     Real, Intent(INOUT) :: RESERRORS (max_results) ! Error in Results
!      calculated from the fit parameters
     Character(Len = *), Intent(INOUT) :: RESNAMES(max_results) ! Names of 
!      results
     Integer, Intent(OUT) :: num_fun_calls ! Number of function calls, set
!      negative if the fit fails
     Real, Intent(INOUT) :: chisqr ! The reduced chi squared or goodness of
!      fit parameter
     Real, Intent(OUT) :: MODEL(maxdat) ! The least squares theoretical
!      fit to the data
     Real, Intent(INOUT) :: COVARIANCE(max_parameters, max_parameters)
!      The estimated fractional variance-covariance matrix
     Integer, Intent(OUT) :: num_text ! Number of lines of defined results
!      text
     Character(Len = *), Intent(OUT) :: TEXT(max_text) ! Output text
     Character(Len = *), Intent(OUT) :: print_type ! Type, of graphics to
!      print. Supported types are:
!    "banner"
!    "image"
!    "contour"
!    "x/y graph"
!    "optimise"
     Real, Intent(OUT) :: WEIGHTS(endelm) ! Work array, only used if
!      "weighted_fit" is .True.,
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Real, Intent(OUT) :: VECTORS(max_vec_values, max_vectors)
!      Multiple 1-D arrays of vector values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.18' ! Version number
     Integer, Parameter :: Maxipar = 100 ! Dimension size for FEATPAR
     Real, Parameter :: Sqr2pi = 2.506628 ! The square root of 2 * Pi
!  Local Variables:
     Double Precision :: bb ! Input for complex error function
     Double Precision :: icmperr ! Imaginary component of complex error function
     Double Precision :: rcmperr ! Real component of complex error function
     Integer :: feat_type ! Type of feature:
!      1 = Normal polynomial
!      2 = Gaussian
!      3 = Lorentzian
!      4 = Voigtian
!      5 = Chebyshev polynomial
!      6 = 2-D Gaussian
!      7 = 2-D polar Gaussian
!      8 = 2-D polar Gaussian, no centre
!      9 = 2-D polynomial (normal)
!     10 = Twin 2-D polar Gaussian
!     11 = Twin 2-D polar Gaussian, no centre
!     12 = Pseudo-Voigt (1-D)
!     13 = Exponential decay function (1-D)
!     20 = 2-D Chebyshev polynomial
!     50 = "Row" line
     Integer :: feature ! Loop varaible for features
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_valid ! Number of valid data points
     Integer :: par ! Loop variable for parameters
     Integer :: par_num ! Number of a parameter of a function
     Integer :: retstat ! Return status variable:
!    -10 = No variable parameters to optimise
!    -3 = The cosine of the angle between "RESIDUALS" and any
!    column of the Jacobian is at most "Abs(tolerance)"
!    -2 = Condition of "0" and "-1" are both true
!    -1 = Actual and predicted relative reductions in the sum
!    of squares are at most "tolerance"
!    0 = Good status: Relative error between two iterations
!    is "tolerance" or below
!    1 = More variable parameters than unmasked data values
!    2 = Not enough available memory
!    3 = Improper input parameters
!    4 = Number of calls to "CAL_MODEL" has reached or
!    exceeded "max_fun_calls"
!    5 = "tolerance" is too small. No further reduction in the
!    the sum of squares is possible
!    6 = "tolerance" is too small. No further improvement in
!    the approximate solution "PARAMS" is possible
!    7 = "tolerance" is too small. "RESIDUALS" is orthogonal to
!    the columns of the Jacobian (to machine precision)
     Integer :: scaling_type ! Type of transform for parameter output
!      0 = No transform required
!      1 = X-axis Simple scaling
!      2 = X-axis Scaling and shift
     Integer :: x ! Loop variable
     Real :: fepsilon ! Estimated error in calculating function
     Real :: gau_fwhm ! FWHM of Gaussian component
     Real :: lor_fwhm ! FWHM of Lorentzian component
     Real :: offset ! Offset for start of parameter scaling
     Real :: r_factor_p ! Sum of residuals divided by the sum of the
!      data values times 100.0
     Real :: scale ! Scaling between pixel and axis values
     Real :: step_bound ! Maximum iteration step bound
     Real :: sum_observations ! Sum of the data values fitted
     Real :: sum_residuals ! Sum of the residuals of the fit
     Real :: sumsq ! Sum of squares
     Real :: tolerance ! Tolerance on termination
     Real :: value ! Scaled parameter value
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(10) ! User messages
     Real :: FEATPAR(Maxipar) ! Array to hold parameters of the 
!      feature to be created; in their correct order
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     Integer, External :: Ma_exdigit ! Extracts digit from an integer number
     External F2D_FUNMFIT
!    function
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQMFIT ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (Min(maxdat, max_parameters, max_results) .Le. 0) Then
        status = St_bad_dim1
     Else If (strelm .Le. 0 .Or. strelm .Gt. endelm .Or. endelm .Gt. maxdat) &
       Then
        status = St_bad_adr1
     Else If (num_parameters .Gt. max_parameters) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQMFIT ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_LSQMFIT'')')

!  Test VECTORS assignment
!     results%VECTORS(1, 1) = 5.0

!     Write (*, '(''F2D_LSQMFIT: After vectors assignment'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Calculate scaling parameters
     offset = AXIS(strelm)
     scale = AXIS(strelm + 1) - AXIS(strelm)
 
!  Calculate maximum number of function calls
     maxfun = Nint(Real(num_parameters) * itsperpar)
 
!  Calculate standard deviations from variances
     If (weighted_fit) Then
 
        Do x = strelm, endelm
           WEIGHTS(x) = Sqrt(VARIANCES(x))
        End Do
 
     End If
 
!  Transfer extra necessary information via common to model function
     f2d_x_pixel_size = x_pixel_size
 
!  Non-linear least squares fitting of model parameters to the data
!  Set up variables for "MA_MODELFIT"
     tolerance = 1.0e-5
     step_bound = 100.0
     fepsilon = 5.0e-7
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_LSQMFIT: Before MA_MODELFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Perform minimisation
     Call MA_MODELFIT (experiment, fitting_info, F2D_FUNMFIT, &
       .True., weighted_fit, &
       .True., maxdat, 1, DATA, MASK, WEIGHTS, strelm, 1, endelm, 1, 1, &
       maxfun, tolerance, step_bound, fepsilon, Max_parameters, &
       num_parameters, PARAM_INFO, Max_parameters, num_parameters, &
       CONSTRAINTS, SCALE_FACTORS, maxdat, 1, .True., PARAMS, MODEL, retstat, &
       num_fun_calls, sumsq, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_LSQMFIT: After MA_MODELFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     If (retstat .Ge. 1) Then
 
        Call IO_WRITE ('WARNING: Problem fitting the model to the data', &
          status)
        Call GS_FWARNING (1, 1, 'WARNING: Problem fitting ' // &
          'the model to the data', status)
 
     Else
 
!     Set negative Voigtian sigma or HWHM values to negative
        Do par = 1, num_parameters
 
           feat_type = Mod(PARAM_INFO(par), 10000) / 100
           If (feat_type .Eq. 4) Then
 
!           The parameter comes from a Voigtian peak
              par_num = Ma_exdigit(PARAM_INFO(par), 6, status) * 10 + &
                Ma_exdigit(PARAM_INFO(par), 5, status)
              If (par_num .Eq. 3 .Or. par_num .Eq. 4) Then
                 PARAMS(par) = Max(0.0, PARAMS(par))
              End If
 
           End If
 
        End Do
 
!     Store integrated intensities in "FRESULTS" array
        num_results = 0
        Do feature = 1, num_1dfeatures
 
!        Find out type of feature from the first parameter of the
!        feature and the number of the parameter is the first
!        parameter of the feature
           Do par = 1, num_parameters
 
              If (Mod(PARAM_INFO(par), 100) .Eq. feature) Then
 
                 feat_type = Mod(PARAM_INFO(par), 10000) / 100
                 par_num = Ma_exdigit(PARAM_INFO(par), 6, status) * 10 + &
                   Ma_exdigit(PARAM_INFO(par), 5, status)
                 scaling_type = Ma_exdigit(PARAM_INFO(par), 8, status)
 
                 If (scaling_type .Eq. 0) Then
                    FEATPAR(par_num) = PARAMS(par)
                 Else If (scaling_type .Eq. 1) Then
                    FEATPAR(par_num) = PARAMS(par) * scale
                 Else If (scaling_type .Eq. 2) Then
                    FEATPAR(par_num) = (PARAMS(par) - Real(strelm) + 0.5) * &
                      scale + offset
                 Else If (scaling_type .Eq. 5) Then
                    FEATPAR(par_num) = PARAMS(par) * 180.0 / Pi
                 Else If (scaling_type .Eq. 6) Then
                    FEATPAR(par_num) = PARAMS(par) / 2.0
                 End If
 
              End If
 
           End Do
 
           If (feat_type .Eq. 2) Then
 
!           Gaussian peak
              num_results = num_results + 1
 
              FRESULTS(num_results) = FEATPAR(2) * FEATPAR(3) * Sqr2pi
!           RESERRS(rescount,y)=sqr2pi*Sqrt((PARAMS(par+1,y)*
!           :                PARERRS(par+2,y))**2+
!           :               (PARAMS(par+2,y)*PARERRS(par+1,y))**2)*xscaling
 
           Else If (feat_type .Eq. 3) Then
 
!           Lorentzian peak
              num_results = num_results + 1
 
              FRESULTS(num_results) = FEATPAR(2) * FEATPAR(3) * Pi
 
!           RESERRS(rescount,y)=pi*Sqrt((PARAMS(par+1,y)*
!           :                PARERRS(par+2,y))**2+
!           :               (PARAMS(par+2,y)*PARERRS(par+1,y))**2)*xscaling
 
           Else If (feat_type .Eq. 4) Then
 
!           Voigtian peak
              num_results = num_results + 1
 
!           Integrated intensity
              If (FEATPAR(4) .Eq. 0.0) Then
 
!              Calculate integrated intensity of Gaussian
                 FRESULTS(num_results) = FEATPAR(2) * FEATPAR(3) * Sqr2pi
 
              Else If (FEATPAR(3) .Eq. 0.0) Then
 
!              Calculate integrated intensity of Lorentzian
                 FRESULTS(num_results) = FEATPAR(2) * FEATPAR(4) * Pi
 
              Else
 
!              Find normalising constant
 
!              Calculate integrated intensity of Voigtian
                 bb = Dble(FEATPAR(4) / (FEATPAR(3) * Sqrt(2.0)))
                 Call MA_WERF(0.0d0, bb, rcmperr, icmperr, status)
 
                 FRESULTS(num_results) = FEATPAR(3) * Sqr2pi * FEATPAR(2) / &
                   Real(rcmperr)
 
              End If
 
!           Voigtian FWHM: Use series expansion approximation
!           given in JAC (1987) Vol 20, p79-83 (Note: Is this for
!           a Voigtian, or a pseudo-Voigtain)
              num_results = num_results + 1
              gau_fwhm = 2.354 * FEATPAR(3)
              lor_fwhm = 2 * FEATPAR(4)
              FRESULTS(num_results) = (gau_fwhm**5 + 2.69269 * gau_fwhm**4 * &
                lor_fwhm    + 2.42843 * gau_fwhm**3 * lor_fwhm**2 + 4.47163 * &
                gau_fwhm**2 * lor_fwhm**3 + 0.07842 * gau_fwhm    * &
                lor_fwhm**4 + lor_fwhm**5)**0.2
 
!           RESERRS(rescount,y)=Sqrt(
!           :               (2.0*2.354*PARAMS(par+2,y)*xscaling*
!           :               2.354*PARERRS(par+2,y)*xscaling/0.69314718)**2+
!           :               (8.0*PARAMS(par+3,y)*xscaling*
!           :               PARERRS(par+3,y)*xscaling)**2)/
!           :               (2.0*RESULTS(rescount,y))
 
           End If
 
        End Do
 
!     Define results text
        Write (TEXT(1), '(''FITTED PARAMETERS'')')
        Write (TEXT(2), '('' '')')
        num_text = 2
 
        Write (TEXT(num_text + 1), '(''No Quantity    Value'')')
!     :       '(''No Quantity    Value      Error'')')
        num_text = num_text + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_LSQMFIT: Before results%VECTORS()'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Add parameter values to text
        Do par = 1, num_parameters
 
!        Take into account scaling requirements for parameter
           scaling_type = Ma_exdigit(PARAM_INFO(par), 8, status)
           If (scaling_type .Eq. 0) Then
              value = PARAMS(par)
           Else If (scaling_type .Eq. 1) Then
              value = PARAMS(par) * scale
           Else If (scaling_type .Eq. 2) Then
              value = (PARAMS(par) - Real(strelm) + 0.5) * scale + offset
           Else If (scaling_type .Eq. 5) Then
              value = PARAMS(par) * 180.0 / Pi
           Else If (scaling_type .Eq. 6) Then
              value = PARAMS(par) / 2.0
           End If
 
           If (y_row .Le. results%max_values .And. &
             par .Le. results%max_vectors) Then
              results%VECTORS(y_row, par) = value
           End If

           num_text = num_text + 1
           Write (TEXT(num_text), '(a10, '' '', 1pe12.5)') PARNAMES(par), &
             value
!        :         '(a10, '' '', 1pe12.5, 1x, 1pe9.2)')
!        :         PARNAMES(par), PARAMS(par),
!        :          PARERRORS(par)
        End Do
 
        results_exist = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_LSQMFIT: After results%VECTORS()'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Add results to text
        Do par = 1, num_results
           num_text = num_text + 1
           Write (TEXT(num_text),'(a10, '' '', 1pe10.3)') RESNAMES(par), &
             FRESULTS(par)
 
           If (y_row .Le. results%max_values .And. &
             num_parameters + par .Le. results%max_vectors) Then
              results%VECTORS(y_row, num_parameters + par) = FRESULTS(par)
           End If

        End Do
 

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_LSQMFIT: After adding results'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     Write (message, '(''INFO: Number of function calls = '', i8)') &
       num_fun_calls
     Call IO_WRITE (message, status)
     Write (message, '(''      Sum of squares = '', g14.7)') sumsq
     Call IO_WRITE (message, status)
     num_valid = 0
     sum_residuals = 0.0
     sum_observations = 0.0
     Do x = strelm, endelm
 
        If (.Not. MASK(x)) Then
           num_valid = num_valid + 1
           sum_residuals = sum_residuals + Abs(DATA(x) - MODEL(x))
           sum_observations = sum_observations + Abs(DATA(x))
        End If
 
     End Do
     Write (message, '(''      RMS mis-fit per data element = '', g14.7)') &
       Sqrt(sumsq / Real(num_valid))
     Call IO_WRITE (message, status)
 
!  Calculate Rp = Sum ( Abs(y_i(obs) - y_i(calc)) / Sum (y_i(obs))
     r_factor_p = 100.0 * sum_residuals / sum_observations
     Write (message, '(''      R_p = '', f7.2, ''%'')') r_factor_p
     Call IO_WRITE (message, status)
 
     If (display) Then
 
!     Display results of model fitting
        Call F2D_MFITDISPLAY (x_pixel_size, variances_exist, maxdat, 1, AXIS, &
          DATA, VARIANCES, MASK, MODEL, strelm, endelm, 1, strelm, endelm, &
          title, x_label, y_label, max_parameters, num_parameters, &
          num_1dfeatures, PARAM_INFO, PARAMS, f2d_1dorder, f2d_min_poly, &
          f2d_max_poly, max_text, num_text, TEXT, status)
 
!     Set print type
        print_type = 'optimise'
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_LSQMFIT: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_LSQMFIT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

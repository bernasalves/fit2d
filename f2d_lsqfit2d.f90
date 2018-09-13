!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_lsqfit2d.f90 *
!  *                  *
!  ********************
 
!+ F2D_LSQFIT2D - FIT 2-D MODEL FITting minimisation
     Subroutine F2D_LSQFIT2D (EXPERIMENT, xmaxdat, ymaxdat, &
       XAXIS, YAXIS, DATA, VARIANCES, MASK, xstrelm, ystrelm, xendelm, &
       yendelm, alpha, weighted_fit, itsperpar, max_parameters, &
       num_parameters, max_results, num_results, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PARAMS, PARERRORS, RESULTS, RESERRORS, RESNAMES, &
       num_fun_calls, chisqr, MODEL, COVARIANCE, status)
!  Description:
!    Fits 2-D data by least squares fitting with geometric functions
!    using "MA_MODELFIT" to refine the parameters.
!    "PARDES(max_parameters)" contains the information to determine what
!    each parameter represents.
!    N.B. The X-coordinate parameters must all be in pixels coordinates.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    30-Mar-2006: V0.12 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.10 Correct rowline parameter definition (Hammersley)
!    01-Dec-1996: V0.9 Change 2-D fitting polynomial to a Chebyshev
!      polynomial (Hammersley)
!    16-Nov-1996: V0.8 Rename for "F2D_NAG" and replace NAG
!      minimisation routine with "MA_MODELFIT" (Hammersley)
!    03-Jan-1996: V0.7 Changes necessary for IBM AIX "xlf" compiler (Hammersley)
!    23-Oct-1995: V0.6 Get rid of NAG warning message when minimisation routine 
!      fails to reach the "global" minimum, and output more informative 
!      messages (Hammersley)
!    28-Feb-1995: V0.5 Change mask pixels to single bytes (Hammersley)
!    15-Feb-1995: V0.4 Change passing of information from "F2D_FIT" and 
!      "F2D_LSQFUN" (Hammersley)
!    24-Jan-1995: V0.3 Input pixel sizes from arguments (Hammersley)
!    07-Apr-1994: V0.2 Use "F2D_PGAUINT" to calculate integrated
!      area of polar Gaussians (Hammersley)
!    17-Mar-1993: V0.1 Original, based on "F2D_MINIMISE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'f2d_lsqfit2d.inc' ! Used to pass extra
!    information to fitting minimisation objective function
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis data values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis data values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region to be
!      fitted
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region to be
!      fitted
     Integer, Intent(IN) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End Y-element of region to be fitted
     Real, Intent(IN) :: alpha ! Accuracy required for fit in average sigma
!      per data point (this does not mean that this accuracy will
!      necessarily be reached)
     Logical, Intent(IN) :: weighted_fit ! .True., if weighted fitting is
!      required
     Real, Intent(IN) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
     Integer, Intent(IN) :: max_parameters ! Dimension size of "PARAMS",  etc.
     Integer, Intent(IN) :: num_parameters ! Total number of variable and
!      constrained parameters in the fit
     Integer, Intent(IN) :: max_results ! Dimension size of "RESULTS",
!      "RESERRORS"
     Integer, Intent(IN) :: PARAM_INFO(Max_parameters) ! Information on each
!      of the parameters of the fit. Describing to which feature it belongs,
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
     Logical, Intent(IN) :: CONSTRAINTS(Max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real, Intent(IN) :: SCALE_FACTORS(Max_parameters) ! Scale factors array
!      for the parameter
!  Import/Export:
     Real, Intent(INOUT) :: PARAMS(Max_parameters) ! The fit model parameters
!  Export:
     Integer, Intent(OUT) :: num_results ! Number of results calculated
     Real, Intent(INOUT) :: PARERRORS(max_parameters) ! Estimated errors for
!      each fitted parameter
     Real, Intent(OUT) :: RESULTS(max_results) ! Results calculated from the
!      fit parameters, i.e. integration intensity
     Real, Intent(OUT) :: RESERRORS (max_results) ! Error in Results
!      calculated from the fit parameters
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
!      Names of results
     Integer, Intent(OUT) :: num_fun_calls ! Number of function calls, set
!      negative if the fit fails
     Real, Intent(INOUT) :: chisqr ! The reduced chi squared or goodness of
!      fit parameter
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat) ! The least squares
!      theoretical fit to the data
     Real, Intent(INOUT) :: COVARIANCE(max_parameters, max_parameters)
!      The estimated fractional variance-covariance matrix
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number
!  Local Variables:
     Integer :: feattype ! Type of feature
     Integer :: feature ! Loop variable for features
     Integer :: high_order ! Highest order of a row-line
     Integer :: low_order ! Lowest order of a row-line
     Integer :: maxfun ! Maximum number of function calls
     Integer :: numrow ! Number of row-lines
     Integer :: order ! Loop variable for order of row-line peaks
     Integer :: par ! Loop variable for parameters
     Integer :: retstat ! Return status variable:
!      -10 = No variable parameters to optimise
!      -3 = The cosine of the angle between "RESIDUALS" and any
!           column of the Jacobian is at most "Abs(tolerence)"
!      -2 = Condition of "0" and "-1" are both true
!      -1 = Actual and predicted relative reductions in the sum
!           of squares are at most "tolerence"
!       0 = Good status: Relative error between two iterations
!           is "tolerence" or below
!       1 = More variable parameters than unmasked data values
!       2 = Not enough available memory
!       3 = Improper input parameters
!       4 = Number of calls to "CAL_MODEL" has reached or exceeded 
!           "max_fun_calls"
!       5 = "tolerence" is too small. No further reduction in the
!           the sum of squares is possible
!       6 = "tolerence" is too small. No further improvement in
!           the approximate solution "PARAMS" is possible
!       7 = "tolerence" is too small. "RESIDUALS" is orthogonal to
!           the columns of the Jacobian (to machine precision)
     Integer stat ! Status return variable for "Allocate"
     Logical :: geometry_defined ! .True., if the experimental geometry
!      is defined
     Real :: angsig ! Angular standard deviation
     Real :: delta_latitude ! Change in latitude
     Real :: distance ! Distance from symmetry centre to peak centre
     Real :: fepsilon ! Estimated error in calculating function
     Real :: intensity ! Peak intensity of a peak
     Real :: intens2 ! Peak intensity of a twin peak
     Real :: peak_angle ! Angle to peak on row-line
     Real :: peak_latitude ! Latitude of peak
     Real :: peak_longitude ! Longitude of peak
     Real :: ppangle ! Angle between row-line peaks (radians)
     Real :: ppashift ! Offset angle from row-line centre to first peak
     Real :: radial_distance ! Distance radially from symmetry centre to
!      row-line centre
     Real :: rowline_angle ! Angle to row-line
     Real :: rowline_latitude ! Latitude of centre of row-line
     Real :: sigma1 ! Standard deviation of first axis
     Real :: sigma2 ! Standard deviation of second axis
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares
     Real :: tolerence ! Tolerence on termination
     Real :: x_up ! X-component of "Up" vector
     Real :: xcenline ! X-centre of a row-line
     Real :: xcenpeak ! X-centre of a peak
     Real :: xpolar ! X-centre of polar symmetry
     Real :: xrel ! Relative X-distance between symmetry centre and row-line 
!      centre
     Real :: y_up ! Y-component of "Up" vector
     Real :: ycenline ! Y-centre of a row-line
     Real :: ycenpeak ! Y-centre of a peak
     Real :: ypolar ! Y-centre of polar symmetry
     Real :: yrel ! Relative Y-distance between symmetry centre and row-line 
!      centre
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(10) ! User messages
     Real, Allocatable :: WORK(:) ! Dynamic work array
!  External Functions:
     Integer, External :: Ma_exdigit ! Extracts digit from an integer number
     Real, External :: Ma_gaussinteg ! Calculates integral of normalised
!      Gaussian probability within defined limits
     External F2D_FUNFIT2D
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQFIT2D ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (Min(xmaxdat, max_parameters, max_results) .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. &
       xendelm.Gt.xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     Else If (num_parameters .Gt. max_parameters) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQFIT2D ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Calculate maximum number of function calls
     maxfun = Nint(Real(num_parameters) * itsperpar)
 
!  Non-linear least squares fitting of model parameters to the data
!  Set up variables for 'MA_MODELFIT'
     tolerence = 1.0e-5
     step_bound = 100.0
     fepsilon = 5.0e-7
 
!  Perform minimisation
     Call MA_MODELFIT (EXPERIMENT, &
       2, F2D_FUNFIT2D, .True., weighted_fit, .True., xmaxdat, &
       ymaxdat, DATA, MASK, VARIANCES, xstrelm, ystrelm, xendelm, yendelm, 1, &
       maxfun, tolerence, step_bound, fepsilon, Max_parameters, &
       num_parameters, PARAM_INFO, Max_parameters, num_parameters, &
       CONSTRAINTS, SCALE_FACTORS, xmaxdat, ymaxdat, .True., PARAMS, MODEL, &
       retstat, num_fun_calls, sumsq, status)
 
     If (retstat .Ge. 1) Then
        Call IO_WRITE ('WARNING: Problem fitting the ' // 'model to the data', &
          status)
     End If
 
     Write (message, '(''INFO: Number of function calls = '', i8)') &
       num_fun_calls
     Call IO_WRITE (message, status)
     Write (message, '(''INFO: Sum of squares = '', g14.5)') sumsq
     Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Calculate integration intensities of peaks and store in 'RESULTS'
 
!  Obtain dynamic memory for calculating polar Gaussian function
     Allocate (WORK(xendelm * yendelm), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_fit2d + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_LSQFIT2D ' // Version)
        Return
     End If
 
     num_results = 0
     numrow = 0
     Do feature = 1, f2d_num_features
 
!     Find out type of feature from the first parameter of the feature and the
!     number of the parameter is the first parameter of the feature
        Call F2D_FUNTYPE (max_parameters, PARAM_INFO, num_parameters, feature, &
          feattype, par, status)
 
        If (feattype .Eq. 6) Then
 
!        2-D Gaussian peak
           num_results = num_results + 1
           intensity = PARAMS(par + 2)
           sigma1 = PARAMS(par + 3)
           sigma2 = PARAMS(par + 4)
           RESULTS(num_results) = 2.0 * Pi * intensity * sigma1 * sigma2
 
        Else If (feattype .Eq. 7) Then
 
!        2-D Polar Gaussian peak, (with centre)
           xpolar = PARAMS(par)
           ypolar = PARAMS(par + 1)
           xcenpeak = PARAMS(par + 2)
           ycenpeak = PARAMS(par + 3)
           intensity = PARAMS(par + 4)
           sigma1 = PARAMS(par + 5)
           angsig = PARAMS(par + 6)
 
!        Calculate integrated intensity (numerically)
           num_results = num_results + 1
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intensity, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
        Else If (feattype .Eq. 8) Then
 
!        2-D Polar Gaussian peak, (no centre)
           xcenpeak = PARAMS(par)
           ycenpeak = PARAMS(par + 1)
           intensity = PARAMS(par + 2)
           sigma1 = PARAMS(par + 3)
           angsig = PARAMS(par + 4)
 
!        Calculate integrated intensity (numerically)
           num_results = num_results + 1
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intensity, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
        Else If (feattype .Eq. 10) Then
 
!        2-D Twin Polar Gaussian peak, (with centre)
           xpolar = PARAMS(par)
           ypolar = PARAMS(par + 1)
           xcenpeak = PARAMS(par + 2)
           ycenpeak = PARAMS(par + 3)
           intensity = PARAMS(par + 4)
           sigma1 = PARAMS(par + 5)
           angsig = PARAMS(par + 6)
           intens2 = PARAMS(par + 7)
 
!        First peak
           num_results = num_results + 1
 
!        Calculate integrated intensity (numerically)
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intensity, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
!        Twin peak
           num_results = num_results + 1
 
!        Calculate integrated intensity (numerically)
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intens2, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
        Else If (feattype .Eq. 8) Then
 
!        2-D Polar Gaussian peak, (no centre)
           xcenpeak = PARAMS(par)
           ycenpeak = PARAMS(par + 1)
           intensity = PARAMS(par + 2)
           sigma1 = PARAMS(par + 3)
           angsig = PARAMS(par + 4)
           intens2 = PARAMS(par + 2)
 
!        First peak
           num_results = num_results + 1
 
!        Calculate integrated intensity (numerically)
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intensity, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
!        Second peak
           num_results = num_results + 1
 
!        Calculate integrated intensity (numerically)
           Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, XAXIS, YAXIS, intens2, xcenpeak, ycenpeak, xpolar, &
             ypolar, sigma1, angsig, WORK, RESULTS(num_results), status)
 
        Else If (feattype .Eq. 50) Then
 
!        Row-line
           numrow = numrow + 1
           low_order = Nint(PARAMS(par))
           high_order = Nint(PARAMS(par + 1))
           xpolar = PARAMS(par + 2)
           ypolar = PARAMS(par + 3)
           xcenline = PARAMS(par + 4)
           ycenline = PARAMS(par + 5)
           rowline_angle = PARAMS(par + 6)
           ppangle = PARAMS(par + 7)
           ppashift = PARAMS(par + 8)
           sigma1 = PARAMS(par + 10)
           angsig = PARAMS(par + 11)
 
!        Calculate relative distances and radial distance from beam centre
           xrel = (xcenline - xpolar) * EXPERIMENT%x_pixel_size
           yrel = (ycenline - ypolar) * EXPERIMENT%y_pixel_size
           radial_distance = Sqrt(xrel**2 + yrel**2)
 
!        Calculate "Up" vector on detector
           x_up = xrel / radial_distance
           y_up = yrel / radial_distance
 
!        Calculate row-line centre radial angle
           rowline_latitude = &
             Atan(radial_distance / EXPERIMENT%detector_distance)
 
!        Calculate row-line integrated intensities
           Do order = low_order, high_order
 
!           Calculate angle along the row-line
              peak_angle = -(Real(order) * ppangle + ppashift)
              delta_latitude = peak_angle * Sin(rowline_angle)
 
!           Calculate peak angles
              peak_longitude = peak_angle * Cos(rowline_angle)
              peak_latitude = rowline_latitude + delta_latitude
 
!           Calculate projection coordinates on detector
              Call F2D_CAL_PROJECTION (EXPERIMENT%detector_distance, &
                EXPERIMENT%x_pixel_size, EXPERIMENT%y_pixel_size, &
                xpolar, ypolar, x_up, y_up, peak_longitude, &
                peak_latitude, xcenpeak, ycenpeak, status)
 
              num_results = num_results + 1
              Write (RESNAMES(num_results),'(''ROW'',i2,''I'',i3)') numrow, &
                order
 
!           Calculate central position of peak
 
              distance = Sqrt((xpolar - xcenpeak)**2 + (ypolar - ycenpeak)**2)
              sigma2 = angsig * distance
!           p1 = Ma_gaussinteg(-distance/sigma1,20.0,status)
!           p2 = Ma_gaussinteg(-pi/angsig,pi/angsig,status)
!           intensity = 2.0*pi*sigma1*sigma2*p1*p2
 
!           RESULTS(num_results) = intensity *
!           :            PARAMS(par+12+order-low_order)
 
!           Calculate integrated intensity (numerically)
              Call F2D_PGAUINT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, XAXIS, YAXIS, PARAMS(par+12+order-low_order), &
                xcenpeak, ycenpeak, xpolar, ypolar, sigma1, angsig, &
                WORK, RESULTS(num_results), status)
 
              RESERRORS(num_results) = 0.0 ! Errors not calculated at present
 
           End Do
 
        End If
 
     End Do
 
!  Free dynamic memory for calculating polar Gaussian function
     Deallocate (WORK)
 
     End Subroutine F2D_LSQFIT2D
!********1*********2*********3*********4*********5*********6*********7**
 
 

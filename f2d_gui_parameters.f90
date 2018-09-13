!********1*********2*********3*********4*********5*********6*********7**
 
!  **************************
!  *                        *
!  * f2d_gui_parameters.f90 *
!  *                        *
!  **************************
 
!+ F2D_GUI_PARAMETERS - FIT2D Graphical User Input of fit PARAMETERS
     Subroutine F2D_GUI_PARAMETERS (gui, EXPERIMENT, &
       xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, &
       title, xlabel, ylabel, zlabel, max_parameters, max_results, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, x_order, y_order, xmin_poly, &
       ymin_poly, xmax_poly, ymax_poly, num_features, num_parameters, PARAMS, &
       PARAM_INFO, CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_results, &
       RESNAMES, MODEL, status)
!  Description:
!    Allows the user to input coordinates to specify a 2-D
!    polynomial background using FIT2BACK. A number of 2-D peaks may
!    then be specified using FIT2PEAK, which takes into account the
!    background specified in FIT2BACK. The parameter values obtained
!    are coded into the 'PARAMS' and 'PARAM_INFO' arrays. 'MODEL'
!    returns the initial fit model.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Mar-2006: V0.15 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    17-Apr-1999: V0.14 Option of call from the GUI (Hammersley)
!    25-Nov-1998: V0.13 Add beam centre arguments and use as the
!      default for the symmetry centre (Hammersley)
!    01-Dec-1996: V0.12 Change 2-D fitting polynomial to a Chebyshev
!      form, resulting in argument list changes (Hammersley)
!    16-Nov-1996: V0.11 Separate parameter constraints to a logical
!      array "CONSTRAINTS" (Hammersley)
!    16-Feb-1996: V0.10 Changes to "f2d_mask" (Hammersley)
!    03-Jan-1996: V0.9 Changes for IBM AIX "xlf" compiler (Hammersley)
!    10-Nov-1995: V0.8 Changes to "F2D_MASK" (Hammersley)
!    23-Oct-1995: V0.7 Set "INPUT ROW-line" default to false (Hammersley)
!    20-Jun-1995: V0.6 Convert to GS graphics library (Hammersley)
!    04-Mar-1995: V0.5 Call to "F2D_MASK" changed (Hammersley)
!    28-Feb-1995: V0.4 Change definition of mask elements to single
!      bytes (Hammersley)
!    24-Jan-1995: V0.3 Input pixel sizes from argument list (Hammersley)
!    21-Feb-1993: V0.2 Allow initialisation polynomial to be of a
!      lower order than fit polynomial (Hammersley)
!    25-Jan-1993: V0.1 Original, based on "FIT2GIN" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if called from the GUI
     TYPE(EXPERIMENTAL_DETAILS), Intent(INOUT) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! The end of the defined data region in
!      the X-direction
     Integer, Intent(IN) :: ynumdat ! The end of the defined data region in
!      the Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: max_parameters ! Dimension size of parameter
!      arrays
     Integer, Intent(IN) :: max_results ! Dimension size of results array
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates
!    masked data point (i.e. point not considered in fitting)
!  Export:
     Integer, Intent(OUT) :: x_order ! Order of background polynomial in
!      X-direction
     Integer, Intent(OUT) :: y_order ! Order of background polynomial in
!      Y-direction
     Real, Intent(OUT) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(OUT) :: ymax_poly ! Y-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Integer, Intent(OUT) :: num_features ! Number of features in fit model
     Integer, Intent(OUT) :: num_parameters ! Number of parameters in fit model
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The initial values for
!      the fit parameters
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters) ! Information on
!      each of the parameters of the fit describing, to which feature it
!    belongs, the type of feature and the number of the parameter
!    within the feature.
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
!    8 = 2nd peak maximum
!
!    For a 2-D twin polar Gaussian 1 = X-centre position
!    without centre                2 = Y-centre position
!    3 = Maximum height (intensity)
!    4 = Radial sigma
!    5 = Angular sigma (radians)
!    6 = 2nd peak maximum
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
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat) ! The initial user defined
!      fit model
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.15' ! Version number
     Integer, Parameter :: Maxorders = 121 ! Dimension size of "INTENSITIES"
     Integer, Parameter :: Maxpeak = 20 ! Dimension size of peak
!      information arrays, maximum number of peaks which may be specified
     Integer, Parameter :: Xmaxcoeff = 40 ! SECOND dimension of 2-D
!      polynomial fit coefficient array
     Integer, Parameter :: Ymaxcoeff = 40 ! FIRST dimension of 2-D
!      polynomial fit coefficient array
!  Local Variables:
     Integer :: elem ! Loop varaible for elements
     Integer :: highorder ! Highest order in row-line
     Integer :: loworder ! Lowest order in row-line
     Integer :: numgauss ! Number of 2-D Gaussians
     Integer :: numpeak ! Number of peaks returned
     Integer :: numpgau ! Number of polar Gaussians
     Integer :: numrow ! Number of row-lines in model
     Integer :: numtpgau ! Number of twin polar Gaussians
     Integer :: numtype ! Number of the feature type
!      700 = 2-D polar Gaussian
!      800 = 2-D polar Gaussian, no centre
!      1000 = 2-D twin polar Gaussian
!      1100 = 2-D twin polar Gaussian, no centre
!      2000 = 2-D chebyshev polynomial
!      5000 = "Row"-line of polar Gaussian peaks
     Integer :: order ! Loop variable for Bragg peak orders
     Integer :: parneed ! Number of parameters needed to defined a function
     Integer :: parnum ! Number of parameter in function
     Integer :: peak ! Loop variable for peaks
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical :: cendefined ! .True., if the centre of the polar coordinate 
!      system has been defined
     Logical :: polynomial_defined ! .True., if the fit model contains
!      a polynomial
     Logical :: rowline_defined ! .True., if the fit model contains a row-line
     Real :: disgaucen ! The distance in pixel units from the polar
!      centre to a polar Gaussian peak
     Real :: ppdistance ! Peak to peak distance for row-line peaks
     Real :: pshift ! Distance from row-line centre to zero-order peak
     Real :: ratio ! Ratio of intensities of reflected row-line peaks to
!      those of the the primary row-line
     Real :: rowangle ! Angle (in radians) from row-line centre to
!      symmetry centre vector, and the row-line direction
     Real :: sigradius ! Standard deviation in the radial direction of peaks
     Real :: sigtheta ! Standard deviation in the angular direction of peaks 
!      (radians)
     Real :: xpolcen ! X-centre of polar coordinate system
     Real :: xrowcen ! X-coordinate of centre of row line
     Real :: ypolcen ! Y-centre of polar coordinate system
     Real :: yrowcen ! Y-coordinate of centre of row line
!  Local Arrays:
     Integer :: PEAKTYPE(Maxpeak) ! Type of peak entered :
!      1 = 2-D Gaussian
!      2 = 2-D Lorentzian
!      3 = 2-D polar Gaussian
!      4 = Twin 2-D polar Gaussian
!    Real ADJUSTED(Maxpeak) ! Adjusted maximum peak intensities
     Real :: COEFFICIENTS(Ymaxcoeff, Xmaxcoeff) ! Polynomial background
!      coefficients in Chebyshev form. NOTE: Unusual order of
!      storage of coefficients for efficiency reasons
     Real :: INTENSITIES(-Maxorders:Maxorders) ! Maximum peak intensities
!      for Bragg peaks of a row-line
     Real :: PEAKMAX(Maxpeak) ! Maximum peak intensities
     Real :: XPEAKCEN(Maxpeak) ! X-coordinate of peak centres
     Real :: YPEAKCEN(Maxpeak) ! Y-coordinate of peak centres
     Real :: ORIENT(Maxpeak) ! Rotation angles of first axis of peaks
!      from the X-axis in radians anti-clockwise
     Real :: MAJHWHM(Maxpeak) ! The half-width at half height of the
!      first axis of the peak
     Real :: MINHWHM(Maxpeak) ! The half-width at half height of the
!      first axis of the peak
     Real :: TWINMAX(Maxpeak) ! The maximum intensity of the second
!      peak in a twin pair
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_PARAMETERS ' // Version)
        Return
     End If
 
!  Check subroutine arguments for validity
     If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. xmaxdat) &
       Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GUI_PARAMETERS ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_GUI_PARAMETERS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Output user info
     If (.Not. gui) Then
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('NOTE: The definition of model 2-D ' // &
          'polynomials has been changed to', status)
        Call IO_WRITE ('      Chebyshev form. User input ' // &
          'has also been changed.', status)
        Call IO_WRITE (' ', status)
     End If
 
     Call IO_WRITE ('INFO: As you input model features the ' // &
       'model is built up. The intensity', status)
     Call IO_WRITE ('      values used to initialise ' // &
       'features are based on the data minus the', status)
     Call IO_WRITE ('      current model value. It is ' // &
       'recommended to select the most intense', status)
     Call IO_WRITE ('      features first, followed by the ' // &
       'next intense features, etc. If the', status)
     Call IO_WRITE ('      background is large and is to ' // &
       'fitted with a 2-D polynomial, this', status)
     Call IO_WRITE ('      should be selected first, but ' // &
       'initialised with a low order', status)
     Call IO_WRITE ('      polynomial so not to fit too ' // &
       'much to the other features', status)
     Call IO_WRITE (' ', status)
 
     If (.Not. gui) Then
 
!     Output user info
        Call IO_WRITE ('INFO: Control passed to ' // 'graphics window', &
          status)
 
!     Output image
        Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, Y_AXIS, &
          xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, &
          status)
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Copy data to model
     Call MA_RCOPY (xmaxdat, ymaxdat, DATA, 1, 1, xnumdat, ynumdat, xmaxdat, &
       ymaxdat, MODEL, status)
 
!  By default use the beam centre as the symmetry centre
     xpolcen = EXPERIMENT%x_beam
     ypolcen = EXPERIMENT%y_beam
 
!  Input model feature parameters
     Call F2D_INPUTMODEL (EXPERIMENT, xmaxdat, ymaxdat, &
       xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
       Xmaxcoeff, Ymaxcoeff, Maxpeak, Maxorders, xstrelm, ystrelm, xendelm, &
       yendelm, MASK, MODEL, polynomial_defined, x_order, y_order, xmin_poly, &
       ymin_poly, xmax_poly, ymax_poly, COEFFICIENTS, xpolcen, ypolcen, &
       numpeak, PEAKTYPE, XPEAKCEN, YPEAKCEN, PEAKMAX, ORIENT, MAJHWHM, &
       MINHWHM, TWINMAX, rowline_defined, loworder, highorder, xrowcen, &
       yrowcen, rowangle, ppdistance, pshift, ratio, sigradius, sigtheta, &
       INTENSITIES, status)
 
     If (.Not. gui) Then
 
!     Output user info
        Call GS_FPROMPT ( 1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', status)
        Call GS_UPDATE (status)
 
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Polynomial fitting region
!  Write (*,'(''xmin_poly, ymin_poly, xmax_poly, ymax_poly = '',
!  :    4e12.5)') xmin_poly, ymin_poly, xmax_poly, ymax_poly
 
!  Output coefficients of polynomial
!  Write (MESSAGE, '(''Zero order, First  X = '',2f12.5/
!  :    ''First Y, first XY = '',2f12.5)')
!  :    COEFFICIENTS(1,1), COEFFICIENTS(2,1),
!  :    COEFFICIENTS(1,2), COEFFICIENTS(2,2)
!  Call IO_TEXT (2, MESSAGE, status)
 
!***   Output results
!  Do peak = 1, numpeak
!  Write (*,'(1x,i3,6f10.3)') PEAKTYPE(peak), XPEAKCEN(peak),
!  :      YPEAKCEN(peak), PEAKMAX(peak), ORIENT(peak),
!  :      MAJHWHM(peak), MINHWHM(peak)
!  End Do
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!- - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Convert background and peak information into parameter values and
!  descriptions for return to fitting subroutines
     num_parameters = 0
     num_features = 0
 
     If (polynomial_defined) Then
 
!     Convert background polynomial information
        num_features = 1
        elem = 0
        Do y = 0, y_order
 
           Do x = 0, x_order
              elem = elem + 1
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = COEFFICIENTS(y + 1, x + 1)
              PARAM_INFO(num_parameters) = 10000 * (elem) + 2000 + &
                num_features
              Write (PARNAMES(num_parameters), '(''PCH'',i2,''X'',i2,''Y'')') &
                x, y
 
!           Typical size of change in parameter value
              SCALE_FACTORS(num_parameters) = &
                Max(4.0*Sqrt(Abs(COEFFICIENTS(y+1, x+1))), 0.2)
              CONSTRAINTS(num_parameters) = .False.
           End Do
 
        End Do
 
     End If
 
!  Convert peak information
     num_results = 0
     numgauss = 0
     numpgau = 0
     numtpgau = 0
     numrow = 0
     cendefined = .False.
 
     Do peak = 1, numpeak
 
        If (PEAKTYPE(peak) .Eq. 1) Then
 
!        2-D Gaussian
           If (num_parameters .Le. max_parameters - 6) Then
              num_features = num_features + 1
              numgauss = numgauss + 1
 
!           X-centre parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = XPEAKCEN(peak)
              PARAM_INFO(num_parameters) = 10000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'', i2, '' XCe'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Y-centre parameter
              num_parameters = num_parameters+1
              PARAMS(num_parameters) = YPEAKCEN(peak)
              PARAM_INFO(num_parameters) = 20000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'',i2, '' YCe'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Maximum intensity parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = PEAKMAX(peak)
              PARAM_INFO(num_parameters) = 30000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'', i2, '' MAX'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = Max(2.0,Sqrt(PEAKMAX(peak))/2.0)
              CONSTRAINTS(num_parameters) = .False.
 
!           First axis standard deviation
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = MAJHWHM(peak) * 2.0 / 2.354
              PARAM_INFO(num_parameters) = 40000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'', i2, '' 1SD'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Second axis standard deviation
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = MINHWHM(peak) * 2.0 / 2.354
              PARAM_INFO(num_parameters) = 50000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'', i2, '' 2SD'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Orientation of peak
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = ORIENT(peak)
              PARAM_INFO(num_parameters) = 60000 + 600 + num_features
              Write (PARNAMES(num_parameters), '(''GAU'', i2, '' ANG'')') &
                numgauss
              SCALE_FACTORS(num_parameters) = 0.17477 ! (10 degrees)
              CONSTRAINTS(num_parameters) = .False.
 
!           Integrated intensity result
              num_results = num_results + 1
              Write (RESNAMES(num_results),'(''GAU'', i2, '' INT'')') numgauss
 
           Else
 
!           Not enough room in array for parameter
              Call IO_WRITE ('WARNING: Parameter array ' // &
                'full, not enough room to store feature ' // 'parameters', &
                status)
 
           End If
 
        Else If (PEAKTYPE(peak) .Eq. 3) Then
 
!        Polar Gaussian
           If (cendefined) Then
              parneed = 5
              numtype = 800
           Else
              parneed = 7
              numtype = 700
           End If
 
           If (num_parameters .Le. max_parameters - parneed) Then
              num_features = num_features + 1
              numpgau = numpgau + 1
 
              If (.Not. cendefined) Then
 
!              Polar coordinate system X-centre parameter
                 num_parameters = num_parameters + 1
                 PARAMS(num_parameters) = xpolcen
                 PARAM_INFO(num_parameters) = 10000 + numtype + num_features
                 Write (PARNAMES(num_parameters), '(''X-CEN SYM'')')
                 SCALE_FACTORS(num_parameters) = 1.0
                 CONSTRAINTS(num_parameters) = .False.
 
!              Polar coordinate system Y-centre parameter
                 num_parameters = num_parameters + 1
                 PARAMS(num_parameters) = ypolcen
                 PARAM_INFO(num_parameters) = 20000 + numtype + num_features
                 Write (PARNAMES(num_parameters), '(''Y-CEN SYM'')')
                 SCALE_FACTORS(num_parameters) = 1.0
                 CONSTRAINTS(num_parameters) = .False.
 
                 cendefined = .True.
                 parnum = 3
              Else
                 parnum = 1
              End If
 
!           X-centre parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = XPEAKCEN(peak)
              PARAM_INFO(num_parameters) = (10000) * parnum + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''PGa'',i2, '' XCe'')') &
                numpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Y-centre parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = YPEAKCEN(peak)
              PARAM_INFO(num_parameters) = 10000*(parnum+1) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''PGa'',i2, '' YCe'')') &
                numpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Maximum intensity parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = PEAKMAX(peak)
              PARAM_INFO(num_parameters) = 10000*(parnum+2) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''PGa'', i2, '' MAX'')') &
                numpgau
              SCALE_FACTORS(num_parameters) = Max(5.0,Sqrt(PEAKMAX(peak))/2.0)
              CONSTRAINTS(num_parameters) = .False.
 
!           Radial standard deviation
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = MAJHWHM(peak) * 2.0 / 2.354
              PARAM_INFO(num_parameters) = 10000*(parnum+3) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''PGa'', i2, '' RAD'')') &
                numpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Angular standard deviation
              num_parameters = num_parameters + 1
              disgaucen = Sqrt((XPEAKCEN(peak)-xpolcen)**2+ &
                (YPEAKCEN(peak)-ypolcen)**2)
              PARAMS(num_parameters) = (MINHWHM(peak)*2.0/2.354)/disgaucen
              PARAM_INFO(num_parameters) = 10000*(parnum+4) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''PGa'', i2, '' ANG'')') &
                numpgau
              SCALE_FACTORS(num_parameters) = 0.17477 ! (10 degrees)
              CONSTRAINTS(num_parameters) = .False.
 
!           Integrated intensity result
              num_results = num_results + 1
              Write (RESNAMES(num_results),'(''PGa'', i2, '' INT'')') numpgau
 
           Else
 
!           Not enough room in array for parameter
              Call IO_WRITE ('WARNING: Parameter array ' // &
                'full, not enough room to store feature ' // 'parameters', &
                status)
 
           End If
 
        Else If (PEAKTYPE(peak) .Eq. 4) Then
 
!        Twin Polar Gaussian
           If (cendefined) Then
              parneed = 6
              numtype = 1100
           Else
              parneed = 8
              numtype = 1000
           End If
 
           If (num_parameters .Le. max_parameters - parneed) Then
              num_features = num_features + 1
              numtpgau = numtpgau + 1
 
              If (.Not. cendefined) Then
 
!              Polar coordinate system X-centre parameter
                 num_parameters = num_parameters + 1
                 PARAMS(num_parameters) = xpolcen
                 PARAM_INFO(num_parameters) = 10000 + numtype + num_features
                 Write (PARNAMES(num_parameters), '(''X-CEN SYM'')')
                 SCALE_FACTORS(num_parameters) = 1.0
                 CONSTRAINTS(num_parameters) = .False.
 
!              Polar coordinate system Y-centre parameter
                 num_parameters = num_parameters + 1
                 PARAMS(num_parameters) = ypolcen
                 PARAM_INFO(num_parameters) = 20000 + numtype + num_features
                 Write (PARNAMES(num_parameters), '(''Y-CEN SYM'')')
                 SCALE_FACTORS(num_parameters) = 1.0
                 CONSTRAINTS(num_parameters) = .False.
 
                 cendefined = .True.
                 parnum = 3
              Else
                 parnum = 1
              End If
 
!           X-centre parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = XPEAKCEN(peak)
              PARAM_INFO(num_parameters) = 10000 * parnum + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'', i2, '' XCe'')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Y-centre parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = YPEAKCEN(peak)
              PARAM_INFO(num_parameters) = 10000 * (parnum + 1) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'', i2, '' YCe'')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Maximum intensity parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = PEAKMAX(peak)
              PARAM_INFO(num_parameters) = 10000 * (parnum + 2) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'', i2, '' MAX'')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = Max(5.0,Sqrt(PEAKMAX(peak))/2.0)
              CONSTRAINTS(num_parameters) = .False.
 
!           Radial standard deviation
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = MAJHWHM(peak) * 2.0 / 2.354
              PARAM_INFO(num_parameters) = 10000*(parnum+3) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'',i2, '' RAD'')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = 1.0
              CONSTRAINTS(num_parameters) = .False.
 
!           Angular standard deviation
              num_parameters = num_parameters + 1
              disgaucen = Sqrt((XPEAKCEN(peak)-xpolcen)**2 + &
                (YPEAKCEN(peak)-ypolcen)**2)
              PARAMS(num_parameters) = (MINHWHM(peak)*2.0/2.354)/disgaucen
              PARAM_INFO(num_parameters) = 10000*(parnum+4) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'', i2, '' ANG'')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = 0.17477 ! (10 degrees)
              CONSTRAINTS(num_parameters) = .False.
 
!           Twin peak maximum parameter
              num_parameters = num_parameters + 1
              PARAMS(num_parameters) = TWINMAX(peak)
              PARAM_INFO(num_parameters) = 10000*(parnum+5) + numtype + &
                num_features
              Write (PARNAMES(num_parameters), '(''TPG'', i2, '' MX2 '')') &
                numtpgau
              SCALE_FACTORS(num_parameters) = Max(5.0,Sqrt(TWINMAX(peak))/2.0)
              CONSTRAINTS(num_parameters) = .False.
 
!           First peak integrated intensity result
              num_results = num_results + 1
              Write (RESNAMES(num_results),'(''TPG'', i2, '' In1'')') numtpgau
 
!           Second peak integrated intensity result
              num_results = num_results + 1
              Write (RESNAMES(num_results),'(''TPG'', i2, '' In2'')') numtpgau
 
           Else
 
!           Not enough room in array for parameter
              Call IO_WRITE ('WARNING: Parameter array ' // &
                'full, not enough room to store feature ' // 'parameters', &
                status)
 
           End If
 
        End If
 
     End Do
 
!  Convert row-line information if necessary
     If (rowline_defined) Then
 
        num_features = num_features + 1
        numrow = numrow + 1
 
!     Low order
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = Real(loworder)
        PARAM_INFO(num_parameters) = 1000000 + 10000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' LOW '')') numrow
        SCALE_FACTORS(num_parameters) = 0.0
        CONSTRAINTS(num_parameters) = .True.
 
!     High order
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = Real(highorder)
        PARAM_INFO(num_parameters) = 1000000 + 20000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' HIG '')') numrow
        SCALE_FACTORS(num_parameters) = 0.0
        CONSTRAINTS(num_parameters) = .True.
 
!     X-centre, polar symmetry
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = xpolcen
        PARAM_INFO(num_parameters) = 20000000 + 30000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''X-CEN SYM'')')
        SCALE_FACTORS(num_parameters) = 1.0
        CONSTRAINTS(num_parameters) = .False.
 
!     Y-centre, polar symmetry
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = ypolcen
        PARAM_INFO(num_parameters) = 20000000 + 40000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''Y-CEN SYM'')')
        SCALE_FACTORS(num_parameters) = 1.0
        CONSTRAINTS(num_parameters) = .False.
 
!     X-centre parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = xrowcen
        PARAM_INFO(num_parameters) = 20000000 + 50000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' XCe'')') numrow
        SCALE_FACTORS(num_parameters) = 1.0
        CONSTRAINTS(num_parameters) = .False.
 
!     Y-centre parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = yrowcen
        PARAM_INFO(num_parameters) = 20000000 + 60000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' YCe'')') numrow
        SCALE_FACTORS(num_parameters) = 1.0
        CONSTRAINTS(num_parameters) = .False.
 
!     Row angle parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = rowangle
        PARAM_INFO(num_parameters) = 50000000 + 70000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' ANG'')') numrow
        SCALE_FACTORS(num_parameters) = 0.017477 ! (1 degree)
        CONSTRAINTS(num_parameters) = .False.
 
!     Peak to peak angle parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = ppdistance
        PARAM_INFO(num_parameters) = 10000000 + 80000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' PPa'')') numrow
        SCALE_FACTORS(num_parameters) = 0.0017477 ! (0.1 degree)
        CONSTRAINTS(num_parameters) = .False.
 
!     Zero-order angle shift parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = pshift
        PARAM_INFO(num_parameters) = 10000000 + 90000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' PSh'')') numrow
        SCALE_FACTORS(num_parameters) = 0.0017477 ! (0.1 degree)
        CONSTRAINTS(num_parameters) = .False.
 
!     Reflected intensity ratio parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = ratio
        PARAM_INFO(num_parameters) = 100000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' Rat'')') numrow
        SCALE_FACTORS(num_parameters) = 0.1
        CONSTRAINTS(num_parameters) = .False.
 
!     Radial standard deviation width parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = sigradius
        PARAM_INFO(num_parameters) = 10000000 + 110000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' SDR'')') numrow
        SCALE_FACTORS(num_parameters) = 1.0
        CONSTRAINTS(num_parameters) = .False.
 
!     Angular standard deviation width parameter
        num_parameters = num_parameters + 1
        PARAMS(num_parameters) = sigtheta
        PARAM_INFO(num_parameters) = 50000000 + 120000 + 5000 + num_features
        Write (PARNAMES(num_parameters),'(''ROW'', i2, '' SDA'')') numrow
        SCALE_FACTORS(num_parameters) = 0.0017477 ! (0.1 degree)
        CONSTRAINTS(num_parameters) = .False.
 
!     Peak order maximum intensity parameters
        Do order = loworder, highorder
           num_parameters = num_parameters + 1
           PARAMS(num_parameters) = INTENSITIES(order)
           PARAM_INFO(num_parameters) = (13+order-loworder)*10000 + 5000 + &
             num_features
           Write (PARNAMES(num_parameters),'(''ROW'',i2, ''I'', i3)') numrow, &
             order
           SCALE_FACTORS(num_parameters) = &
             Max(5.0,Sqrt(INTENSITIES(order))/2.0)
           CONSTRAINTS(num_parameters) = .False.
 
        End Do
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!    Write (*,'(1x,''num_parameters = '',i3)') num_parameters
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_GUI_PARAMETERS
!********1*********2*********3*********4*********5*********6*********7**
 
 
 

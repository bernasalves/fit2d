!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_1dfeature.f90 *
!  *                   *
!  *********************
 
!+ F2D_1DFEATURE - FIT 2-D 1-D FEATURE calculation
     Subroutine F2D_1DFEATURE (pixel_size, feature, max_parameters, PARAMS, &
       PARAM_INFO, num_parameters, maxdat, strelm, endelm, poly_order, &
       min_poly, max_poly, AXIS, DATA, status)
!  Description:
!    Inputs parameters of a multi-feature function through the values
!    storaged in "PARAMS" using the description array "PARAM_INFO" to find
!    out how to handle the parameters. The function number "feature"
!    is then added to the array "DATA" as defined by "strelm" and "endelm".
!  Keywords:
!    Calculate.Fit.1-D~Feature, 1-D~Feature.Calculation
!  Method:
!    Decodes "PARAM_INFO" to find the parameters corresponding to the
!    required feature and creates that feature according to the
!    feature type defined by the first parameter of the feature.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Jan-1997: V0.3 Add exponential decay and sinusoidal functions 
!      (Hammersley)
!    14-Dec-1996: V0.2 Correct problem with Lorentzian, also
!      adding a Voigtian (Hammersley)
!    05-Dec-1996: V0.1 Original, based on "f2d_feature" (Hammersley)
!  Modules:
     Use IO_LIB
!  Use MA_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Real, Intent(IN) :: pixel_size ! Size of one pixel (metres)
     Integer, Intent(IN) :: feature ! Loop variable for the adding of the
!      features
     Integer, Intent(IN) :: max_parameters ! Dimension size of "PARAMS" and
!      "PARAM_INFO" arrays
     Real, Intent(IN) :: PARAMS(max_parameters) ! Total parameter array
!      (Variable and constrained)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
!      of the parameters of the fit describing, to which feature it belongs,
!      the type of feature and the number of the parameter within the
!      feature, and whether the parameter is constrained or free to be
!      varied.
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
!    12 = Pseudo-Voigt (1-D)
!    13 = Exponential decay function (1-D)
!    20 = 2-D Chebyshev polynomial
!    50 = "Row" lines of polar Gaussians
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
!    For a Pseudo-Voigtian :  1 = Centre position
!    2 = Maximum height (intensity)
!    3 = FWHM (peak)
!    4 = Mixing fraction( Lorentzian)
!
!    For an Exponential     : 1 = Zero position
!    Decay Function           2 = Maximum or zero intensity
!    3 = Half-life (distance)
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
!    13 = Lowest order peak maximum intensity
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
     Integer, Intent(IN) :: num_parameters ! Total number of parameters
!      (variable and constrained) in the model
     Integer, Intent(IN) :: maxdat ! Dimension size for DATA
     Integer, Intent(IN) :: strelm ! First element of region for calculating
!      function
     Integer, Intent(IN) :: endelm ! End element of region for calculating
!      function
     Integer, Intent(IN) :: poly_order ! Order of polynomial
     Real, Intent(IN) :: min_poly ! Coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: max_poly ! Coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: AXIS(maxdat) ! World coordinate values for pixels
!  Import/Export:
     Real, Intent(INOUT) :: DATA(maxdat) ! The array to have the function
!      added to it
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Maxipar = 100 ! Dimension size for FEATPAR
!  Local Variables:
     Integer :: feattype ! Type of feature to be added:
!       1 = Normal polynomial
!       2 = Gaussian
!       3 = Lorentzian
!       4 = Voigtian
!       5 = Chebyshev polynomial
!       6 = 2-D Gaussian
!       7 = 2-D polar Gaussian
!       8 = 2-D polar Gaussian, no centre
!       9 = 2-D polynomial (normal)
!      10 = Twin 2-D polar Gaussian
!      11 = Twin 2-D polar Gaussian, no centre
!      12 = Pseudo-Voigt (1-D)
!      13 = Exponential decay function (1-D)
!      14 = Sinusoidal function
!      20 = 2-D Chebyshev polynomial
!      50 = "Row" line
     Integer :: numpar ! Number of parameters in feature
     Integer :: par ! Loop variable for parameters
     Real :: amplitude ! Amplide of trig function
     Real :: half_life ! Half life of exponential decay
     Real :: hwhm ! The standard deviation of the first axis of the
!      Lorentzian (pixels)
     Real :: peak_centre ! Centre coordinate of a peak
     Real :: peak_max ! Maximum intensity of a peak
     Real :: period ! Period of trig function
     Real :: sigma ! Standard deviation width of a Gaussian peak
!  Local Arrays:
     Real :: FEATPAR(Maxipar) ! Array to hold parameters of the feature to be 
!      created; in their correct order
!  External Functions:
     Integer, External :: Ma_exdigit ! Extract digit from an integer number
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(1x,''feature = '',i6)') feature
!  Write (*,'(1x,''max_parameters = '',i6)') max_parameters
!  Do par = 1, num_parameters
!  Write (*,'(1x,''PARAMS('',i2,'') = '',f)') par, PARAMS(par)
!  End Do
 
!  Do par = 1, num_parameters
!  Write (*,'(1x,''PARAM_INFO('',i2,'') = '',i)')
!  par, PARAM_INFO(par)
!  End Do
 
!  Write (*,'(1x,''num_parameters = '',i6)') num_parameters
!  Write (*,'(1x,''xmaxdat = '',i6)') xmaxdat
!  Write (*,'(1x,''ymaxdat = '',i6)') ymaxdat
!  Write (*,'(1x,''xstrelm = '',i6)') xstrelm
!  Write (*,'(1x,''ystrelm = '',i6)') ystrelm
!  Write (*,'(1x,''xendelm = '',i6)') xendelm
!  Write (*,'(1x,''yendelm = '',i6)') yendelm
!  Write (*,'(1x,''status = '',i8)') status
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DFEATURE ' // Version)
        Return
     End If
 
!  Initialise variables
     numpar = 0
 
!  Search parameter array for all the parameters for the required
!  numbered feature
     Do par = 1, num_parameters
 
        If (Mod(PARAM_INFO(par),100) .Eq. feature) Then
           FEATPAR(Ma_exdigit(PARAM_INFO(par), 6, status) * 10 + &
             Ma_exdigit(PARAM_INFO(par), 5, status)) = PARAMS(par)
           numpar = numpar + 1
 
           If (Ma_exdigit(PARAM_INFO(par), 6, status) * 10 + &
             Ma_exdigit(PARAM_INFO(par), 5, status) .Eq. 1) Then
              feattype = Ma_exdigit(PARAM_INFO(par), 4, status) * 10 &
                + Ma_exdigit(PARAM_INFO(par), 3, status)
           End If
 
        End If
 
     End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(1x,''feattype = '',i6)') feattype
!  Write (*,'(1x,''numpar = '',i6)') numpar
!  Do par = 1, numpar
!  Write (*,'(1x,''FEATPAR('',i2,'') = '',1pe12.5)')
!  :      par, FEATPAR(par)
!  End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Add appropriate feature
     If (feattype .Eq. 2) Then
 
!     Gaussian
        peak_centre = FEATPAR(1)
        peak_max = Max(FEATPAR(2), 1.0e-37)
        sigma = Max(FEATPAR(3), 1.0e-37)
 
!     Add a Gaussian peak to the model
        Call MA_1DGAUSSIAN (maxdat, strelm, endelm, peak_max, peak_centre, &
          sigma, 5.0, 1, DATA, status)
 
     Else If (feattype .Eq. 3) Then
 
!     Lorentzian
        peak_centre = FEATPAR(1)
        peak_max = Max(FEATPAR(2), 1.0e-37)
        hwhm = Max(FEATPAR(3), 1.0e-37)
        Call MA_1DLORENTZIAN (maxdat, strelm, endelm, peak_max, peak_centre, &
          hwhm, 1, DATA, status)
 
     Else If (feattype .Eq. 4) Then
 
!     Voigtian
        peak_centre = FEATPAR(1)
        peak_max = Max(FEATPAR(2), 1.0e-37)
        sigma = Max(FEATPAR(3), 1.0e-37)
        hwhm = Max(FEATPAR(4), 1.0e-37)
        Call MA_1DVOIGTIAN (maxdat, strelm, endelm, peak_max, peak_centre, &
          sigma, hwhm, 0, DATA, status)
 
     Else If (feattype .Eq. 5) Then
 
!     Chebyshev polynomial
        Call MA_CAL_POLYNOMIAL (.True., min_poly, max_poly, Maxipar, &
          poly_order, FEATPAR, endelm - strelm + 1, endelm - strelm + 1, &
          AXIS(strelm), DATA(strelm), status)
 
     Else If (feattype .Eq. 13) Then
 
!     Exponential decay function
        peak_centre = FEATPAR(1)
        peak_max = Max(FEATPAR(2), 1.0e-37)
        half_life = Max(FEATPAR(3), 1.0e-37)
        Call MA_1DEXPDECAY (maxdat, strelm, endelm, peak_max, peak_centre, &
          half_life, 0, DATA, status)
 
     Else If (feattype .Eq. 14) Then
 
!     Sinusoidal function
        peak_centre = FEATPAR(1)
        amplitude = Max(FEATPAR(2), 1.0e-37)
        period = Max(FEATPAR(3), 1.0e-37)
        Call MA_1DSINUSOIDAL (maxdat, strelm, endelm, amplitude, peak_centre, &
          period, 0, DATA, status)
 
     End If
 
     End Subroutine F2D_1DFEATURE
!********1*********2*********3*********4*********5*********6*********7*********8
 

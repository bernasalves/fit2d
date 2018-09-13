!********1*********2*********3*********4*********5*********6*********7**
 
!  *******************
!  *                 *
!  * f2d_feature.f90 *
!  *                 *
!  *******************
 
!+ F2D_FEATURE - FIT 2-D FEATURE calculation
     Subroutine F2D_FEATURE (EXPERIMENT, feature, &
       max_parameters, PARAMS, PARAM_INFO, num_parameters, xmaxdat, ymaxdat, &
       xstrelm, ystrelm, xendelm, yendelm, x_order, y_order, xmin_poly, &
       ymin_poly, xmax_poly, ymax_poly, XAXIS, YAXIS, DATA, status)
!  Description:
!    Inputs parameters of a multi-feature function through the values
!    storaged in "PARAMS" using the description array "PARAM_INFO" to find
!    out how to handle the parameters. The function number "feature"
!    is then added to the array "DATA" as defined by "(xstrelm, ystrelm)"
!    and "(xendelm, yendelm)".
!  Keywords:
!    Calculate.Fit.Feature, Feature.Calculation
!  Method:
!    Decodes "PARAM_INFO" to find the parameters corresponding to the
!    required feature and creates that feature according to the
!    feature type defined by the first parameter of the feature.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.8 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    01-Dec-1996: V0.6 Change 2-D fitting polynomial to a Chebyshev
!      polynomial, thus changes to argument list (Hammersley)
!    16-Nov-1996: V0.5 Make variable names consistent with rest of
!      "FIT" sub-menu (Hammersley)
!    24-Jan-1995: V0.4 Input pixel sizes from arguments (Hammersley)
!    13-Sep-1993: V0.3 Allow row-line peak intensities to go negative 
!      (Hammersley)
!    21-Feb-1993: V0.2 Check order of polynomial and calculate
!      lowest order possible (Hammersley)
!    04-Feb-1993: V0.1 Original, based on "FIT2FEAT" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: feature ! Loop variable for the adding of the
!       features
     Integer, Intent(IN) :: max_parameters ! Dimension size of "PARAMS" and
!      "PARAM_INFO" arrays
     Real, Intent(IN) :: PARAMS(max_parameters) ! Total parameter array
!      (Variable and constrained)
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
!      of the parameters of the fit describing, to which feature it belongs,
!    the type of feature and the number of the parameter within the
!    feature, and whether the parameter is constrained or free to be
!    varied.
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
!    14 = Trigonometric function
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
     Integer, Intent(IN) :: xmaxdat ! X Dimension size for DATA
     Integer, Intent(IN) :: ymaxdat ! Y Dimension size for DATA
     Integer, Intent(IN) :: xstrelm ! First X-element of region for
!      calculating function
     Integer, Intent(IN) :: ystrelm ! First Y-element of region for
!      calculating function
     Integer, Intent(IN) :: xendelm ! End X-element of region for
!      calculating function
     Integer, Intent(IN) :: yendelm ! End Y-element of region for
!      calculating function
     Integer, Intent(IN) :: x_order ! Order of polynomial in X-direction
     Integer, Intent(IN) :: y_order ! Order of polynomial in Y-direction
     Real, Intent(IN) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymax_poly ! Y-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: XAXIS(xmaxdat) ! World coordiante values for
!      X-direction pixels
     Real, Intent(IN) :: YAXIS(ymaxdat) ! World coordiante values for
!      Y-direction pixels
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The array to have the
!      function added to it
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Maxipar = 200 ! Dimension size for FEATPAR
     Integer, Parameter :: Maxorders = 100 ! Dimension size for "INTENSITES"
     Integer, Parameter :: Xmaxcoeff = 40 ! X-dimension of COEFFS
     Integer, Parameter :: Ymaxcoeff = 40 ! Y-dimension of COEFFS
!  Local Variables:
     Integer :: elem ! Loop variable for elements
     Integer :: feattype ! Type of feature to be added :
!      1 = Normal polynomial
!      2 = Gaussian
!      3 = Lorentzian
!      4 = Voigt
!      5 = Chebyshev polynomial
!      6 = 2-D Gaussian
!      7 = 2-D polar Gaussian
!      8 = 2-D polar Gaussian, no centre
!      9 = 2-D polynomial (normal)
!      10 = Twin 2-D polar Gaussian
!      11 = Twin 2-D polar Gaussian, no centre
!      12 = Pseudo-Voigt (1-D)
!      13 = Exponential decay function (1-D)
!      20 = 2-D Chebyshev polynomial
!      50 = "Row" line
     Integer :: highorder ! Highest order in row-line
     Integer :: loworder ! Lowest order in row-line
     Integer :: numpar ! Number of parameters in feature
     Integer :: order ! Loop variable for peak orders
     Integer :: par ! Loop variable for parameters
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
     Real :: intensity ! Maximum intensity of peak
     Real :: inten2 ! Maximum intensity of twin peak
     Real :: ppdistance ! Peak to peak spacing of row-ine peaks
     Real :: pshift ! Distance from row-line centre to zero-order peak
     Real :: ratio ! Ratio of reflected row-line peak intensities to
!      those of the primary row-line
     Real :: rotation ! Angle of rotation in radians from the X-axis
!      anti-clockwise to the first axis of the peak
     Real :: rowangle ! Angle from row-line centre to polar symmetry
!      centre vector, to the row-line direction
     Real :: sigang ! Angular sigma in radians
     Real :: sigrad ! Radial sigma in pixel units
     Real :: sigma1 ! Sigma for first axis in pixel units
     Real :: sigma2 ! Sigma for second axis in pixel units
     Real :: xpeakcen ! X-coordinate of centre of peak
     Real, Save :: xpolcen ! X-coordinate of centre of polar coordinate system
     Real :: xrowcen ! X-coordinate of row-line centre
     Real :: xtwincen ! The X-centre of the second peak in a twin pair
     Real :: ypeakcen ! Y-coordinate of centre of peak
     Real, Save :: ypolcen ! Y-coordinate of centre of polar coordinate system
     Real :: yrowcen ! Y-coordinate of row-line centre
     Real :: ytwincen ! The Y-centre of the second peak in a twin pair
!  Local Arrays:
     Real :: COEFFS(Ymaxcoeff, Xmaxcoeff) ! Polynomial coefficient array
     Real, Allocatable :: COEFFS_1D(:) ! Dynamic work array
     Real, Allocatable :: X_COEFFS(:) ! Dynamic work array
     Real :: FEATPAR(Maxipar) ! Array to hold parameters of the feature to be
!      created; in their correct order
     Real :: INTENSITIES(-Maxorders: Maxorders) ! Intensities of primary
!      row-line peaks
!  External Functions:
     Integer, External :: Ma_exdigit ! Extract digit from an integer number
!--------1---------2---------3---------4---------5---------6---------7--
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
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FEATURE ' // Version)
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
              feattype = Ma_exdigit(PARAM_INFO(par), 4, status) * 10 + &
                Ma_exdigit(PARAM_INFO(par), 3, status)
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Add appropriate feature
     If (feattype .Eq. 6) Then
 
!     2-D Gaussian
        xpeakcen = FEATPAR(1)
        ypeakcen = FEATPAR(2)
        intensity = Max(FEATPAR(3), 1.0e-37)
        sigma1 = Max(FEATPAR(4), 1.0e-37)
        sigma2 = Max(FEATPAR(5), 1.0e-37)
        rotation = FEATPAR(6)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*,'(1x,''2-D Gaussian Parameters'')')
!     Write (*,'(1x,''X/Y Centre = '',2(1x,1pe12.5))')
!     :      xcentre,ycentre
!     Write (*,'(1x,''Intensity = '',1pe12.5)') intensity
!     Write (*,'(1x,''sigma 1, sigma 2 = '',2(1x,1pe12.5))')
!     :      sigma1,sigma2
!     Write (*,'(1x,''rotation = '',1pe12.5)') rotation
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Call MA_2DGAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xpeakcen, ypeakcen, sigma1, &
          sigma2, rotation, 4.0, 1, DATA, status)
 
     Else If (feattype .Eq. 7) Then
 
!     2-D Polar Gaussian
        xpolcen = FEATPAR(1)
        ypolcen = FEATPAR(2)
        xpeakcen = FEATPAR(3)
        ypeakcen = FEATPAR(4)
        intensity = FEATPAR(5)
        sigrad = FEATPAR(6)
        sigang = FEATPAR(7)
 
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xpeakcen, ypeakcen, xpolcen, &
          ypolcen, sigrad, sigang, 4.0, 1, DATA, status)
 
     Else If (feattype .Eq. 8) Then
 
!     2-D Polar Gaussian using existing centre of polar coordinates
        xpeakcen = FEATPAR(1)
        ypeakcen = FEATPAR(2)
        intensity = FEATPAR(3)
        sigrad = FEATPAR(4)
        sigang = FEATPAR(5)
 
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xpeakcen, ypeakcen, xpolcen, &
          ypolcen, sigrad, sigang, 4.0, 1, DATA, status)
 
     Else If (feattype .Eq. 20) Then
 
!     2-D Chebyshev polynomial
        If (x_order .Lt. Xmaxcoeff .And. y_order .Lt. Ymaxcoeff) Then
           elem = 0
           Do y = 0, y_order
 
              Do x = 0, x_order
                 elem = elem + 1
                 COEFFS(y + 1, x + 1) = FEATPAR(elem)
              End Do
 
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*,'(1x,''2-D Polynomial Coefficients'')')
!        Write (*,'(4(1x,1pe12.5))') COEFFS(1,1),COEFFS(1,2),
!        :        COEFFS(2,1),COEFFS(2,2)
!        Write (*,'(''COEFFS(1,1) = '', g)') COEFFS(1,1)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Allocate work space
           Allocate (X_COEFFS(x_order + 1), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FEATURE ' // Version)
              Return
           End If
           Allocate (COEFFS_1D((x_order + 1) * (yendelm - ystrelm + 1)), &
             Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_FEATURE ' // Version)
              Return
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Polynomial fitting region
!        Write (*,'(
!        :          ''xmin_poly, ymin_poly, xmax_poly, ymax_poly = '',
!        :          4e12.5)') xmin_poly, ymin_poly, xmax_poly, ymax_poly
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Add 2-D polynomial to model
           Call MA_CAL_2DCHEBYSHEV (.True., xmin_poly, ymin_poly, xmax_poly, &
             ymax_poly, Xmaxcoeff, Ymaxcoeff, COEFFS, x_order, y_order, &
             xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, x_order + &
             1, yendelm - ystrelm + 1, XAXIS, YAXIS, DATA, COEFFS_1D, &
             X_COEFFS, status)
 
!        Free temporary work space
           Deallocate (COEFFS_1D)
           Deallocate (X_COEFFS)
 
        Else
 
!        Order of polynomial is to high for internal arrays
           Call ST_SAVE ('Subroutine F2D_FEATURE ' // Version)
           If (x_order .Ge. Xmaxcoeff) Then
              status = St_mod_fit2d + St_bad_lim1
           Else
              status = St_mod_fit2d + St_bad_lim2
           End If
 
           Return
 
        End If
 
     Else If (feattype .Eq. 10) Then
 
!     Twin 2-D Polar Gaussian
        xpolcen = FEATPAR(1)
        ypolcen = FEATPAR(2)
        xpeakcen = FEATPAR(3)
        ypeakcen = FEATPAR(4)
        intensity = FEATPAR(5)
        sigrad = FEATPAR(6)
        sigang = FEATPAR(7)
        inten2 = FEATPAR(8)
 
!     Add first peak
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xpeakcen, ypeakcen, xpolcen, &
          ypolcen, sigrad, sigang, 4.0, 1, DATA, status)
 
!     Add second peak
        xtwincen = 2.0 * xpolcen - xpeakcen
        ytwincen = 2.0 * ypolcen - ypeakcen
 
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, inten2, xtwincen, ytwincen, xpolcen, ypolcen, &
          sigrad, sigang, 4.0, 1, DATA, status)
 
     Else If (feattype .Eq. 11) Then
 
!     Twin 2-D Polar Gaussian, no centre
        xpeakcen = FEATPAR(1)
        ypeakcen = FEATPAR(2)
        intensity = FEATPAR(3)
        sigrad = FEATPAR(4)
        sigang = FEATPAR(5)
        inten2 = FEATPAR(6)
 
!     Add first peak
        Call MA_2DPOLARGAU  (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xpeakcen, ypeakcen, xpolcen, &
          ypolcen, sigrad, sigang, 4.0, 1, DATA, status)
 
!     Add second peak
        xtwincen = 2.0 * xpolcen - xpeakcen
        ytwincen = 2.0 * ypolcen - ypeakcen
 
        Call MA_2DPOLARGAU (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, inten2, xtwincen, ytwincen, xpolcen, ypolcen, &
          sigrad, sigang, 4.0, 1, DATA, status)
 
     Else If (feattype .Eq. 50) Then
 
!     "Row" line of polar Gaussian peaks
        loworder = Nint(FEATPAR(1))
        highorder = Nint(FEATPAR(2))
        xpolcen = FEATPAR(3)
        ypolcen = FEATPAR(4)
        xrowcen = FEATPAR(5)
        yrowcen = FEATPAR(6)
        rowangle = FEATPAR(7)
        ppdistance = FEATPAR(8)
        pshift = FEATPAR(9)
        ratio = FEATPAR(10)
        sigrad = Max(FEATPAR(11),0.0)
        sigang = Max(FEATPAR(12),0.0)
 
!     Set peak intensities
        Do order = loworder, highorder
 
!**MODIFICATION***MODIFICATION***MODIFICATION***MODIFICATION***MODIFICAT
!        OLD          INTENSITIES(order) = Max(FEATPAR(13+order-loworder),0.0)
!        NEW
!        Allow negative intensities
           INTENSITIES(order) = FEATPAR(13 + order - loworder)
 
!**MODIFICATION***MODIFICATION***MODIFICATION***MODIFICATION***MODIFICAT
 
        End Do
 
!     Create row line
        Call F2D_ROWLINE (EXPERIMENT, xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, 1, Maxorders, loworder, &
          highorder, xpolcen, ypolcen, xrowcen, yrowcen, rowangle, ppdistance, &
          pshift, ratio, sigrad, sigang, INTENSITIES, XAXIS, YAXIS, DATA, &
          status)
 
     End If
 
     End Subroutine F2D_FEATURE
!********1*********2*********3*********4*********5*********6*********7**

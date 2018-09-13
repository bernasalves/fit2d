!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_savemodel.f90 *
!  *                   *
!  *********************
 
!+ F2D_SAVEMODEL: SAVE experimental GEOMETRY
     Subroutine F2D_SAVEMODEL (max_parameters, PARAMS, PARAM_INFO, &
       CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_features, num_parameters, &
       x_order, y_order, xmin_poly, ymin_poly, xmax_poly, ymax_poly, &
       weighted_fit, alpha, itsperpar, status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Geometry.Experimental.Save, Save.Geometry.Experimental,
!    Store.Geometry.Experimental
!  Method:
!    Saves geometry parameters to a named file
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-Feb-2005: V0.2 Output array values with keywords (Hammersley)
!    26-Jan-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc' ! Circle least squares
!    fitting common
!  Import:
     Integer, Intent(IN) :: max_parameters ! The dimension size of "PARAMS",
!      etc.
     Real, Intent(IN) :: PARAMS(max_parameters) ! The returned values for
!      the fit parameters
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each of 
!      the parameters of the fit describing, to which feature it belongs, the 
!      type of feature and the number of the parameter within the feature.
!
!      The digits of each element are defined as follows :
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
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters) ! .True., if a model
!      parameter is fixed
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      A description of each of the parameters
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters) ! The typical change
!      sizes expected for the parameter values
     Integer, Intent(IN) :: num_features ! Number of features
     Integer, Intent(IN) :: num_parameters ! Number of parameters
     Integer, Intent(IN) :: x_order ! X-order of 2-D background polynomial
     Integer, Intent(IN) :: y_order ! Y-order of 2-D background polynomial
     Real, Intent(IN) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymax_poly ! Y-coordinate of maximum extent of
!    Chebyshev interval (scaled to -1.0: +1.0)
     Logical, Intent(IN) :: weighted_fit ! .True. if weighting is to be applied
     Real, Intent(IN) :: alpha ! Accuracy level
     Real, Intent(IN) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Io_out = 20 ! Unit for output
!  Local Variables:
     Character(Len = 256), Save :: file_name = 'fit2d.mdl'
!      Name of output file
     Integer :: par ! Loop varible for parameters
     Integer :: retstat ! Return status variable
     Real :: detector_rotation ! Angle of rotation from ideal detector X-axis 
!      (laboratory Y-axis) TO X-axis of actual detector i.e. scanned film, 
!      or image plate (radians)
     Real :: sample_distance ! Distance from sample to detector (metres)
     Real :: tilt_rotation ! Rotation angle of tilt plane anti-clockwise from 
!      the X-axis (radians)
     Real :: tilt_angle ! Tilt angle of detector wrt to the beam in the tilt
!      plane (radians)
     Real :: wavelength ! Wavelength in metres
     Real :: x_beam_centre ! X-coordinate of beam centre (pixels)
     Real :: y_beam_centre ! Y-coordinate of beam centre (pixels)
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SAVEMODEL ' // Version)
        Return
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SAVEMODEL ' // Version)
     Else
 
!     Input name of file for saving geometrical parameters
        Call IO_INPC (.True., 'MODEL PARAMETERS FILE NAME', 1, &
          'Enter Name of file to save model parameters', 1, &
          'Unacceptable input', 11, file_name, status)
        Call IO_OPEN_ASCIIFILE (.False., 'WRITE', Io_out, file_name, retstat, &
          status)
 
        If (retstat .Ne. 0) Then
           Return
        End If
 
!     Output simple header
        Write (Io_out, '(''! FIT2D Fit Model Parameter File'')')
        Write (Io_out, '('' '')')
 
!     Output values
        Write (Io_out, '(''X_ORDER = '', i10)') x_order
        Write (Io_out, '(''Y_ORDER = '', i10)') y_order
        Write (Io_out, '(''X_MIN_POLY = '', g12.5)') xmin_poly
        Write (Io_out, '(''Y_MIN_POLY = '', g12.5)') ymin_poly
        Write (Io_out, '(''X_MAX_POLY = '', g12.5)') xmax_poly
        Write (Io_out, '(''Y_MAX_POLY = '', g12.5)') ymax_poly
        If (weighted_fit) Then
           Write (Io_out, '(''WEIGHTED_FIT = True'')')
        Else
           Write (Io_out, '(''WEIGHTED_FIT = False'')')
        End If
        Write (Io_out, '(''ALPHA = '', g12.5)') alpha
        Write (Io_out, '(''ITERATIONS_PER_PARAMETER = '', g12.5)') itsperpar
        Write (Io_out, '(''NUMBER_OF_FUNCTIONS = '', i10)') num_features
        Write (Io_out, '(''NUMBER_OF_PARAMETERS = '', i10)') num_parameters
 
        Do par = 1, num_parameters
 
           Write (Io_out, '(''PARAMETER_NAMES('', i2, '') = '', a10)') par, &
             PARNAMES(par)
           Write (Io_out, '(''MODEL_PARAMETERS_INFO('', i2, '') = '', i10)') &
             par, PARAM_INFO(par)
           Write (Io_out, '(''MODEL_PARAMETERS('', i2, '') = '', g12.5)') par, &
             PARAMS(par)
 
           If (CONSTRAINTS(par)) Then
              Write (Io_out, '(''PARAMETER_CONSTRAINTS('', i2, '') = True'')') &
                par
           Else
              Write (Io_out, &
                '(''PARAMETER_CONSTRAINTS('', i2, '') = False'')') par
           End If
 
           Write (Io_out, '(''SCALE_FACTORS('', i2, '') = '', g12.5)') par, &
             SCALE_FACTORS(par)
 
        End Do
 
!     Close file
        Close (Io_out)
 
     End If
 
     End Subroutine F2D_SAVEMODEL
!********1*********2*********3*********4*********5*********6*********7*********8

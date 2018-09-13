!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_loadmodel.f90 *
!  *                   *
!  *********************
 
!+ F2D_LOADMODEL: LOAD fit MODEL parameters
     Subroutine F2D_LOADMODEL (max_parameters, params_exist, PARAMS, &
       PARAM_INFO, CONSTRAINTS, PARNAMES, SCALE_FACTORS, num_features, &
       num_parameters, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, weighted_fit, alpha, itsperpar, status)
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
!    07-Feb-2005: V0.2 Support array keywords (Hammersley)
!    26-Jan-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!    fitting common
!  Import:
     Integer, Intent(IN) :: max_parameters ! The dimension size of "PARAMS",
!      etc.
!  Export:
     Logical, Intent(OUT) :: params_exist ! .True., if parameters have been
!      defined
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The returned values for
!      the fit parameters
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters) ! Information on each of
!      the parameters of the fit describing, to which feature it belongs, the 
!      type of feature and the number of the parameter within the feature.
!
!      The digits of each element are defined as follows :
!
!      ......XX - Number of feature (1:99)
!
!      ....XX.. - Type of feature     1 = Standard polynomial curve
!      2 = Gaussian
!      3 = Lorentzian
!      4 = Voigtian
!      5 = Chebyshev form polynomial
!      6 = 2-D Gaussian
!      7 = 2-D polar Gaussian
!      8 = 2-D polar Gaussian, but with no centre of coordinate system
!      9 = 2-D polynomial (normal)
!     10 = 2-D twin polar Gaussian
!     11 = 2-D twin polar Gaussian (no centre)
!     12 = Pseudo-Voigt
!     13 = Exponential decay
!     20 = 2-D Chebyshev polynomial
!     50 = "Row-line"
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
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters) ! .True., if a model 
!      parameter is fixed
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
!      A description of each of the parameters
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters) ! The typical change
!      sizes expected for the parameter values
     Integer, Intent(OUT) :: num_features ! Number of features
     Integer, Intent(OUT) :: num_parameters ! Number of parameters
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
     Logical, Intent(OUT) :: weighted_fit ! .True. if weighting is to be applied
     Real, Intent(OUT) :: alpha ! Accuracy level
     Real, Intent(OUT) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
     Integer, Parameter :: Io_in = 20 ! Unit for output
!  Local Variables:
     Character(Len = 256), Save :: file_name = 'fit2d.mdl' ! Name of output file
     Character(Len = 256) :: keyword ! Keyword separated from line
     Character(Len = 256) :: line ! Line of text input from file
     Character(Len = 256) :: value ! Value separated from line
     Integer :: iostat ! I/O status return variable
     Integer :: index1 ! Index for 1st dimension array value, or 0 for scalar
     Integer :: index2 ! Index for 2nd dimension array value, or 0 for 1-D or
!      scalar
     Integer :: len_file_name ! Number of defined characters in file name
     Integer :: retstat ! Return status variable
     Logical :: convert ! .True., if a token is converted successfully
     Logical :: more ! .True., whilst more lines to input
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LOADMODEL ' // Version)
        Return
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LOADMODEL ' // Version)
     End If
 
!  Input name of file for saving geometrical parameters
     Call IO_INPS (.True., 'GEOMETRY FILE NAME', 1, &
       'Enter Name of file containing geometrical values', 1, &
       'Unacceptable input', 11, len_file_name, file_name, status)
     Call IO_OPEN_ASCIIFILE (.False., 'READ', io_in, file_name, retstat, &
       status)
 
     If (retstat .Ne. 0) Then
 
        Call IO_WRITE ('WARNING: File not found', status)
        Return
 
     End If
 
!  Input header
     Read (Io_in, Fmt = '(a)', Iostat = iostat) line
     If (line .Ne. '! FIT2D Fit Model Parameter File') Then
 
        Call IO_WRITE ( 'WARNING: File is not a FIT2D model parameter file', &
          status)
        Return
 
     End If
 
     more = .True.
     Do While (more)
 
!     Input line
        Read (Io_in, Fmt = '(a)', Iostat = iostat) line
 
        If (iostat .Ne. 0) Then
           more = .False.
        Else
 
!        Parse line for keyword, index, and value
           Call IO_PARSEKEYWORDVALUE (line, convert, keyword, index1, index2, &
             value, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Line = '', a70)') line
!        Write (*, '(''convert = '', l)') convert
!        Write (*, '(''keyword = '', a60)') keyword
!        Write (*, '(''index1  = '', i6, '', index2 = '', i6)')
!        :          index1, index2
!        Write (*, '(''value = '', a60)') value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           If (convert) Then
 
              If (keyword .Eq. 'X_ORDER') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (value, convert, x_order, status)
 
              Else If (keyword .Eq. 'Y_ORDER') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (value, convert, y_order, status)
 
              Else If (keyword .Eq. 'X_MIN_POLY') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, xmin_poly, status)
 
              Else If (keyword .Eq. 'Y_MIN_POLY') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, ymin_poly, status)
 
              Else If (keyword .Eq. 'X_MAX_POLY') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, xmax_poly, status)
 
              Else If (keyword .Eq. 'Y_MAX_POLY') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, ymax_poly, status)
 
              Else If (keyword .Eq. 'ALPHA') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, alpha, status)
 
              Else If (keyword .Eq. 'ITERATIONS_PER_PARAMETER') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, itsperpar, status)
 
              Else If (keyword .Eq. 'WEIGHTED_FIT') Then
 
                 If (value .Eq. 'True') Then
                    weighted_fit = .True.
                 Else
                    weighted_fit = .False.
                 End If
 
              Else If (keyword .Eq. 'NUMBER_OF_FUNCTIONS') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (value, convert, num_features, status)
 
              Else If (keyword .Eq. 'NUMBER_OF_PARAMETERS') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (value, convert, num_parameters, status)
 
              Else If (keyword .Eq. 'MODEL_PARAMETERS') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, PARAMS(index1), status)
 
              Else If (keyword .Eq. 'MODEL_PARAMETERS_INFO') Then
 
!              Convert key value to an integer
                 Call IO_TOKTI (value, convert, PARAM_INFO(index1), status)
 
              Else If (keyword .Eq. 'SCALE_FACTORS') Then
 
!              Convert key value to a real
                 Call IO_TOKTR (value, convert, SCALE_FACTORS(index1), status)
 
              Else If (keyword .Eq. 'PARAMETER_NAMES') Then
 
                 PARNAMES(index1) = value
 
              Else If (keyword .Eq. 'PARAMETER_CONSTRAINTS') Then
 
                 If (value .Eq. 'True') Then
                    CONSTRAINTS(index1) = .True.
                 Else
                    CONSTRAINTS(index1) = .False.
                 End If
 
              End If
 
           End If
 
        End If
 
     End Do
 
!  Close file
     Close (Io_in)
 
     params_exist = .True.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Iterations per par = '' g12.5)') itsperpar
!  Write (*, '(''num_parameters = '' i10)') num_parameters
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_LOADMODEL
!********1*********2*********3*********4*********5*********6*********7*********8

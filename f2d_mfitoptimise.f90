!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_mfitoptimise.f90 *
!  *                      *
!  ************************
 
!+ F2D_MFITOPTIMISE - MFIT OPTIMISE'ation
     Subroutine F2D_MFITOPTIMISE ( & !max_vec_values, max_vectors, 
       x_pixel_size, &
       variances_exist, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, VARIANCES, MASK, &
       xstrelm, xendelm, ystrelm, yendelm, y_start, title, x_label, y_label, &
       alpha, fitting_info, weighted_fit, itsperpar, display_frequency, &
       model_evolution, max_parameters, num_parameters, num_1dfeatures, &
       max_results, max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
       SCALE_FACTORS, PAR_BASE, PARAMS, PARERRORS, results_exist, FRESULTS, &
       RESERRORS, RESNAMES, num_fun_calls, chisqr, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, MXAXIS, MYAXIS, MODEL, &
       COVARIANCE, num_text, TEXT, print_type, results, status)
!num_vectors, STR_VECTORS, &
!       END_VECTORS, VECTORS, VECTOR_TITLES, status)
!  Description:
!    Fits 1-D data by least squares fitting with geometric functions
!    using "MA_MODELFIT" to refine the parameters.
!    "PARDES(max_parameters)" contains the information to determine what
!    each parameter represents.
!    N.B. The X-coordinate parameters must all be in pixels coordinates.
!  Method:
!    Repeated calls to "F2D_LSQMFIT"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Dec-2014: V0.9 Protect "results" array against too many parameters
!      (Hammersley)
!    09-Dec-2014: V0.8 Use result vectors data structure (Hammersley)
!    05-Dec-2014: V0.7 Need to allocate "WEIGHTS" array even if it is not used,
!      as the Intel compiler calls this an error, at least in check:all and 
!      debug mode !!! (Hammersley)
!    30-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    16-Mar-2006: V0.5 Debugging problem in plotting vectors (Hammersley)
!    30-Oct-1998: V0.4 Add possibility to start in the middle and
!      fit in both directions (Hammersley)
!    29-Oct-1998: V0.3 Save results in "VECTORS" array (Hammersley)
!    17-Dec-1996: V0.2 Enable display frequency support (Hammersley)
!    15-Dec-1996: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
!     Integer, Intent(IN) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
     Real, Intent(IN) :: x_pixel_size ! Size of a raw pixel in the
!      X-direction (metres)
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-Axis data values
     Real, Intent(IN) :: YAXIS(xmaxdat) ! Y-Axis data values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Array containing
!      variances in the data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Integer, Intent(IN) :: xstrelm ! Starting element of region to be fitted
     Integer, Intent(IN) :: xendelm ! End element of region to be fitted
     Integer, Intent(IN) :: ystrelm ! Starting element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End element of region to be fitted
     Integer, Intent(IN) :: y_start ! Row to be used for initialisation
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
     Integer, Intent(IN) :: display_frequency ! Frequency with which to
!      display fitted model results
     Logical, Intent(IN) :: model_evolution ! .True., if the fitting model
!      parameters "evolve" during a multiple row fit, or always start from the
!      initialisation values
     Integer, Intent(IN) :: max_parameters ! Dimension size of "PARAMS",
!      etc.
     Integer, Intent(IN) :: num_parameters ! Total number of variable and
!      constrained parameters in the fit
     Integer, Intent(IN) :: num_1dfeatures ! Number of features in 1-D model
     Integer, Intent(IN) :: max_results ! Dimension size of "FRESULTS",
!      "RESERRORS"
     Integer, Intent(IN) :: max_text ! Dimension size of "TEXT"
     Integer, Intent(IN) :: num_results ! Number of results calculated
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      A description of each fit parameter
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Information on each
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
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters) ! Constraints array,
!      .True. if a model parameter is fixed and not to be fitted
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters) ! Scale factors array
!      for the parameter
!  Import/Export:
     Real, Intent(INOUT) :: PAR_BASE(max_parameters) ! The initial fit model
!      parameters
!  Export:
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The fit model parameters
     Real, Intent(OUT) :: PARERRORS(max_parameters) ! Estimated errors for
!      each fitted parameter
     Logical, Intent(OUT) :: results_exist ! .True. if fit results exist
     Real, Intent(OUT) :: FRESULTS(max_results) ! Results calculated from the
!      fit parameters, i.e. integration intensity
     Real, Intent(OUT) :: RESERRORS (max_results) ! Error in Results
!      calculated from the fit parameters
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
!      Names of results
     Integer, Intent(OUT) :: num_fun_calls ! Number of function calls, set
!      negative if the fit fails
     Real, Intent(OUT) :: chisqr ! The reduced chi squared or goodness of
!      fit parameter
     Integer, Intent(OUT) :: mxnumdat ! Number of defined elements in
!      X-direction
     Integer, Intent(OUT) :: mynumdat ! Number of defined elements in
!      Y-direction
     Integer, Intent(OUT) :: mxstrelm ! Starting element of region to be
!      fitted
     Integer, Intent(OUT) :: mystrelm ! Starting element of region to be
!      fitted
     Integer, Intent(OUT) :: mxendelm ! End element of region to be fitted
     Integer, Intent(OUT) :: myendelm ! End element of region to be fitted
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Model X-axis values
     Real, Intent(OUT) :: MYAXIS(xmaxdat) ! Model Y-axis values
     Real, Intent(OUT) :: MODEL(xmaxdat, ymaxdat) ! The least squares
!      theoretical fit to the data
     Real, Intent(OUT) :: COVARIANCE(max_parameters, max_parameters)
!      The estimated fractional variance-covariance matrix
     Integer, Intent(OUT) :: num_text ! Number of lines of defined results
!      text
     Character(Len = *), Intent(OUT) :: TEXT(max_text) ! Output text
     Character(Len = *), Intent(OUT) :: print_type ! Type, of graphics to
!      print: supported types are:
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
!        "optimise"
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!     Integer, Intent(OUT) :: num_vectors ! Number of values defined in the
!      "time"-series for each vector
!     Integer, Intent(OUT) :: STR_VECTORS(max_vectors) ! Starting defined
!      element for "VECTORS"
!     Integer, Intent(OUT) :: END_VECTORS(max_vectors) ! End defined elements
!      for "VECTORS"
!     Real, Intent(OUT) :: VECTORS(max_vec_values, max_vectors)
!      Multiple 1-D arrays of vector values
!     Character(Len = *), Intent(OUT) :: VECTOR_TITLES(max_vectors)
!      Titles for the 1-D data-sets
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
     Character(Len = 20) :: row_text ! Text to add to the title for the
!      particular row being fitted
     Character(Len = 132) :: row_title ! Title for row of values
     Integer :: i ! Loop variable for results
     Integer :: par ! Loop variable for parameter values
     Integer stat ! Status return variable for "Allocate"
     Integer :: y_row ! Loop variable for rows to be fitted
     Logical :: display ! .True., if the fitted results are to be
!      displayed graphically
!  Local Arrays:
     Real, Allocatable :: WEIGHTS(:) ! Dynamically allocated work array
!  External Functions:
!     Character(Len = 12), External :: Io_itoc ! Convert an integer to character
!      form
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MFITOPTIMISE ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (Min(xmaxdat, max_parameters, max_results) .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
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
        Call ST_SAVE ('Subroutine F2D_MFITOPTIMISE ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_MFITOPTIMISE'')')
!     Write (*, '(''results%max_values = '', i8)') results%max_values
!     Write (*, '(''results%max_vectors = '', i8)') results%max_vectors

!  Test VECTORS assignment
!     results%VECTORS(1, 1) = 5.0

!     Write (*, '(''F2D_MFITOPTIMISE: After vectors assignment'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     If (weighted_fit) Then
 
!     Allocate space to calculate standard deviations
        Allocate (WEIGHTS(xendelm), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MFITOPTIMISE ' // Version)
           Return
        End If

     Else

!!!!!!PLEASE INTEL COMPILER BY ALLOCATING 1 element !!!!!
        Allocate (WEIGHTS(1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MFITOPTIMISE ' // Version)
           Return
        End If

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_MFITOPTIMISE: After Allocate'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Transfer initialisation parameters to refinement array
     Do par = 1, num_parameters
        PARAMS(par) = PAR_BASE(par)
     End Do
 
!  Fit from starting row to last row
     Do y_row = y_start, yendelm
 
!     Calculate where or not to display the results graphically
        display = Mod(y_row - y_start, display_frequency) .Eq. 0
 
!     Create row title text
        row_text = ': Row ' // Io_itoc (y_row)
        If (Len_trim(title) .Gt. 0) Then
           row_title = Trim(title) // row_text
        Else
           row_title = row_text
        End If
 
!     Fit row of data
        Call F2D_LSQMFIT ( & ! max_vec_values, max_vectors, 
          y_row, x_pixel_size, &
          variances_exist, xmaxdat, XAXIS, DATA(1, y_row), VARIANCES(1, &
          y_row), MASK(1, y_row), xstrelm, xendelm, row_title, x_label, &
          y_label, alpha, fitting_info, weighted_fit, itsperpar, display, &
          max_parameters, num_parameters, num_1dfeatures, max_results, &
          max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
          SCALE_FACTORS, PARAMS, PARERRORS, results_exist, FRESULTS, RESERRORS,&
          RESNAMES, num_fun_calls, chisqr, MODEL(1, y_row), COVARIANCE, &
          num_text, TEXT, print_type, WEIGHTS, results, status)
!VECTORS, status)
 
        If (model_evolution) Then
 
           If (y_row .Eq. y_start) Then
 
!           Update starting model values
              Do par = 1, num_parameters
                 PAR_BASE(par) = PARAMS(par)
              End Do
 
           End If
 
        Else
 
!        Re-set initialisation parameters
           Do par = 1, num_parameters
              PARAMS(par) = PAR_BASE(par)
           End Do
 
        End If
 
     End Do
 
!  Set starting values
     Do par = 1, num_parameters
        PARAMS(par) = PAR_BASE(par)
     End Do
 
!  Fit from starting row to first row (backwards)
     Do y_row = y_start - 1, ystrelm, -1
 
!     Calculate where or not to display the results graphically
        display = Mod(y_start - y_row, display_frequency) .Eq. 0
 
!     Create row title text
        row_text = ': Row ' // Io_itoc (y_row)
        If (Len_trim(title) .Gt. 0) Then
           row_title = Trim(title) // row_text
        Else
           row_title = row_text
        End If
 
!     Fit row of data
        Call F2D_LSQMFIT ( & !max_vec_values, max_vectors, 
          y_row, x_pixel_size, &
          variances_exist, xmaxdat, XAXIS, DATA(1, y_row), VARIANCES(1, &
          y_row), MASK(1, y_row), xstrelm, xendelm, row_title, x_label, &
          y_label, alpha, fitting_info, weighted_fit, itsperpar, display, &
          max_parameters, num_parameters, num_1dfeatures, max_results, &
          max_text, num_results, PARNAMES, PARAM_INFO, CONSTRAINTS, &
          SCALE_FACTORS, PARAMS, PARERRORS, results_exist, FRESULTS, RESERRORS,&
          RESNAMES, num_fun_calls, chisqr, MODEL(1, y_row), COVARIANCE, &
          num_text, TEXT, print_type, WEIGHTS, results, status)
!VECTORS, status)
 
        If (.Not. model_evolution) Then
 
!        Re-set initialisation parameters
           Do par = 1, num_parameters
              PARAMS(par) = PAR_BASE(par)
           End Do
 
        End If
 
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_MFITOPTIMISE: After F2D_LSQMFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Check size of result vectors
     If (num_parameters + num_results .Gt. results%max_vectors) Then
        Call IO_WRITE ('WARNING: Not enough space in results vectors ' // &
          'for all parameters', status)
        Call IO_WRITE ('         and derived results.', status)
     End If

!  Set vector names and ROI
     Do i = 1, Min(results%max_vectors, num_parameters)
        results%TITLES(i) = PARNAMES(i)
     End Do
     Do i = 1, num_results

        If (num_parameters + i .Lt. results%max_vectors) Then
           results%TITLES(num_parameters + i) = RESNAMES(i)
        End If

     End Do
 
     results%num_vectors = &
       Min(results%max_vectors, num_parameters + num_results)
 
     Do i = 1, results%num_vectors
        results%STARTS(i) = Min(ystrelm, results%STARTS(i))
        results%ENDS(i) = Max(yendelm, results%ENDS(i))
     End Do
 
!  Free space
     Deallocate (WEIGHTS)
 
!  Transfer axis values
     Do y_row = 1, xendelm
        MXAXIS(y_row) = XAXIS(y_row)
     End Do
 
     Do y_row = 1, yendelm
        MYAXIS(y_row) = YAXIS(y_row)
     End Do
 
!  Transfer ROI
     mxstrelm = xstrelm
     mystrelm = ystrelm
     mxendelm = xendelm
     myendelm = yendelm
 
!  Set defined memory/model region
     mxnumdat = Max(mxnumdat, xendelm)
     mynumdat = Max(mynumdat, yendelm)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_MFITOPTIMISE: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_MFITOPTIMISE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

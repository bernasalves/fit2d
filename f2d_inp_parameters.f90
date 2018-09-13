!********1*********2*********3*********4*********5*********6*********7**
 
!  **************************
!  *                        *
!  * f2d_inp_parameters.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_PARAMETERS - file INput of fit model PARAMETERS
     Subroutine F2D_INP_PARAMETERS (xmaxdat, ymaxdat, max_parameters, &
       max_results, xnumdat, ynumdat, MASK, PARAMS, PARAM_INFO, CONSTRAINTS, &
       PARNAMES, SCALE_FACTORS, RESNAMES, num_features, num_parameters, &
       num_results, x_order, y_order, xmin_poly, ymin_poly, xmax_poly, &
       ymax_poly, weighted_fit, alpha, itsperpar, evolve, disfreq, fastdis, &
       haltcrit, haltval, parexist, status)
!  Description:
!    Inputs parameters from a file previously created by "F2D_OUT_PARAMETERS".
!  Method:
!    Uses flexible file format calls
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    23-Mar-1999: V0.7 Change to use "FIO_FREAD_*" routines instead of old 
!      "IO_FREAD_*" routines (Hammersley)
!    19-Dec-1997: V0.6 Account for byte swapping between different systems 
!      (Hammersley)
!    01-Dec-1996: V0.5 Change 2-D fitting polynomial to a Chebyshev form, 
!      resulting in argument list changes (Hammersley)
!    16-Nov-1996: V0.4 Separate parameter constraints to a logical array 
!      "CONSTRAINTS" (Hammersley)
!    14-Dec-1995: V0.3 Translate tildes in file names (Hammersley)
!    28-Feb-1995: V0.2 Make mask elements single byte (Hammersley)
!    23-Feb-1993: V0.1 Original, based on "FIT2FIN" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of "MASK"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of "MASK"
     Integer, Intent(IN) :: max_parameters ! The dimension size of "PARAMS",
!      etc.
     Integer, Intent(IN) :: max_results ! The dimension size of RESNAMES
!  Export:
     Integer, Intent(OUT) :: xnumdat ! Number of mask elements in X
     Integer, Intent(OUT) :: ynumdat ! Number of mask elements in Y
     Logical*1, Intent(OUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Real, Intent(OUT) :: PARAMS(max_parameters) ! The returned values for the
!      fit parameters
     Integer, Intent(OUT) :: PARAM_INFO(max_parameters) ! Attributes of each
!      parameter packed into each element
     Logical, Intent(OUT) :: CONSTRAINTS(max_parameters) ! .True., if a model 
!      parameter is fixed
     Character(Len = *), Intent(OUT) :: PARNAMES(max_parameters)
!      A description of each of the parameters
     Real, Intent(OUT) :: SCALE_FACTORS(max_parameters) ! The typical size
!      of changes expected in each of the fit parameters
     Character(Len = *), Intent(OUT) :: RESNAMES(max_results)
!      A description of each of the results
     Integer, Intent(OUT) :: num_features ! Number of "features" in fit
     Integer, Intent(OUT) :: num_parameters ! Total of number of parameters
!      in fit
     Integer, Intent(OUT) :: num_results ! Number of results
     Integer, Intent(OUT) :: x_order ! X-order of polynomial
     Integer, Intent(OUT) :: y_order ! Y-order of polynomial
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
     Logical, Intent(OUT) :: evolve ! .True. if the fit model is allowed to
!      evolve
     Integer, Intent(OUT) :: disfreq ! Frequency of graphical output during
!      fitting
     Logical, Intent(OUT) :: fastdis ! .True. if minimal text to be added to
!      diagrams
     Integer, Intent(OUT) :: haltcrit ! Criterion used to prematurely halt the
!      fitting
     Real, Intent(OUT) :: haltval ! Value for the halt criterion
     Logical, Intent(OUT) :: parexist ! .True., if parameters have been defined
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: In_unit = 9 ! Fortran I/O channel for input
!  Local Variables:
     Character(Len = 80) :: cbig_endian ! TRUE, or FALSE
     Character(Len = 80) :: in_file ! Name of file to be opened to input data
     Integer :: dummy ! Unused, second dimension variable
     Integer :: filstat ! Fortran open file status variable
     Integer :: input_status ! Status of item input
!      -1 = item found, but not enough room for whole of item to be read in
!       0 = good, item value returned
!       1 = Item not found
!       2 = Item of wrong type
!       3 = Item of wrong dimension
     Integer :: num_recl_bytes ! Number of bytes in single machine record length
!      unit
     Integer :: par ! Loop variable for parameters
     Integer :: recl ! Record length in machine dependent units
     Integer :: retstat ! "IO_TRANS_PATH" return status:
!      0 = Good status
!      1 = Bad status, user not known
!      2 = Bad status, full path length too long for storage space
!      3 = Bad status, user string contains a slash
!      4 = Bad status, user string is too short
     Logical :: file_big_endian ! .True., if the file is stored big-endian
     Logical :: swap ! .True., if byte swapping is necessary
!  Local Arrays:
!  External Functions:
     Logical, External :: Io_big_endian ! .True., if the architecture is big 
!      endian
     Integer, External :: Ma_exdigit ! Extracts a particular digit from a
!      decimal number
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_PARAMETERS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Initialise variables
 
!     Input file name for output
        in_file = 'fit2d.par'
        Call IO_INPC (.True., 'PARAMETER FILE NAME', 1, &
          'Name of input file containing fit parameters',  1, &
          'Unacceptable input', 1, in_file, status)
 
!     If user escape return immediately
        If (status .Eq. St_escapevalue) Then
           Return
        End If
 
!     Translate (if necessary) the entered file directory path
        Call IO_TRANS_PATH (in_file, retstat, in_file, status)
 
        If (retstat .Eq. 0) Then
 
!        Open file for input
           Call IO_RECL (num_recl_bytes, status)
           recl = 512 / num_recl_bytes
           Open (Unit = In_unit, File = in_file, Access = 'DIRECT', Recl = &
             recl, Status = 'OLD', Form = 'UNFORMATTED', Iostat = filstat)
 
        Else
           filstat = 1
        End If
 
        If (filstat .Ne. 0) Then
           Call IO_WRITE ( 'WARNING: Error in opening file', status)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input FFF start identifier
        Call FIO_FREAD_STRING (In_unit, '$FFF_START', input_status, &
          cbig_endian, status)
 
        If (input_status .Ne. 0) Then
           Call IO_WRITE ('WARNING: This is not a Flexible ' // &
             'File Format (FFF) file and cannot be read', status)
           Return
        End If
 
!     Find-out if big or little endian
        Call FIO_FREAD_STRING (In_unit, '$BIG_ENDIAN', input_status, &
          cbig_endian, status)
        file_big_endian = cbig_endian .Eq. 'TRUE'
 
        swap = file_big_endian .Neqv. Io_big_endian(status)
 
!     Input mask
        Call FIO_FREAD_L1ARRAY (In_unit, 'data_mask', xmaxdat, ymaxdat, &
          input_status, xnumdat, ynumdat, MASK, status)
 
        If (input_status .Gt. 0) Then
 
!        MASK not found, or wrong type
           Call IO_WRITE ('WARNING: Data mask not found ' // &
             'setting mask to zero (no mask)', status)
 
!        Set to .False.
           Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, .False., &
             MASK, status)
 
        Else If (input_status .Eq. -1) Then
 
           Call IO_WRITE ('WARNING: Not all of the ' // &
             'mask data could be input (too big)', status)
 
        End If
 
!     Read scalar items
        Call FIO_FREAD_INTEGER (In_unit, 'total_params', input_status, &
          num_parameters, status)
        Call FIO_FREAD_INTEGER (In_unit, 'number_features', input_status, &
          num_features, status)
        Call FIO_FREAD_INTEGER (In_unit, 'number_results', input_status, &
          num_results, status)
        Call FIO_FREAD_INTEGER (In_unit, 'x_order', input_status, x_order, &
          status)
        Call FIO_FREAD_INTEGER (In_unit, 'y_order', input_status, y_order, &
          status)
        Call FIO_FREAD_REAL (In_unit, 'x_min_poly', input_status, xmin_poly, &
          status)
 
!     Check input status
        If (input_status .Ne. 0) Then
 
!        Old parameter file, polynomial incompatible
           Call IO_WRITE (' ', status)
           Call IO_WRITE ('ERROR: Old parameter file: not ' // &
             'compatible with new polynomial fitting', status)
           Call IO_WRITE (' ', status)
           status = St_escapevalue
           Return
        End If
 
        Call FIO_FREAD_REAL (In_unit, 'y_min_poly', input_status, ymin_poly, &
          status)
        Call FIO_FREAD_REAL (In_unit, 'x_max_poly', input_status, xmax_poly, &
          status)
        Call FIO_FREAD_REAL (In_unit, 'y_max_poly', input_status, ymax_poly, &
          status)
        Call FIO_FREAD_INTEGER (In_unit, 'apply_weights', input_status, &
          weighted_fit, status)
        Call FIO_FREAD_REAL (In_unit, 'accuracy', input_status, alpha, status)
        Call FIO_FREAD_REAL (In_unit, 'iters_per_param', input_status, &
          itsperpar, status)
        Call FIO_FREAD_INTEGER (In_unit, 'model_evolve', input_status, evolve, &
          status)
        Call FIO_FREAD_INTEGER (In_unit, 'display_frequency', input_status, &
          disfreq, status)
        Call FIO_FREAD_INTEGER (In_unit, 'fast_display', input_status, &
          fastdis, status)
        Call FIO_FREAD_INTEGER (In_unit, 'halt_criterion', input_status, &
          haltcrit, status)
        Call FIO_FREAD_REAL (In_unit, 'halt_value', input_status, haltval, &
          status)
 
!     Input Parameter values
        Call FIO_FREAD_RARRAY (In_unit, swap, 'parameter_values', &
          max_parameters, 1, input_status, num_parameters, dummy, PARAMS, &
          status)
 
!     Input Parameter descriptions
        Call FIO_FREAD_IARRAY (In_unit, swap, 'parameter_descriptions', &
          max_parameters, 1, input_status, num_parameters, dummy, PARAM_INFO, &
          status)
 
!     Input Parameter scales
        Call FIO_FREAD_RARRAY (In_unit, swap, 'parameter_scales', &
          max_parameters, 1, input_status, num_parameters, dummy, &
          SCALE_FACTORS, status)
 
!     Input Parameter values
        Call FIO_FREAD_CARRAY (In_unit, 'parameter_names', max_parameters, &
          input_status, num_parameters, PARNAMES, status)
 
        If (num_results .Gt. 0) Then
 
!        Input Result names
           Call FIO_FREAD_CARRAY (In_unit, 'result_names', max_results, &
             input_status, num_results, RESNAMES, status)
 
           If (input_status .Gt. 0) Then
              num_results = 0
           End If
 
        End If
 
!     Close input file
        Close (In_unit)
 
!     Set "CONSTRAINTS" array
        Do par = 1, num_parameters
           CONSTRAINTS(par) = Ma_exdigit(PARAM_INFO(par), 7, status) .Eq. 1
        End Do
 
!     Declare fit parameters to exist
        parexist = .True.
 
     End If
 
     End Subroutine F2D_INP_PARAMETERS
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_out_parameters.f90 *
!  *                        *
!  **************************
 
!+ F2D_OUT_PARAMETERS: OUTputs PARAMETERS in simple binary format
     Subroutine F2D_OUT_PARAMETERS (xmaxdat, ymaxdat, xnumdat, ynumdat, MASK, &
       max_parameters, max_results, PARAMS, PARAM_INFO, CONSTRAINTS, PARNAMES, &
       SCALE_FACTORS, RESNAMES, num_features, num_parameters, num_results, &
       x_order, y_order, xmin_poly, ymin_poly, xmax_poly, ymax_poly, &
       weighted_fit, alpha, itsperpar, evolve, disfreq, fastdis, haltcrit, &
       haltval, status)
!  Description:
!    Outputs fit parameters to an output file.
!    The data can be read back in with "F2D_INP_PARAMETERS"
!
!    The file format is described in "data_format.tex"
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    01-Dec-1996: V0.5 Change 2-D fitting polynomial to a Chebyshev polynomial 
!      (Hammersley)
!    16-Nov-1996: V0.4 Separate parameter constraints to a logical array 
!      "CONSTRAINTS" (Hammersley)
!    14-Dec-1995: V0.3 Translate tildes in file names (Hammersley)
!    28-Feb-1995: V0.2 Make mask elements single bytes (Hammersley)
!    20-Feb-1993: V0.1 Original, based on "F2D_OUTBIN" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! The first dimension size of "MASK"
     Integer, Intent(IN) :: ymaxdat ! The second dimension size of "MASK"
     Integer, Intent(IN) :: xnumdat ! The number of elements in the first
!      dimension of "MASK"
     Integer, Intent(IN) :: ynumdat ! The number of elements in the second
!      dimension of "MASK"
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Integer, Intent(IN) :: max_parameters ! The dimension size of "PARAMS",
!      etc.
     Integer, Intent(IN) :: max_results ! The dimension size of RESNAMES
     Real, Intent(IN) :: PARAMS(max_parameters) ! The values of the fit model 
!      parameters
     Logical, Intent(IN) :: CONSTRAINTS(max_parameters) ! .True., if a model
!      parameter is fixed
     Character(Len = *), Intent(IN) :: PARNAMES(max_parameters)
!      A description of each of the parameters
     Real, Intent(IN) :: SCALE_FACTORS(max_parameters) ! The typical size of
!      changes expected for each parameter value
     Character(Len = *), Intent(IN) :: RESNAMES(max_results)
!      A description of each of the results
     Integer, Intent(IN) :: num_features ! Number of "features" in fit
     Integer, Intent(IN) :: num_parameters ! Total of number of parameters in 
!      fit
     Integer, Intent(IN) :: num_results ! Number of results
     Integer, Intent(IN) :: x_order ! X order of background polynomial
     Integer, Intent(IN) :: y_order ! Y order of background polynomial
     Real, Intent(IN) :: xmin_poly ! X-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymin_poly ! Y-coordinate of minimum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: xmax_poly ! X-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Real, Intent(IN) :: ymax_poly ! Y-coordinate of maximum extent of
!      Chebyshev interval (scaled to -1.0: +1.0)
     Logical, Intent(IN) :: weighted_fit ! .True. if weighting is to be applied
     Real, Intent(IN) :: alpha ! Accuracy level
     Real, Intent(IN) :: itsperpar ! Maximum number of iterations per
!      unconstrained parameter
     Logical, Intent(IN) :: evolve ! .True. if the fit model is allowed to
!       evolve
     Integer, Intent(IN) :: disfreq ! Frequency of graphical output during
!      fitting
     Logical, Intent(IN) :: fastdis ! .True. if minimal text to be added to
!      diagrams
     Integer, Intent(IN) :: haltcrit ! Criterion used to prematurely halt the 
!      fitting
     Real, Intent(IN) :: haltval ! Value for the halt criterion
!  Import/Export:
     Integer, Intent(INOUT) :: PARAM_INFO(max_parameters) ! Attributes of each 
!      parameter packed into each element
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Out_unit = 9 ! Logical unit number for input
!  Local Variables:
     Character(Len = 5) :: cbig_endian ! "TRUE" if the architecture is big
!      endian otherwise "FALSE"
     Character(Len = 80) :: outfile ! Name of the output file
     Integer :: filstat ! IOSTAT status return variable
     Integer :: num_recl_bytes ! Number of bytes in single machine record length
!      unit
     Integer :: par ! Loop variable for parameters
     Integer :: recl ! Record length in machine dependent units
     Integer :: record ! Number of output record
     Integer :: retstat ! "IO_TRANS_PATH" return status:
!      0 = Good status
!      1 = Bad status, user not known
!      2 = Bad status, full path length too long for storage space
!      3 = Bad status, user string contains a slash
!      4 = Bad status, user string is too short
!  Local Arrays:
!  External Functions:
     Integer, External :: Ma_exdigit ! Extract digit from an integer number
     Integer, External :: St_errorcode ! Returns error code part of status value
     Logical, External :: Io_big_endian ! .True., if architecture is big endian
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OUT_PARAMETERS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_OUT_PARAMETERS ' // Version)
     Else
 
!     Set digit in "PARAM_INFO" depending on whether or not the parameter is 
!     constrained
        Do par = 1, num_parameters
 
           If (CONSTRAINTS(par)) Then
 
              If (Ma_exdigit(PARAM_INFO(par), 7, status) .Eq. 0) Then
                 PARAM_INFO(par) = PARAM_INFO(par) + 1000000
              End If
 
           Else
 
              If (Ma_exdigit(PARAM_INFO(par), 7, status) .Eq. 1) Then
                 PARAM_INFO(par) = PARAM_INFO(par) - 1000000
              End If
 
           End If
 
        End Do
 
!     Input file name for output
        outfile = 'fit2d.par'
        Call IO_INPC (.True., 'OUTPUT FILE NAME', 1, &
          'Name of file to contain output data',  1, 'Unacceptable input', 1, &
          outfile, status)
 
!     If user escape return immediately
        If (St_errorcode(status) .Eq. St_escapevalue) Then
           Return
        End If
 
!     Translate (if necessary) the entered file directory path
        Call IO_TRANS_PATH (outfile, retstat, outfile, status)
 
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Problem with file ' // &
             'directory path (directory doesn''t exist ?)', status)
           Return
        End If
 
!     Open file for output
        Call IO_RECL (num_recl_bytes, status)
        recl = 512 / num_recl_bytes
        Open (UNIT = Out_unit, FILE = outfile, ACCESS = 'DIRECT', RECL = recl, &
          STATUS = 'UNKNOWN', FORM = 'UNFORMATTED', IOSTAT = filstat)
 
        If (filstat .Ne. 0) Then
           Call IO_WRITE ( 'WARNING: Error in opening file', status)
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output file header
 
!     Find out if architecture is big-endian or little-endian
        If (Io_big_endian(status)) Then
           cbig_endian = 'TRUE'
        Else
           cbig_endian = 'FALSE'
        End If
 
!     Output general FFF header
        record = 1
        Call IO_FWRITE_STRING (Out_unit, '$FFF_START', 'FIT2D Parameter File', &
          record, status)
        Call IO_FWRITE_STRING (Out_unit, '$BIG_ENDIAN', cbig_endian, record, &
          status)
 
!     Output file header
        Call IO_FWRITE_STRING (Out_unit, 'header', 'FIT2D parameter file', &
          record, status)
 
!     Output data mask array
        Call IO_FWRITE_L1ARRAY (Out_unit, 'data_mask', xmaxdat, ymaxdat, 1, 1, &
          xnumdat, ynumdat, MASK, record, status)
 
!     Write scalar items
        Call IO_FWRITE_INTEGER (Out_unit, 'total_params', num_parameters, &
          record, status)
        Call IO_FWRITE_INTEGER (Out_unit, 'number_features', num_features, &
          record, status)
        Call IO_FWRITE_INTEGER (Out_unit, 'number_results', num_results, &
          record, status)
        Call IO_FWRITE_INTEGER (Out_unit, 'x_order', x_order, record, status)
        Call IO_FWRITE_INTEGER (Out_unit, 'y_order', y_order, record, status)
        Call IO_FWRITE_REAL (Out_unit, 'x_min_poly', xmin_poly, record, &
          status)
        Call IO_FWRITE_REAL (Out_unit, 'y_min_poly', ymin_poly, record, &
          status)
        Call IO_FWRITE_REAL (Out_unit, 'x_max_poly', xmax_poly, record, &
          status)
        Call IO_FWRITE_REAL (Out_unit, 'y_max_poly', ymax_poly, record, &
          status)
        Call IO_FWRITE_INTEGER (Out_unit, 'apply_weights', weighted_fit, &
          record, status)
        Call IO_FWRITE_REAL (Out_unit, 'accuracy', alpha, record, status)
        Call IO_FWRITE_REAL (Out_unit, 'iters_per_param', itsperpar, record, &
          status)
        Call IO_FWRITE_INTEGER (Out_unit, 'model_evolve', evolve, record, &
          status)
        Call IO_FWRITE_INTEGER (Out_unit, 'display_frequency', disfreq, &
          record, status)
        Call IO_FWRITE_INTEGER (Out_unit, 'fast_display', fastdis, record, &
          status)
        Call IO_FWRITE_INTEGER (Out_unit, 'halt_criterion', haltcrit, record, &
          status)
        Call IO_FWRITE_REAL (Out_unit, 'halt_value', haltval, record, status)
 
!     Output Parameter values
        Call IO_FWRITE_RARRAY (Out_unit, 'parameter_values', max_parameters, &
          1, 1, 1, num_parameters, 1, PARAMS, record, status)
 
!     Output Parameter descriptions
        Call IO_FWRITE_IARRAY (Out_unit, 'parameter_descriptions', &
          max_parameters, 1, 1, 1, num_parameters, 1, PARAM_INFO, record, &
          status)
 
!     Output Parameter scales
        Call IO_FWRITE_RARRAY (Out_unit, 'parameter_scales', max_parameters, &
          1, 1, 1, num_parameters, 1, SCALE_FACTORS, record, status)
 
!     Output Parameter values
        Call IO_FWRITE_CARRAY (Out_unit, 'parameter_names', max_parameters, 1, &
          num_parameters, PARNAMES, record, status)
 
        If (num_results .Gt. 0) Then
 
!        Output Result names
           Call IO_FWRITE_CARRAY (Out_unit, 'result_names', max_results, 1, &
             num_results, RESNAMES, record, status)
 
        End If
 
!     Write end item
        Call IO_FWRITE_STRING (Out_unit, '$FFF_END', 'End of parameter data', &
          record, status)
 
!     Close output file
        Close (Out_unit)
 
     End If
 
     End Subroutine F2D_OUT_PARAMETERS
!********1*********2*********3*********4*********5*********6*********7*********8

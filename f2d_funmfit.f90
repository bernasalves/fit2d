!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_funmfit.f90 *
!  *                 *
!  *******************
 
!+ F2D_FUNMFIT - FIT2D: calculate FUNction for FIT2D model fitting
     Subroutine F2D_FUNMFIT (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, xstrelm, &
       ystrelm, xendelm, yendelm, max_parameters, num_parameters, PARAM_INFO, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
!  Description:
!    Calculates model function for "MA_MODELFIT". The parameter
!    order must not be altered.
!  Keywords:
!    Function.Fit, Fit~Function.Calculation
!  Method:
!    Other necessary parameters are passed through COMMOM
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.2 Add "DATA" to argument list (Hammersley)
!    05-Dec-1996: V0.1 Original, based on "F2D_FUNFIT2D" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_lsqmfit.inc' ! Fitting Common
     Include 'f2d_fit2d.inc' ! Main array pointers
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc") (Not used)
     Logical, Intent(IN) :: mask_data ! .True., if a data mask exists
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The observed data to be
!      fitted
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! If "mask_data" is
!      .True. then this array defines masked-off elements
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_parameters ! Dimension of "PARAM_INFO" array
     Integer, Intent(IN) :: num_parameters ! Number of "PARAM_INFO" parameters
!      for the model
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Special control
!      parameters to pass to the model calculating routine
     Integer, Intent(IN) :: max_par ! Dimension of parameters array
     Integer, Intent(IN) :: num_par ! Number of parameter values
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters
     Integer, Intent(IN) :: xmaxmod ! First dimension of "MODEL" array
     Integer, Intent(IN) :: ymaxmod ! Second dimension of "MODEL" array
     Logical, Intent(IN) :: model_align ! .True., if the model is to be
!      aligned to the data, in which case "xmaxmod" must be at least
!      "xendelm" and "ymaxmod" must be at least "yendelm".
!      Otherwise only "xendelm - xstrelm + 1" by
!      "yendelm - ystrelm + 1" elements are used
!  Export:
     Integer, Intent(OUT) :: mod_stat ! Return status variable:
!      0 = Good status
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod) ! The calculated model values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: feature ! Loop variable for features
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Initialise the fit model to zero
     Call MA_RVALUE (xmaxmod, 1, xstrelm, 1, xendelm, 1, 0.0, MODEL, status)
 
!  Add each feature to the fit
     Do feature = 1, f2d_num_1dfeatures
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Feature = '', i)') feature
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Call F2D_1DFEATURE (f2d_x_pixel_size, feature, max_par, PARAMS, &
          PARAM_INFO, num_par, xmaxmod, xstrelm, xendelm, f2d_1dorder, &
          f2d_min_poly, f2d_max_poly, %val(pXAXIS), MODEL, status)
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
     End Subroutine F2D_FUNMFIT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

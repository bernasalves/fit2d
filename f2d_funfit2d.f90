!********1*********2*********3*********4*********5*********6*********7********8
 
!  ********************
!  *                  *
!  * f2d_funfit2d.f90 *
!  *                  *
!  ********************
 
!+ F2D_FUNFIT2D - FIT2D: calculate FUNction for FIT2D model fitting
     Subroutine F2D_FUNFIT2D (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_parameters, num_parameters, &
       PARAM_INFO, max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, &
       mod_stat, MODEL, status)
!  Description:
!    Calculates model function for "MA_MODELFIT". The parameter order must not 
!    be altered.
!  Keywords:
!    Function.Fit, Fit~Function.Calculation
!  Method:
!    Other necessary parameters are passed through COMMOM
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.7 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.6 Add "DATA" to argument list (Hammersley)
!    01-Dec-1996: V0.5 Change 2-D fitting polynomial to a Chebyshev polynomial 
!      (Hammersley)
!    16-Nov-1996: V0.4 Converted from "F2D_LSQFUN" to be called from 
!      "MA_MODELFIT" (Hammersley)
!    28-Feb-1995: V0.3 Change mask elements to single bytes (Hammersley)
!    24-Jan-1995: V0.2 "F2D_FEATURE" needs pixel sizes passed to it (Hammersley)
!    04-Feb-1993: V0.1 Original, based on "FIT2OBFUN" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_lsqfit2d.inc' ! Fitting Common
     Include 'f2d_fit2d.inc' ! Main array pointers
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: mask_data ! .True., if a data mask exists
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The observed data to be fitted
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! If "mask_data" is .True.
!      then this array defines masked-off elements
     Integer, Intent(IN) :: xstrelm
     Integer, Intent(IN) :: ystrelm
     Integer, Intent(IN) :: xendelm
     Integer, Intent(IN) :: yendelm
     Integer, Intent(IN) :: max_parameters ! Dimension of "PARAM_INFO" array
     Integer, Intent(IN) :: num_parameters ! Number of PARAM_INFO parameters
!      for the model
     Integer, Intent(IN) :: PARAM_INFO(max_parameters) ! Special control
!      parameters to pass to the model calculating routine
     Integer, Intent(IN) :: max_par ! Dimension of parameters array
     Integer, Intent(IN) :: num_par ! Number of parameter values
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters:
     Integer, Intent(IN) :: xmaxmod ! First dimension of "MODEL" array
     Integer, Intent(IN) :: ymaxmod ! Second dimension of "MODEL" array
     Logical, Intent(IN) :: model_align ! .True., if the model is to be
!      aligned to the data, in which case "xmaxmod" must be at least
!      "xendelm" and "ymaxmod" must be at least "yendelm".
!      Otherwise only "xendelm - xstrelm + 1" by "yendelm - ystrelm + 1" 
!      elements are used
!  Export:
     Integer, Intent(OUT) :: mod_stat ! Return status variable:
!      0 = Good status
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod) ! The calculated model values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Integer :: feature ! Loop variable for features
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Initialise the fit model to zero
     Call MA_RVALUE (xmaxmod, ymaxmod, xstrelm, ystrelm, xendelm, yendelm, &
       0.0, MODEL, status)
 
!  Add each feature to the fit
     Do feature = 1, f2d_num_features
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Feature = '', i)') feature
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Call F2D_FEATURE (experiment, feature, &
          max_par, PARAMS, PARAM_INFO, num_par, xmaxmod, ymaxmod, xstrelm, &
          ystrelm, xendelm, yendelm, f2d_x_order, f2d_y_order, f2d_xmin_poly, &
          f2d_ymin_poly, f2d_xmax_poly, f2d_ymax_poly, %val(pXAXIS), &
          %val(pYAXIS), MODEL, status)
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
     End Subroutine F2D_FUNFIT2D
!********1*********2*********3*********4*********5*********6*********7********8
 
 
 
 

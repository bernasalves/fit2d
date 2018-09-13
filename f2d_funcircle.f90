!********1*********2*********3*********4*********5*********6*********7**
 
!  *********************
!  *                   *
!  * f2d_funcircle.f90 *
!  *                   *
!  *********************
 
!+ F2D_FUNCIRCLE - FIT2D: FUNCTION residuals (CIRCLE) calculation
     Subroutine F2D_FUNCIRCLE (EXPERIMENT, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
!  Description:
!    Calculates model function for 'MA_MODELFIT'. The parameter
!    order must not be altered.
!  Keywords:
!    Function.Fit.Circle, Fit~Function.Calculation
!  Method:
!    Other necessary parameters are passed through COMMOM
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    15-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.5 Add "DATA" to argument list (Hammersley)
!    22-Oct-1996: V0.4 Changes to call from "MA_MODELFIT" (Hammersley)
!    19-Oct-1996: V0.3 Convert to using "MA_MODELFIT" (Hammersley)
!    26-Jan-1995: V0.2 Change of names of coordinate arrays (Hammersley)
!    17-Aug-1994: V0.1 Original, based on "F2D_LSQFUN" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'f2d_fitcircle.inc' ! Circle least squares fitting common
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
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
     Integer, Intent(IN) :: max_control ! Dimension of "CONTROL" array
     Integer, Intent(IN) :: num_control ! Number of control parameters for
!      the model
     Integer, Intent(IN) :: CONTROL(max_control) ! Special control
!      parameters to pass to the model calculating routine
     Integer, Intent(IN) :: max_par ! Dimension of parameters array
     Integer, Intent(IN) :: num_par ! Number of parameter values
     Real, Intent(IN) :: PARAMS(num_par) ! The function parameters
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
     Real, Intent(OUT) :: MODEL(xmaxmod, ymaxmod) ! The calculated model
!      values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7--
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Parameters'')')
!  Write (*, '(3g14.7)') (PARAMS(par), par = 1, 3)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Define residuals
     Do coordinate = xstrelm, xendelm
        MODEL(coordinate, 1) = Sqrt( (F2D_X_COORDINATES(coordinate) - &
          PARAMS(1))**2 + (F2D_Y_COORDINATES(coordinate) - PARAMS(2))**2) - &
          PARAMS(3)
     End Do
 
!  Set good return status
     mod_stat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(6g14.7)') (MODEL(coordinate, 1), coordinate =
!  :    xstrelm, xendelm)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FUNCIRCLE
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 

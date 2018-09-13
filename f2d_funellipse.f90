!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_funellipse.f90 *
!  *                    *
!  **********************
 
!+ F2D_FUNELLIPSE - FIT2D: FUNCTION residuals (ELLIPSE) calculation
     Subroutine F2D_FUNELLIPSE (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
!  Description:
!    Calculates model function for "MA_MODELFIT". The parameter
!    order must not be altered.
!  Keywords:
!    Function.Fit.Ellipse, Fit~Function.Calculation
!  Method:
!    Other necessary parameters are passed through COMMOM. Rotate
!    the coordinates by "-angle1", about the centre, transform
!    coordinates to equivalent circular geometry, then calculate
!    distances from centre of circle to rotated coordinates.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.3 Add "DATA" to argument list (Hammersley)
!    22-Oct-1996: V0.2 Convert to "MA_MODELFIT" (Hammersley)
!    17-Aug-1994: V0.1 Original, based on "F2D_LSQFUN" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic status constants
     Include 'f2d_fitcircle.inc' ! Ellipse least squares fitting common
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
     Integer, Intent(IN) :: max_control ! Dimension of "CONTROL" array
     Integer, Intent(IN) :: num_control ! Number of control parameters for
!      the model
     Integer, Intent(IN) :: CONTROL(max_control) ! Special control
!      parameters to pass to the model calculating routine
     Integer, Intent(IN) :: max_par ! Dimension of parameters array
     Integer, Intent(IN) :: num_par ! Number of parameter values
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters
!      (1) = x_centre
!      (2) = y_centre
!      (3) = radius1
!      (4) = radius2
!      (5) = angle1
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
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Real :: angle1 ! Rotation angle from X-axis to axis 1 of ellipse
     Real :: cos_angle1 ! The cosine of "-angle1"
     Real :: radius1 ! Ellipse axis 1 radius
     Real :: radius2 ! Ellipse axis 2 radius
     Real :: sin_angle1 ! The sine of "-angle1"
     Real :: x_centre ! X-centre of ellipse
     Real :: x_transform ! Transformed X-coordinates
     Real :: y_centre ! Y-centre of ellipse
     Real :: y_transform ! Transformed Y-coordinates
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  "Decode" variables
     x_centre = PARAMS(1)
     y_centre = PARAMS(2)
     radius1 = PARAMS(3)
     radius2 = PARAMS(4)
     angle1 = PARAMS(5)
 
!  Define rotation cosine and sine
     cos_angle1 = Cos (-angle1)
     sin_angle1 = Sin (-angle1)
 
!  Define residuals
     Do coordinate = xstrelm, xendelm
 
!     Rotate coordinates by "-angle1", such that the transformed
!     coordinates are compared to an ellipse whose axis "radius1"
!     is on the X-axis. The rotated Y-coordinate is then scaled by
!     "radius1/radius2" to be fitted to a circle of radius "radius1"
        x_transform = cos_angle1 * (F2D_X_COORDINATES(coordinate) - x_centre) &
          - sin_angle1 * (F2D_Y_COORDINATES(coordinate) - y_centre)
        y_transform = sin_angle1 * (F2D_X_COORDINATES(coordinate) - x_centre) &
          + cos_angle1 * (F2D_Y_COORDINATES(coordinate) - y_centre)
 
!     Calculate distance from transformed coordinate to circle
        MODEL(coordinate, 1) = (Sqrt( x_transform**2 / radius1**2 + &
          y_transform**2 / radius2**2) - 1.0) * radius1
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(5e12.5)') x_centre, y_centre, radius1, radius2,
!  :    angle1
!  Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FUNELLIPSE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

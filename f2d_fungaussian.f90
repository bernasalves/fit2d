!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_fungaussian.f90 *
!  *                     *
!  ***********************
 
!+ F2D_FUNGAUSSIAN - FIT2D: calculate FUNction GAUSSIAN
     Subroutine F2D_FUNGAUSSIAN (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       MODEL, status)
!  Description:
!    Calculates function for "MA_MODELFIT". The parameter order must not be 
!    altered.
!  Keywords:
!    Function.Fit.Gaussian, Fit~Function.Calculation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.4 Add "DATA" to argument list (Hammersley)
!    22-Oct-1996: V0.3 Convert to using "MA_MODELFIT" (Hammersley)
!    26-Apr-1996: V0.2 Explicit conversion of double precision
!      variables to real for stricter Ultrix compiler (Hammersley)
!    06-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
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
     Real, Intent(IN) :: PARAMS(max_par) ! The variable parameters of the fit 
!      function
!        (1) = x_centre
!        (2) = y_centre
!        (3) = intensity
!        (4) = sigma1
!        (5) = sigma2
!        (6) = angle
!        (7) = background
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
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Real, Parameter :: Limit = 4.5 ! The number of sigma, beyond which the
!      value of the Gaussian is assumed to equal zero (this will not overide
!      inbuilt limits). "limit" must be greater than zero.
!  Local Variables:
!    Integer par
     Integer :: x ! Loop variable for X-direction
     Integer :: x_offset ! Offset between data elements and model
!      elements in the X-direction
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_offset ! Offset between data elements and model
!      elements in the Y-direction
     Real :: angle ! Orientation angle of first axis of Gaussian
     Real :: background ! Background value
     Real :: cos_angle ! Cosine of "angle"
     Real :: intensity ! Maximum intensity of Gaussian
     Real :: rsigma ! Larger scaled sigma value
     Real :: sigma1 ! First axis standard deviation of Gaussian
     Real :: sigma2 ! Second axis standard deviation of Gaussian
     Real :: sin_angle ! Sine of "angle"
     Real :: x_centre ! X-centre of ellipse
     Real :: xgau ! Rotated X-coordinate
     Real :: xpos ! Relative X-coordinate
     Real :: y_centre ! Y-centre of ellipse
     Real :: ygau ! Rotated Y-coordinate
     Real :: ypos ! Relative Y-coordinate
!  Local Arrays:
!  Local Data:
!  External Functions:
     Real, External :: Ma_gaussian ! Calculates value on Gaussian
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FUNGAUSSIAN'')')
!  Write (*, '(''xstrelm, ystrelm, xendelm, yendelm = '', 4i6)')
!  :    xstrelm, ystrelm, xendelm, yendelm
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  "Decode" variables
     x_centre = PARAMS(1)
     y_centre = PARAMS(2)
     intensity = PARAMS(3)
     sigma1 = Max(0.001, Real(PARAMS(4)))
     sigma2 = Max(0.001, Real(PARAMS(5)))
     angle = PARAMS(6)
     background = PARAMS(7)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(7g11.4)') (PARAMS(par), par = 1, 7)
!  Write (*, '(''x_centre, y_centre = '', 2g)') x_centre, y_centre
!  Write (*, '(''intensity, angle = '', 2g)') intensity, angle
!  Write (*, '(''sigma1, sigma2 = '', 2g)') sigma1, sigma2
!  Write (*, '(''background = '', g)') background
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Initialise variables
     cos_angle = Cos(angle)
     sin_angle = Sin(angle)
 
!  Set offset between model and data
     If (model_align) Then
        x_offset = 0
        y_offset = 0
     Else
        x_offset = 1 - xstrelm
        y_offset = 1 - ystrelm
     End If
 
!  Loop through active data region, which contains the source
     Do y = ystrelm, yendelm
 
        ypos = Real(y) - 0.5 - y_centre
        Do x = xstrelm, xendelm
 
           xpos = Real(x) - 0.5 - x_centre
 
!        Convert (x,y) coordinate to Gaussian coordinates
           xgau = xpos * cos_angle + ypos * sin_angle
           ygau = -xpos * sin_angle + ypos * cos_angle
 
           rsigma = Max(Abs(xgau) / sigma1, Abs(ygau) / sigma2)
 
           If (rsigma .Le. Limit) Then
 
!           The position is within "Limit" sigma of the centre, so
!           need to calculate the value
              MODEL(x + x_offset, y + y_offset) = background + intensity * &
                Ma_gaussian(Abs(xgau) / sigma1, 1, status) * &
                Ma_gaussian(Abs(ygau) / sigma2, 1, status)
 
           Else
              MODEL(x + x_offset, y + y_offset) = background
           End If
 
        End Do
 
     End Do
 
     mod_stat = 0
 
     End Subroutine F2D_FUNGAUSSIAN
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_funpowder.f90 *
!  *                   *
!  *********************
 
!+ F2D_FUNPOWDER - FIT2D: FUNCTION residuals (POWDER ring) calculation
     Subroutine F2D_FUNPOWDER (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
!  Description:
!    Calculates objective function for "MA_MODELFIT". The
!    parameter order must not be altered.
!  Keywords:
!    Function.Fit.Powder, Fit~Function.Calculation
!  Method:
!    Other necessary parameters are passed through COMMOM. Rotate
!    the coordinates by "-tilt_plane_rotation", about the centre,
!    transform coordinates to equivalent circular geometry, then
!    calculate distances from powder ring to rotated coordinates.
!
!    See 15:8
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.10 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.9 Add "DATA" to argument list (Hammersley)
!    15-Nov-1996: V0.8 Convert to call from "MA_MODELFIT" (Hammersley)
!    22-Aug-1996: V0.7 Add option to not fit tilt (Hammersley)
!    10-Feb-1996: V0.6 Debugging very long distance instability (Hammersley)
!    19-Feb-1995: V0.5 Add option of weighted fitting (Hammersley)
!    27-Jan-1995: V0.4 Change of names of common block variables (Hammersley)
!    26-Jan-1995: V0.3 Simultaneous fitting of several powder rings (Hammersley)
!    30-Aug-1994: V0.2 Convert internal calculations to double precision 
!      (Hammersley)
!    23-Aug-1994: V0.1 Original, based on "F2D_LSQFUN" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_fitrings.inc' ! Powder least squares fitting coordinate data
     Include 'f2d_lsqpowder.inc' ! Powder ring fitting control parameters
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc") (Not used)
     Logical, Intent(IN) :: mask_data ! .True., if a data mask exists
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The observed data to be
!      fitted
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! If "mask_data" is .True.
!      then this array defines masked-off elements
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
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters:
!      x_beam
!      y_beam
!      distance
!      tilt_plane_rotation
!      tilt_angle
!      ANGLE_CONES(1)
!      ANGLE_CONES(2)
!      etc.
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
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod) ! The calculated
!      residual values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
!  Local Variables:
     Double Precision :: cos_rotation ! The cosine of "-tilt_plane_rotation"
     Double Precision :: cos_tilt_sqr ! Square of cosine of "tilt_angle"
     Double Precision :: radius ! "Radius" of cone
     Double Precision :: reference_weight ! Weighting value of first
!      coordinate on first ring
     Double Precision :: tilt_plane_rotation ! Rotation angle of
!      detector tilt plane
     Double Precision :: sample_distance ! Distance from sample to
!      detector (beam centre) in pixels
     Double Precision :: sin_rotation ! The sine of "-rotation"
     Double Precision :: sin_tilt ! Sine of "tilt_angle"
     Double Precision :: sin_tilt_sqr ! Square of sine of "tilt_angle"
!    Double Precision sumsq ! Sum of squares of residuals (DEBUG)
     Double Precision :: tan_cone_sqr ! Square of tangent of "angle_cone"
     Double Precision :: term_c ! Constant term in formula for the
!      intersection of a cone with an inclined plane
     Double Precision :: term_x ! The term in x, of the formula for the
!      intersection of a cone with an inclined plane
     Double Precision :: term_xx ! The term in x^2, of the formula for the
     Double Precision :: tilt_angle ! Tilt of detector from orthoganal to
!      beam intersection of a cone with an inclined plane
     Double Precision :: weighting ! Weighting to apply to a residual
     Double Precision :: x_beam ! X-centre beam on detector
     Double Precision :: x_transform ! Transformed X-coordinates
     Double Precision :: y_beam ! Y-centre of beam on detector
     Double Precision :: y_transform ! Transformed Y-coordinates
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: residual ! Loop variable for the residual values
     Integer :: ring ! Loop variable for the powder rings
!  Local Arrays:
     Double Precision :: ANGLE_CONES(15) ! 2 theta angles of powder rings
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_FUNPOWDER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  "Decode" variables
     x_beam = PARAMS(1)
     y_beam = PARAMS(2)
     sample_distance = PARAMS(3)
     tilt_plane_rotation = PARAMS(4)
     tilt_angle = PARAMS(5)
 
!  Diffraction cone angles
     Do ring = 1, f2d_num_rings
        ANGLE_CONES(ring) = PARAMS(ring + 5)
     End Do
 
!  Calculate reference weight
     reference_weight = Sqrt(F2D_RINTENSITIES(1, 1) / f2d_detector_gain)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(
!  :    ''x/y beam, tilt_plane_rotation, tilt_angle = '', 2f8.2,
!  :    2f8.3)')
!  :    x_beam*1000.0, y_beam*1000.0, tilt_plane_rotation*180.0/Pi,
!  :    tilt_angle*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Define rotation cosines and sines
     cos_rotation = Cos (-tilt_plane_rotation)
     sin_rotation = Sin (-tilt_plane_rotation)
     cos_tilt_sqr = (Cos(tilt_angle))**2
     sin_tilt = Sin(tilt_angle)
     sin_tilt_sqr = sin_tilt**2
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''term_c, term_xx, term_x = '', 3g)')
!  :    term_c, term_xx, term_x
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  sumsq = 0.0
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Define residuals
     residual = 0
     Do ring = 1, f2d_num_rings
 
!     Define common values
        tan_cone_sqr = (Tan(ANGLE_CONES(ring)))**2
        radius = sample_distance * Tan(ANGLE_CONES(ring))
        term_c = sample_distance**2 * tan_cone_sqr
        term_xx = sin_tilt_sqr * tan_cone_sqr  - cos_tilt_sqr
        term_x = 2.0 * sample_distance * sin_tilt * tan_cone_sqr
 
        Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
           residual = residual + 1
 
!        Rotate coordinates by "-tilt_plane_rotation", such that the
!        transformed coordinates are compared to an powder ring whose
!        tilt plane is in the Y=0 plane.
           x_transform = cos_rotation * (F2D_X_RCOORDINATES(coordinate, ring) &
             - x_beam) - sin_rotation * (F2D_Y_RCOORDINATES(coordinate, ring) &
             - y_beam)
           y_transform = sin_rotation * (F2D_X_RCOORDINATES(coordinate, ring) &
             - x_beam) + cos_rotation * (F2D_Y_RCOORDINATES(coordinate, ring) &
             - y_beam)
 
!        Calculate distance from transformed coordinate to circle
           If (f2d_weighted_fitting) Then
              weighting = Sqrt(F2D_RINTENSITIES(coordinate, ring) / &
                f2d_detector_gain) / reference_weight
 
              RESIDUALS(residual, 1) = weighting * Sqrt(-term_xx * &
                x_transform**2 - term_x * x_transform + y_transform**2) - &
                radius
 
           Else
              RESIDUALS(residual, 1) = Sqrt(-term_xx * x_transform**2 - term_x &
                * x_transform + y_transform**2) - radius
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''coordinate, x/y_transform, residual = '',
!        :          i3, 3f12.3)') coordinate,
!        :          x_transform*1000.0, y_transform*1000.0,
!        :          RESIDUALS(residual, 1)
!        sumsq = sumsq + RESIDUALS(residual, 1)**2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Sum of squares = '', 1pe12.5)') sumsq
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FUNPOWDER
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

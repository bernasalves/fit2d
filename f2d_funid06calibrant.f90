!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_funid06calibrant.f90 *
!  *                          *
!  ****************************
 
!+ F2D_FUNID06CALIBRANT - FIT2D: FUNCTION residuals (CALIBRANT powder rings) 
!    calculation
     Subroutine F2D_FUNID06CALIBRANT (experiment, mask_data, &
       xmaxdat, ymaxdat, DATA, MASK, &
       xstrelm, ystrelm, xendelm, yendelm, max_control, num_control, CONTROL, &
       max_par, num_par, PARAMS, xmaxmod, ymaxmod, model_align, mod_stat, &
       RESIDUALS, status)
!  Description:
!    Calculates objective function for "MA_MODELFIT". The parameter
!    order must not be altered.
!  Keywords:
!    Function.Fit.Calibrant, Fit~Function.Calculation.Calibrant
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
!    30-Oct-2014: V0.8 Add "detector_offset" parameter and allow beam centre to
!      be different from rotation centre (Hammersley)
!    15-Oct-2014: V0.7 Testing (Hammersley)
!    06-Oct-2014: V0.6 Debugging (Hammersley)
!    26-Sep-2014: V0.5 Debugging (Hammersley)
!    24-Sep-2014: V0.4 Transform polar coordinates to 2-theta values (Hammersley)
!    16-Sep-2014: V0.3 Debugging (Hammersley)
!    12-Sep-2014: V0.2 Change to using polar coordinates directly (Hammersley)
!    05-Sep-2014: V0.1 Original, based on "F2D_FUNCALIBRANT" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_fitrings.inc' ! Powder least squares fitting coordinate data
     Include 'f2d_lsqpowder.inc' ! Calibrant Powder ring fitting
!    control parameters
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
     Real, Intent(IN) :: PARAMS(max_par) ! The function parameters:
!      detector_offset
!      x_beam
!      y_beam
!      distance
!      wavelength
!      tilt_plane_rotation
!      tilt_angle
     Integer, Intent(IN) :: xmaxmod ! First dimension of "MODEL" array
     Integer, Intent(IN) :: ymaxmod ! Second dimension of "MODEL" array
     Logical, Intent(IN) :: model_align ! .True., if the model is to be
!      aligned to the data, in which case "xmaxmod" must be at least
!      "xendelm" and "ymaxmod" must be at least "yendelm".
!      Otherwise only "xendelm - xstrelm + 1" by
!      "yendelm - ystrelm + 1" elements are used
!  Export:
     Integer, Intent(OUT) :: mod_stat ! Return status variable:
!    0 = Good status
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod) ! The calculated model
!      values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Real :: azimuth ! The azimuth of the polar coordinate
     Integer :: coordinate ! Loop variable for coordinates
     Real :: cos_rotation ! The cosine of "-tilt_plane_rotation"
     Real :: cos_tilt_sqr ! Square of cosine of "tilt_angle"
     Real :: detector_distance ! Distance from sample to
!      detector (beam centre) in pixels
     Real :: detector_offset ! Distance from rotation axis to start of detector
!      (metres)
     Real :: radius ! "Radius" of cone
     Real :: reference_weight ! Weighting value of first
!      coordinate on first ring
     Integer :: residual ! Loop variable for the residual values
     Integer :: ring ! Loop variable for the powder rings
     Real :: sin_rotation ! The sine of "-rotation"
     Real :: sin_tilt ! Sine of "tilt_angle"
     Real :: sin_tilt_sqr ! Square of sine of "tilt_angle"
!    Real sumsq ! Sum of squares of residuals (DEBUG)
     Real :: tan_cone_sqr ! Square of tangent of "angle_cone"
     Real :: term_c ! Constant term in formula for the
!      intersection of a cone with an inclined plane
     Real :: term_x ! The term in x, of the formula for the
!      intersection of a cone with an inclined plane
     Real :: term_xx ! The term in x^2, of the formula for the
     Real :: tilt_angle ! Tilt of detector from orthoganal to beam
!      intersection of a cone with an inclined plane
     Real :: tilt_plane_rotation ! Rotation angle of detector tilt plane
     Real :: two_theta ! Two theta opening angle of a diffraction cone
     Real :: wavelength ! Wavelength of experiment (metres)
     Real :: weighting ! Weighting to apply to a residual
     Real :: x_beam ! X-centre beam on detector (metres)
     Real :: x_detector ! X-coordinate of polar coordinate on 2-D detector 
!      (metres)
     Real :: x_transform ! Transformed X-coordinates
     Real :: y_beam ! Y-centre of beam on detector (metres)
     Real :: y_detector ! Y-coordinate of polar coordinate on 2-D detector 
!      (metres)
     Real :: y_transform ! Transformed Y-coordinates
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''ENTERED F2D_FUNID06CALIBRANT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  "Decode" fit parameters
     detector_offset = PARAMS(1)
     x_beam = PARAMS(2)
     y_beam = PARAMS(3)
     detector_distance = PARAMS(4)
     wavelength = PARAMS(5)
     tilt_plane_rotation = PARAMS(6)
     tilt_angle = PARAMS(7)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, &
!       '(''x_beam (mm), y_beam, distance, wavelength(A), rotation(deg), tilt'')')
!     Write (*, '(6g12.3)') x_beam * 1000.0, y_beam * 1000.0, &
!       detector_distance * 1000.0, wavelength * 1.0e10, &
!       tilt_plane_rotation * 180.0 / Pi, tilt_angle * 180.0 / Pi
!     Write (*, '(''detector_gain = '', f12.5)') f2d_detector_gain
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Calculate reference weight
     reference_weight = Sqrt(F2D_RINTENSITIES(1, 1) / f2d_detector_gain)
 
!  Reduce angles to sensible range
     Do While (tilt_plane_rotation .Gt. 2.0 * Pi)
        tilt_plane_rotation = tilt_plane_rotation - (2.0 * Pi)
     End Do
     Do While (tilt_plane_rotation .Lt. (-2.0 * Pi))
        tilt_plane_rotation = tilt_plane_rotation + (2.0 * Pi)
     End Do
     Do While (tilt_angle .Gt. 2.0 * Pi)
        tilt_angle = tilt_angle - (2.0 * Pi)
     End Do
     Do While (tilt_angle .Lt. (-2.0 * Pi))
        tilt_angle = tilt_angle - (2.0 * Pi)
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''tilt_plane_rotation, tilt_angle'')')
!  Write (*, '(2f12.3)')
!  :    tilt_plane_rotation*180.0/Pi,
!  :    tilt_angle*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
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
 
!     Calculate two theta opening angle of cone
        two_theta = 2.0 * Asin (wavelength / (2.0 * F2D_D_SPACINGS(ring)))
 
!     Define common values
        tan_cone_sqr = (Tan(two_theta))**2
        radius = detector_distance * Tan(two_theta)
        term_c = detector_distance**2 * tan_cone_sqr
        term_xx = sin_tilt_sqr * tan_cone_sqr  - cos_tilt_sqr
        term_x = 2.0 * detector_distance * sin_tilt * tan_cone_sqr
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''radius (mm) = '', f12.3)') radius * 1000.0
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
           residual = residual + 1

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x_polar (pix), azimuth (deg) = '', 2f12.3)') &
!             F2D_X_POLAR(coordinate, ring), &
!             F2D_AZIMUTHS(coordinate, ring) * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Calculate Cartesians coordinates relative beam centre
           azimuth = F2D_AZIMUTHS(coordinate, ring)
           x_detector = &
             ((F2D_X_POLAR(coordinate, ring) * experiment%x_pixel_size &
              + detector_offset) * Cos(azimuth)) - x_beam
           y_detector = &
             ((F2D_X_POLAR(coordinate, ring) * experiment%x_pixel_size &
              + detector_offset) * Sin(azimuth)) - y_beam

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x_detector (mm), y_detector (mm) = '', 2f12.3)') &
!             x_detector * 1000.0, y_detector * 1000.0
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!        Rotate coordinates by "-tilt_plane_rotation", such that the
!        transformed coordinates are compared to an powder ring whose
!        tilt plane is in the Y=0 plane.
!           x_transform = cos_rotation * (x_detector - x_beam) - &
!             sin_rotation * (y_detector - y_beam)
!           y_transform = sin_rotation * (x_detector - x_beam) + &
!             cos_rotation * (y_detector - y_beam)
           x_transform = cos_rotation * x_detector - sin_rotation * y_detector
           y_transform = sin_rotation * x_detector + cos_rotation * y_detector
 
!        Calculate distance from transformed coordinate to circle
           If (f2d_weighted_fitting) Then
              weighting = Sqrt(F2D_RINTENSITIES(coordinate, ring) / &
                f2d_detector_gain) / reference_weight
 
              RESIDUALS(residual, 1) = weighting * (Sqrt(-term_xx * &
                x_transform**2 - term_x * x_transform + y_transform**2) - &
                radius)
 
           Else

              RESIDUALS(residual, 1) = 1000.0 * (Sqrt(-term_xx * &
                x_transform**2 - term_x * x_transform + y_transform**2) - &
                radius)
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, &
!             '(''coordinate, x / y_transform, residual = '', i3, 3f12.3)') &
!             coordinate, x_transform * 1000.0, y_transform * 1000.0, &
!             RESIDUALS(residual, 1)
!           Read (*, *)
!        sumsq = sumsq + RESIDUALS(residual, 1)**2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Read (*, *)
!     Write (*, '(''Number residuals = '', i6, '' Sum of squares = '', g14.7)')&
!       residual, sumsq
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FUNID06CALIBRANT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 

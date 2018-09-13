!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_funpolarisation.f90 *
!  *                         *
!  ***************************
 
!+ F2D_FUNPOLARISATION - FIT2D: FUNCTION residuals POLARISATION calculation
     Subroutine F2D_FUNPOLARISATION (experiment, mask_data, &
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
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    27-May-1998: V0.2 Add "DATA" to argument list (Hammersley)
!    09-Jul-1997: V0.1 Original, based on "F2D_FUNCALIBRANT" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'f2d_fitrings.inc' ! Powder least squares fitting coordinate data
     Include 'f2d_lsqpowder.inc' ! Calibrant Powder ring fitting control
!      parameters
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
!      polarisation
!      sample distance (fixed)
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
     Real, Intent(OUT) :: RESIDUALS(xmaxmod, ymaxmod) ! The calculated model
!      values
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: residual ! Loop variable for the residual values
     Integer :: ring ! Loop variable for the powder rings
     Real :: average_intensity ! Average polarisation corrected
!      intensity for a ring
     Real :: cos2_2theta ! The square of the cosine of the 2-theta angle
     Real :: polarisation ! Polarisation, defined as
!      (I_h - I_v) / (I_h + I_v), where horizontal should normally
!      correspond to the X-direction of the image
     Real :: polarisation_factor ! Correction factor owing to the
!      effect of polarisation
     Real :: sample_distance ! Distance from the sample to the detector
!      (beam centre) (in metres)
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_FUNPOLARISATION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  "Decode" fit parameters
     polarisation = PARAMS(1)
     sample_distance = PARAMS(2)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''polarisation = '', g14.5)') polarisation
!  Write (*, '(''sample_distance = '', g14.5)') sample_distance
!  Write (*, '(''F2D_RADIA(1) = '', g14.5)') F2D_RADIA(1)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Define residuals
     residual = 0
     Do ring = 1, f2d_num_rings
 
        cos2_2theta = sample_distance**2 / &
          (sample_distance**2 + F2D_RADIA(ring)**2)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''cos2_2theta = '', g14.5)') cos2_2theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
           residual = residual + 1
 
!        Calculate corrected intensity value
           polarisation_factor = 0.5 * ( 1.0 + cos2_2theta - polarisation * &
             Cos(2.0 * F2D_AZIMUTHS(coordinate, ring)) * (1.0 - cos2_2theta))
 
           RESIDUALS(residual, 1) = F2D_RINTENSITIES(coordinate, ring) / &
             polarisation_factor
 
        End Do
 
!     Calculate average corrected intensity
        residual = residual - F2D_NUM_RCOORDINATES(ring)
        average_intensity = 0.0
        Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
           residual = residual + 1
           average_intensity = average_intensity + RESIDUALS(residual, 1)
        End Do
 
        average_intensity = average_intensity / &
          Real(F2D_NUM_RCOORDINATES(ring))
 
!     Subtract average corrected intensity from each corrected intensity value
        residual = residual - F2D_NUM_RCOORDINATES(ring)
        Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
           residual = residual + 1
           RESIDUALS(residual, 1) = RESIDUALS(residual, 1) - average_intensity
        End Do
 
     End Do
 
!  Set good return status
     mod_stat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''residual = '', i6)') residual
!  Write (*, '(''Sum of squares = '', g14.7)') sumsq
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FUNPOLARISATION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 

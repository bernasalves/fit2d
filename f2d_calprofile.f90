!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_calprofile.f90 *
!  *                    *
!  **********************
 
!+ F2D_CALPROFILE: CALculate PROFILE for correcting flat
!    field measurements
     Subroutine F2D_CALPROFILE (lower_range, upper_range, max_order, order, &
       POLYNOMIAL, distance, x_pixel_size, absorption, max_pixels, PROFILE, &
       status)
!  Description:
!    Calculates the reciprocal of the expected radial profile on the detector 
!    given the source distribution (as a polynomial), and the geometry of the 
!    calibration measurement. The resulting 1-D profile may be multipled with 
!    the measurement flat field taking into account the distance from the 
!    centre. Also corrects for extra off-axis absorption if on-axis absorption
!    is present
!  Keywords:
!    Calculate.Profile
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    01-Dec-1996: V0.6 Extra argument for "MA_CAL_POLNOMIAL" (Hammersley)
!    16-Nov-1996: V0.5 Convert to using "MA_CAL_POLYNOMIAL",
!      and hence single precision (Hammersley)
!    17-Feb-1996: V0.4 Correction: Take into account obliqueness of
!      flat detector to isotropic point source emission, compared to
!      the 2-theta scan which not only is at a constant distance, but
!      is always facing the source (Hammersley)
!    04-Jul-1994: V0.3 Extrapolate values beyond range of
!      polynomial (set to same value as lower or upper limit) (Hammersley)
!    10-Apr-1994: V0.2 Include effect of extra off-axis absorption (Hammersley)
!    03-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: lower_range ! Lower limit of calculated polynomial
     Real, Intent(IN) :: upper_range ! Upper limit of calculated polynomial
     Integer, Intent(IN) :: max_order ! Maximum order of polynomial which
!      may be fitted
     Integer, Intent(IN) :: order ! Order of fitted polynomial
     Real, Intent(IN) :: POLYNOMIAL(max_order + 1) ! Coefficients of
!      polynomial of required order
     Real, Intent(IN) :: distance ! Distance from source to detector for
!      flat field measurement
     Real, Intent(IN) :: x_pixel_size ! Size of pixel in metres in X-direction
     Real, Intent(IN) :: absorption ! Fractional on-axis absorption
     Integer, Intent(IN) :: max_pixels ! Dimension size of pixel correction
!      array
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PROFILE(0: max_pixels) ! Reciprocal of expected
!      profile on the detector. The flat field can be multiplied by this array
!      at the correct pixel distance from the centre of the flat field
!      measurement
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Real :: value ! Temporary storage
     Integer :: pixel ! Loop variable for pixels
     Logical :: bad_angle ! .True., if the detector subtends a greater angle 
!      than the source calibration scan range
     Real :: absorption_corr ! Correction for relative absorption for off-axis 
!      pixels
     Real :: angle ! Angle for calculating relative source strength
     Real :: extra_depth ! Fraction extra absorption depth for off-axis pixels
     Real :: geometric_correction ! Correction for a flat detector relative to a
!      2-theta scan. As the 2-theta scan is at a constant distance there is a 
!      1/r**2 correction for distance plus a 1/Cos(angle) correction for 
!      obliqueness. In total this is a 1/Cos**3(angle) correction
     Real :: ref_source ! Reference strength of source distribution at 0.0 
!      degrees (for normalisation)
     Real :: off_axis_trans ! The transmission for an off-axis pixel
!    Real r_squared ! The relative strength at a pixel owing to
!      1/r**2 fall-off
     Real :: radial_distance ! Distance on detector from centre
     Real :: rel_source ! The relative strength of the source at a pixel
     Real :: transmission ! On-axis transmission (1.0 - "absorption")
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALPROFILE ' // Version)
     Else
 
!     Initialise variables
        transmission = 1.0 - absorption
 
!     Calculate value of source at 0.0 degrees (for normalisation)
        Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, order + 1, &
          order, POLYNOMIAL, 1, 1, 0.0, value, status)
 
        ref_source = Real(value)
 
!     Calculate reciprocal of expected profile for each pixel
        bad_angle = .False.
        Do pixel = 0, max_pixels
 
!        Calculate angle at pixel
           radial_distance = Real(pixel) * x_pixel_size
           angle = Atan2(radial_distance, distance)
           extra_depth = 1.0 / Cos(angle)
 
           If (absorption .Gt. 1.0e-6) Then
 
!           Calculate extra fractional absorption depth owing to off-axis 
!           position
 
!           Calculate off-axis transmission
              off_axis_trans = transmission**extra_depth
 
!           Calculate absorption correction
              absorption_corr = (1.0 - off_axis_trans) / absorption
 
           Else
              absorption_corr = 1.0
           End If
 
!        Convert angle to degrees
           angle = angle * 180.0 / Pi
 
!        Check that angle is within scanned range, if not note problem
           If (angle .Lt. lower_range .Or. angle .Gt. upper_range) Then
              bad_angle = .True.
           End If
 
!        Calculate value of source at angle
           Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, order + &
             1, order, POLYNOMIAL, 1, 1, angle, value, status)
           rel_source = Real(value) / ref_source
 
!        Calculate reduction in intensity owing to 1/r**2 fall-off with 
!        distance and obliqueness of flat detector relative to a 2-theta scan
           geometric_correction = extra_depth**3
 
!        Calculate reciprocal of expected relative source intensity
           PROFILE(pixel) = geometric_correction * absorption_corr / &
             rel_source
 
!        OLD-CODE
!        r_squared = (distance**2 + radial_distance**2) /
!        :          distance**2
 
!        Calculate reciprocal of expected relative source intensity
!        PROFILE(pixel) = r_squared * absorption_corr / rel_source
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Pixel = '', i4, '' intensity = '' f12.7)')
!        :          pixel, PROFILE(pixel)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''radial_distance = '' f12.7)') radial_distance
!     Write (*, '(''angle = '' f12.7)') angle
!     Write (*, '(''rel_source = '' f12.7)') rel_source
!     Write (*, '(''r_squared = '' f12.7)') r_squared
!     Write (*, '(''absorption_corr = '' f12.7)') absorption_corr
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End If
 
     End Subroutine F2D_CALPROFILE
!********1*********2*********3*********4*********5*********6*********7**
 
 
 

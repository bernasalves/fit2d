!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_tiltradial.f90 *
!  *                    *
!  **********************
 
!+ F2D_TILTRADIAL - FIT2D: (TILTED detector) RADIAL profile from 2-D image
     Subroutine F2D_TILTRADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, variances_exist, experiment, &
       lorentz_geometry, angular_scan, radial_pixel_size, &
       max_radial, num_radial, RAD_AXIS, PROFILE, PROVARS, NUMPIXELS, status)
!  Description:
!    Calculates the average radial profile given the values in
!    "DATA" in the region from "(xstrelm, ystrelm)" to
!    "(xendelm, yendelm)". If "angular_scan = .True." then a scan
!    with equal angle size is calculated, otherwise a scan based on
!    equal radial distance on an orthogonal detector is calculated.
!    Data values are not to be included if they are "mask-off"
!    which is defined by .True. in the corresponding element of
!    "MASK". The profile is calculated from the centre given by
!    the coordinate "(x_beam, y_beam)" and allows for detector tilt,
!    which is defined as a tilt angle of "tilt" in a plane which is
!    at a rotation angle of 'rotation' from the X-axis (in the ideal
!    detector plane). The image pixels are assumed to be regular rectangles.
!  Keywords:
!    Radial~Profile.Calculation, Calculate.Radial~Profile,
!    Profile.Radial.Calculation
!  Method:
!    The radial distance of the centre of each data pixel is
!    calculated, taking into account the detector tilt. First each
!    pixel is rotated to the "tilt plane", and then the radial
!    distance for the rotated pixel position is calculated, taking
!    into account the tilt (see 15:8).
!
!    Assuming the pixel to be aligned to the radial direction
!    (approximation), the start and end position of output pixels
!    "covered" by the input pixel is calculated. The intensity in the
!    input pixel is distributed in proportion to coverage of output
!    pixels. The fraction of input pixels added to each output is
!    recorded. At the end the counts in all bins are normalised by the
!    fractional number of contributing input bins.
!  Deficiencies:
!    The manner in which the counts from different input pixels are
!    divided up into different output pixels does not take account of
!    the pixel orientation.
!
!    Doesn't (yet) take into account differing number of
!    independent results when calculating variances.
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    15-Jul-1997: V0.10 Correction to polarisation factor as a function of 
!      azimuth (Hammersley)
!    16-Dec-1996: V0.9 Avoid open strings crossing lines (Hammersley)
!    23-Oct-1995: V0.8 Change to argument list (Hammersley)
!    12-Sep-1995: V0.7 Protect against completetly on-axis position
!      leading to NaN in polarisation calculation (Hammersley)
!    01-Sep-1995: V0.6 Option of applying polarisation correction (Hammersley)
!    28-Aug-1995: V0.5 Correct for polarisation (Hammersley)
!    28-Feb-1995: V0.4 Make mask elements single bytes (Hammersley)
!    22-Feb-1995: V0.3 Option of 2 theta scan (Hammersley)
!    19-Jan-1995: V0.2 Take into account non-square pixels (Hammersley)
!    28-Sep-1994: V0.1 Original, based on "MA_RADIAL" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! 1st dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: ymaxdat ! 2nd dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: xstrelm ! X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Logical, Intent(IN) :: angular_scan ! .True., if the 1-D scan is to be
!      produced in equal angle elements as opposed to equal radial distance
!    elements
     Real, Intent(IN) :: radial_pixel_size ! Size of pixel to be used for
!      the radial 1-D histogram. If an equal radial distance scan is to
!      be calculated ("angular_scan = .False.") the units are
!      metres, and for an equal angle pixel scan ("angular_scan = .True.") the 
!      units are radians
     Integer, Intent(IN) :: max_radial ! Dimension of array "RAD_AXIS",
!      "PROFILE" and "NUMPIXELS"
!  Export:
     Integer, Intent(OUT) :: num_radial ! Number of defined radial profile
!      pixels
     Real, Intent(OUT) :: RAD_AXIS(max_radial) ! The world coordinate
!      positions for which the radial profile is to be calculated
     Real, Intent(OUT) :: PROFILE(max_radial) ! The averaged radial profile
     Real, Intent(OUT) :: PROVARS(max_radial) ! The estimated variance at
!      each radial profile position (only if "variances_exist" is .True.)
     Real, Intent(OUT) :: NUMPIXELS(max_radial) ! The number of pixels used
!      to average each radial profile pixel
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: high_pixel ! Pixel number of highest output pixel effected
!      by an input pixel
     Integer :: low_pixel ! Pixel number of lowest output pixel effected
!      by an input pixel
     Integer :: pixel ! Pixel number in radial profile
     Integer :: x ! X pixel number
     Integer :: y ! Y pixel number
     Real :: abs_azimuth ! The absolute azimuthal angle of the pixel
     Real :: cos_rotation ! Cosine of minus the rotation angle
     Real :: cos_tilt_sqr ! Square of the Cosine of the detector tilt angle
     Real :: cos2_2theta ! Square of the cosine of the 2-theta angle
     Real :: fraction ! Fraction of an input pixel covering an output pixel
     Real :: inner_pc ! Radial distance, or angle to inner edge of pixel
     Real :: intensity ! Intensity of an input pixel, corrected if
!      necessary for polarisation effects
     Real :: radial_distance ! Tilt corrected radial distance from the
!      radial symmetry centre to the data value position
     Real :: min_polarisation ! The lowest polarisation factor affecting the ROI
     Real :: out_pixels_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: outer_pc ! Radial distance, or angle to outer edge of pixel
     Real :: polarisation_factor ! The intensity reduction factor
!      owing to the beam polarisation for a given 2-theta and azimuth
     Real :: pixel_half_width ! Half width of "circular" pixel
     Real :: sin_rotation ! Sine of minus the rotation angle
     Real :: sin_tilt ! Sine of the detector tilt angle
     Real :: x_rotate ! The rotated X-coordinate
     Real :: xwc ! X-world coordinate of data point
     Real :: y_rotate ! The rotated Y-coordinate
     Real :: ywc ! Y-world coordinate of data point
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_TILTRADIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TILTRADIAL ' // Version)
        Return
     End If

 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_radial .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_TILTRADIAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x/y beam = '', 2g)') x_beam, y_beam
!     Write (*, '(''x/y pixel size (metres) = '', 2g)')
!     :       x_pixel_size, y_pixel_size
!     Write (*, '(''radial pixel size (metres) = '', g)')
!     :       radial_pixel_size
!     Write (*, '(''rotation, tilt (degrees) = '', 2g)')
!     :       rotation*180.0/Pi, tilt*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Initialise variables
        num_radial = 0
 
!     Calculate average half width of a "circular" pixel
        pixel_half_width = (experiment%x_pixel_size + experiment%y_pixel_size) &
          / 2.0 * Sqrt(1.0 / Pi)
 
!     Calculate number of output pixels covered by a "circular"
!     input pixel
        out_pixels_covered = 2.0 * pixel_half_width / radial_pixel_size
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''out_pixels_covered = '', g)')
!     :       out_pixels_covered
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate cosines/sines of rotation angles
        cos_rotation = Cos(-experiment%tilt_plane_rotation)
        sin_rotation = Sin(-experiment%tilt_plane_rotation)
        cos_tilt_sqr = (Cos(experiment%tilt_angle))**2
        sin_tilt = Sin(experiment%tilt_angle)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Cos(tilt)^2, Sin(tilt) = '', 2g)')
!     :       cos_tilt_sqr, sin_tilt
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Initialise pixel count array
        Do pixel = 1, max_radial
           PROFILE(pixel) = 0.0
           NUMPIXELS(pixel) = 0.0
        End Do
        If (variances_exist) Then
           Do pixel = 1, max_radial
              PROVARS(pixel) = 0.0
           End Do
        End If
 
!     User information
        Call IO_WRITE ('INFO: Starting to re-bin 2-D data ' // &
          'to a 1-D profile, this can take some', status)
        Call IO_WRITE ('      time for large arrays.', status)
 
!     Go through region of interest treating each valid data value
        min_polarisation = 1.7e38
        Do y = ystrelm, yendelm
           ywc = (Real(y) - 0.5 - experiment%y_beam) * experiment%y_pixel_size
 
           Do x = xstrelm, xendelm
 
              If (.Not. MASK(x, y)) Then
                 xwc = (Real(x) - 0.5 - experiment%x_beam) * &
                   experiment%x_pixel_size
 
!              Rotate coordinate to "tilt plane"
                 x_rotate = cos_rotation * xwc - sin_rotation * ywc
                 y_rotate = sin_rotation * xwc + cos_rotation * ywc
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''xwc, ywc = '', 2g)') xwc, ywc
!              Write (*, '(''x/y rotate = '', 2g)')
!              :                x_rotate, y_rotate
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate "x" vector in ideal detector plane
                 radial_distance = experiment%detector_distance * &
                   Sqrt( (cos_tilt_sqr * x_rotate**2 + y_rotate**2) / &
                   (experiment%detector_distance + sin_tilt * x_rotate)**2)
 
!              For angular scans calculate corresponding angle
!              Note: a look-up table may be much more efficient manner
!              to calculate all the angular ranges
                 If (angular_scan) Then
                    inner_pc = Atan2(radial_distance - pixel_half_width, &
                      experiment%detector_distance) / radial_pixel_size
                    outer_pc = Atan2(radial_distance + pixel_half_width, &
                      experiment%detector_distance) / radial_pixel_size
                    out_pixels_covered = outer_pc - inner_pc
 
                 Else
 
!                 Convert extreme radial distances for "circular" pixel
!                 to radial profile pixel coordinate
                    inner_pc = (radial_distance - pixel_half_width) / &
                      radial_pixel_size
                    outer_pc = (radial_distance + pixel_half_width) / &
                      radial_pixel_size
 
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!              Polarisation corrections
                 If (experiment%correct_polarisation) Then
 
                    If (xwc .Gt. 1.0e-6 .Or. ywc .Gt. 1.0e-6) Then
 
!                    Calculate square of Cosine of azimuth
!                    WARNING THIS DOES NOT TAKE INTO ACCOUNT TILT !!!!!!!!!
!                    OLD CODE
!                    cos2_azimuth = xwc**2 /
!                    :                      (xwc**2 + ywc**2)
!                    NEW CODE
                       abs_azimuth = Atan2(ywc, xwc)
 
!                    Calculate square of Cosine of 2 theta angle
                       cos2_2theta = experiment%detector_distance**2 / &
                         (experiment%detector_distance**2 + radial_distance**2)
 
!                    Calculate polarisation factor
 
!                    OLD CODE
!                    polarisation_factor = 0.5 *
!                    :                   ( 1.0 + cos2_2theta -
!                    :                   polarisation * (cos2_azimuth - 1.0) *
!                    :                   (1.0 - cos2_2theta))
 
!                    NEW CODE
                       polarisation_factor = 0.5 * ( 1.0 + cos2_2theta - &
                         experiment%polarisation * Cos(2.0 * abs_azimuth) * &
                         (1.0 - cos2_2theta))
 
                       If (polarisation_factor .Lt. min_polarisation) Then
                          min_polarisation = polarisation_factor
                       End If
 
                    Else
 
                       polarisation_factor = 1.0
 
                    End If
 
                    intensity = DATA(x, y) / polarisation_factor
                 Else
                    intensity = DATA(x, y)
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!              Calculate affected pixels
                 low_pixel = Int(inner_pc) + 1
                 high_pixel = Int(outer_pc) + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Pixel X/Y = '', 2i)') x, y
!              Write (*, '(''radial_distance (metres) = '', g)')
!              :                radial_distance
!              Write (*, '(''Low/High Pixel = '', 2i)')
!              :                low_pixel, high_pixel
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Set maximum affected pixel
                 num_radial = Max(num_radial, high_pixel)
 
                 Do pixel = low_pixel, Min(high_pixel, max_radial)
 
!                 Calculate fractions of "circular" input pixel
!                 covering each output pixel
                    If (pixel .Eq. low_pixel) Then
                       fraction = Min(1.0, (Real(pixel) - inner_pc) / &
                         out_pixels_covered)
                    Else If (pixel .Eq. high_pixel) Then
                       fraction = Min(1.0, (outer_pc - Real(pixel - 1)) / &
                         out_pixels_covered)
                    Else
                       fraction = Min(1.0, 1.0 / out_pixels_covered)
                    End If
 
!                 Add fraction of input pixel intensity into output pixel
                    PROFILE(pixel) = PROFILE(pixel) + intensity * fraction
                    NUMPIXELS(pixel) = NUMPIXELS(pixel) + fraction
 
                    If (variances_exist) Then
                       PROVARS(pixel) = PROVARS(pixel) + intensity**2 * &
                         fraction
                    End If
 
                 End Do
 
              End If
 
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Output progress report
           If (Mod(y - ystrelm + 1, 300) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows treated = '', ' // &
                'i6, '' ('',i3, ''%)'')') y - ystrelm + 1, Int(100.0 * &
                Real(y - ystrelm + 1) / Real(yendelm - ystrelm + 1))
              Call IO_WRITE (message, status)
 
           End If
 
        End Do
 
        If (experiment%correct_polarisation) Then
 
!        Output minimum polarisation factor
           Call IO_WRITE (' ', status)
           Write (message, '(''INFO: Minimum fractional ' // &
             'intensity decrease owing to polarisation = '', f7.4)') &
             min_polarisation
           Call IO_WRITE (message, status)
           Call IO_WRITE ('INFO: (The reciprocal value ' // &
             'is applied to the data.)', status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set number of pixels in radial profile
        num_radial = Min(num_radial, max_radial)
 
!     Normalise radial profile
        Do pixel = 1, num_radial
 
           If (NUMPIXELS(pixel) .Gt. 1.0) Then
 
              PROFILE(pixel) = PROFILE(pixel) / NUMPIXELS(pixel)
 
              If (variances_exist) Then
                 PROVARS(pixel) = Max(0.0, (PROVARS(pixel) - NUMPIXELS(pixel) &
                   * PROFILE(pixel)**2) / (NUMPIXELS(pixel) - 1) )
              End If
 
           Else If (NUMPIXELS(pixel) .Gt. 0.01) Then
 
              PROFILE(pixel) = PROFILE(pixel) / NUMPIXELS(pixel)
 
              If (variances_exist) Then
                 PROVARS(pixel) = PROFILE(pixel)
              End If
 
           Else
              PROFILE(pixel) = 0.0
              If (variances_exist) Then
                 PROVARS(pixel) = 0.0
              End If
 
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate radial profile axis values
        If (angular_scan) Then
 
           Do pixel = 1, num_radial
              RAD_AXIS(pixel) = (Real(pixel) * radial_pixel_size - &
                radial_pixel_size / 2.0) * 180.0 / Pi
           End Do
 
        Else
 
           Do pixel = 1, num_radial
              RAD_AXIS(pixel) = (Real(pixel) * radial_pixel_size - &
                radial_pixel_size / 2.0) * 1000.0
           End Do
 
        End If
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''num_radial = '', i)') num_radial
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_TILTRADIAL
!********1*********2*********3*********4*********5*********6*********7**
 
 
 

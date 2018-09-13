!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_rtheta2.f90 *
!  *                 *
!  *******************
 
!+ F2D_RTHETA2 - FIT2D: R/THETA re-binning
!    profile from 2-D image
     Subroutine F2D_RTHETA2 (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, angular_scan, experiment, &
       lorentz_geometry, maximum_2theta, &
       maximum_radius, max_angular, max_radial, num_angular, num_radial, &
       xmax_work, ymax_work, WORK, R_THETA, status)
!  Description:
!    Re-bins the values in "DATA" in the region from "(xstrelm,
!    ystrelm)" to "(xendelm, yendelm)" according to angular/radial
!    output pixels. Data values are not to be included if they are
!    "mask-off" which is defined by .True. in the corresponding element
!    of "MASK". The profile is calculated from the centre given by the
!    coordinate "(x_beam, y_beam)" and allows for detector tilt, which
!    is defined as a tilt angle of "tilt" in a plane which is at a
!    rotation angle of "rotation" from the X-axis (in the ideal detector
!    plane). The image pixels are assumed to be regular rectangles.
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
!    independent results when calculating variances
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.10 Avoid open strings crossing lines (Hammersley)
!    16-Feb-1996: V0.9 Add 1/cos**3(2-theta) correction (Hammersley)
!    23-Oct-1995: V0.8 Lorentz correction to an uncorrected 2-theta scan 
!      added (Hammersley)
!    12-Sep-1995: V0.7 Protect against completetly on-axis position
!      leading to NaN in polarisation calculation (Hammersley)
!    01-Sep-1995: V0.6 Option of polarisation correction and add
!      correction for Lorentz effect (Hammersley)
!    28-Aug-1995: V0.5 Take account of polarisation (Hammersley)
!    12-May-1995: V0.4 Add maximum 2 theta angle variable (Hammersley)
!    03-May-1995: V0.3 Include choice of equal 2 theta angle bins (Hammersley)
!    03-Mar-1995: V0.2 Intensity conservation option (Hammersley)
!    08-Feb-1995: V0.1 Original, based on "MA_TILTRADIAL" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: ymaxdat ! 2nd dimension of array "DATA" and "MASK"
     Integer, Intent(IN) :: xstrelm ! X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat) ! Data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: conserve_intensity ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Logical, Intent(IN) :: angular_scan ! .True., if the 1-D scan is to be
!      produced in equal angle elements as opposed to equal radial distance
!      elements
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Real, Intent(IN) :: maximum_2theta ! Maximum 2 theta angle for
!      re-binning to a 1-D scan (radians)
     Real, Intent(IN) :: maximum_radius ! Maximum radial distance in metres
!      for radial re-binning
     Integer, Intent(IN) :: max_angular ! First dimension of "R_THETA"
     Integer, Intent(IN) :: max_radial ! Second dimension of "R_THETA"
     Integer, Intent(IN) :: num_angular ! Number of defined angular bins
     Integer, Intent(IN) :: num_radial ! Number of defined radial bins
     Integer, Intent(IN) :: xmax_work ! First dimension of "WORK"
     Integer, Intent(IN) :: ymax_work ! Second dimension of "WORK"
!  Export:
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work) ! Work array for storing 
!      number of fractions input pixels contributing to an output pixel
     Real, Intent(OUT) :: R_THETA(max_angular, max_radial)
!      The angle/radius re-binned data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: ang_pixel ! Angular pixel number in radial profile
     Integer :: high_ang_pixel ! Pixel number of highest angular output
!      pixel effected by an input pixel
     Integer :: high_rad_pixel ! Pixel number of highest radial output
!      pixel effected by an input pixel
     Integer :: low_ang_pixel ! Pixel number of lowest angular output
!      pixel effected by an input pixel
     Integer :: low_rad_pixel ! Pixel number of lowest radial output
!      pixel effected by an input pixel
     Integer :: pixel ! Angular pixel value converted to primary range
     Integer :: rad_pixel ! Radial pixel number in radial profile
     Integer :: x ! X pixel number
     Integer :: y ! Y pixel number
     Real :: ang_fraction ! Fraction of an input pixel covering an output
!      angular pixel
     Real :: ang_pixels_covered ! Number of output pixels covered by
!      an input pixel in the angular direction
     Real :: angle ! Angle of pixel relative to beam centre
     Real :: azimuth_pixel_size ! Angular extent of "circularised" pixel
     Real :: correction_factor ! Geometrical correction factor
     Real :: cos_rotation ! Cosine of minus the rotation angle
     Real :: cos_tilt_sqr ! Square of the Cosine of the detector tilt angle
     Real :: cos2_azimuth ! Square of the cosine of the azimuth
     Real :: cos2_2theta ! Square of the cosine of the 2 theta angle
     Real :: half_angular_width ! Half of the angular width of a pixel
     Real :: inner_pc ! Radial distance, or angle to inner edge of pixel
     Real :: intensity ! Intensity of an input pixel, corrected if
!      necessary for polarisation effects
     Real :: maximum_angle ! Highest angle covered by a pixel
     Real :: maximum_scale ! Maximum scaling for angle rebinning or for
!      radial re-binning
     Real :: min_polarisation ! The lowest polarisation factor affecting the ROI
     Real :: minimum_angle ! Lowest angle covered by a pixel
     Real :: polarisation_factor ! The intensity reduction factor
!      owing to the beam polarisation for a given 2-theta and azimuth
!    Real r_distance ! Temporary debugging variable ???
     Real :: rad_fraction ! Fraction of an input pixel covering an output
!    radial pixel
     Real :: radial_distance ! Tilt corrected radial distance from the
!      radial symmetry centre to the data value position
     Real :: radial_pixel_size ! Width of each radial bin in metres
     Real :: rad_pixels_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: outer_pc ! Radial distance, or angle to outer edge of pixel
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
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_RTHETA2'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RTHETA2 ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_angular .Le. 0 .Or. xmax_work .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. max_radial .Le. 0 .Or. ymax_work .Le. 0) &
       Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. &
       xstrelm.Gt.xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. &
       ystrelm.Gt.yendelm) Then
        status = St_bad_adr2
     Else If (num_angular .Gt. xmax_work) Then
        status = St_bad_adr1
     Else If (num_radial .Gt. ymax_work) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RTHETA2 ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
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
 
!     Set angular and radial pixel sizes
        If (angular_scan) Then
           radial_pixel_size =  maximum_2theta / Real(num_radial)
           maximum_scale = maximum_2theta
        Else
           radial_pixel_size =  maximum_radius / Real(num_radial)
           maximum_scale = maximum_radius
        End If
 
        azimuth_pixel_size = 2.0 * Pi / Real(num_angular)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Angular pixel size, radial_pixel_size = '',
!     :       2g)') azimuth_pixel_size, radial_pixel_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate average distance half width of a "circular" pixel
        pixel_half_width = (experiment%x_pixel_size + &
          experiment%y_pixel_size) / 2.0 * Sqrt(1.0 / Pi)
 
!     Calculate number of radial output pixels covered by a "circular"
!     input pixel
        rad_pixels_covered = 2.0 * pixel_half_width / radial_pixel_size
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''rad_pixels_covered = '', g)')
!     :       rad_pixels_covered
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
        Do ang_pixel = 1, num_angular
 
           Do rad_pixel = 1, num_radial
              R_THETA(ang_pixel, rad_pixel) = 0.0
              WORK(ang_pixel, rad_pixel) = 0.0
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User information
        Call IO_WRITE ('INFO: Starting to transform active ' // &
          'data region (this may take some time)', status)
 
!     Go through region of interest treating each valid data value
        min_polarisation = 1.7e38
        Do y = ystrelm, yendelm
           ywc = (Real(y) - 0.5 - experiment%y_beam) * experiment%y_pixel_size
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Starting y = '', i4)') y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Do x = xstrelm, xendelm
 
              If (.Not. MASK(x, y)) Then
                 xwc = (Real(x) - 0.5 - experiment%x_beam) * &
                   experiment%x_pixel_size
 
!              Rotate coordinate to "tilt plane"
                 x_rotate = cos_rotation * xwc - sin_rotation * ywc
                 y_rotate = sin_rotation * xwc + cos_rotation * ywc
 
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
                    rad_pixels_covered = outer_pc - inner_pc
 
                 Else
 
!                 Convert extreme radial distances for "circular" pixel
!                 to radial profile pixel coordinate
                    inner_pc = (radial_distance - pixel_half_width) / &
                      radial_pixel_size
                    outer_pc = (radial_distance + pixel_half_width) / &
                      radial_pixel_size
 
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!              Polarisation corrections
                 If (experiment%correct_polarisation) Then
 
                    If (xwc .Gt. 1.0e-6 .Or. ywc .Gt. 1.0e-6) Then
 
!                    Calculate square of Cosine of azimuth
!                    WARNING THIS DOES NOT TAKE INTO ACCOUNT TILT !!!!!!!!!
                       cos2_azimuth = xwc**2 / (xwc**2 + ywc**2)
 
!                    Calculate square of Cosine of 2 theta angle
                       cos2_2theta = experiment%detector_distance**2 / &
                         (experiment%detector_distance**2 &
                         + radial_distance**2)
 
!                    Calculate polarisation factor
                       polarisation_factor = 0.5 * ( 1.0 + cos2_2theta - &
                         experiment%polarisation * &
                         (cos2_azimuth - 1.0) * (1.0 - cos2_2theta))
 
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!              Calculate affected radial pixels
                 low_rad_pixel = Int(inner_pc) + 1
                 high_rad_pixel = Int(outer_pc) + 1
 
!              Protect against both values being zero
                 If (Abs(xwc) .Lt. 1.0e-8 .And. Abs(ywc) .Lt. 1.0e-8) Then
                    angle = 0.0
                 Else
 
!                 Calculate angle of input pixel
!                 WARNING THIS DOES NOT TAKE INTO ACCOUNT TILT !!!!!!!!!
                    angle = Atan2(ywc, xwc)
                    If (angle .Lt. 0.0) Then
                       angle = angle + 2.0 * Pi
                    End If
 
                 End If
 
!              Angular width of pixel (small angle approximation)
                 half_angular_width = pixel_half_width / radial_distance
 
!              Calculate extreme angular extents for "circular" pixel
!              in angular pixel coordinates
                 minimum_angle = (angle - half_angular_width) / &
                   azimuth_pixel_size
                 maximum_angle = (angle + half_angular_width) / &
                   azimuth_pixel_size
                 ang_pixels_covered = maximum_angle - minimum_angle
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              If (x .Eq. 256 .And. y .Gt. 1000) Then
!              Write (*, '(''pixel_half_width, '',
!              :                   ''radial_distance = '', 2g)')
!              :                   pixel_half_width, radial_distance
!              Write (*, '(''angle, half_angular_width = '',
!              :                   2g)') angle, half_angular_width
!              Write (*, '(''minimum/maximum_angle = '', 2g
!              :                   )') minimum_angle*180.0/Pi,
!              :                   maximum_angle*180.0/Pi
!              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate affected angular pixels
                 low_ang_pixel = Int(minimum_angle) + 1
                 high_ang_pixel = Int(maximum_angle) + 1
 
                 Do rad_pixel = low_rad_pixel, Min(high_rad_pixel, num_radial)
 
!                 Calculate fractions of "circular" input pixel
!                 covering each radial output pixel
                    If (rad_pixel .Eq. low_rad_pixel) Then
                       rad_fraction = Min(1.0, (Real(rad_pixel) - inner_pc) / &
                         rad_pixels_covered)
                    Else If (rad_pixel .Eq. high_rad_pixel) Then
                       rad_fraction = Min(1.0, (outer_pc - Real(rad_pixel-1)) &
                         / rad_pixels_covered)
                    Else
                       rad_fraction = Min(1.0, 1.0 / rad_pixels_covered)
                    End If
 
                    Do ang_pixel = low_ang_pixel, high_ang_pixel
 
!                    Calculate fractions of "circular" input pixel
!                    covering each angular output pixel
                       If (ang_pixel .Eq. low_ang_pixel) Then
                          ang_fraction = Min(1.0, (Real(ang_pixel) - &
                            minimum_angle) / ang_pixels_covered)
                       Else If (ang_pixel .Eq. high_ang_pixel) Then
                          ang_fraction = Min(1.0, (maximum_angle - &
                            Real(ang_pixel - 1)) / ang_pixels_covered)
                       Else
                          ang_fraction = Min(1.0, 1.0 / ang_pixels_covered)
                       End If
 
!                    Account for angular wrap-around
                       If (ang_pixel .Gt. num_angular) Then
                          pixel = ang_pixel - num_angular
                       Else If (ang_pixel .Lt. 1) Then
                          pixel = ang_pixel + num_angular
 
                          If (pixel .Lt. 1) Then
                             pixel = pixel + num_angular
                          End If
 
                       Else
                          pixel = ang_pixel
                       End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    If (pixel .Le. 0 .Or. pixel .Gt. xmax_work)
!                    :                      Then
!                    Write (*, '(''x, y, rad_fraction, '',
!                    :                         ''angle_fraction = '', 2i4,
!                    2g)')
!                    :                         x, y, rad_fraction, ang_fraction
!                    Write (*, '(''rad_pixel, ang_pixel = '',
!                    :                         2i)') rad_pixel, ang_pixel
!                    Write (*, '(''pixel, xmax_work = '', 2i)')
!                    :                         pixel, xmax_work
!                    End If
!                    If (rad_pixel .Le. 0 .Or.
!                    :                      rad_pixel .Gt. ymax_work)
!                    :                      Then
!                    Write (*, '(''rad_pixel, ymax_work = '',
!                    :                         2i)')
!                    :                         rad_pixel, xmax_work
!                    End If
!
!                    If (x .Eq. 256 .And. y .Gt. 1000) Then
!                    Write (*, '(''x, y, rad_fraction, '',
!                    :                         ''angle_fraction = '', 2i4,
!                    2g)')
!                    :                         x, y, rad_fraction, ang_fraction
!                    Write (*, '(''rad_pixel, ang_pixel = '',
!                    :                         2i)') rad_pixel, ang_pixel
!                    End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                    Add fraction of input pixel intensity into
!                    output pixel
                       R_THETA(pixel, rad_pixel) = R_THETA(pixel, rad_pixel) + &
                         intensity * rad_fraction * ang_fraction
                       WORK(pixel, rad_pixel) = WORK(pixel, rad_pixel) + &
                         rad_fraction * ang_fraction
 
                    End Do
 
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
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Finished re-binning ROI'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Only normalise if intensity is not to be conserved
        If (.Not. conserve_intensity) Then
 
!        Normalise output bins, by fractions of input bins
           Do rad_pixel = 1, num_radial
 
              Do ang_pixel = 1, num_angular
 
                 If (WORK(ang_pixel, rad_pixel) .Gt. 0.01) Then
 
                    R_THETA(ang_pixel, rad_pixel) = R_THETA(ang_pixel, &
                      rad_pixel) / WORK(ang_pixel, rad_pixel)
 
                 Else
                    R_THETA(ang_pixel, rad_pixel) =  0.0
                 End If
 
              End Do
 
              If (lorentz_geometry .Eq. 1 .And. angular_scan) Then
 
!              Correct intensities to equivalent to equal distance 2-theta scan
                 angle = Real(rad_pixel - 1) * radial_pixel_size
                 correction_factor = 1.0 / (Cos(angle))**3
 
                 Do ang_pixel = 1, num_angular
                    R_THETA(ang_pixel, rad_pixel) = R_THETA(ang_pixel, &
                      rad_pixel) * correction_factor
                 End Do
 
              End If
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_RTHETA2
!********1*********2*********3*********4*********5*********6*********7*********8

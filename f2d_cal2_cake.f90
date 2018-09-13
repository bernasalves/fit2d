!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_cal2_cake.f90 *
!  *                   *
!  *********************
 
!+ F2D_CAL2_CAKE - FIT2D: CALculate (2) CAKE region
     Subroutine F2D_CAL2_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, conserve_intensity, scan_type, maximum_d, &
       experiment, start_azimuth, end_azimuth, inner_limit, outer_limit, &
       num_2theta, num_azimuthal, lorentz_geometry, correct_parallax, &
       max_radial, max_azimuthal, xmax_work, &
       ymax_work, az_pixel_size, rad_pixel_size, inner_2theta, WORK, R_THETA, &
       status)
!  Description:
!    Re-bins the values in "DATA" in the region from "(xstrelm,
!    ystrelm)" to "(xendelm, yendelm)" according to angular/radial
!    output pixels. The output region, covers data from azimuth
!    "start_azimuth" to "end_azimuth" and from "inner_limit" to
!    "outer_limit". Data values are not to be included if they are
!    "masked-off" which is defined by .True. in the corresponding element
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
!    (approximation), the start and end positions of output pixels
!    "covered" by the input pixel are calculated. The intensity in the
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
!    02-Oct-2012: V0.17 Implementing parallax correction (Hammersley)
!    21-Sep-2012: V0.16 Option to correct parallax effect (Hammersley)
!    08-Feb-2005: V0.15 Investigating problem when beam centre is outside
!      the detector image (Hammersley)
!    22-Jul-2003: V0.14 Correct azimuthal range calculation. Previously,
!      angles before start were being forgotten (Hammersley)
!    21-Jul-2003: V0.13 azimuthal range converted by "f2d_cal_cake"
!      and checked on input. Testing negative ranges (Hammersley)
!    11-Jul-2003: V0.12 Remove debugging output which was using 2g output
!      format and crashed on a Mac, but change all formats to 2g12.5
!      at the same time (Hammersley)
!    27-May-2003: V0.11 Checking problem with negative azimuth input.
!      Convert azimuth angles to -Pi to Pi range (Hammersley)
!    24-Jul-1998: V0.10 Convert beam centre from data coordinates
!      to pixel coordinates (Hammersley)
!    25-Feb-1998: V0.9 Reverse order of D-spacings output, and
!      controlable maximum D-spacing. Correction to D-spacings, multiple
!      by 10 (Hammersley)
!    24-Feb-1998: V0.8 Swap X/Y outputting re-binning sense, for
!      Lorentz correction (Hammersley)
!    20-Feb-1998: V0.7 Swap X/Y outputting re-binning sense,
!      so that a 2-theta scan is output in the X-direction (Hammersley)
!    29-Jan-1998: V0.6 Add option of D-space re-binning (Hammersley)
!    28-Jan-1998: V0.5 Add option of Q-space re-binning; change
!      definition of "angular_scan" argument (Hammersley)
!    16-Jun-1997: V0.4 Correction to azimuthal variation of
!      polarisation factor (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    16-Feb-1996: V0.2 Add 1/Cos**3(angle) correction (Hammersley)
!    03-Feb-1996: V0.1 Original, based on "F2D_RTHETA2" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA" and
!      "MASK"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA" and
!      "MASK"
     Integer, Intent(IN) :: xstrelm ! X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Y-start of active data region
     Integer, Intent(IN) :: xendelm ! X-end of active data region
     Integer, Intent(IN) :: yendelm ! Y-end of active data region
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat) ! Data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: conserve_intensity ! .True., if the total
!      intensity in to be conserved in the re-binning operation
     Integer, Intent(IN) :: scan_type ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta/2) )
!      3 = D-space: equal d-space scan
     Real, Intent(IN) :: maximum_d ! Maximum D-spacing to calculate for
!      D-spacing scans
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: start_azimuth ! Angle of azimuth of start of region
!      in radians
     Real, Intent(IN) :: end_azimuth ! Angle of azimuth of end of region in
!      radians
     Real, Intent(IN) :: inner_limit ! Inner radius in pixel units
     Real, Intent(IN) :: outer_limit ! Outer radius in pixel units
     Integer, Intent(IN) :: num_2theta ! Number of bins in the 2-theta or
!      radial direction
     Integer, Intent(IN) :: num_azimuthal ! Number of bins in the azimuth
     Integer, Intent(IN) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the
!            equivalent of a 2-theta scan i.e. detector at equal distance
     Logical, Intent(IN) :: correct_parallax ! .True., if the effect of parallax
!      on angular position is to be corrected
     Integer, Intent(IN) :: max_radial ! First dimension of "R_THETA"
     Integer, Intent(IN) :: max_azimuthal ! Second dimension of "R_THETA"
     Integer, Intent(IN) :: xmax_work ! First dimension of "WORK"
     Integer, Intent(IN) :: ymax_work ! Second dimension of "WORK"
!  Export:
     Real, Intent(OUT) :: az_pixel_size ! Angular extent of "circularised"
!      pixel
     Real, Intent(OUT) :: rad_pixel_size ! Width of each radial bin in
!      metres, or radians for an angular scan, or inverse Angstroms for a
!      Q-space scan
     Real, Intent(OUT) :: inner_2theta ! Inner 2-theta angle
     Real, Intent(OUT) :: WORK(xmax_work, ymax_work) ! Work array for
!      storing number of fractional input pixels contributing to an output
!      pixel
     Real, Intent(OUT) :: R_THETA(max_radial, max_azimuthal)
!      The angle/radius re-binned data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.17' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: az_pixel ! Azimuth pixel number
     Integer :: high_az_pixel ! Pixel number of highest azimuth
!      pixel effected by an input pixel
     Integer :: high_rad_pixel ! Pixel number of highest radial output
!      pixel effected by an input pixel
     Integer :: low_az_pixel ! Pixel number of lowest azimuth
!      pixel effected by an input pixel
     Integer :: low_rad_pixel ! Pixel number of lowest radial output
!      pixel effected by an input pixel
     Integer :: rad_pixel ! Radial pixel number in radial profile
     Integer :: x ! X pixel number
     Integer :: y ! Y pixel number
     Real :: abs_azimuth ! The absolute azimuthal angle of the pixel
     Real :: angle ! Angle of a bin or line of bins
     Real :: ang_pixel_size ! Size of an equal angle pixel
     Real :: az_fraction ! Fraction of an input pixel covering an output
!      azimuth pixel
     Real :: az_pixels_covered ! Number of output pixels covered by
!      an input pixel in the azimuth
     Real :: azimuth ! Azimuth angle of a pixel relative to beam centre
     Real :: correction_factor ! Geometrical correction factor
     Real :: cos_rotation ! Cosine of minus the rotation angle
     Real :: cos_tilt_sqr ! Square of the Cosine of the detector tilt angle
!    Real cos2_azimuth ! Square of the Cosine of the azimuth
     Real :: cos2_2theta ! Square of the cosine of the 2 theta angle
     Real :: half_azimuth_width ! Half of the angular width of a pixel
!      in the azimuth direction
     Real :: inner_angle ! Angle of inner edge of pixel (radians)
     Real :: inner_pc ! Radial distance, or angle to inner edge of pixel
     Real :: inner_q ! Inner Q-space value, or inner D-spacing
     Real :: intensity ! Intensity of an input pixel, corrected if
!      necessary for polarisation effects
     Real :: maximum_azimuth ! Highest azimuth covered by a pixel
     Real :: min_polarisation ! The lowest polarisation factor
!      affecting the ROI
     Real :: minimum_azimuth ! Lowest azimuth covered by a pixel
     Real :: nearest_degree ! Nearest degree to pixel position angle
     Real :: outer_2theta ! Outer 2-theta angle
     Real :: outer_angle ! Angle of outer edge of pixel (radians)
     Real :: outer_pc ! Radial distance, or angle to outer edge of pixel
     Real :: outer_q ! Outer Q-space value, or outer D-spacing
     Real :: pixel_half_width ! Half width of "circular" pixel
     Real :: polarisation_factor ! The intensity reduction factor
!      owing to the beam polarisation for a given 2-theta and azimuth
     Real :: rad_fraction ! Fraction of an input pixel covering an output
!      radial pixel
     Real :: radial_distance ! Tilt corrected radial distance from the
!      radial symmetry centre to the data value position
     Real :: rad_pixels_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: sin_rotation ! Sine of minus the rotation angle
     Real :: sin_tilt ! Sine of the detector tilt angle
     Real :: x_rotate ! The rotated X-coordinate
     Real :: xwc ! X-world coordinate of data point
     Real :: y_rotate ! The rotated Y-coordinate
     Real :: ywc ! Y-world coordinate of data point
!  Local Arrays:
     Real :: PARALLAX_CORRECTION(0: 90) ! Parallax correction at each degree
!      from 0 to 85 degrees
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_CAL2_CAKE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL2_CAKE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_azimuthal .Le. 0 .Or. xmax_work .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. max_radial .Le. 0 .Or. ymax_work .Le. 0) &
       Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     Else If (num_2theta .Gt. xmax_work .Or. num_2theta .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (start_azimuth .Lt. -Pi .Or. start_azimuth .Gt. Pi) Then
        status = St_bad_range1
     Else If (start_azimuth .Gt. end_azimuth) Then
        status = St_bad_rel1
     Else If (num_azimuthal .Gt. ymax_work .Or. num_azimuthal .Gt. ymaxdat) &
       Then
        status = St_bad_adr2
     Else If (scan_type .Lt. 0 .Or. scan_type .Gt. 3) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CAL2_CAKE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x/y beam = '', 2g)') x_beampc, y_beampc
!     Write (*, '(''x/y pixel size (metres) = '', 2g12.5)')
!     :       x_pixel_size, y_pixel_size
!     Write (*, '(''radial pixel size (metres) = '', g12.5)')
!     :       rad_pixel_size
!     Write (*, '(''rotation, tilt (degrees) = '', 2g12.5)')
!     :       rotation*180.0/Pi, tilt*180.0/Pi
!     Write (*, '(''start_azimuth, end_azimuth = '', 2g12.5)')
!     :       start_azimuth * 180.0 / Pi, end_azimuth * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Calculate 2-theta limits angles
        inner_2theta = Atan2 (inner_limit, experiment%detector_distance)
        outer_2theta = Atan2 (outer_limit, experiment%detector_distance)
        ang_pixel_size = (outer_2theta - inner_2theta) / Real(num_2theta)
 
        If (scan_type .Eq. 0) Then
 
           rad_pixel_size = (outer_limit - inner_limit) / Real(num_2theta)
 
        Else If (scan_type .Eq. 1) Then
 
!        Set angular and radial pixel sizes
 
           rad_pixel_size = ang_pixel_size
 
        Else If (scan_type .Eq. 2) Then
 
!        Set Q-space pixel sizes
 
!        Calculate Q space limits
           inner_q = 4.0 * Pi / (experiment%wavelength * 1.0e9) * &
             Sin(Atan2 (inner_limit, experiment%detector_distance) / 2.0)
           outer_q = 4.0 * Pi / (experiment%wavelength * 1.0e9) * &
             Sin(Atan2 (outer_limit, experiment%detector_distance) / 2.0)
 
           rad_pixel_size = (outer_q - inner_q) / Real(num_2theta)
 
        Else If (scan_type .Eq. 3) Then
 
!        Set D-space pixel sizes
 
!        Calculate D space limits
           inner_q = experiment%wavelength * 1.0e10 / (2.0 * Sin( &
             Atan2(outer_limit, experiment%detector_distance) / 2.0))
           outer_q = Min(maximum_d, experiment%wavelength * 1.0e10 / (2.0 * &
             Sin( Atan2(inner_limit, experiment%DETECTOR_distance) / 2.0)))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''inner_q, outer_q = '', 2g14.7)')
!        :          inner_q, outer_q
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           rad_pixel_size = (outer_q - inner_q) / Real(num_2theta)
 
        End If
 
        az_pixel_size = (end_azimuth - start_azimuth) / Real(num_azimuthal)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Angular pixel size, rad_pixel_size = '',
!     :       2g14.5)') az_pixel_size*180.0/Pi,
!     :       rad_pixel_size*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate average distance half width of a "circular" pixel
        pixel_half_width = (experiment%x_pixel_size + experiment%y_pixel_size) &
          / 2.0 * Sqrt(1.0 / Pi)
 
!     Calculate number of radial output pixels covered by a "circular"
!     input pixel
        rad_pixels_covered = 2.0 * pixel_half_width / rad_pixel_size
 
!     Calculate parallax effect for each degree from 0 to 85 degrees
        If (correct_parallax) Then
           Call F2D_CAL_PARALLAX (experiment, PARALLAX_CORRECTION, status)
        End If

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
        Do az_pixel = 1, num_azimuthal
 
           Do rad_pixel = 1, num_2theta
              R_THETA(rad_pixel, az_pixel) = 0.0
              WORK(rad_pixel, az_pixel) = 0.0
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User information
        Call IO_WRITE ('INFO: Starting to transform active ' // &
          'data region (this may take some time)', status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_beampc = '', g12.5)') x_beampc
!     Write (*, '(''y_beampc = '', g12.5)') y_beampc
!     Write (*, '(''inner_2theta = '', g12.5)') inner_2theta
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Go through active data region treating each valid data value
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
 
!              Check that pixel in within radial / 2-theta limits
                 If (radial_distance .Ge. inner_limit .And. radial_distance &
                   .Le. outer_limit) Then
 
!                 For angular scans calculate corresponding angle
!                 Note: a look-up table may be much more efficient manner
!                 to calculate all the angular ranges
                    If (scan_type .Eq. 0) Then
 
!                    Convert extreme radial distances for "circular" pixel
!                    to radial profile pixel coordinate
                       inner_pc = (radial_distance - pixel_half_width - &
                         inner_limit) / rad_pixel_size
                       outer_pc = (radial_distance + pixel_half_width - &
                         inner_limit) / rad_pixel_size
 
                    Else If (scan_type .Eq. 1) Then
                       inner_angle = Atan2(radial_distance - pixel_half_width, &
                         experiment%detector_distance)
                       If (correct_parallax) Then
                          nearest_degree = Nint(inner_angle * 180.0 / Pi)
                          inner_pc = (inner_angle + &
                            PARALLAX_CORRECTION(nearest_degree) &
                            - inner_2theta) / rad_pixel_size
                       Else
                          inner_pc = (inner_angle - inner_2theta) / &
                            rad_pixel_size
                       End If

                       outer_angle = Atan2(radial_distance + pixel_half_width, &
                         experiment%detector_distance)
                       If (correct_parallax) Then
                          nearest_degree = Nint(outer_angle * 180.0 / Pi)
                          outer_pc = (outer_angle + &
                            PARALLAX_CORRECTION(nearest_degree) &
                            - inner_2theta) / rad_pixel_size
                       Else
                          outer_pc = (outer_angle - inner_2theta) / &
                            rad_pixel_size
                       End If

                       rad_pixels_covered = outer_pc - inner_pc
 
                    Else If (scan_type .Eq. 2) Then
                       inner_pc = (4.0 * Pi / (experiment%wavelength * 1.0e9) &
                         * Sin(Atan2(radial_distance - pixel_half_width, &
                         experiment%detector_distance) / 2.0) - inner_q) / &
                         rad_pixel_size
                       outer_pc = (4.0 * Pi / (experiment%wavelength * 1.0e9) &
                         * Sin(Atan2(radial_distance + pixel_half_width, &
                         experiment%detector_distance) / 2.0) - inner_q) / &
                         rad_pixel_size
                       rad_pixels_covered = outer_pc - inner_pc
 
                    Else If (scan_type .Eq. 3) Then
 
                       inner_pc = (experiment%wavelength * 1.0e10 / &
                         (2.0 * Sin(Atan2(radial_distance + pixel_half_width, &
                         experiment%detector_distance) / 2.0)) - inner_q) / &
                         rad_pixel_size
                       outer_pc = (experiment%wavelength * 1.0e10 / &
                         (2.0 * Sin(Atan2(radial_distance-pixel_half_width, &
                         experiment%detector_distance) / 2.0)) - inner_q) / &
                           rad_pixel_size
 
                       rad_pixels_covered = outer_pc - inner_pc
 
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''inner_pc, outer_pc = '', 2g14.5)')
!                 :                   inner_pc, outer_pc
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Calculate azimuth limits
 
!                 Protect against both values being zero
                    If (Abs(xwc) .Lt. 1.0e-8 .And. Abs(ywc) .Lt. 1.0e-8) Then
                       azimuth = 0.0
                    Else
 
!                    Calculate angle of input pixel relative to
!                    start azimuth
!                    WARNING THIS DOES NOT TAKE INTO ACCOUNT TILT !!!!!!
                       abs_azimuth = Atan2(ywc, xwc)
 
                       azimuth = abs_azimuth - start_azimuth
 
!                    Place within range
                       If (azimuth .Lt. 0.0) Then
                          azimuth = azimuth + 2.0 * Pi
                       End If
 
                    End If
 
!                 Check that pixel in within azimuth range
                    If (azimuth .Le. end_azimuth - start_azimuth) Then
 
!                    Angular width of pixel (small angle approximation)
                       half_azimuth_width = pixel_half_width / radial_distance
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''Pixel within region x, y = '',
!                    :                      2i6)') x, y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                    Calculate extreme angular extents for "circular"
!                    pixel in angular pixel coordinates
                       minimum_azimuth = (azimuth - half_azimuth_width) / &
                         az_pixel_size
                       maximum_azimuth = (azimuth + half_azimuth_width) / &
                         az_pixel_size
                       az_pixels_covered = maximum_azimuth - minimum_azimuth
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(
!                    :                      ''minimum_azimuth, maximum_azimuth
!                    = '',
!                    :                      2g14.5)') minimum_azimuth,
!                    maximum_azimuth
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!                    Polarisation corrections
                       If (experiment%correct_polarisation) Then
 
!                       Calculate square of Cosine of 2 theta angle
                          cos2_2theta = experiment%detector_distance**2 / &
                            (experiment%detector_distance**2 + &
                            radial_distance**2)
 
!                       Calculate polarisation factor
                          polarisation_factor = 0.5 * ( 1.0 + cos2_2theta - &
                            experiment%polarisation * Cos(2.0 * abs_azimuth) * &
                            (1.0 - cos2_2theta))
 
                          If (polarisation_factor .Lt. min_polarisation) Then
                             min_polarisation = polarisation_factor
                          End If
 
 
                          intensity = DATA(x, y) / polarisation_factor

                       Else
                          intensity = DATA(x, y)
                       End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!                    Calculate affected radial pixels
                       low_rad_pixel = Int(inner_pc) + 1
                       high_rad_pixel = Int(outer_pc) + 1
 
!                    Calculate affected angular pixels
                       low_az_pixel = Int(minimum_azimuth) + 1
                       high_az_pixel = Int(maximum_azimuth) + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(
!                    :                      ''low_rad_pixel, high_rad_pixel,
!                    '',
!                    :                      ''low_az_pixel, high_az_pixel, =
!                    '', 4i5)')
!                    :                      low_rad_pixel, high_rad_pixel,
!                    :                      low_az_pixel, high_az_pixel
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                       Do az_pixel = Max(1, low_az_pixel), Min(num_azimuthal, &
                         high_az_pixel)
 
!                       Calculate fractions of "circular" input pixel
!                       covering each angular output pixel
                          If (az_pixel .Eq. low_az_pixel) Then
                             az_fraction = Min(1.0, (Real(az_pixel) - &
                               minimum_azimuth) / az_pixels_covered)
                          Else If (az_pixel .Eq. high_az_pixel) Then
                             az_fraction = Min(1.0, (maximum_azimuth - &
                               Real(az_pixel - 1)) / az_pixels_covered)
                          Else
                             az_fraction = Min(1.0, 1.0 / az_pixels_covered)
                          End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                       Write (*,
!                       :                        '(''az_pixel, rad_pixel,
!                       intensity '',
!                       :                        2i6, g41.5)')
!                       :                        az_pixel, rad_pixel, intensity
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                          Do rad_pixel = Max(1, low_rad_pixel), &
                            Min(high_rad_pixel, num_2theta)
 
!                          Calculate fractions of "circular" input pixel
!                          covering each radial output pixel
                             If (rad_pixel .Eq. low_rad_pixel) Then
                                rad_fraction = Min(1.0, (Real(rad_pixel) - &
                                  inner_pc) / rad_pixels_covered)
                             Else If (rad_pixel .Eq. high_rad_pixel) Then
                                rad_fraction = Min(1.0, (outer_pc - &
                                  Real(rad_pixel - 1)) / rad_pixels_covered)
                             Else
                                rad_fraction = Min(1.0, 1.0 / &
                                  rad_pixels_covered)
                             End If
 
!                          Add fraction of input pixel intensity into
!                          output pixel
                             R_THETA(rad_pixel, az_pixel) = R_THETA(rad_pixel, &
                               az_pixel) + intensity * rad_fraction * &
                               az_fraction
                             WORK(rad_pixel, az_pixel) = WORK(rad_pixel, &
                               az_pixel) + rad_fraction * az_fraction
 
                          End Do
 
                       End Do
 
                    End If
 
                 End If
 
              End If
 
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Output progress report
           If (Mod(y - ystrelm, 500) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows treated = '', ' // &
                'i6, '' ('',i3, ''%)'')') y - ystrelm, Int(100.0 * Real(y &
                - ystrelm) / Real(yendelm - ystrelm + 1))
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
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Only normalise if intensity is not to be conserved
        If (.Not. conserve_intensity) Then
 
!        Normalise output bins, by fractions of input bins
           Do az_pixel = 1, num_azimuthal
 
              Do rad_pixel = 1, num_2theta
 
                 If (WORK(rad_pixel, az_pixel) .Gt. 0.01) Then
 
                    R_THETA(rad_pixel, az_pixel) = R_THETA(rad_pixel, &
                      az_pixel) / WORK(rad_pixel, az_pixel)
 
                 Else
                    R_THETA(rad_pixel, az_pixel) =  0.0
                 End If
 
              End Do
 
           End Do
 
           If (lorentz_geometry .Eq. 1 .And. (scan_type .Eq. 1 .Or. scan_type &
             .Eq. 2)) Then
 
              Do rad_pixel = 1, num_2theta
 
!              Correct intensities to equivalent to equal distance 2-theta scan
                 angle = inner_2theta + Real(rad_pixel - 1) * ang_pixel_size
                 correction_factor = 1.0 / (Cos(angle))**3
 
                 Do az_pixel = 1, num_azimuthal
                    R_THETA(rad_pixel, az_pixel) = R_THETA(rad_pixel, &
                      az_pixel) * correction_factor
                 End Do
 
              End Do
 
           End If
 
        End If
 
!     Set inner edge of scan
        If (scan_type .Eq. 2 .Or. scan_type .Eq. 3) Then
           inner_2theta = inner_q
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        Else If (scan_type .Eq. 3) Then
 
!        D-spacings should be in reverse order
           Call MA_RFLIP (.True., max_radial, max_azimuthal, 1, 1, num_2theta, &
             num_azimuthal, R_THETA, status)
           rad_pixel_size = -rad_pixel_size
           inner_2theta = outer_q
 
        End If
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_CAL2_CAKE: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_CAL2_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8
 

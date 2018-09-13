!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_cal_lut_cake.f90 *
!  *                      *
!  ************************
 
!+ F2D_CAL_LUT_CAKE - FIT2D: CALculate by LUT CAKE region
     Subroutine F2D_CAL_LUT_CAKE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
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
!    Assuming the pixel to be aligned to the radial direction (approximation), 
!    the start and end positions of output pixels "covered" by the input pixel 
!    are calculated and stored in a LUT. The intensity in the
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
!    02-Oct-2012: V0.4 Implementing parallax correction (Hammersley)
!    21-Sep-2012: V0.3 Option to correct parallax effect (Hammersley)
!    23-Aug-2012: V0.2 Only calculate lut if not done, or parameters have 
!      changed(Hammersley)
!    30-Sep-2011: V0.1 Original, based on "F2D_CAL2_CAKE" (Hammersley)
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
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Real :: abs_azimuth ! The absolute azimuthal angle of the pixel
     Real :: angle ! Angle of a bin or line of bins
     Real :: ang_pixel_size ! Size of an equal angle pixel
     Real :: az_fraction ! Fraction of an input pixel covering an output
!      azimuth pixel
     Integer :: az_pixel ! Azimuth pixel number
     Real :: az_pixels_covered ! Number of output pixels covered by
!      an input pixel in the azimuth
     Real :: azimuth ! Azimuth angle of a pixel relative to beam centre
     Real :: correction_factor ! Geometrical correction factor
     Real :: cos_rotation ! Cosine of minus the rotation angle
     Real :: cos_tilt_sqr ! Square of the Cosine of the detector tilt angle
!    Real cos2_azimuth ! Square of the Cosine of the azimuth
     Real :: cos2_2theta ! Square of the cosine of the 2 theta angle
     Real :: end_cpu ! CPU time at end of re-binning
     Real :: end_elapse ! Elapse time at end of re-binning
     Real :: half_azimuth_width ! Half of the angular width of a pixel
!      in the azimuth direction
     Integer :: high_az_pixel ! Pixel number of highest azimuth
!      pixel effected by an input pixel
     Integer :: high_rad_pixel ! Pixel number of highest radial output
!      pixel effected by an input pixel
     Real :: inner_angle ! Angle of inner edge of pixel (radians)
     Real :: inner_pc ! Radial distance, or angle to inner edge of pixel
     Real :: inner_q ! Inner Q-space value, or inner D-spacing
     Real :: intensity ! Intensity of an input pixel, corrected if
!      necessary for polarisation effects
     Integer :: low_az_pixel ! Pixel number of lowest azimuth
!      pixel effected by an input pixel
     Integer :: low_rad_pixel ! Pixel number of lowest radial output
!      pixel effected by an input pixel
     Logical, Save :: lut_allocated = .False. ! .True., when the look-up table
!      has been allocated
     Logical, Save :: lut_calculated = .False. ! .True., when the look-up table
!      has been calculated
     Real :: maximum_azimuth ! Highest azimuth covered by a pixel
     Character(Len = 80) :: message ! User messages
     Real :: min_polarisation ! The lowest polarisation factor
!      affecting the ROI
     Real :: minimum_azimuth ! Lowest azimuth covered by a pixel
     Real :: nearest_degree ! Nearest degree to pixel position angle
     Type(EXPERIMENTAL_DETAILS), Save :: old_experiment ! Details of
!      experiment (see "io.inc")
     Logical, Save :: old_correct_parallax ! .True., if the effect of parallax
!      on angular position was corrected
     Real, Save :: old_end_azimuth ! Previous angle of azimuth of end of region 
!      in radians
     Real, Save :: old_inner_limit ! Previous inner radius in pixel units
     Integer, Save :: old_lorentz_geometry ! Previous Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the
!            equivalent of a 2-theta scan i.e. detector at equal distance
     Real, Save :: old_maximum_d ! Previous maximum D-spacing to calculate for
!      D-spacing scans
     Integer, Save :: old_num_2theta ! Previous number of bins in the 2-theta or
!      radial direction
     Integer, Save :: old_num_azimuthal ! Previous number of bins in the azimuth
     Real, Save :: old_outer_limit ! Previous outer radius in pixel units
     Integer, Save :: old_scan_type ! Previous type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(theta/2) )
!      3 = D-space: equal d-space scan
     Real, Save :: old_start_azimuth ! Previous angle of azimuth of start of 
!      region in radians
     Real :: outer_2theta ! Outer 2-theta angle
     Real :: outer_pc ! Radial distance, or angle to outer edge of pixel
     Real :: outer_q ! Outer Q-space value, or outer D-spacing
     Real :: rad_fraction ! Fraction of an input pixel covering an output
!      radial pixel
     Integer :: rad_pixel ! Radial pixel number in radial profile
     Real :: rad_pixels_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: radial_distance ! Tilt corrected radial distance from the
!      radial symmetry centre to the data value position
     Real :: sin_rotation ! Sine of minus the rotation angle
     Real :: sin_tilt ! Sine of the detector tilt angle
     Real :: start_cpu ! CPU time at start of re-binning
     Real :: start_elapse ! Elapse time at start of re-binning
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! X pixel number
     Real :: x_rotate ! The rotated X-coordinate
     Real :: xwc ! X-world coordinate of data point
     Real :: y_rotate ! The rotated Y-coordinate
     Real :: ywc ! Y-world coordinate of data point
     Integer :: y ! Y pixel number
!  Local Arrays:
     Real, Allocatable, Save :: INV_POLAR_FACTOR(:, :) ! Dynamic work array for 
!      inverse of polarisation intensity factor applied to each pixel
     Real :: PARALLAX_CORRECTION(0: 90) ! Parallax correction in degrees at each
!      degree from 0 to 85 degrees
     Real, Allocatable, Save :: STR_AZ(:, :) ! Dynamic work array for starting 
!      azimuth position of each pixel (end at next start, hence "n + 1" pixels).
!      -1.0 is used to indicate a pixel which is masked or outside output range
     Real, Allocatable, Save :: STR_RAD(:, :) ! Dynamic work array for starting 
!      radial position of each pixel in mm, 2-theta, q, etc units (end at next 
!      start, hence "n + 1" pixels)
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_CAL_LUT_CAKE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_LUT_CAKE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_CAL_LUT_CAKE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Calculate 2-theta limits angles
        inner_2theta = Atan2 (inner_limit, experiment%detector_distance)
        outer_2theta = Atan2 (outer_limit, experiment%detector_distance)
        ang_pixel_size = (outer_2theta - inner_2theta) / Real(num_2theta)


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Angular pixel size = '', f12.6, '' degrees'')') &
!          ang_pixel_size * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

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
 
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''rad_pixels_covered = '', g)')
!     :       rad_pixels_covered
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate parallax effect for each degree from 0 to 85 degrees
        If (correct_parallax) Then
           Call F2D_CAL_PARALLAX (experiment, PARALLAX_CORRECTION, status)
        End If

!     Calculate cosines/sines of rotation angles
        cos_rotation = Cos(-experiment%tilt_plane_rotation)
        sin_rotation = Sin(-experiment%tilt_plane_rotation)
        cos_tilt_sqr = (Cos(experiment%tilt_angle))**2
        sin_tilt = Sin(experiment%tilt_angle)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!      Calculate the look-up tables if not done, or if anything has changed
        If (.Not. lut_calculated .Or. &
          experiment%x_pixel_size .Ne. old_experiment%x_pixel_size .Or. &
          experiment%y_pixel_size .Ne. old_experiment%y_pixel_size .Or. &
          experiment%wavelength .Ne. old_experiment%wavelength .Or. &
          experiment%correct_polarisation .Ne. &
          old_experiment%correct_polarisation  .Or. &
          experiment%polarisation .Ne. old_experiment%polarisation .Or. &
          experiment%detector_distance.Ne.old_experiment%detector_distance.Or. &
          experiment%x_beam .Ne. old_experiment%x_beam .Or. &
          experiment%y_beam .Ne. old_experiment%y_beam .Or. &
          experiment%tilt_plane_rotation .Ne. &
          old_experiment%tilt_plane_rotation .Or. &
          experiment%tilt_angle .Ne. old_experiment%tilt_angle .Or. &
          experiment%vertical_2theta .Ne. old_experiment%vertical_2theta .Or. &
          experiment%two_theta .Ne. old_experiment%two_theta .Or. &
          scan_type .Ne. old_scan_type .Or. &
          maximum_d .Ne. old_maximum_d .Or. &
          start_azimuth .Ne. old_start_azimuth .Or. &
          end_azimuth .Ne. old_end_azimuth .Or. &
          inner_limit .Ne. old_inner_limit .Or. &
          outer_limit .Ne. old_outer_limit .Or. &
          num_2theta .Ne. old_num_2theta .Or. &
          num_azimuthal .Ne. old_num_azimuthal .Or. &
          correct_parallax .Ne. old_correct_parallax .Or. &
          experiment%detection_depth .Ne. old_experiment%detection_depth .Or. &
          experiment%detection_attenuation .Ne. &
          old_experiment%detection_attenuation) Then

!        Free dynamic work array space if allocated
           If (lut_allocated) Then
              Deallocate (STR_AZ, Stat = stat)
              Deallocate (STR_RAD, Stat = stat)
              Deallocate (INV_POLAR_FACTOR, Stat = stat)
           End If

!        Get dynamic work array space
           Allocate (STR_AZ(xendelm + 1, yendelm + 1), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_CAL_LUT_CAKE ' // Version)
              Return
           End If
 
!        Get dynamic work array space
           Allocate (STR_RAD(xendelm + 1, yendelm + 1), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_CAL_LUT_CAKE ' // Version)
              Return
           End If
 
!        Get dynamic work array space
           Allocate (INV_POLAR_FACTOR(xendelm + 1, yendelm + 1), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_CAL_LUT_CAKE ' // Version)
                Return
           End If
 
           lut_allocated = .True.

!        User information
           Call IO_WRITE ('INFO: Starting to calculate look-up table ' // &
             '(this may take some time)', status)

!        Get start time
           Call IO_TIMES (start_elapse, start_cpu, status)
 
!        Go through active data region calculating lut values
           min_polarisation = 1.7e38
           Do y = ystrelm, yendelm + 1
              ywc = (Real(y) - 1.0 - experiment%y_beam) *experiment%y_pixel_size
 
              Do x = xstrelm, xendelm + 1
 
                 xwc = (Real(x) - 1.0 - experiment%x_beam) * &
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
                    If (scan_type .Eq. 0) Then
 
!                    Convert extreme radial distances for pixel to radial 
!                    profile pixel coordinate
                       inner_pc = (radial_distance - inner_limit) / &
                         rad_pixel_size
 
                    Else If (scan_type .Eq. 1) Then
                       inner_angle = Atan2(radial_distance, &
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

                    Else If (scan_type .Eq. 2) Then
                       inner_pc = (4.0 * Pi / (experiment%wavelength * 1.0e9) &
                         * Sin(Atan2(radial_distance, &
                         experiment%detector_distance) / 2.0) - inner_q) / &
                         rad_pixel_size
                    Else If (scan_type .Eq. 3) Then
                       inner_pc = (experiment%wavelength * 1.0e10 / &
                         (2.0 * Sin(Atan2(radial_distance, &
                         experiment%detector_distance) / 2.0)) - inner_q) / &
                         rad_pixel_size
                    End If

!                 Store Start radial position 
                    STR_RAD(x, y) = inner_pc
 
!                 Calculate azimuth limits
 
!                 Protect against both values being zero
                    If (Abs(xwc) .Lt. 1.0e-8 .And. Abs(ywc) .Lt. 1.0e-8) Then
                       azimuth = 0.0
                    Else
 
!                    Calculate angle of input pixel relative to start azimuth
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
 
!                    Calculate extreme angular extents for "circular"
!                    pixel in angular pixel coordinates
                       STR_AZ(x, y) = azimuth / az_pixel_size
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!                    Polarisation corrections
                       If (experiment%correct_polarisation) Then
 
!                       Calculate square of Cosine of 2 theta angle
                          cos2_2theta = experiment%detector_distance**2 / &
                            (experiment%detector_distance**2 + &
                            radial_distance**2)
 
!                       Calculate polarisation factor
                          INV_POLAR_FACTOR(x, y) = 1.0 / (0.5 * &
                            (1.0 + cos2_2theta - experiment%polarisation * &
                            Cos(2.0 * abs_azimuth) * (1.0 - cos2_2theta)))
 
                       End If
 
                    Else
                       STR_AZ(x, y) = -1.0 ! Not in azimuthal range
                    End If

                 Else
                    STR_AZ(x, y) = -1.0 ! Not in Radial range
                 End If

              End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''1725 y = '', 2i6, '' STR_RAD = '', f10.3, ' // &
!            ''' STR_AZ = '', f14.7, '' INV_POLAR_FACTOR = '', f10.3)') &
!            1725, y, STR_RAD(1725, y), STR_AZ(1725, y), INV_POLAR_FACTOR(1725,y)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do

!        Save parameters used to calculate look-up tables
           lut_calculated = .True.
           old_experiment = experiment
           old_scan_type = scan_type
           old_maximum_d = maximum_d
           old_start_azimuth = start_azimuth
           old_end_azimuth = end_azimuth
           old_inner_limit = inner_limit
           old_outer_limit = outer_limit
           old_num_2theta = num_2theta
           old_num_azimuthal = num_azimuthal
           old_correct_parallax = correct_parallax

!        Get end time
           Call IO_TIMES (end_elapse, end_cpu, status)

!        Output time for calculating LUT's
           Write (message, '(''INFO: Elapse time for lut calculation = '',' // &
             'f6.2, '' seconds'')')  end_elapse - start_elapse
           Call IO_WRITE (message, status)
           Write (message, '(''      CPU Time = '', f6.2, '' seconds'')') &
             end_cpu - start_cpu
           Call IO_WRITE (message, status)

        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     User information
        Call IO_WRITE ('INFO: Starting to transform region of interest' // &
          '(this may take some time)', status)

!     Get start time
        Call IO_TIMES (start_elapse, start_cpu, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CAL_LUT_CAKE: After cal LUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Initialise pixel count array
        Do az_pixel = 1, num_azimuthal
 
           Do rad_pixel = 1, num_2theta
              R_THETA(rad_pixel, az_pixel) = 0.0
              WORK(rad_pixel, az_pixel) = 0.0
           End Do
 
        End Do
 
!     Re-bin data using LUT
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm

!              Check that pixel is not masked and in in output region
              If (.Not. MASK(x, y) .And. STR_AZ(x, y) .Ge. 0.0 .And. &
                STR_AZ(x + 1, y + 1) .Ge. 0.0) Then

                 If (experiment%correct_polarisation) Then
                    intensity = DATA(x, y) * INV_POLAR_FACTOR(x, y)
                 Else
                    intensity = DATA(x, y)
                 End If
 
!              Calculate affected radial pixels
                 inner_pc = Min(STR_RAD(x, y), STR_RAD(x + 1, y + 1))
                 outer_pc = Max(STR_RAD(x, y), STR_RAD(x + 1, y + 1))
                 low_rad_pixel = Int(inner_pc) + 1
                 high_rad_pixel = Int(outer_pc) + 1
                 rad_pixels_covered = outer_pc - inner_pc

!              Calculate affected angular pixels
                 minimum_azimuth = Min(STR_AZ(x, y), STR_AZ(x + 1, y + 1))
                 maximum_azimuth = Max(STR_AZ(x, y), STR_AZ(x + 1, y + 1))
                 low_az_pixel = Int(minimum_azimuth) + 1
                 high_az_pixel = Int(maximum_azimuth) + 1
                 az_pixels_covered = maximum_azimuth - minimum_azimuth

                 Do az_pixel = Max(1, low_az_pixel), Min(num_azimuthal, &
                   high_az_pixel)
 
!                 Calculate fractions of "circular" input pixel
!                 covering each angular output pixel
                    If (az_pixel .Eq. low_az_pixel) Then
                       az_fraction = Min(1.0, (Real(az_pixel) - &
                         minimum_azimuth) / az_pixels_covered)
                    Else If (az_pixel .Eq. high_az_pixel) Then
                       az_fraction = Min(1.0, (maximum_azimuth - &
                         Real(az_pixel - 1)) / az_pixels_covered)
                    Else
                       az_fraction = Min(1.0, 1.0 / az_pixels_covered)
                    End If
 
                    Do rad_pixel = Max(1, low_rad_pixel), &
                      Min(high_rad_pixel, num_2theta)
 
!                    Calculate fractions of input pixel
!                    covering each radial output pixel
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
 
!                    Add fraction of input pixel intensity into output pixel
                       R_THETA(rad_pixel, az_pixel) = R_THETA(rad_pixel, &
                         az_pixel) + intensity * rad_fraction * az_fraction
                       WORK(rad_pixel, az_pixel) = WORK(rad_pixel, &
                         az_pixel) + rad_fraction * az_fraction
 
                    End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                       Write (*, '(''x y = '', 2i6, '' rad_pixel = '',' // &
!                         ' i6, ''az_pixel = '', i6)') x, y, rad_pixel, az_pixel
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 End Do
 
              End If

           End Do

        End Do

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
 
!     Get end time
        Call IO_TIMES (end_elapse, end_cpu, status)

!     Output time for re-binning
        Write (message, '(''INFO: Elapse time for lut integration = '',' // &
          'f6.2, '' seconds'')')  end_elapse - start_elapse
        Call IO_WRITE (message, status)
        Write (message, '(''      CPU Time = '', f6.2, '' seconds'')') &
          end_cpu - start_cpu
        Call IO_WRITE (message, status)

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_CAL_LUT_CAKE: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_CAL_LUT_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8
 

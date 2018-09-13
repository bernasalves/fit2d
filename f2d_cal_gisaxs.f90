!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_cal_gisaxs.f90 *
!  *                    *
!  **********************
 
!+ F2D_CAL_GISAXS - Fit2D: CALculate Grazing Incidence SAXS
     Subroutine F2D_CAL_GISAXS (mask_data, scan_type, experiment, &
       geometrical_correction, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, X_REGION, Y_REGION, &
       method, var_exist, bin_size, max_bins, num_bins, retstat, AXIS, BINS, &
       BIN_VARS, NORMALISATION, status)
!  Description:
!    Calculates the values of a 1-D projection taken of the data held
!    in "DATA" in the rectangle defined by "X_REGION, Y_REGION". The
!    data may be masked or not. The projection is relative to the
!    centre defined by "(x_beam_centre, y_beam_centre)". A number of
!    different output scan types are available as defined by
!    "scan_type". "bin_size" defines the size of output bins in units
!    suitable for the type of output scan.
!
!    The output values will be put into "BINS" starting at
!    element 1 and working up to element "num_bins".
!  Keywords:
!    Projection.2-D~Image, 2-D~Image.Slice, Cut.2-D~Image,
!    2-D~Image.Projection
!  Method:
!    The line equations of the rectangle are defined and used to
!    calculate the minimum and maximum X-pixels which are within the
!    rectangle for each line. The projected distance is calculated
!    from the relative vector and components of the distance change.
!    The inner and outer bin coordinates are calculated for the
!    output scan, and intensity is distributed in proportion to
!    covered distance.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    07-May-1998: V0.6 Convert all arguments to Min and Max functions
!      to integers (previously was mixed, and this was picked up
!      by the Absoft Fortran compiler) (Hammersley)
!    18-Apr-1998: V0.5 Take account of detector tilt (Hammersley)
!    17-Apr-1998: V0.4 Add summation options and option to correct
!      for flat plate geometry (Hammersley)
!    15-Apr-1998: V0.3 Debugging (Hammersley)
!    14-Apr-1998: V0.2 Add efficient determination of pixels inside
!      the defined rectangle (Hammersley)
!    31-Mar-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if the data mask is to be
!      used to ignore masked pixels
     Integer, Intent(IN) :: scan_type ! Type of 1-D scan to be produced:
!      0 = Radial: equal distance scan
!      1 = "2-theta": equal angle element scan
!      2 = Q-space: Equal Q element scan
!          (Q = (4 * Pi / wavelength) Sin(2 - theta / 2) )
!      3 = D-spacing scan
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: geometrical_correction ! .True., if a
!      geometrical correction is to be applied to correct for the 1/r**2
!      effect and obliqueness effects of the flat plate geometry.
!      Not applied to D-spacing scans.
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines starting X-point of ROI
     Integer, Intent(IN) :: ystrelm ! Defines starting Y-point of ROI
     Integer, Intent(IN) :: xendelm ! Defines end X-point of ROI
     Integer, Intent(IN) :: yendelm ! Defines end Y-point of ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Input image
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that a pixel is masked off
     Real, Intent(IN) :: X_REGION(5) ! X-coordinates of projection region,
!      the first two coordinates define the projection line. The fifth
!      coordinate must be set equal to the first coordinate
     Real, Intent(IN) :: Y_REGION(5) ! Y-coordinates of projection region,
!      the first two coordinates define the projection line. The fifth
!      coordinate must be set equal to the first coordinate
     Integer, Intent(IN) :: method ! Method to be used to calculate values:
!      0 = The centre of each pixel is projected onto the line, and
!      the approximate extent is calculated for an averaged
!      circular pixel. The intensity is assumed to be uniform
!      within this region, and distributed in proprotion to
!      covered output pixels other values may be defined
     Logical, Intent(IN) :: var_exist ! .True., if the variances of the 1-D
!      scan should also be calculated
     Real, Intent(IN) :: bin_size ! Size of a bin in the output projection
!      For the different scan types the units are different:
!        "scan_type" = 0: metres
!        "scan_type" = 1: degrees
!        "scan_type" = 2: Inverse nanometres
!        "scan_type" = 3: Angstroms
     Integer, Intent(IN) :: max_bins ! Dimension of array "BINS"
     Integer, Intent(IN) :: num_bins ! Number of bins to define
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status variable
!      0 = Normal return
     Real, Intent(OUT) :: AXIS(max_bins) ! X-axis values for output projection
     Real, Intent(OUT) :: BINS(max_bins) ! Projected / integrated data
     Real, Intent(OUT) :: BIN_VARS(max_bins) ! Estimated variances
     Real, Intent(OUT) :: NORMALISATION(num_bins) ! Work array used to
!      normalise contributions
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Real, Parameter :: Sloppiness = 1.0e-4 ! This is a smallish number to
!      take into account rounding errors when looking at lines at see if they 
!      are parallel, or if they intercept at the same point. Experience
!      shows that rounding errors can be considerable, so "small" is
!      much greater than the machine precision
!  Local Variables:
     Integer :: bin ! Loop variable for output bins
     Integer :: bin_zero ! Output bin which starts at zero
     Integer :: edge ! Loop variable for edge of the rectangular projection 
!      region
     Integer :: high_bin ! Highest bin contributed by an input pixel
     Integer :: low_bin ! Lowest bin contributed by an input pixel
     Integer :: x ! Loop variable for X-direction
     Integer :: x_end ! Last X-pixel on a line within rectangle
     Integer :: x_start ! First X-pixel on a line within rectangle
     Integer :: y ! Loop variable for Y-direction
     Integer :: ymaxpix ! Y-maximum pixel of search region
     Integer :: yminpix ! Y-minimum pixel of search region
     Real :: angle ! Angle in radians of the centre of a pixel
     Real :: correction_factor ! Geometrical correction factor
     Real :: cos_tilt ! Cosine of the tilt angle in the scan direction
     Real :: d_xrelative ! Projected Distance change owing to a
!      relative change in X
     Real :: d_yrelative ! Projected distance change owing to a
!      relative change in Y
     Real :: distance ! Distance in metres from beam centre to centre of
!      projected pixel
     Real :: fraction ! Fraction of an input pixel covering an output pixel
     Real :: inner_pc ! Radial distance, or angle to inner edge of pixel
     Real :: out_bins_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: outer_pc ! Radial distance, or angle to outer edge of pixel
     Real :: pc1 ! First pixel coordinate for re-binned region of a pixel
     Real :: pc2 ! Second pixel coordinate for re-binned region of a pixel
     Real :: pixel_half_width ! Half width of "circular" pixel
     Real :: scan_angle ! Angle of scan direction in radians
     Real :: scan_tilt ! Angle of tilt in the scan direction in radians
     Real :: sin_tilt ! Sine of the tilt angle in the scan direction
     Real :: x_relative ! X-component of relative vector from beam centre
     Real :: xinter ! X-coordinate of projected intersection
     Real :: y_middle ! Y-coordinate of centre of Y-line
     Real :: y_relative ! Y-component of relative vector from beam centre
!  Local Arrays:
     Logical :: DEFINED(4) ! .True., if the equation of the line is defined
     Logical :: VERTICAL(4) ! .True., if the line is vertical
     Real :: INTERCEPTS(4) ! Intercepts of the lines with the X-axis
     Real :: SLOPES(4) ! Slopes of the lines
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entry to F2D_CAL_GISAXS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_GISAXS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_bins .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_ma + status
        Call ST_SAVE ('Subroutine F2D_CAL_GISAXS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_CAL_GISAXS'')')
!     Write (*, '(''num_bins, bin_size = '', i6, g14.7)')
!     :       num_bins, bin_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Calculate angle of scan
        scan_angle = Atan2( (Y_REGION(2) - Y_REGION(1)) * &
          experiment%y_pixel_size, &
          (X_REGION(2) - X_REGION(1)) * experiment%x_pixel_size)
 
!     Set scan angle positive
        If (scan_angle .Gt. Pi / 2.0) Then
           scan_angle = scan_angle - Pi
        Else If (scan_angle .Lt. -Pi / 2.0) Then
           scan_angle = scan_angle + Pi
        End If
 
!     Calculate component of tilt in direction of scan
        scan_tilt = experiment%tilt_angle * &
          Cos (scan_angle - experiment%tilt_plane_rotation)
 
!     Calculate trigonomic values from tilt
        cos_tilt = Cos(scan_tilt)
        sin_tilt = Sin(scan_tilt)
 
!     Set bin which starts at zero
        bin_zero = num_bins / 2
 
!     Initialise projection and normalisation arrays
        Do x = 1, num_bins
           BINS(x) = 0.0
           NORMALISATION(x) = 0.0
        End Do
 
!     Calculate average half width of a pixel
        pixel_half_width = &
          (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0
 
!     Calculate minimum and maximum in Y-direction
        yminpix = Int(Min(Y_REGION(1), Y_REGION(2), Y_REGION(3), Y_REGION(4)))
        yminpix = Max(1, yminpix)
        ymaxpix = Int(Max(Y_REGION(1), Y_REGION(2), Y_REGION(3), Y_REGION(4))) &
          + 1
        ymaxpix = Min(yendelm, ymaxpix)
 
!     Calculate equations of rectangle edges
        Do edge = 1, 4
 
!        Initialise variables
           DEFINED(edge) = .True.
           VERTICAL(edge) = .False.
 
!        Check for difference in X-values
           If (X_REGION(edge) .Ne. X_REGION(edge + 1)) Then
 
!           Non-vertical, so slope and intercept are defined
              SLOPES(edge) = (Y_REGION(edge + 1) - Y_REGION(edge)) / &
                (X_REGION(edge + 1) - X_REGION(edge))
              INTERCEPTS(edge) = Y_REGION(edge) - SLOPES(edge) * &
                X_REGION(edge)
 
           Else
 
              If (Y_REGION(edge) .Ne. Y_REGION(edge + 1)) Then
 
!              Vertical line
                 VERTICAL(edge) = .True.
              Else
                 DEFINED(edge) = .False.
              End If
 
           End If
 
        End Do
 
!     Calculate changes in projected distance owing to X/Y positions
!     relative to the beam centre
        d_xrelative = Cos( Atan2( (Y_REGION(2) - Y_REGION(1)) * &
          experiment%y_pixel_size, &
          (X_REGION(2) - X_REGION(1)) * experiment%x_pixel_size))
        d_yrelative = Cos( Atan2( (X_REGION(2) - X_REGION(1)) * &
          experiment%x_pixel_size, &
          (Y_REGION(2) - Y_REGION(1)) * experiment%y_pixel_size))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''d_xrelative, d_yrelative = '', 2g14.7)')
!     :       d_xrelative, d_yrelative
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Loop over search rectangle
        Do y = yminpix, ymaxpix
 
!        Calculate intersections of edges of polygon with
!        the middle of the line
           x_start = Real(xendelm + 1)
           x_end = -1.0
           y_middle = Real(y) - 0.5
 
           Do edge = 1, 4
 
!           Only consider edges which cross the Y-line
              If (y_middle .Ge. Y_REGION(edge) .And. y_middle .Le. &
                Y_REGION(edge + 1) .Or. y_middle .Le. Y_REGION(edge) .And. &
                y_middle .Ge. Y_REGION(edge + 1)) Then
 
                 If (DEFINED(edge)) Then
 
                    If (VERTICAL(edge)) Then
 
!                    Edge is vertical
                       xinter = X_REGION(edge)
                       retstat = 0
 
                    Else
 
!                    Both lines properly defined, and neither
!                    is vertical
                       If (Abs(SLOPES(edge)) .Gt. Sloppiness) Then
 
!                       The slopes are not the same, so the lines
!                       intersect at a unique point
                          xinter = (INTERCEPTS(edge) - y_middle) / &
                            (-SLOPES(edge))
                          retstat = 0
 
                       Else
 
!                       The slopes of the lines are the same
                          If (Abs(y_middle - INTERCEPTS(edge)) .Gt. &
                            Sloppiness) Then
 
!                          The lines are parallel, but not co-linear
                             retstat = 2
                          Else
 
!                          The lines are parallel, and co-linear
                             retstat = 1
                          End If
 
                       End If
 
                    End If
 
                 Else
 
!                 One of the two lines has been input with
!                 both coordinates the same
                    retstat = -1
 
                 End If
 
                 If (retstat .Eq. 0) Then
 
!                 Unique intersection
 
!                 Check if the intersection point is with the edge
!                 segment
                    If (xinter .Ge. X_REGION(edge) .And. xinter .Le. &
                      X_REGION(edge + 1) .Or. xinter .Le. X_REGION(edge) .And. &
                      xinter .Ge. X_REGION(edge + 1)) Then
 
!                    The intersection occurs with the line
                       x_start = Min(x_start, Int(xinter))
                       x_end = Max(x_end, Int(xinter))
 
                    End If
 
                 Else If (retstat .Eq. 1) Then
 
!                 Intersection at all points
                    x_start = Min(x_start, Int(X_REGION(edge)), &
                      Int(X_REGION(edge + 1)))
                    x_end = Max(x_end, Int(Y_REGION(edge)), Int(Y_REGION(edge &
                      + 1)))
 
                 End If
 
              End If
 
           End Do
 
           y_relative = (y_middle - experiment%y_beam) * &
             experiment%y_pixel_size
 
           Do x = Max(1, x_start), Min(xendelm, x_end)
 
              If (.Not. mask_data .Or. (mask_data .And. .Not. MASK(x, y))) Then
 
!              Calculate relative vectors of centre of pixel to
!              beam centre
                 x_relative = (x - 0.5 - experiment%x_beam) * &
                   experiment%x_pixel_size
 
!              Calculate relative distance on detector
                 distance = x_relative * d_xrelative + y_relative * &
                   d_yrelative
 
!              Calculate projection of pixel position onto
!              integration line taking into account the tilt angle
                 distance = experiment%detector_distance * distance * &
                   cos_tilt / &
                   (experiment%detector_distance + distance * sin_tilt)
 
!              Convert extreme projection distances
!              to projected pixel coordinates
                 If (scan_type .Eq. 0) Then
 
                    inner_pc = (distance - pixel_half_width) / bin_size
                    outer_pc = (distance + pixel_half_width) / bin_size
 
                 Else If (scan_type .Eq. 1) Then
 
                    inner_pc = (Atan2(distance - pixel_half_width, &
                      experiment%detector_distance) / bin_size) * 180.0 / Pi
                    outer_pc = (Atan2(distance + pixel_half_width, &
                      experiment%detector_distance) / bin_size) * 180.0 / Pi
 
                 Else If (scan_type .Eq. 2) Then
 
                    inner_pc = 4.0 * Pi / (experiment%wavelength * 1.0e9) * &
                      Sin(Atan2(distance - pixel_half_width, &
                      experiment%detector_distance) / 2.0) / bin_size
                    outer_pc = 4.0 * Pi / (experiment%wavelength * 1.0e9) * &
                      Sin(Atan2(distance + pixel_half_width, &
                      experiment%detector_distance) / 2.0) / bin_size
 
                 Else If (scan_type .Eq. 3) Then
 
                    pc1 = experiment%wavelength * 1.0e10 / &
                      (2.0 * Sin( Atan2(distance + &
                      pixel_half_width, experiment%detector_distance) / 2.0)) &
                      / bin_size
                    pc2 = experiment%wavelength * 1.0e10 / &
                      (2.0 * Sin( Atan2(distance - &
                      pixel_half_width, experiment%detector_distance) / 2.0)) &
                      / bin_size
 
                    inner_pc = Min(pc1, pc2)
                    outer_pc = Max(pc1, pc2)
 
                 End If
 
!              Account for non-zero starting pixel
                 inner_pc = inner_pc + Real(bin_zero)
                 outer_pc = outer_pc + Real(bin_zero)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''x, y, inner_pc, outer_pc = '', 2i6,
!              :                2g14.7)') x, y, inner_pc, outer_pc
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate affected pixels
                 out_bins_covered = outer_pc - inner_pc
                 low_bin = Max(1, Int(inner_pc))
                 high_bin = Min(num_bins, Int(outer_pc))
 
                 Do bin = low_bin, Min(high_bin, num_bins)
 
!                 Calculate fractions of "circular" input pixel
!                 covering each output pixel
                    If (bin .Eq. low_bin) Then
                       fraction = Min(1.0, (Real(bin + 1) - inner_pc) / &
                         out_bins_covered)
                    Else If (bin .Eq. high_bin) Then
                       fraction = Min(1.0, (outer_pc - Real(bin - 1)) / &
                         out_bins_covered)
                    Else
                       fraction = Min(1.0, 1.0 / out_bins_covered)
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 If (fraction .Lt. 0.0) Then
!                 Write (*, '(/''fraction = '', g14.7)')
!                 :                      fraction
!                 Write (*, '(''bin, low_bin, high_bin = '',
!                 :                      3i6)') bin, low_bin, high_bin
!                 Write (*, '(''out_bins_covered = '', g14.7)')
!                 :                      out_bins_covered
!                 Write (*, '(''inner_pc = '', g14.7)')
!                 :                      inner_pc
!                 Write (*, '(''outer_pc = '', g14.7)')
!                 :                      outer_pc
!                 End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Add fraction of input pixel intensity into output pixel
                    BINS(bin) = BINS(bin) + DATA(x, y) * fraction
                    NORMALISATION(bin) = NORMALISATION(bin) + fraction
 
                    If (var_exist) Then
                       BIN_VARS(bin) = BIN_VARS(bin) + DATA(x, y)**2 * &
                         fraction
                    End If
 
                 End Do
 
              End If
 
           End Do
 
        End Do
 
!     Normalise projection
        Do x = 1, num_bins
 
           If (NORMALISATION(x) .Gt. 0.0) Then
              BINS(x) = BINS(x) / NORMALISATION(x)
           End If
 
        End Do
 
!     Define axis values
        If (scan_type .Eq. 0) Then
 
           Do x = 1, num_bins
              AXIS(x) = (Real(x - bin_zero) - 0.5) * bin_size * 1.0e3
           End Do
 
        Else
 
           Do x = 1, num_bins
              AXIS(x) = (Real(x - bin_zero) - 0.5) * bin_size
           End Do
 
        End If
 
!     Correct for flat plate geometry
        If (geometrical_correction .And. scan_type .Ne. 3) Then
 
           Do x = 1, num_bins
 
!           Need to convert "angle" for different scan types
              If (scan_type .Eq. 0) Then
 
!              Radial scan (mm)
                 angle = Atan2(AXIS(x) / 1.0e3, experiment%detector_distance)
 
              Else If (scan_type .Eq. 1) Then
 
!              2-theta scan (degrees)
                 angle = AXIS(x) * Pi / 180.0
 
              Else If (scan_type .Eq. 2) Then
 
!              Q-space scan
                 angle = Asin( AXIS(x) * experiment%wavelength * 1.0e9 / &
                   (4.0 * Pi))
 
              End If
 
!           Calculate geometrical correction factor
              correction_factor = 1.0 / (Cos(angle))**3
              BINS(x) = BINS(x) * correction_factor
 
              If (var_exist) Then
                 BIN_VARS(x) = BIN_VARS(x) * correction_factor**2
              End If
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_CAL_GISAXS
!********1*********2*********3*********4*********5*********6*********7*********8

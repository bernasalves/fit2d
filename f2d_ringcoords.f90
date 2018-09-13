!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_ringcoords.f90 *
!  *                    *
!  **********************
 
!+ F2D_RINGCOORDS -  find powder RING COORDinateS by centre of gravity
     Subroutine F2D_RINGCOORDS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, experiment, &
       radius, half_search_distance, num_sections, integrated_ints, &
       max_coordinates, max_profiles, num_profiles, PROFILES, WORK, &
       num_coordinates, X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, &
       status)
!  Description:
!    Up to 360 X/Y coordinates are calculated around the powder ring.
!    The angular parameter is the centre of each segment and the
!    radial parameter is the radial centre of mass for that
!    segment. These polar coordinates are converted to cartesian
!    coordinates. Only pixels within +-"half_search_distance"
!    the of the radius "radius" are used, and angular sections
!    which include "masked-off" pixels are discounted.
!
!    A non-zero background causes a problem for simple centre of
!    gravity calculation, so first the average 1-D profile is
!    calculated for each section. Then the centre of gravity is
!    calculated for the profile with an estimated linear background
!    subtracted.
!  Keywords:
!  Method:
!    The powder ring is divided into "num_sections" sections
!    starting from 0 degrees (X-axis) anti-clockwise. For each
!    section an estimate of the radial centre is calculated.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    08-Jul-1997: V0.8 Calculate azimuth angles of data points and option 
!      to calculate integrated intensities of data points (Hammersley)
!    03-Jul-1997: V0.7 Stop crash when predicted central pixel is outside 
!      valid profile range (Hammersley)
!    18-Feb-1996: V0.6 Debugging crash owing to zero intensity areas 
!      (Hammersley)
!    17-Feb-1995: V0.5 Calculate peak coordinates intensity arrays (Hammersley)
!    26-Jan-1995: V0.4 Ignore profile and section if it contains a missing 
!      radial profile pixel (Hammersley)
!    25-Jan-1995: V0.3 Input centre of search radius and half width
!      of search limits (Hammersley)
!    23-Jan-1995: V0.2 Account for, and remove slooping background (Hammersley)
!    22-Jan-1995: V0.1 Original, based on "F2D_RINGCOORDS" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: radius ! Radius of centre search zone (metres)
     Real, Intent(IN) :: half_search_distance ! Distance of half the width
!      for the search region (metres)
     Integer, Intent(IN) :: num_sections ! Number of angular sections to be
!      used to calculate ring centre coordinates. The minimum accepted
!    value is 4 and the maximum is 360 (must be less than or
!    equal to "max_coordinates")
     Logical, Intent(IN) :: integrated_ints ! .True., if the integrated
!      intensity is to be calculated for the data points, otherwise the average
!      at the centre of mass +- 1 pixel is calculated
     Integer, Intent(IN) :: max_coordinates ! Dimension of coordinate arrays
     Integer, Intent(IN) :: max_profiles ! First dimension of "PROFILES" and
!      "WORK" arrays; the maximum number histogramming bins to be used for
!      the radial profile
     Integer, Intent(IN) :: num_profiles ! Number of histogram bins to used
!      for calculating the 1-D profiles
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: PROFILES(max_profiles, max_coordinates) ! Contains
!      1-D histogram of averaged radial profiles for each angular section
     Real, Intent(OUT) :: WORK(max_profiles, max_coordinates)
!      Work array used in the calculation of the 1-D profiles. Contains sum of
!      fractions of input pixels which contributed to each output radial 
!      profile pixel
     Integer, Intent(OUT) :: num_coordinates ! Number of calculated
!      coordinates (this may be less than "num_sections"  owing to masked off
!      regions and sections outside the ROI)
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates) ! X-coordinates of
!      coordinates "on" the powder ring (metres)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates) ! Y-coordinates of
!      coordinates "on" the powder ring (metres)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates) ! If
!      "integrated_ints" is .True., then this is the integrated intensity of the
!      profile minus a linear background. Otherwise the averaged
!      intensity of centre of mass positions of radial profiles
!      is calculated. The average of the 3 pixels centred around
!      the calculated position are used (the intensity is
!      background subtracted from the estimated background)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates) ! Azimuth angles in
!      radians of the data points
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
!  Local Variables:
     Integer :: centre_pixel ! The pixel which contains the centre of
!      the averaged radial profile for each section
     Integer :: high_pixel ! Pixel number of highest output pixel effected
!      by an input pixel
     Integer :: low_pixel ! Pixel number of lowest output pixel effected
!      by an input pixel
     Integer :: pixel ! Loop variable for pixels
     Integer :: section ! The angular element number for the different
!      angular sections
     Integer :: x ! Loop variable for X-direction
     Integer :: x_inner_max ! The maximum X-pixel number for the inner
!      rectangle limit defining the search region
     Integer :: x_inner_min ! The minimum X-pixel number for the inner
!      rectangle limit defining the search region
     Integer :: x_outer_max ! The maximum X-pixel number for the outer
!      rectangle limit defining the search region
     Integer :: x_outer_min ! The minimum X-pixel number for the outer
!      rectangle limit defining the search region
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_inner_max ! The maximum Y-pixel number for the inner
!      rectangle limit defining the search region
     Integer :: y_inner_min ! The minimum Y-pixel number for the inner
!      rectangle limit defining the search region
     Integer :: y_outer_max ! The maximum Y-pixel number for the outer
!      rectangle limit defining the search region
     Integer :: y_outer_min ! The minimum Y-pixel number for the outer
!      rectangle limit defining the search region
     Real :: angle ! Angular coordinate (polar) of pixel relative to
!      ellipse centre
     Real :: average_intensity ! The average background subtracted
!      profile intensity of the pixel at the centre of the profile
!      and one either side
     Real :: centre_intensity ! The background subtracted profile
!      intensity of the pixel at the centre of the profile
     Real :: data_value ! Value of profile minus estimated background
     Real :: fraction ! Fraction of an input pixel covering an output pixel
     Real :: inner_distance ! Radial distance of inner edge of pixel
     Real :: inner_offset ! Average value of the inner three pixels,
!      used to calculate the background
     Real :: inner_radius ! Inner radius of search zone (metres)
     Real :: inner_radius_sqr ! The square of "inner_radius"
     Real :: minus1_intensity ! The background subtracted profile
!      intensity of the pixel to the left of the centre of the profile
     Real :: normalise ! Normalisation for centre of gravity
     Real :: out_pixels_covered ! Number of output pixels covered by a
!      "circular" input pixel
     Real :: outer_distance ! Radial distance of outer edge of pixel
     Real :: outer_radius ! Outer radius of search zone (metres)
     Real :: outer_offset ! Average value of the outer three pixels,
!      used to calculate the background
     Real :: outer_radius_sqr ! The square of "outer_radius"
     Real :: pixel_half_width ! Half width of "circular" pixel
     Real :: plus1_intensity ! The background subtracted profile
!      intensity of the pixel to the right of the centre of the profile
     Real :: radial_pixel_size ! The size (metres) of each radial
!      profile histogram bin
     Real :: radial_distance ! Radial distance from pixel centre
!      to ellipse centre (metres)
     Real :: radius_sqr ! Square of radial distance from pixel centre
!      to ellipse centre
     Real :: section_angle ! Angular size of each angular section used to
!      calculate the coordinates
     Real :: slope ! Gradient of estimated linear background
     Real :: slope_start ! Zero point for calculating slope
     Real :: weight ! Centre of gravity accumulator
     Real :: x_beam ! X-coordinate of beam centre in metres
     Real :: x_dc ! X-direction data coordinate
     Real :: y_beam ! Y-coordinate of beam centre in metres
     Real :: y_dc ! Y-direction data coordinate
     Real :: y_dc_sqr ! Square of "y_dc"
!  Local Arrays:
     Integer :: NUM_PIXELS (360) ! Number of pixels contributing to a
!    section coordinate value
     Logical :: GOOD(360) ! .True., if the angular segment does not
!      contain any missing (masked-off) pixels
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_RINGCOORDS'')')
!  Write (*, '(''x_beam, y_beam = '', 2g)') x_beam, y_beam
!  Write (*, '(''x_pixel_size, y_pixel_size = '', 2g)')
!  :    x_pixel_size, y_pixel_size
!  Write (*, '(''radius, half_search_distance = '', 2g)') radius,
!  :    half_search_distance
!  Write (*, '(''max_profiles, num_profiles = '', 2i)')
!  :    max_profiles, num_profiles
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RINGCOORDS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_coordinates .Le. 4) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     Else If (num_sections .Lt. 4 .Or. num_sections .Gt. max_coordinates) Then
        status = St_bad_int1
     Else If (experiment%x_pixel_size .Le. 0.0) Then
        status = St_bad_real1
     Else If (experiment%y_pixel_size .Le. 0.0) Then
        status = St_bad_real2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RINGCOORDS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Convert beam centre to metre units
        x_beam = experiment%x_beam * experiment%x_pixel_size
        y_beam = experiment%y_beam * experiment%y_pixel_size

!     The ring is contained within the annulus defined by the beam
!     centre, and the inner and outer radial search limits. Taking
!     into account the pixel dimensions this defined an outer
!     pixel rectangle which contains all of the annulus and an
!     inner pixel rectangle which is smaller than the inner radius
!     of the annulus. The exclusive intersection of these two
!     rectangles defines pixels which are worth considering for
!     further processing.
 
!     Inner and outer radius
        inner_radius = radius - half_search_distance
        outer_radius = radius + half_search_distance
 
!     Calculate limits of outer rectangle
        x_outer_min = Max(xstrelm, Int((x_beam - outer_radius) / &
          experiment%x_pixel_size))
        y_outer_min = Max(ystrelm, Int((y_beam - outer_radius) / &
          experiment%y_pixel_size))
        x_outer_max = Min(xendelm, Int((x_beam + outer_radius) / &
          experiment%x_pixel_size) + 1 )
        y_outer_max = Min(yendelm, Int((y_beam + outer_radius) / &
          experiment%y_pixel_size) + 1 )
 
!     Calculate limits of inner rectangle
        x_inner_min = Max(xstrelm, Int((x_beam - inner_radius * 0.7071) / &
          experiment%x_pixel_size) + 1 )
        y_inner_min = Max(ystrelm, Int((y_beam - inner_radius * 0.7071) / &
          experiment%y_pixel_size) + 1 )
        x_inner_max = Min(xendelm, Int((x_beam + inner_radius * 0.7071) / &
          experiment%x_pixel_size) - 1 )
        y_inner_max = Min(yendelm, Int((y_beam + inner_radius * 0.7071) / &
          experiment%y_pixel_size) - 1 )
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_outer_min, y_outer_min = '', 2i)')
!     :       x_outer_min, y_outer_min
!     Write (*, '(''x_outer_max, y_outer_max = '', 2i)')
!     :       x_outer_max, y_outer_max
!     Write (*, '(''x_inner_min, y_inner_min = '', 2i)')
!     :       x_inner_min, y_inner_min
!     Write (*, '(''x_inner_max, y_inner_max = '', 2i)')
!     :       x_inner_max, y_inner_max
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Angular size of each section
        section_angle = 2.0 * Pi / Real(num_sections)
 
!     Size of each radial histogram bin
        radial_pixel_size = (outer_radius - inner_radius) / Real(num_profiles)
 
!     Calculate average half width of a "circular" pixel
        pixel_half_width = (experiment%x_pixel_size + experiment%y_pixel_size) &
        / 2.0 * Sqrt(1.0 / Pi)
 
!     Calculate number of output pixels covered by a "circular"
!     input pixel
        out_pixels_covered = 2.0 * pixel_half_width / radial_pixel_size
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''radial_pixel_size = '', g)') radial_pixel_size
!     Write (*, '(''pixel_half_width = '', g)') pixel_half_width
!     Write (*, '(''out_pixels_covered = '', g)')
!     :       out_pixels_covered
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Initialise arrays
        Do section = 1, num_sections
           NUM_PIXELS(section) = 0
           GOOD(section) = .True.
        End Do
        Do section = 1, num_sections
 
           Do pixel = 1, num_profiles
              PROFILES(pixel, section) = 0.0
              WORK(pixel, section) = 0.0
           End Do
 
        End Do
 
        inner_radius_sqr = inner_radius**2
        outer_radius_sqr = outer_radius**2
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop over potentially interesting region (outer rectangle,
!     but ignoring inner rectangle)
        Do y = y_outer_min, y_outer_max
 
           y_dc = (Real(y) - 0.5) * experiment%y_pixel_size - y_beam
           y_dc_sqr = y_dc**2
 
           Do x = x_outer_min, x_outer_max
 
!           Don't consider pixels which are within the inner rectangle
              If (x .Ge. x_inner_min .And. x .Le. x_inner_max .And. y .Ge. &
                y_inner_min .And. y .Le. y_inner_max) Then
 
!              The pixel is within the smaller rectangle, so ignore
                 Continue
              Else
 
!              The pixel is possibly within the annulus, so
!              calculate the radial distance (squared) and make
!              sure that it is
                 x_dc = (Real(x) - 0.5) * experiment%x_pixel_size - x_beam
 
!              Calculate radius square of pixel position
                 radius_sqr = x_dc**2 + y_dc_sqr
 
!              Check that pixel is within required radial search range
                 If (radius_sqr .Ge. inner_radius_sqr .And. radius_sqr .Le. &
                   outer_radius_sqr) Then
 
!                 Pixel is within search annulus, find angular bin
!                 (in degrees) for pixel
                    angle = Atan2(y_dc, x_dc)
 
!                 Convert to range 0.0 to 2.0*Pi
                    If (angle .Lt. 0) Then
                       angle = angle + 2.0 * Pi
                    End If
 
!                 Convert angle into an element number in degrees
!                 (account for rounding errors)
                    section = Int(angle / section_angle) + 1
                    If (section .Gt. num_sections) Then
                       section = num_sections
                    End If
 
                    If (MASK(x, y)) Then
                       GOOD(section) = .False.
                    Else
 
!                    Calculate radial profile bins to update
                       radial_distance = Sqrt(radius_sqr)
                       inner_distance = (radial_distance - inner_radius - &
                         pixel_half_width) / radial_pixel_size
                       outer_distance = (radial_distance - inner_radius + &
                         pixel_half_width) / radial_pixel_size
 
!                    Calculate affected pixels
                       low_pixel = Int(inner_distance) + 1
                       high_pixel = Int(outer_distance) + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    If (section .Eq. 1) Then
!                    Write (*, '(
!                    :                         ''x, y, low_pixel, high_pixel =
!                    '',
!                    :                         4i5)') x, y, low_pixel,
!                    high_pixel
!                    End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                       Do pixel = low_pixel, Min(high_pixel, max_profiles)
 
!                       Calculate fractions of "circular" input pixel
!                       covering each radial output pixel
                          If (pixel .Eq. low_pixel) Then
                             fraction = Min(1.0, (Real(pixel) - &
                               inner_distance) / out_pixels_covered)
                          Else If (pixel .Eq. high_pixel) Then
                             fraction = Min(1.0, (outer_distance - &
                               Real(pixel - 1)) / out_pixels_covered)
                          Else
                             fraction = Min(1.0, 1.0 / out_pixels_covered)
                          End If
 
!                       Add fraction of input pixel intensity into
!                       output pixel
                          PROFILES(pixel, section) = PROFILES(pixel, section) &
                            + DATA(x, y) * fraction
                          WORK(pixel, section) = WORK(pixel, section) + &
                            fraction
 
                       End Do
 
                        NUM_PIXELS(section) = NUM_PIXELS(section) + 1
 
                    End If
 
                 End If
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''SEGMENTS'')')
!     Do section = 1, num_sections
!     Write (*, '(''Section '', i3, '' num pixels = '',
!     :          i6)') section, NUM_PIXELS(section)
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Normalise radial profile bins
        Do section = 1, num_sections
 
           If (GOOD(section)) Then
 
              Do pixel = 1, num_profiles
 
                 If (WORK(pixel, section) .Gt. 0.0) Then
                    PROFILES(pixel, section) = PROFILES(pixel, section) / &
                      WORK(pixel, section)
                 Else
                    GOOD(section) = .False. ! For present ignore
                    GOOD(section) = .False. ! complete profile if
                    GOOD(section) = .False. ! there is a missing part
                    PROFILES(pixel, section) = -1.7e38
                 End If
 
              End Do
 
           End If
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''PROFILE 1, num_pixels 1'')')
!     Do pixel = 1, num_profiles
!     Write (*, '(i3, 2f10.2)') pixel, PROFILES(pixel, 1),
!     :          WORK(pixel, 1)
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate centre of mass polar coordinate for each "good"
!     angular segment from 1-D profile taking into account a 1-D
!     linear sloping background. The radial, angular coordinates
!     are converted to cartesian coordinates.
        num_coordinates = 0
        Do section = 1, num_sections
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''section = '', i6)') section
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           If (GOOD(section) .And. NUM_PIXELS(section) .Ge. 10) Then
 
!           Calculate average background values at start and end of
!           the 1-D profile
              If (num_profiles .Ge. 9) Then
                 inner_offset =  (PROFILES(1, section) + PROFILES(2, section) &
                   + PROFILES(3, section)) / 3.0
                 outer_offset =  (PROFILES(num_profiles - 2, section) + &
                   PROFILES(num_profiles-1, section) + PROFILES(num_profiles, &
                   section)) / 3.0
                 slope_start = 1.5
                 slope = (outer_offset - inner_offset) / Real(num_profiles - 1 &
                   - 2)
              Else
                 inner_offset = PROFILES(1, section)
                 outer_offset = PROFILES(num_profiles, section)
                 slope_start = 0.5
                 slope = (outer_offset - inner_offset) / Real(num_profiles - 1)
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''slope = '', f14.5)') slope
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              weight = 0.0
              normalise = 0.0
              Do pixel = 1, num_profiles
 
                 If (WORK(pixel, section) .Gt. 0.0) Then
                    radial_distance = (Real(pixel) - 0.5) * radial_pixel_size &
                      + inner_radius
 
!                 Subtract estimated linear background
                    data_value = PROFILES(pixel, section) - inner_offset - &
                      slope * (Real(pixel) - 0.5 - slope_start)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''pixel, profile = '', i, f10.2)')
!                 :                   pixel, data_value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Calculate quantities for centre of gravity calculation
                    weight = weight + data_value * radial_distance
                    normalise = normalise + data_value
                 End If
 
              End Do
 
!           Calculate radius and angle
              If (normalise .Gt. 0.0) Then
                 radial_distance = weight / normalise
                 angle = (Real(section) - 0.5) * section_angle
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Section radius, angle = '', 2g)')
!              :                radial_distance*1000.0, angle*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate centroid pixel position in radial profile
!              pixel units
                 centre_pixel = Int((radial_distance - inner_radius) / &
                   radial_pixel_size) + 1
 
                 If (centre_pixel .Ge. 1 .And. centre_pixel .Le. num_profiles) &
                   Then
 
                    centre_intensity = PROFILES(centre_pixel, section) - &
                      inner_offset - &
                      slope * (Real(centre_pixel) - 0.5 - slope_start)
 
                 Else
                    centre_intensity = 0.0
                 End If
 
              Else
 
!              Set bad position and bad section
                 centre_pixel = -1000
                 GOOD(section) = .False.
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''section = '', i3, ' //
!           :             ''' centre_pixel = '', i3, '' centre_intensity = '''
!           :             // 'f14.2)') section, centre_pixel, centre_intensity
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Only accept coordinate if the centre pixel is within
!           the profile (-1 pixel) and the centre pixel is positive
              If (centre_pixel .Gt. 1 .And. centre_pixel .Lt. num_profiles &
                .And. centre_intensity .Gt. 0.0) Then
 
                 If (integrated_ints) Then
 
                    average_intensity = 0.0
                    Do pixel = 2, num_profiles - 1
                       average_intensity = average_intensity + PROFILES(pixel, &
                         section) - inner_offset - slope*(Real(pixel - 1) - &
                         0.5 - slope_start)
 
                    End Do
 
                 Else
 
!                 Calculate average intensity
                    minus1_intensity = PROFILES(centre_pixel - 1, section) - &
                      inner_offset - slope * (Real(centre_pixel - 1) - 0.5 - &
                      slope_start)
                    plus1_intensity = PROFILES(centre_pixel + 1, section) - &
                      inner_offset - &
                      slope * (Real(centre_pixel + 1) - 0.5 - slope_start)
                    average_intensity = (minus1_intensity + centre_intensity + &
                      plus1_intensity) / 3.0
 
                 End If
 
                 If (average_intensity .Gt. 0.0) Then
 
!                 Convert to X/Y coordinate
                    num_coordinates = num_coordinates + 1
                    X_COORDINATES(num_coordinates) = x_beam + radial_distance &
                      * Cos(angle)
                    Y_COORDINATES(num_coordinates) = y_beam + radial_distance &
                      * Sin(angle)
                    INTENSITIES(num_coordinates) = average_intensity
                    AZIMUTHS(num_coordinates) = angle
                 End If
 
              End If
 
           End If
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''OUTPUT COORDINATES'')')
!     Do section = 1, num_coordinates
!     Write (*, '(''Coordinate '', i3, '' (mm) = '',
!     :          2f10.3)') section,
!     :          X_COORDINATES(section) * 1000.0,
!     :          Y_COORDINATES(section) * 1000.0
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End If
 
     End Subroutine F2D_RINGCOORDS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_find_line_coords.f90 *
!  *                          *
!  ****************************
 
!+ F2D_FIND_LINE_COORDS -  FIND LINE COORDinateS by centre of gravity
     Subroutine F2D_FIND_LINE_COORDS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, low_centre, &
       d_spacing, half_search_distance, azimuth_start, azimuth_end, &
       num_sections, integrated_ints, max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
!  Description:
!    Up to 360 X/Y coordinates are calculated for a line.
!    The angular parameter is the centre of each segment and the
!    radial parameter is the radial centre of mass for that
!    segment. These polar coordinates are converted to cartesian
!    coordinates. Only pixels within +-"half_search_distance"
!    the of the position calculated from "d_spacing".
!
!    A non-zero background causes a problem for simple centre of
!    gravity calculation, so first the average 1-D profile is
!    calculated for each section. Then the centre of gravity is
!    calculated for the profile with an estimated linear background
!    subtracted.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Oct-2014: V0.8 Make serch region start at first pixel or greater
!      (Hammersley)
!    30-Oct-2014: V0.7 Add "detector_offset" parameter and allow beam centre to
!      be different from rotation centre (Hammersley)
!    29-Apr-2014: V0.6 Allow low or high beam-centre (Hammersley)
!    01-Apr-2014: V0.5 Take into account azimuthal limits (Hammersley)
!    04-Mar-2014: V0.4 Debugging (Hammersley)
!    24-Feb-2014: V0.3 Debugging (Hammersley)
!    30-Jan-2014: V0.2 Continue adding code (Hammersley)
!    29-Jan-2014: V0.1 Original, based on "F2D_RINGCOORDS" (Hammersley)
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
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: low_centre ! .True., if beam centre is low in 
!      X-direction
     Real, Intent(IN) :: d_spacing ! D-spacing for line to be used
     Real, Intent(IN) :: half_search_distance ! Distance of half the width
!      for the search region (pixels)
     Real, Intent(IN) :: azimuth_start ! Start azimuth for full data region
     Real, Intent(IN) :: azimuth_end ! End azimuth for full data region
     Integer, Intent(IN) :: num_sections ! Number of angular sections to be
!      used to calculate ring centre coordinates. The minimum accepted
!    value is 4 and the maximum is 360 (must be less than or
!    equal to "max_coordinates")
     Logical, Intent(IN) :: integrated_ints ! .True., if the integrated
!      intensity is to be calculated for the data points, otherwise the average
!      at the centre of mass +- 1 pixel is calculated
     Integer, Intent(IN) :: max_coordinates ! Dimension of coordinate arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_coordinates ! Number of calculated
!      coordinates (this may be less than "num_sections"  owing to masked off
!      regions and sections outside the ROI)
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates) ! X-coordinates of
!      coordinates "on" the powder ring (metres)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates) ! Y-coordinates of
!      coordinates "on" the powder ring (metres)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates) ! Azimuth angles in
!      radians of the data points
     Real, Intent(OUT) :: INTENSITIES(max_coordinates) ! If
!      "integrated_ints" is .True., then this is the integrated intensity of the
!      profile minus a linear background. Otherwise the averaged
!      intensity of centre of mass positions of radial profiles
!      is calculated. The average of the 3 pixels centred around
!      the calculated position are used (the intensity is
!      background subtracted from the estimated background)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Real :: angle ! Azimuth coordinate (polar) of pixel relative to
!      ellipse centre
     Real :: average_intensity ! The average background subtracted
!      profile intensity of the pixel at the centre of the profile
!      and one either side
     Real :: centre_intensity ! The background subtracted profile
!      intensity of the pixel at the centre of the profile
     Real :: centre_line ! Pixel coordinate of centre of the line
     Integer :: centre_pixel ! The pixel which contains the centre of
!      the averaged radial profile for each section
     Integer :: coordinate ! Loop variable for coordinates
     Real :: data_value ! Value of profile minus estimated background
     Real :: distance ! Distance from pixel centre to beam centre (pixels)
     Real :: inner_offset ! Average value of the inner three pixels,
!      used to calculate the background
     Integer :: max_profiles ! First dimension of "PROFILES" and "WORK" arrays;
!      the maximum number histogramming bins to be used for the radial profile
     Real :: minus1_intensity ! The background subtracted profile
!      intensity of the pixel to the left of the centre of the profile
     Real :: normalise ! Normalisation for centre of gravity
     Integer :: num_profiles ! Number of histogram bins to used for
!      calculating the 1-D profiles
     Real :: outer_offset ! Average value of the outer three pixels,
!      used to calculate the background
     Real :: plus1_intensity ! The background subtracted profile
!      intensity of the pixel to the right of the centre of the profile
     Integer :: section ! The angular element number for the different
!      angular sections
     Real :: section_angle ! Angular size of each angular section used to
!      calculate the coordinates
     Real :: slope ! Gradient of estimated linear background
     Real :: slope_start ! Zero point for calculating slope
     Integer :: start_x ! X-pixel for start of profile
     Integer :: stat ! Status return variable for "Allocate"
     Real :: weight ! Centre of gravity accumulator
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!  Local Arrays:
     Logical :: GOOD(360) ! .True., if the angular segment does not
!      contain any missing (masked-off) pixels
     Integer :: NUM_PIXELS (360) ! Number of pixels contributing to a
!      section coordinate value
     Real, Allocatable :: PROFILES(:, :) ! Profiles for peak position
     Real, Allocatable :: WORK(:, :) ! Normalisation array
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_FIND_LINE_COORDS'')')
!     Read (*, *)
!  Write (*, '(''x_beam, y_beam = '', 2g)') x_beam, y_beam
!  Write (*, '(''x_pixel_size, y_pixel_size = '', 2g)')
!  :    x_pixel_size, y_pixel_size
!  Write (*, '(''radius, half_search_distance = '', 2g)') radius,
!  :    half_search_distance
!  Write (*, '(''max_profiles, num_profiles = '', 2i)')
!  :    max_profiles, num_profiles
!     Write (*, '(''azimuth_start, azimuth_end = '', 2(1pe12.5))') &
!       azimuth_start, azimuth_end 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FIND_LINE_COORDS ' // Version)
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
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_FIND_LINE_COORDS: status = '', i12)') status
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FIND_LINE_COORDS ' // Version)
     Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_FIND_LINE_COORDS: Before allocate'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Allocate arrays
        max_profiles = Int(half_search_distance * 2.0) + 1
        num_profiles = max_profiles


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''num_profiles = '', i6)') num_profiles
!        Write (*, '(''num_sections = '', i6)') num_sections
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Allocate (PROFILES(max_profiles, num_sections), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIND_LINE_COORDS ' // Version)
           Return
        End If
        Allocate (WORK(max_profiles, num_sections), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIND_LINE_COORDS ' // Version)
           Return
        End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_FIND_LINE_COORDS: Arrays allocated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Angular size of each section
        section_angle = (azimuth_end - azimuth_start) / Real(num_sections)

        If (low_centre) Then
           centre_line = (experiment%detector_distance * &
             Tan( 2.0 * Asin (experiment%wavelength / (2.0 * d_spacing))) &
             - experiment%detector_offset) / experiment%x_pixel_size  
        Else
           centre_line = (experiment%detector_distance * &
             Tan( 2.0 * Asin (experiment%wavelength / (2.0 * d_spacing))) &
             - experiment%detector_offset) / experiment%x_pixel_size 
        End If

        start_x = Max(1, Int(centre_line - half_search_distance) + 1)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''detector_offset(m) = '', f12.6)') &
!           experiment%detector_offset
!        Write (*, '(''detector_distance = '', f12.6)') &
!          experiment%detector_distance
!        Write (*, '(''wavelength = '', 1pe14.7)') experiment%wavelength
!        Write (*, '(''d_spacing = '', 1pe14.7)') d_spacing
!        Write (*, '(''centre_line = '', f12.6)') centre_line
!        Write (*, '(''start_x = '', i6)') start_x
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Initialise arrays
        Do section = 1, num_sections
           NUM_PIXELS(section) = 0
           GOOD(section) = .True.
        End Do
        Do section = 1, num_sections
 
           Do x = 1, num_profiles
              PROFILES(x, section) = 0.0
              WORK(x, section) = 0.0
           End Do
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_FIND_LINE_COORDS: Arrays initialised'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Form profiles
        Do y = ystrelm, yendelm

           section = Int((Real(y - 1) / Real(ynumdat)) * Real(num_sections)) + 1

           Do x = Max(1, start_x), Min(xendelm, start_x + num_profiles - 1)

              If (MASK(x, y)) Then
                 GOOD(section) = .False.
              Else
                 PROFILES(x - start_x + 1, section) = &
                   PROFILES(x - start_x + 1, section) + DATA(x, y)
                 WORK(x - start_x + 1, section) = &
                   WORK(x - start_x + 1, section) + 1.0
                 NUM_PIXELS(section) = NUM_PIXELS(section) + 1
               End If
              
           End Do

        End Do

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_FIND_LINE_COORDS: Profiles formed'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Normalise radial profile bins
        Do section = 1, num_sections
 
           If (GOOD(section)) Then
 
              Do x = 1, num_profiles
 
                 If (WORK(x, section) .Gt. 0.0) Then
                    PROFILES(x, section) = PROFILES(x, section) / &
                      WORK(x, section)
                 Else
                    GOOD(section) = .False. ! For present ignore
                    GOOD(section) = .False. ! complete profile if
                    GOOD(section) = .False. ! there is a missing part
                    PROFILES(x, section) = -1.7e38
                 End If
 
              End Do
 
           End If
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_FIND_LINE_COORDS: Profiles normalised'')')
!        Do x = 1, num_profiles
!           Write (*, '(''x, value = '', i3, f12.4)') &
!             x, PROFILES(x, num_sections / 2)
!        End Do
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
                 slope = (outer_offset - inner_offset) / &
                   Real(num_profiles - 1 - 2)
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
              Do x = 1, num_profiles
 
                 If (WORK(x, section) .Gt. 0.0) Then
                    distance = Real(x) - 0.5
 
!                 Subtract estimated linear background
                    data_value = PROFILES(x, section) - inner_offset - &
                      slope * (Real(x) - 0.5 - slope_start)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''x, profile = '', i, f10.2)')
!                 :                   x, data_value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Calculate quantities for centre of gravity calculation
                    weight = weight + data_value * distance
                    normalise = normalise + data_value
                 End If
 
              End Do
 
!           Calculate distance and angle
              If (normalise .Gt. 0.0) Then
                 distance = weight / normalise
                 angle = azimuth_start + (Real(section) - 0.5) * section_angle
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''Section = '', i2, '' angle = '', 1pe12.5)') &
!                section, angle * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Calculate centroid pixel position in radial profile pixel units
                 centre_pixel = Int(distance) + 1
 
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
!              Write (*, '(''section = '', i3, ' // &
!                ''' centre_pixel = '', i6, '' centre_intensity = ''' &
!                // 'f14.2)') section, centre_pixel, centre_intensity
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Only accept coordinate if the centre pixel is within
!           the profile (-1 pixel) and the centre pixel is positive
              If (centre_pixel .Gt. 1 .And. centre_pixel .Lt. num_profiles &
                .And. centre_intensity .Gt. 0.0) Then
 
                 If (integrated_ints) Then
 
                    average_intensity = 0.0
                    Do x = 2, num_profiles - 1
                       average_intensity = average_intensity + &
                         PROFILES(x, section) - inner_offset - slope * &
                         (Real(x - 1) - 0.5 - slope_start)
 
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
                    X_COORDINATES(num_coordinates) = &
                      distance + Real(start_x) - 0.5
                    Y_COORDINATES(num_coordinates) = (Real(section) - 0.5) * &
                      Real(ynumdat) / Real(num_sections)

                    INTENSITIES(num_coordinates) = average_intensity
                    AZIMUTHS(num_coordinates) = angle

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''num_coords = '', i2, '' AZ = '', 1pe12.5)') &
!                      num_coordinates, AZIMUTHS(num_coordinates) * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

                 End If
 
              End If
 
           End If
 
        End Do


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write(*, '(''Line coordinates'')')
!        Write (*, '(''num_coordinates = '', i5)') num_coordinates
!        Do coordinate = 1, num_coordinates
!           Write (*, '(''coordinate, X, Y = '', i3, 2f12.3)') coordinate, &
!             X_COORDINATES(coordinate), Y_COORDINATES(coordinate)
!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Free dynamically allocated arrays
        Deallocate (PROFILES)
        Deallocate (WORK)

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End F2D_FIND_LINE_COORDS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_FIND_LINE_COORDS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_findrings.f90 *
!  *                   *
!  *********************
 
!+ F2D_FINDRINGS -  FIND powder RINGS
     Subroutine F2D_FINDRINGS (full_info, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, experiment, &
       max_rings, num_rings, RADIA, half_search_distance, &
       num_sections, integrated_ints, max_coordinates, NUM_COORDINATES, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
!  Description:
!    One by one coordinates are calculated for a number of rings.
!  Keywords:
!  Method:
!    The powder ring is divided into "num_sections" section
!    starting from 0 degrees (X-axis) anti-clockwise. For each
!    section an estimate of the radial centre is calculated.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Mar-2006: V0.7 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.6 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    08-Jul-1997: V0.5 Add "AZIMUTHS" array to store azimuth of
!      data points, and add option for calculating integrated
!      intensities (Hammersley)
!    16-Dec-1996: V0.4 Avoid open strings crossing lines (Hammersley)
!    12-Jun-1996: V0.3 Output number of found coordinates (properly,
!      before this was debug output) (Hammersley)
!    17-Feb-1995: V0.2 Calculate peak coordinates intensity arrays (Hammersley)
!    25-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by the user
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates  masked data point (i.e. point not considered in fitting)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_rings ! Dimension of "RADIA" array
     Integer, Intent(IN) :: num_rings ! Number of powder rings to search
     Real, Intent(IN) :: RADIA(max_rings) ! The radius of different powder rings
     Real, Intent(IN) :: half_search_distance ! Distance of half the width
!      for the search region (metres)
     Integer, Intent(IN) :: num_sections ! Number of angular sections to be
!      used to calculate ring centre coordinates. The minimum accepted
!      value is 4 and the maximum is 360 (must be less than or equal to 
!      "max_coordinates")
     Logical, Intent(IN) :: integrated_ints ! .True., if the integrated
!      intensity is to be calculated for the data points, otherwise the average
!      at the centre of mass +- 1 pixel is calculated
     Integer, Intent(IN) :: max_coordinates ! First dimension size of
!      coordinate arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: NUM_COORDINATES(max_rings) ! Number of
!      calculated coordinates for each powder ring (this may be less than
!      "num_sections" owing to masked off regions and sections outside the ROI)
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates, max_rings)
!      X-coordinates of coordinates "on" the powder rings (metres)
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates, max_rings)
!      Y-coordinates of coordinates "on" the powder rings (metres)
     Real, Intent(OUT) :: INTENSITIES(max_coordinates, max_rings)
!      Averaged intensity of centre of mass positions of radial profiles. 
!      The average of the 3 pixels centred around the calculated position are 
!      used (the intensity is background subtracted by the estimated
!    background)
     Real, Intent(OUT) :: AZIMUTHS(max_coordinates, max_rings)
!      Azimuth in radians of each data point
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User output messages
!    Integer coordinate ! Loop variable for output of coordinates (DEBUG ONLY)
     Integer :: max_profiles ! First dimension of "PROFILES" and "WORK" arrays;
!      the maximum number histogramming bins to be used for the radial profile
     Integer :: num_profiles ! Number of histogram bins to used for
!      calculating the 1-D profiles
     Integer :: ring ! Loop variable for rings
     Integer :: stat ! Status return variable for "Allocate"
!  Local Arrays:
     Real, Allocatable :: PROFILES(:, :) ! Contains 1-D histogram of averaged 
!      radial profiles for each angular section
     Real, Allocatable :: WORK(:, :) ! Work array used in the calculation of the
!      1-D profiles. Contains sum of fractions of input pixels which contributed
!      to each output radial profile pixel
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FINDRINGS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FINDRINGS ' // Version)
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
        Write (*, '(''ERROR: num_sections, max_coordinates = '', 2i8)') &
          num_sections, max_coordinates
     Else If (experiment%x_pixel_size .Le. 0.0) Then
        status = St_bad_real1
     Else If (experiment%y_pixel_size .Le. 0.0) Then
        status = St_bad_real2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FINDRINGS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Number of pixels to be used to calculate averaged 1-D radial
!     profiles in different sections of the ring
        max_profiles = Int( (half_search_distance * 2.0) / &
          ( (experiment%x_pixel_size + experiment%y_pixel_size) * 0.5 ) ) + 1
        num_profiles = max_profiles

        Allocate (PROFILES(max_profiles, num_sections), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FINDRINGS ' // Version)
           Return
        End If
        Allocate (WORK(max_profiles, num_sections), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FINDRINGS ' // Version)
           Return
        End If

!     Output information if required
        If (full_info) Then
           Call IO_WRITE (' ', status)
           If (num_rings .Eq. 1) Then
 
              If (integrated_ints) Then
                 Call IO_WRITE ('INFO: Calculating ' // &
                   'integrated intensities on 1 powder ring', status)
              Else
                 Call IO_WRITE ('INFO: Calculating centre ' // &
                   'of gravity coordinates on 1 powder ring', status)
              End If
 
           Else
 
              If (integrated_ints) Then
                 Call IO_WRITE ('INFO: Calculating ' // &
                   'integrated intensities on powder rings', status)
              Else
                 Call IO_WRITE ('INFO: Calculating centre ' // &
                   'of gravity coordinates on powder rings', status)
              End If
 
           End If
 
        End If
 
!     Find coordinates of each ring one by one
        Do ring = 1, num_rings
 
           Call F2D_RINGCOORDS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, DATA, MASK, experiment, &
             RADIA(ring), half_search_distance, num_sections, integrated_ints, &
             max_coordinates, max_profiles, num_profiles, PROFILES, &
             WORK, NUM_COORDINATES(ring), X_COORDINATES(1, ring), &
             Y_COORDINATES(1, ring), INTENSITIES(1, ring), AZIMUTHS(1, ring), &
             status)
 
           If (full_info) Then
              Write (message, '(''INFO: Ring '', i2, '', Number ' // &
                'of coordinates = '', i3)') ring, NUM_COORDINATES(ring)
              Call IO_WRITE (message, status)
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Do coordinate = 1, NUM_COORDINATES(ring)
!        Write (*, '(''Coordinate '', i3, ' //
!        :             ''' position (mm) = '', 2f10.3, ' //
!        :             ''' Intensity = '', f14.2)') coordinate,
!        :             X_COORDINATES(coordinate, ring)*1000.0,
!        :             Y_COORDINATES(coordinate, ring)*1000.0,
!        :             INTENSITIES(coordinate, ring)
!        End Do
!        Do coordinate = 1, NUM_COORDINATES(ring)
!        Write (*,
!        :             '(''ring, azimuth, intensity '', i3, 2g14.5)')
!        :             ring, AZIMUTHS(coordinate, ring),
!        :             INTENSITIES(coordinate, ring)
!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
!     Free dynamically allocated arrays
        Deallocate (PROFILES)
        Deallocate (WORK)
 
     End If
 
     End Subroutine F2D_FINDRINGS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

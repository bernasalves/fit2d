!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_find_lines.f90 *
!  *                    *
!  **********************
 
!+ F2D_FIND_LINES -  FIND powder LINES
     Subroutine F2D_FIND_LINES (full_info, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, low_centre, &
       max_rings, num_rings, D_SPACINGS, half_search_distance, &
       azimuth_start, azimuth_end, &
       num_sections, integrated_ints, max_coordinates, NUM_COORDINATES, &
       X_COORDINATES, Y_COORDINATES, INTENSITIES, AZIMUTHS, status)
!  Description:
!    One by one coordinates are calculated for a number of lines.
!  Keywords:
!  Method:
!    The powder line is divided into "num_sections" section
!    starting from 0 degrees (X-axis) anti-clockwise. For each
!    section an estimate of the radial centre is calculated.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    29-Apr-2014: V0.3 Allow low or high beam-centre (Hammersley)
!    10-Apr-2014: V0.2 Take into account azimuth limits (Hammersley)
!    04-Mar-2014: V0.1 Original (Hammersley)
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
     Integer, Intent(IN) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates  masked data point (i.e. point not considered in fitting)
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: low_centre ! .True., if beam centre is low in 
!      X-direction
     Integer, Intent(IN) :: max_rings ! Dimension of "D_SPACINGS" array
     Integer, Intent(IN) :: num_rings ! Number of powder rings to search
     Real, Intent(IN) :: D_SPACINGS(max_rings) ! The d-spacings of different 
!      powder lines
     Real, Intent(IN) :: half_search_distance ! Distance of half the width
!      for the search region (metres)
     Real, Intent(IN) :: azimuth_start ! Start azimuth for full data region
     Real, Intent(IN) :: azimuth_end ! End azimuth for full data region
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
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
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
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FIND_LINES'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FIND_LINES ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FIND_LINES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find coordinates of each ring one by one
        Do ring = 1, num_rings

           Call F2D_FIND_LINE_COORDS (xmaxdat, ymaxdat, xnumdat, ynumdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, experiment, &
             low_centre, D_SPACINGS(ring), half_search_distance, &
             azimuth_start, azimuth_end, &
             num_sections, integrated_ints, max_coordinates, &
             NUM_COORDINATES(ring), X_COORDINATES(1, ring), &
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
 
     End If
 
     End Subroutine F2D_FIND_LINES
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

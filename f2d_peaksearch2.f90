!********1*********2*********3*********4*********5*********6*********7********8

!  ***********************
!  *                     *
!  * f2d_peaksearch2.f90 *
!  *                     *
!  ***********************
 
!+ F2D_PEAKSEARCH2 - FIT 2-D PEAK SEARCH 2 (diffraction)
     Subroutine F2D_PEAKSEARCH2 (experiment, mask_data, mode, &
       xmaxdat, ymaxdat, DATA, MASK, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, max_peaks, num_peaks, &
       MDATA, retstat, num_saturated, num_bad, PEAKS, status)
!  Description:
!    Searches the region "(xstrelm, ystrelm)" to "(xendelm, yendelm)"
!    for peaks. These are output in "PEAKS" which contains the coordinates and
!    status of all peak found.
!  Keywords:
!    Peak~Search, Search~Peaks
!  Method:
!    First the data is smoothed by a top hat convolution with a square of 
!    "mode%blur_size" pixels in dimension. Then the image is systematically 
!    searched for separate peaks.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Oct-2007: V0.7 Use "MA_SIGNIFICANTPIXELS "instead of 
!      "MA_WINDOW_MEAN_SIGMAS" (Hammersley)
!    17-Oct-2007: V0.6 Use "MA_WINDOW_MEAN_SIGMAS" instead of "MA_SIGNIFICANT"
!      (Hammersley)
!    12-Feb-2007: V0.5 Use "MA_SIGNIFICANTPIXELS" to find significant pixels
!      (Hammersley)
!    14-Dec-2006: V0.4 Testing (Hammersley)
!    07-Dec-2006: V0.3 Debugging (Hammersley)
!    29-Nov-2006: V0.2 Call "F2D_PEAKINTEGRATE2" (Hammersley)
!    27-Nov-2006: V0.1 Original, base on "F2D_PEAKSEARCH" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! GS constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: mask_data ! .True., if the data mask is to be used
     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode ! Mode and control of peak 
!      search
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True. if element is
!      masked-off
     Integer, Intent(IN) :: xnumdat ! Size of defined data in X-direction
     Integer, Intent(IN) :: ynumdat ! Size of defined data in Y-direction
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
!  Import/Export:
     Integer, Intent(INOUT) :: num_peaks ! On input should be set to zero, or
!      to the number of previously found peak, if peaks are to be added to
!      an existing peak search. On exit: total peaks found, including problem 
!      peaks
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Blurred data
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Work array not big enough
     Integer, Intent(OUT) :: num_saturated ! Number of saturated peaks
     Integer, Intent(OUT) :: num_bad ! Number of bad "peaks"
     Type(PEAK_STRUCTURE), Intent(OUT) :: PEAKS(max_peaks) ! Found peaks
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Real :: angle ! Angle of a reflection
     Real :: bad_threshold ! The intensity value below which a pixel will
!      be considered as being bad, and not take part in mean background 
!      calculations
     Real :: edge_max ! Maximum value found around the edge of a "peak"
     Real :: maximum ! Maximum value in a "peak" area
     Double Precision :: mean ! Mean value in data region
     Integer :: num_peak_pixels ! Number of pixels considered as peak pixels 
!      within the background box
     Integer :: num_pixels ! Number of pixels used to calculate running mean
     Integer :: num_threshold ! Number of pixels above the threshold level
     Integer :: peak ! Loop variable for peaks
     Integer :: peak_retstat ! Return status from "PEAKINTEGRATE"
     Integer :: width ! Number of pixel in X-direction in ROI
     Integer :: x ! Loop variable for X-direction
     Integer :: x1 ! Loop variable for X-direction
     Integer :: x_back ! X-pixel number for "IS_PEAK" calculation
     Integer :: x_data ! X-pixel number for element in data array
     Integer :: y ! Loop variable for Y-direction
     Integer :: y1 ! Loop variable for Y-direction
     Integer :: y_back ! Y-pixel number for "IS_PEAK" calculation
     Integer :: y_data ! Y-pixel number for element in data array
     Logical :: new_peak ! .True., if a pixel position is considered a new peak
     Real :: old_peak_threshold ! Previous iteration value of "peak_threshold"
!      be considered as being part of a peak
     Real :: peak_threshold ! The intensity value above which a pixel
!      is considered to be part of a peak
     Real :: sqr_min_spacing ! Square of the minimum allowed spacing in pixels 
!      between peak centres, should be greater than "mode%blur_size"
     Integer :: num_start ! Number of defined peaks at start of call
     Integer :: start_peak ! First peak in found arrays, for which it
!      is necessary to compare positions
     Integer :: stat ! Status return variable for "Allocate"
     Real :: value ! Value of a pixel
     Real :: radial_distance ! Distance from spot to beam centre radially
!  Local Arrays:
     Logical*1, Allocatable :: SIGNIFICANT(:, :) ! Dynamically allocated array
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKSEARCH2 ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. max_peaks .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (mode%mode .Ne. 0 .And. mode%mode .Ne. 1) Then
        status = St_bad_int1
     Else If (mode%blur_size .Lt. 1) Then
        status = St_bad_int1
     Else If (mode%min_spacing .Le. mode%blur_size) Then
        status = St_bad_rel1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PEAKSEARCH2 ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_PEAKSEARCH2'')')
!        Write (*, '(''max_peaks = '', i6)') max_peaks
!        Write (*, '(''num_peaks = '', i6)') num_peaks
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Start by allocating memory for peak search
        Allocate (SIGNIFICANT(xendelm, yendelm), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PEAKSEARCH2 ' // Version)
           Return
        End If

        retstat = 0

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_PEAKSEARCH2: Memory allocated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Smooth data, output in "memory"
        Call MA_TOPHATCON (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
          xendelm, yendelm, mode%blur_size, mode%blur_size, .True., &
          xmaxdat, ymaxdat, MDATA, status)
 
!     User message
        Call IO_WRITE ('INFO: Data smoothed or copied', status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find pixels which are significantly above the local average
        Call MA_SIGNIFICANTPIXELS (xmaxdat, ymaxdat, MDATA, MASK, &
          xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, mode, &
          xendelm, yendelm, SIGNIFICANT, status)

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Significant pixels found'')')
!        Write (*, '(''Stage Two'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Stage two: search for peaks
        start_peak = num_peaks + 1
        sqr_min_spacing = mode%min_spacing**2

        Do y = ystrelm + 2, yendelm - 2
 
           Do x = xstrelm + 2, xendelm - 2
 
              If (SIGNIFICANT(x, y)) Then
 
                 value = MDATA(x, y)
 
!              Check that this is the highest value of the 10 "following" values
                 If (value .Gt. MDATA(x + 1, y) .And. value .Gt. MDATA(x + 2, &
                   y) .And. value .Gt. MDATA(x - 1, y + 1) .And. value .Gt. &
                   MDATA(x, y + 1) .And. value .Gt. MDATA(x + 1, y + 1) .And. &
                   value .Gt. MDATA(x + 2, y + 1) .And. value .Gt. MDATA(x - &
                   1, y + 2) .And. value .Gt. MDATA(x, y + 2) .And. value .Gt. &
                   MDATA(x + 1, y + 2) .And. value .Gt. MDATA(x + 2, y + 2)) &
                   Then
 
!                 Count number of peak pixels in 5 by 5
!                 square centred on the pixel
                    num_threshold = 0
                    Do y1 = -2, 2
 
                       Do x1 = -2, 2
 
                          If (SIGNIFICANT(x + x1, y + y1)) Then
                             num_threshold = num_threshold + 1
                          End If
 
                       End Do
 
                    End Do
 
!                 Only count "num_adjacent" or more threshold pixels
                    If (num_threshold .Ge. mode%num_adjacent) Then

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                       Write (*, '(''Candidate peak at '', 2i6)') x, y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!                    Candidate peak pixel
                       new_peak = .True.
                       peak = num_peaks
                       Do While (new_peak .And. peak .Ge. start_peak)
 
!                       Compare coordinates with previous peaks
                          If ((Abs((Real(x) - 0.5) - PEAKS(peak)%x))**2 + &
                            (Abs((Real(y) - 0.5) - PEAKS(peak)%y))**2 .Lt. &
                            sqr_min_spacing) Then
 
!                          Too close to an existing peak
                             new_peak = .False.
 
                          End If
 
                          peak = peak - 1
 
                       End Do
 
                       If (new_peak) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                          Write (*, '(''New peak at '', 2i6)') x, y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!                       Check that values around peak descend below
!                       the maximum value
                          edge_max = -1.7e38
                          Do y1 = Max(ystrelm, y - mode%min_spacing / 2 - 1), &
                            Min(yendelm, y + mode%min_spacing / 2 + 1)

                             If ((x - mode%min_spacing / 2) .Ge. xstrelm) Then
                                edge_max = Max(DATA(x - mode%min_spacing / 2, &
                                  y1), edge_max)
                             End If
 
                             If ((x + mode%min_spacing / 2) .Le. xendelm) Then
                                edge_max = Max(DATA(x + mode%min_spacing / 2, &
                                  y1), edge_max)
                             End If
 
                          End Do
 
                          Do x1 = Max(xstrelm, x - mode%min_spacing / 2), &
                            Min(xendelm, x + mode%min_spacing / 2)

                             If ((y - mode%min_spacing / 2) .Ge. ystrelm) Then
                                edge_max = &
                                  Max(DATA(x1, y - mode%min_spacing / 2), &
                                  edge_max)
                             End If
 
                             If ((y + mode%min_spacing / 2) .Le. yendelm) Then
                                edge_max = &
                                  Max(DATA(x1, y + mode%min_spacing / 2), &
                                  edge_max)
                             End If
 
                          End Do
 
!                       Calculate maximum
                          maximum = -1.7e38
                          Do y1 = Max(ystrelm, y - mode%min_spacing / 2), &
                            Min(yendelm, y + mode%min_spacing / 2)
 
                             Do x1 = Max(xstrelm, x - mode%min_spacing / 2), &
                               Min(xendelm, x + mode%min_spacing / 2)
                                maximum = Max(maximum, DATA(x1, y1))
                             End Do
 
                          End Do
 
!                       Add peak to list
                          If (num_peaks .Lt. max_peaks) Then

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                            Write (*, '(''Add peak to list '')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

                             num_peaks = num_peaks + 1
 
                             PEAKS(num_peaks)%x = x
                             PEAKS(num_peaks)%y = y
 
                             If (DATA(x, y) .Lt. mode%saturation_level) Then
                                PEAKS(num_peaks)%status = 0
                             Else
 
!                             Saturated peak
                                PEAKS(num_peaks)%status = 1
                                num_saturated = num_saturated + 1

                             End If
 
!                          Test that the maximum edge value is at most
!                          90% of the peak value, both background corrected
                             If ((maximum - mean) * 0.9 .Lt. edge_max - mean) &
                               Then
 
                                If (PEAKS(num_peaks)%status .Eq. 1) Then
                                   num_saturated = num_saturated - 1
                                End If
 
                                PEAKS(num_peaks)%status = &
                                  PEAKS(num_peaks)%status + 10
                                num_bad = num_bad + 1
 
                             Else If (mode%mode .Ge. 1) Then
 
!                             Estimate integrated intensity and standard 
!                             deviation of estimate
                                Call F2D_PEAKINTEGRATE2 (xmaxdat, ymaxdat, &
                                  xstrelm, ystrelm, xendelm, yendelm, DATA, &
                                  MASK, experiment%detector_gain, mode, &
                                  SIGNIFICANT, PEAKS(num_peaks), status)

                                If (peak_retstat .Ne. 0) Then
                                   PEAKS(num_peaks)%status = &
                                     PEAKS(num_peaks)%status + 100
                                End If
 
                             End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                             Write (*, '(''Peak X/Y = '', 2i6)') x, y
!                             Write (*, '(''num_peaks = '', i8)') num_peaks
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                          Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                             Write (*, '(''F2D_PEAKSEARCH2: retstat = '', i6)'&
!                               ) retstat
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                          Not enough room for peaks
                             retstat = 1
                             Return
 
                          End If
 
                       End If
 
                    End If
 
                 End If
 
              End If
 
           End Do
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_PEAKSEARCH22: End of stage two'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Free memory for work arrays
        Deallocate (SIGNIFICANT)
 
!     Calculate D-spacings for peaks
        If (experiment%beam_centre_set .And. experiment%detector_distance_set &
          .And. experiment%wavelength_set) Then
 
           Do peak = num_start + 1, num_peaks
 
!           Calculate d-spacing of peak
              radial_distance = &
                Sqrt(((PEAKS(peak)%x_centre - experiment%x_beam) * &
                experiment%x_pixel_size)**2 + &
                ((PEAKS(peak)%y_centre - experiment%y_beam) * &
                experiment%y_pixel_size)**2)
              angle = 0.5 * Atan(radial_distance / experiment%detector_distance)
              PEAKS(peak)%d_spacing = (experiment%wavelength / &
                (2.0 * Sin(angle))) * 1.0e10
 
           End Do
 
        Else
 
!        Set obviously false value
           Do peak = num_start + 1, num_peaks
              PEAKS(peak)%d_spacing = -1.0
           End Do
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_PEAKSEARCH2: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_PEAKSEARCH2
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

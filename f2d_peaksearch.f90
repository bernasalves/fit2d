!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_peaksearch.f90 *
!  *                    *
!  **********************
 
!+ F2D_PEAKSEARCH - FIT 2-D PEAK SEARCH (diffraction)
     Subroutine F2D_PEAKSEARCH (EXPERIMENT, mask_data, mode, xmaxdat, ymaxdat, &
       DATA, MASK, xstrelm, ystrelm, xendelm, yendelm, max_peaks, num_peaks, &
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
!    16-Nov-2006: V0.13 Use "PEAK_SEARCH_CONTROL" structure (Hammersley)
!    15-Nov-2006: V0.12 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    28-Mar-2006: V0.11 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    13-Mar-2006: V0.10 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    26-Jun-1998: V0.9 Add calculation of D-spacings (Hammersley)
!    21-Mar-1997: V0.8 Make use of the data mask optional. Correct out of 
!      bounds error in "F2D_PEAKSTRIP" (Hammersley)
!    10-Mar-1997: V0.7 Use data masking (Hammersley)
!    21-Feb-1997: V0.6 Make strip size at least as big as the minimum spacing 
!      between peaks (Hammersley)
!    12-Feb-1996: V0.5 Add detector gain (Hammersley)
!    09-Feb-1996: V0.4 Add "data_limit" minimum for data values (Hammersley)
!    08-Feb-1996: V0.3 Add number of bad "peaks" (Hammersley)
!    07-Feb-1996: V0.2 Add number bad sigma (Hammersley)
!    28-Jan-1996: V0.1 Original (Hammersley)
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
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number
!  Local Variables:
     Integer :: num_start ! Number of defined peaks at start of call
     Integer :: old_start_peak ! "start_peak" value for the previous strip
     Integer :: peak ! Loop variable for peaks
     Integer :: start_peak ! First peak in found arrays, for which it
!      is necessary to compare positions
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: strip_width ! Width of a strip
     Integer :: xmax_work ! X-Number of elements in "IS_PEAK" array
     Integer :: y_lower ! Lower y value covered by present background
     Integer :: y_ref ! Reference y value for background estimation
     Integer :: y_top ! Top line of strip
     Integer :: y_upper ! Upper y value covered by present background
     Integer :: ymax_work ! Y-Number of elements in "IS_PEAK" array
     Real :: angle ! Angle of a reflection
     Real :: radial_distance ! Distance from spot to beam centre radially
!  Local Arrays:
     Byte, Allocatable :: IS_PEAK(:, :) ! Dynamically allocated array
     Real, Allocatable :: PEAK_PIXELS(:) ! Dynamically allocated array
     Double Precision, Allocatable :: SQUARES(:) ! Dynamically allocated array
     Double Precision, Allocatable :: SUMS(:) ! Dynamically allocated array
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''Entered F2D_PEAKSEARCH'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate suitable width for a strip
        strip_width = Max(mode%min_spacing + 9, mode%box_size)
 
!     Start by allocating memory for peak search
        xmax_work = xendelm - xstrelm + 1
        ymax_work = strip_width
        Allocate (IS_PEAK(xmax_work, ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
           Return
        End If
        Allocate (PEAK_PIXELS(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
           Return
        End If
        Allocate (SUMS(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
           Return
        End If
        Allocate (SQUARES(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PEAKSEARCH ' // Version)
           Return
        End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_PEAKSEARCH: Memory allocated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        If (mode%blur_size .Gt. 1) Then
 
!        Smooth data, output in "memory"
           Call MA_TOPHATCON (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
             xendelm, yendelm, mode%blur_size, mode%blur_size, .True., &
             xmaxdat, ymaxdat, MDATA, status)
 
        Else
 
!        Copy data to memory
           Call MA_RCOPY (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
             yendelm, xmaxdat, ymaxdat, MDATA, status)
 
        End If
 
!     Check status
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     User message
        Call IO_WRITE ('INFO: Data smoothed or copied', status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        num_start = num_peaks ! Number of defined peaks at start of call
        old_start_peak = Max(1, num_peaks)
        start_peak = Max(1, num_peaks)
        num_saturated = 0
        num_bad = 0
        y_ref = ystrelm
        y_lower = ystrelm + 2
!     y_upper = ystrelm + 3 * (strip_width - 2) / 4
        y_upper = ystrelm + strip_width - 3
        y_top = 0
        retstat = 0 
        Do While (y_top .Lt. yendelm .And. retstat .Eq. 0)
 
           y_top = y_ref + strip_width - 2
 
!        Special case for last strip
           If (y_top .Ge. yendelm) Then
              y_upper = yendelm - 2
           End If
 
!        Perform peak search on one strip
           Call F2D_PEAKSTRIP (mode, mask_data, strip_width, &
             experiment%detector_gain, xmaxdat, ymaxdat, DATA, MASK, &
             MDATA, xstrelm, ystrelm, xendelm, yendelm, y_ref, y_lower, &
             y_upper, start_peak, xmax_work, ymax_work, max_peaks, num_peaks, &
             num_saturated, num_bad, PEAKS, IS_PEAK, PEAK_PIXELS, SUMS, &
             SQUARES, retstat, status)
 
!        y_ref = y_ref + (strip_width - 1) / 2
!        y_lower = y_upper + 1
!        y_upper = y_upper + (strip_width - 1) / 2
 
           y_ref = y_ref + strip_width - 2
           y_lower = y_ref + 2
           y_upper = y_upper + strip_width - 2
 
!        Only need to search from previous strips "start_peak"
           start_peak = old_start_peak
           old_start_peak = Max(1, num_peaks)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''num_peaks = '', i6)') num_peaks
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
!     Free memory for work arrays
        Deallocate (IS_PEAK)
        Deallocate (PEAK_PIXELS)
        Deallocate (SUMS)
        Deallocate (SQUARES)
 
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
        Write (*, '(''F2D_PEAKSEARCH: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_PEAKSEARCH
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

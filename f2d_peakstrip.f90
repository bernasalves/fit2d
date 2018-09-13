!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_peakstrip.f90 *
!  *                   *
!  *********************
 
!+ F2D_PEAKSTRIP - FIT 2-D PEAK search (diffraction) of a STRIP
     Subroutine F2D_PEAKSTRIP (mode, mask_data, strip_width, gain, &
       xmaxdat, ymaxdat, DATA, MASK, MDATA, xstrelm, ystrelm, xendelm, yendelm,&
       y_ref, y_lower, y_upper, start_peak, xmax_work, ymax_work, max_peaks, &
       num_peaks, num_saturated, num_bad, PEAKS, IS_PEAK, PEAK_PIXELS, SUMS, &
       SQUARES, retstat, status)
!  Description:
!    Searches the strip "(xstrelm, y_lower)" to "(xendelm, y_upper)" for peaks.
!    These are output in "PEAKS" which contains information on each peak.
!  Keywords:
!    Image~Display, Display~Image
!  Method:
!    Calculates if each pixel is above a threshold level, so may be considered 
!    as a peak pixel. This is stored in "IS_PEAK" for the strip. Then the peak
!    search begins.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Nov-2006: V0.12 Use "PEAK_SEARCH_CONTROL" structure (Hammersley)
!    15-Nov-2006: V0.11 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    26-Jun-1998: V0.10 Check that centre is highest of two rows and columns 
!      of following pixels (Hammersley)
!    21-Mar-1997: V0.9 Make data masking optional (Hammersley)
!    10-Mar-1997: V0.8 Allow data mask (Hammersley)
!    21-Feb-1997: V0.7 Allow up to 25 pixels to be counted as peak pixels 
!      (Hammersley)
!    16-Dec-1996: V0.6 Avoid open strings crossing lines (Hammersley)
!    12-Feb-1996: V0.5 Add gain (Hammersley)
!    09-Feb-1996: V0.4 Add "data_limit" threshold for good data values 
!      (Hammersley)
!    07-Feb-1996: V0.3 Add extra pass to check local background (Hammersley)
!    31-Jan-1996: V0.2 Calculate mean around peak (Hammersley)
!    29-Jan-1996: V0.1 Original, based on "f2d_peaksearch" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io.inc' ! Data structure definitions
!  Import:
     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode ! Mode and control of peak 
!      search
     Logical, Intent(IN) :: mask_data ! .True., if the data mask is to be used
     Integer, Intent(IN) :: strip_width ! Width of a strip
     Real, Intent(IN) :: gain ! Detector gain
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if the data is
!      masked-off
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat) ! The smoothed data values
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: y_ref ! Reference Y-pixel number for strip
     Integer, Intent(IN) :: y_lower ! The lower Y-pixel number of search region
     Integer, Intent(IN) :: y_upper ! The upper Y-pixel number of search region
     Integer, Intent(IN) :: start_peak ! First peak in found arrays, for
!      which it is necessary to compare positions
     Integer, Intent(IN) :: xmax_work ! Dimension of "IS_PEAK" array
     Integer, Intent(IN) :: ymax_work ! Dimension of "IS_PEAK" array
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
!  Import/Export:
     Integer, Intent(INOUT) :: num_peaks ! Total peaks found, including
!      problem peaks
     Integer, Intent(INOUT) :: num_saturated ! Number of saturated peaks
     Integer, Intent(INOUT) :: num_bad ! Number of bad "peaks"
     Type(PEAK_STRUCTURE), Intent(INOUT) :: PEAKS(max_peaks) ! Found peaks
!  Export:
     Byte, Intent(OUT) :: IS_PEAK(xmax_work, ymax_work) ! Work array for pixel
!      status !
!        0: if considered part of the general background
!        1: if candidate for part of a peak
!        2: if rejected
     Integer, Intent(OUT) :: PEAK_PIXELS(xmax_work) ! Number of peak pixels
!      in a box column
     Double Precision, Intent(OUT) :: SUMS(xmax_work) ! Contains sums of
!      counts
     Double Precision, Intent(OUT) :: SQUARES(xmax_work) ! Contains sums of
!      squares of counts
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Peak arrays not big enough
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.12' ! Version number
!  Local Variables:
     Double Precision :: mean ! Mean value in data region
     Double Precision :: sigma ! Standard deviation in data region
     Double Precision :: sum ! Sum of pixel values
     Double Precision :: sum_squared ! Sum of squared pixel values
     Double Precision :: term ! Term used to calculate sigma
     Integer :: num_peak_pixels ! Number of pixels considered as peak pixels 
!      within the background box
     Integer :: num_pixels ! Number of pixels used to calculate running mean
     Integer :: num_threshold ! Number of pixels above the threshold level
     Integer :: peak ! Loop variable for peaks
     Integer :: peak_retstat ! Return status from "PEAKINTEGRATE"
     Integer :: strip_height ! Height of strip to calculate
     Integer :: width ! Number of pixel in X-direction in ROI
     Integer :: x ! Loop variable for X-direction
     Integer :: x1 ! Loop variable for X-direction
     Integer :: x_back ! X-pixel number for "IS_PEAK" calculation
     Integer :: x_data ! X-pixel number for element in data array
     Integer :: y ! Loop variable for Y-direction
     Integer :: y1 ! Loop variable for Y-direction
     Integer :: y_back ! Y-pixel number for "IS_PEAK" calculation
     Integer :: y_data ! Y-pixel number for element in data array
     Logical :: bad_statistics ! .True., if not enough pixel values are
!      above the minimum accepted data value
     Logical :: new_peak ! .True., if a pixel position is considered a new peak
     Real :: bad_threshold ! The intensity value below which a pixel will
!      be considered as being bad, and not take part in mean background 
!      calculations
     Real :: edge_max ! Maximum value found around the edge of a "peak"
     Real :: maximum ! Maximum value in a "peak" area
     Real :: old_peak_threshold ! Previous iteration value of "peak_threshold"
!      be considered as being part of a peak
     Real :: peak_threshold ! The intensity value above which a pixel
!      is considered to be part of a peak
     Real :: sqr_min_spacing ! Square of the minimum allowed spacing in pixels 
!      between peak centres, should be greater than "mode%blur_size"
     Real :: value ! Value of a pixel
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKSTRIP ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. xmax_work .Le. 0 .Or. max_peaks .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. ymax_work .Le. 0) Then
        status = St_bad_dim2
     Else If (mode%mode .Ne. 0 .And. mode%mode .Ne. 1) Then
        status = St_bad_int1
     Else If (mode%blur_size .Lt. 1) Then
        status = St_bad_int1
     Else If (mode%min_spacing .Le. mode%blur_size) Then
        status = St_bad_rel1
     Else If (xmax_work .Lt. xendelm - xstrelm + 1) Then
        status = St_bad_elim2
     Else If (ymax_work .Lt. mode%box_size) Then
        status = St_bad_elim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PEAKSTRIP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_PEAKSTRIP'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Initialise variables
        width = xendelm - xstrelm + 1
        strip_height = Min(strip_width, yendelm - y_ref + 1)
        sqr_min_spacing = mode%min_spacing**2
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xstrelm, ystrelm, xendelm, yendelm = '', 4i6)')
!     :       xstrelm, ystrelm, xendelm, yendelm
!     Write (*, '(''width, height = '', 2i6)') width, height
!     Write (*, '(''y_ref, y_lower, y_upper = '', 3i6)')
!     :       y_ref, y_lower, y_upper
!     Write (*, '(''start_peak = '', i6)') start_peak
!     Write (*, '(''xmax_work, ymax_work = '', 2i12)')
!     :       xmax_work, ymax_work
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set good status
        retstat = 0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_PEAKSTRIP: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_PEAKSTRIP
!********1*********2*********3*********4*********5*********6*********7*********8
 

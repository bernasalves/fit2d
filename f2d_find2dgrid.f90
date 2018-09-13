!*********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_find2dgrid.f90 *
!  *                    *
!  **********************
 
!+ F2D_FIND2DGRID - Fit2D: FIND GRID peaks positions
     Subroutine F2D_FIND2DGRID (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, x_start, y_start, x_axis1, y_axis1, x_axis2, y_axis2, &
       output_frequency, xmax_peaks, ymax_peaks, ref_correlation, retstat, &
       xnum_grid, ynum_grid, num_peaks, num_left, num_down, X_2DPEAKS, &
       Y_2DPEAKS, status)
!  Description:
!    Tries to find approximately regularly spaced peaks which are
!    placed in a grid e.g. a calibration grid which has
!    been measured by a detector which introduces some spatial
!    distortion. The search is limited to the region of the data
!    from "(xstrelm, ystrelm) to "(xendelm, yendelm)".
!  Keywords:
!    Grid.Peak~Search, Centre.Peak.Determination.Grid,
!    Peak.Centre.Determination, Calibration.Grid~Positions,
!    Grid~Positions.Calibration
!  Method:
!    The method consists of the following stages:
!    1  Define starting grid vectors
!    2  Store peak positions; Array element (1, 1) is used for the
!    starting peak, (2, 1) for the next horizontal peak, and (1, 2)
!    for the next vertical peak
!    3  Search to right edge of ROI, jumping over missing peaks
!    if necessary (See below)
!    4  Search to left edge of ROI, jumping over missing peaks if
!    necessary (See below). Peaks are wrapped to the opposite
!    side of the peak position array
!    5  Search to upper edge of ROI, jumping over missing peaks
!    if necessary (See below)
!    6 Search to lower edge of ROI, jumping over missing peaks if
!    necessary (See below). Peaks are wrapped to the opposite
!    side of the peak position array
!    7  For each quadrant find all remaining peaks, using predicted
!    position from the two peak vectors already available to the
!    sides of the peak. Storing maximum left/right up/down
!    numbers of peaks
!    8  When the grid search is complete, rotate (in place) the
!    peaks position arrays, so that the element (1, 1) contains
!    the lower left limit of the grid (even if the peak is
!    missing).
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    04-Jan-1994: V0.3 Output return status (Hammersley)
!    23-Dec-1993: V0.2 Pass size of quadrant to quadrant searching routines 
!      (Hammersley)
!    26-Oct-1993: V0.1 Original, based on "F2D_FINGRID" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First Dimension size for "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second Dimension size for "DATA"
     Integer, Intent(IN) :: xstrelm ! First X-element of data region
     Integer, Intent(IN) :: ystrelm ! First Y-element of data region
     Integer, Intent(IN) :: xendelm ! Last X-element of data region
     Integer, Intent(IN) :: yendelm ! Last Y-element of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data,
!      with grid peaks
     Integer, Intent(IN) :: xmaxtemplate ! First Dimension size for "TEMPLATE"
     Integer, Intent(IN) :: ymaxtemplate ! Second Dimension size for "TEMPLATE"
     Integer, Intent(IN) :: xnumtemplate ! Number of elements in first
!      dimension in "TEMPLATE"
     Integer, Intent(IN) :: ynumtemplate ! Number of elements in second
!      dimension in "TEMPLATE"
     Real, Intent(IN) :: TEMPLATE(xmaxtemplate, ymaxtemplate) ! Template
!    function at pixel resolution
     Integer, Intent(IN) :: xmaxsubtemplate ! First Dimension size for
!      "SUBTEMPLATE"
     Integer, Intent(IN) :: ymaxsubtemplate ! Second Dimension size for
!      "SUBTEMPLATE"
     Integer, Intent(IN) :: xnumsubtemplate ! Number of elements in first
!      dimension in "SUBTEMPLATE"
     Integer, Intent(IN) :: ynumsubtemplate ! Number of elements in second
!      dimension in "SUBTEMPLATE"
     Integer, Intent(IN) :: sub_pixelling ! The number of sub-pixels to use
!      for over-sampling the data, when cross-correlating with the template 
!      function
     Real, Intent(IN) :: SUBTEMPLATE(xmaxsubtemplate, ymaxsubtemplate)
!      Template function e.g. calculated peak profile (If "sub_pixelling"
!      is greater than 1 then, this function must be calculated on this 
!      resolution scale)
     Integer, Intent(IN) :: search_limit ! Maximum distance from the starting
!      position at which a peak is accepted as found
     Real, Intent(IN) :: detect_ratio ! Ratio of a cross-correlation value
!      found for a peak divided by previous found peak value, above which
!      a new "peak" is accepted (avoids noise being classed as peaks)
     Real, Intent(IN) :: x_start ! The X-pixel coordinate of the starting peak
     Real, Intent(IN) :: y_start ! The Y-pixel coordinate of the starting peak
     Real, Intent(IN) :: x_axis1 ! The X-pixel coordinate of the next axis 1
!      peak
     Real, Intent(IN) :: y_axis1 ! The Y-pixel coordinate of the next axis 1
!      peak
     Real, Intent(IN) :: x_axis2 ! The X-pixel coordinate of the next axis 2
!      peak
     Real, Intent(IN) :: y_axis2 ! The Y-pixel coordinate of the next axis 2
!      peak
     Integer, Intent(IN) :: output_frequency ! Frequency with which progress
!      report is given to the user (use very big numbers to avoid output)
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_2DPEAKS"
!      and "Y_2DPEAKS"
     Integer, Intent(IN) :: ymax_peaks ! First dimension size of "X_2DPEAKS"
!      and "Y_2DPEAKS"
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation ! On input this is the reference
!      correlation value for comparison of the first found peak correlation 
!      value. On output this is the cross-correlation value of last found peak
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status, grid too large for internal array
!      2 = Bad status, peaks missing from central cross
     Integer, Intent(OUT) :: xnum_grid ! The maximum grid size in the 
!      X-direction (horizontal)
     Integer, Intent(OUT) :: ynum_grid ! The maximum grid size in the
!      Y-direction (vertical)
     Integer, Intent(OUT) :: num_peaks ! The number of found peaks
     Integer, Intent(OUT) :: num_left ! Number of grid positions found to
!      left of starting peak
     Integer, Intent(OUT) :: num_down ! Number of grid positions found below
!      starting peak
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
!      The best correlation positions (pixel coordinates)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!      The best correlation positions (pixel coordinates)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: num_right ! Number of grid positions found to right of starting
!      peak
     Integer :: num_up ! Number of grid positions found above starting peak
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FIND2DGRID'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status.
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FIND2DGRID ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0 .Or. xmaxtemplate .Le. 0 .Or. xmaxsubtemplate .Le. 0 &
       .Or. xmax_peaks .Le. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. ymaxtemplate .Le. 0 .Or. ymaxsubtemplate &
       .Le. 0 .Or. ymax_peaks .Le. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xendelm .Gt. xmaxdat .Or. xstrelm .Gt. &
       xendelm) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. yendelm .Gt. ymaxdat .Or. ystrelm .Gt. &
       yendelm) Then
        status = St_bad_adr2
     Else If (sub_pixelling .Le. 0 .Or. search_limit .Le. 0) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_ma + status
        Call ST_SAVE ('Subroutine F2D_FIND2DGRID ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input parameters appear to be correct
 
!     Store initial three peaks
        X_2DPEAKS(1, 1) = x_start
        Y_2DPEAKS(1, 1) = y_start
        X_2DPEAKS(2, 1) = x_axis1
        Y_2DPEAKS(2, 1) = y_axis1
        X_2DPEAKS(1, 2) = x_axis2
        Y_2DPEAKS(1, 2) = y_axis2
        num_peaks = 3
 
!     Find all peaks to right of starting point
        Call F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, 0, xmax_peaks, ymax_peaks, &
          ref_correlation, num_peaks, num_right, X_2DPEAKS, Y_2DPEAKS, status)
        num_right = num_right + 2
 
!     Find all peaks above starting point
        Call F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, 1, xmax_peaks, ymax_peaks, &
          ref_correlation, num_peaks, num_up, X_2DPEAKS, Y_2DPEAKS, status)
        num_up = num_up + 2
 
!     Find all peaks to left of starting point
        Call F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, 2, xmax_peaks, ymax_peaks, &
          ref_correlation, num_peaks, num_left, X_2DPEAKS, Y_2DPEAKS, status)
 
!     Find all peaks below starting point
        Call F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, 3, xmax_peaks, ymax_peaks, &
          ref_correlation, num_peaks, num_down, X_2DPEAKS, Y_2DPEAKS, status)
 
!     Progress report
        If (num_peaks .Gt. output_frequency) Then
           Call IO_WRITE ('INFO: Central cross peaks found', status)
        End If
 
!     Calculate size of grid
        xnum_grid = num_left + num_right
        ynum_grid = num_up + num_down
 
!     Check arrays are big enough and that no peaks are missing
        If (xnum_grid .Gt. xmax_peaks .Or. ynum_grid .Gt. ymax_peaks) Then
 
!        Arrays are too small
           Call IO_WRITE ('ERROR: Peak position arrays are too small', &
             status)
           retstat = 1
           Return
 
        Else If (num_peaks .Lt. xnum_grid + ynum_grid - 1) Then
 
!        Peaks are missing from central cross
           Call IO_WRITE ('ERROR: Peaks are missing from central cross', &
             status)
           retstat = 2
           Return
        End If
 
        retstat = 0
 
!     Find all peaks in upper right quadrant
        Call F2D_FINDURQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, output_frequency, xmax_peaks, &
          ymax_peaks, num_right, num_up, ref_correlation, num_peaks, &
          X_2DPEAKS, Y_2DPEAKS, status)
 
!     Read (*, *)
 
!     Find all peaks in upper left quadrant
        Call F2D_FINDULQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, output_frequency, xmax_peaks, &
          ymax_peaks, num_left, num_up, ref_correlation, num_peaks, X_2DPEAKS, &
          Y_2DPEAKS, status)
 
!     Read (*, *)
 
!     Find all peaks in lower right quadrant
        Call F2D_FINDLRQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, output_frequency, xmax_peaks, &
          ymax_peaks, num_right, num_down, ref_correlation, num_peaks, &
          X_2DPEAKS, Y_2DPEAKS, status)
 
!     Read (*, *)
 
!     Find all peaks in lower left quadrant
        Call F2D_FINDLLQUAD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, &
          ynumtemplate, TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, &
          xnumsubtemplate, ynumsubtemplate, sub_pixelling, SUBTEMPLATE, &
          search_limit, detect_ratio, output_frequency, xmax_peaks, &
          ymax_peaks, num_left, num_down, ref_correlation, num_peaks, &
          X_2DPEAKS, Y_2DPEAKS, status)
 
     End If
 
     End Subroutine F2D_FIND2DGRID
!********1*********2*********3*********4*********5*********6*********7**
 

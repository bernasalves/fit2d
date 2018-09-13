!*********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_findline.f90 *
!  *                  *
!  ********************
 
!+ F2D_FINDLINE - Fit2D: FIND LINE of peaks positions
     Subroutine F2D_FINDLINE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, xmaxtemplate, ymaxtemplate, xnumtemplate, ynumtemplate, &
       TEMPLATE, xmaxsubtemplate, ymaxsubtemplate, xnumsubtemplate, &
       ynumsubtemplate, sub_pixelling, SUBTEMPLATE, search_limit, &
       detect_ratio, search_direction, xmax_peaks, ymax_peaks, &
       ref_correlation, num_peaks, num_grid, X_2DPEAKS, Y_2DPEAKS, status)
!  Description:
!    Tries to find approximately regularly spaced peaks which are placed in a 
!    line e.g. part of a calibration grid which has been measured by a detector
!    which introduces some spatial distortion. The search is limited to the 
!    region of the data from "(xstrelm, ystrelm)" to "(xendelm, yendelm)".
!    The search starts from the peaks positions (1, 1) together with (2, 1) 
!    or (1, 2) depending on the direction of the search.
!  Keywords:
!    Line.Peak~Search, Centre.Peak.Determination.Line,
!    Peak.Centre.Determination, Calibration.Line~Positions,
!    Line~Positions.Calibration
!  Method:
!    The method consists of searching towards an edge of the ROI,
!    jumping over missing peaks if necessary.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    04-Jan-1994: V0.2 More robust for X-ray image intensifier data (Hammersley)
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
!      resolution scale
     Integer, Intent(IN) :: search_limit ! Maximum distance from the starting
!      position at which a peak is accepted as found
     Real, Intent(IN) :: detect_ratio ! Ratio of a cross-correlation value
!      found for a peak divided by previous found peak value, above which
!      a new "peak" is accepted (avoids noise being classed as peaks)
     Integer, Intent(IN) :: search_direction ! Direction of search:
!      0 = Right from (1, 1) and (2, 1)
!      1 = Up from (1, 1) and (1, 2)
!      2 = Left from (2, 1) and (1, 1)
!      3 = Down from (1, 2) and (1, 1)
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_2DPEAKS"
!      and "Y_2DPEAKS"
     Integer, Intent(IN) :: ymax_peaks ! First dimension size of "X_2DPEAKS"
!      and "Y_2DPEAKS"
!  Import/Export:
     Real, Intent(INOUT) :: ref_correlation ! On input this is the reference
!    correlation value for comparison of the first found peak
!    correlation value. On output this is the cross-correlation
!    value of last found peak
     Integer, Intent(INOUT) :: num_peaks ! Total number of found peaks
!      found, each time a new peak is found this number is incremented
!  Export:
     Integer, Intent(OUT) :: num_grid ! Grid size found in search direction
     Real, Intent(OUT) :: X_2DPEAKS(xmax_peaks, ymax_peaks)
!      The best correlation positions (pixel coordinates)
     Real, Intent(OUT) :: Y_2DPEAKS(xmax_peaks, ymax_peaks)
!      The best correlation positions (pixel coordinates)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: gap ! Number of grid spacings since last found peak
     Integer :: x_increment ! Change to X-element for one grid peak
     Integer :: x_last ! Element of last found X-peak
     Integer :: x_subpixel ! X-sub-pixel coordinate for peak centre
     Integer :: xpeak ! Array element for X-direction
     Integer :: xprevious ! Array element for X-direction for
!      previously defined peak
     Integer :: y_increment ! Change to Y-element for one grid peak
     Integer :: y_last ! Element of last found Y-peak
     Integer :: y_subpixel ! Y-sub-pixel coordinate for peak centre
     Integer :: ypeak ! Array element for Y-direction
     Integer :: yprevious ! Array element for Y-direction for
!      previously defined peak
     Logical :: continue ! .True., until edge of ROI reached
     Logical :: peak_found ! .True., if a peak is found
     Logical :: success ! .True., if a peak is found in the search area
     Real :: correlation ! Cross-correlation value of peak
     Real :: x_coordinate ! X-coordinate for a peak centre
     Real :: x_vector ! X-component of vector between peaks
     Real :: y_coordinate ! Y-coordinate for a peak centre
     Real :: y_vector ! Y-component of vector between peaks
!  Local Data:
!    Internal Functions:
     Integer :: Pc2subpixel ! Convert pixel coordinate to sub-pixel number
     Real :: Subpixel2pc ! Convert sub-pixel number to pixel coordinate
!--------1---------2---------3---------4---------5---------6---------7--
!  Define in-line functions
 
!  Convert pixel coordinate to sub-pixel number
     Pc2subpixel(x_coordinate) = Int(x_coordinate * Real(sub_pixelling)) + 1
 
!  Convert sub-pixel number to pixel coordinate
     Subpixel2pc(x_subpixel) = Real(x_subpixel) / Real(sub_pixelling) - 1.0 / &
       Real(sub_pixelling * 2)
 
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FINDLINE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status.
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FINDLINE ' // Version)
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
     Else If (search_direction .Lt. 0 .Or. search_direction .Gt. 3) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_ma + status
        Call ST_SAVE ('Subroutine F2D_FINDLINE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input parameters appear to be correct
 
!     Calculate starting search position and vector
        If (search_direction .Eq. 0) Then
 
!        Search towards right
           xpeak = 3
           ypeak = 1
           xprevious = 2
           yprevious = 1
           x_increment = 1
           y_increment = 0
           x_last = 2
           x_vector = X_2DPEAKS(2, 1) - X_2DPEAKS(1, 1)
           y_vector = Y_2DPEAKS(2, 1) - Y_2DPEAKS(1, 1)
           x_coordinate = X_2DPEAKS(2, 1) + x_vector
           y_coordinate = Y_2DPEAKS(2, 1) + y_vector
 
        Else If (search_direction .Eq. 1) Then
 
!        Search towards top
           xpeak = 1
           ypeak = 3
           xprevious = 1
           yprevious = 2
           x_increment = 0
           y_increment = 1
           y_last = 2
           x_vector = X_2DPEAKS(1, 2) - X_2DPEAKS(1, 1)
           y_vector = Y_2DPEAKS(1, 2) - Y_2DPEAKS(1, 1)
           x_coordinate = X_2DPEAKS(1, 2) + x_vector
           y_coordinate = Y_2DPEAKS(1, 2) + y_vector
 
        Else If (search_direction .Eq. 2) Then
 
!        Search towards left
           xpeak = xmax_peaks
           ypeak = 1
           xprevious = 1
           yprevious = 1
           x_increment = -1
           y_increment = 0
           x_last = xmax_peaks + 1
           x_vector = X_2DPEAKS(1, 1) - X_2DPEAKS(2, 1)
           y_vector = Y_2DPEAKS(1, 1) - Y_2DPEAKS(2, 1)
           x_coordinate = X_2DPEAKS(1, 1) + x_vector
           y_coordinate = Y_2DPEAKS(1, 1) + y_vector
 
        Else If (search_direction .Eq. 3) Then
 
!        Search towards bottom
           xpeak = 1
           ypeak = ymax_peaks
           xprevious = 1
           yprevious = 1
           x_increment = 0
           y_increment = -1
           y_last = ymax_peaks + 1
           x_vector = X_2DPEAKS(1, 1) - X_2DPEAKS(1, 2)
           y_vector = Y_2DPEAKS(1, 1) - Y_2DPEAKS(1, 2)
           x_coordinate = X_2DPEAKS(1, 1) + x_vector
           y_coordinate = Y_2DPEAKS(1, 1) + y_vector
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Search for peaks until edge of ROI or storage array is full
        peak_found = .True.
        gap = 1
        continue = x_coordinate .Ge. Real(xstrelm - 1) .And. x_coordinate .Le. &
          Real(xendelm) .And. y_coordinate .Ge. Real(ystrelm - 1) .And. &
          y_coordinate .Le. Real(yendelm)
 
        Do While (continue)
 
           If (peak_found) Then
 
!           Convert pixel coordinates to sub-pixel numbers
              x_subpixel = Pc2subpixel(x_coordinate)
              y_subpixel = Pc2subpixel(y_coordinate)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''input x, y sub-pixel = '', 2i10)')
!           :             x_subpixel, y_subpixel
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Calculate optimum centre to nearest sub-pixel
              Call MA_BESTCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, DATA, xmaxsubtemplate, ymaxsubtemplate, &
                xnumsubtemplate, ynumsubtemplate, sub_pixelling, &
                sub_pixelling, SUBTEMPLATE, search_limit*sub_pixelling, &
                search_limit*sub_pixelling, x_subpixel, y_subpixel, success, &
                correlation, status)
 
!           Convert sub-pixel numbers to pixel coordinates
              x_coordinate = Subpixel2pc(x_subpixel)
              y_coordinate = Subpixel2pc(y_subpixel)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''output x, y sub-pixel = '', 2i10)')
!           :             x_subpixel, y_subpixel
!           Write (*, '(''output x, y pc = '', 2f12.5)')
!           :             x_coordinate, y_coordinate
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Else
 
!           First conduct search to nearest pixel over defined
!           region, followed by a sub-pixel search
              Call F2D_FINDCENTRE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
                xendelm, yendelm, DATA, xmaxtemplate, ymaxtemplate, &
                xnumtemplate, ynumtemplate, TEMPLATE, xmaxsubtemplate, &
                ymaxsubtemplate, xnumsubtemplate, ynumsubtemplate, &
                sub_pixelling, SUBTEMPLATE, search_limit, x_coordinate, &
                y_coordinate, success, correlation, status)
 
           End If
 
           If (success .And. correlation / ref_correlation .Gt. detect_ratio) &
             Then
 
!           Peak found
              peak_found = .True.
              num_peaks = num_peaks + 1
 
!           Convert sub-pixel values to pixel coordinates
              X_2DPEAKS(xpeak, ypeak) = x_coordinate
              Y_2DPEAKS(xpeak, ypeak) = y_coordinate
              ref_correlation = correlation
 
!           Update vector
              x_vector = (x_coordinate - X_2DPEAKS(xprevious, yprevious)) / &
                Real(gap)
              y_vector = (y_coordinate - Y_2DPEAKS(xprevious, yprevious)) / &
                Real(gap)
              x_coordinate = x_coordinate + x_vector
              y_coordinate = y_coordinate + y_vector
              xprevious = xpeak
              yprevious = ypeak
              gap = 1
 
!           Save index of last found peak
              x_last = xpeak
              y_last = ypeak
 
           Else
 
              peak_found = .False.
              gap = gap + 1
              x_coordinate = X_2DPEAKS(xprevious, yprevious) + x_vector * gap
              y_coordinate = Y_2DPEAKS(xprevious, yprevious) + y_vector * gap
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''xpeak, ypeak = '', 2i6)') xpeak, ypeak
!        Write (*, '(''X, Y coordinate, correlation = '', 3e14.7)')
!        :       X_2DPEAKS(xpeak, ypeak), Y_2DPEAKS(xpeak, ypeak),
!        :       correlation
!        Write (*, '(''X, Y new peak = '', 2f12.5)')
!        :       x_coordinate, y_coordinate
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Update array element for new peaks
           xpeak = xpeak + x_increment
           ypeak = ypeak + y_increment
 
!        Check that search is still within ROI
           continue = x_coordinate .Ge. Real(xstrelm - 1) .And. x_coordinate &
             .Le. Real(xendelm) .And. y_coordinate .Ge. Real(ystrelm - 1) &
             .And. y_coordinate .Le. Real(yendelm) .And. xpeak .Ge. 1 .And. &
             xpeak .Le. xmax_peaks .And. ypeak .Ge. 1 .And. ypeak .Le. &
             ymax_peaks
 
        End Do
 
!     Set size of grid found
        If (search_direction .Eq. 0) Then
           num_grid = x_last - 2
        Else If (search_direction .Eq. 1) Then
           num_grid = y_last - 2
        Else If (search_direction .Eq. 2) Then
           num_grid = xmax_peaks - x_last + 1
        Else If (search_direction .Eq. 3) Then
           num_grid = ymax_peaks - y_last + 1
        End If
 
     End If
 
     End Subroutine F2D_FINDLINE
!********1*********2*********3*********4*********5*********6*********7**

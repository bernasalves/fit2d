!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_gstatistics.f90 *
!  *                     *
!  ***********************
 
!+ F2D_GSTATISTICS: Graphically input region STATISTICS
     Subroutine F2D_GSTATISTICS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, zlabel, &
       status)
!  Description:
!    1. Graphical input of polygon by user.
!    2. Display polygon
!    3. Calculate all points inside polygon
!    4. Calculate statistical quantities for polygon region
!  Keywords:
!    Statistics.Polygon~Region, Polygon~Region.Statistics
!  Method:
!    Double Precision arithmetic is used to avoid rounding errors
!    leading to negative square roots when calculating sigma.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.16 Include "MASK", and use masking (Hammersley)
!    25-Jun-2009: V0.15 Add that no coordinates gives statistics for the current
!      ROI (Hammersley)
!    15-Jan-2008: V0.14 Output position of data maximum (Hammersley)
!    31-Mar-2006: V0.13 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    25-Sep-1998: V0.12 Set internal variable values correctly
!      (previous ##TOTAL, ##MEAN, ##RMS were being set incorrectly
!      owing to being Double Precision and passed as Real) (Hammersley)
!    21-Aug-1998: V0.11 Replace "SYMBOL" calls with "VARIABLE" calls 
!      (Hammersley)
!    11-Aug-1998: V0.10 Changes to "IO_SET_SYMBOL" (Hammersley)
!    16-Dec-1996: V0.9 Avoid open strings crossing lines (Hammersley)
!    27-Aug-1996: V0.8 Don't change message style (Hammersley)
!    26-Aug-1996: V0.7 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.6 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    29-Mar-1996: V0.5 Output "working" message whilst calculating statistics
!      (Hammersley)
!    10-Feb-1996: V0.4 Add output of estimated gain (Hammersley)
!    26-Oct-1995: V0.3 Use "spy-glass" output during coordinate input, and set
!      symbol values (Hammersley)
!    20-Jun-1995: V0.2 Change to use GS graphics library (Hammersley)
!    04-Apr-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines start of ROI in X-direction
     Integer, Intent(IN) :: ystrelm ! Defines start of ROI in Y-direction
     Integer, Intent(IN) :: xendelm ! Defines end of ROI in X-direction
     Integer, Intent(IN) :: yendelm ! Defines end of ROI in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Array to contain X-coordinate
!      grid data
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Array to contain Y-coordinate
!      grid data
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array to contain data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.16' ! Version number
     Integer, Parameter :: Max_coordinates = 200 ! Dimension size for
!      coordinate areas
     Integer, Parameter :: Max_cross = 100 ! Maximum number of intersections of
!      lines of equal y, with the boundary of the polygon, at values of y
!      which are local minima and maxima defined for one polygon
     Integer, Parameter :: Max_message = 11 ! Dimension for message array
!  Local Variables:
     Character(Len = 1) :: string_value ! Dummy value for "IO_SET_VARIABLE"
     Double Precision :: mean ! Average value
     Double Precision, Save :: previous_mean = 0.0d0 ! Average value from
!      previous call
     Double Precision :: sigma ! Standard deviation of data values in polygon
     Double Precision :: sum ! Sum of values inside polygon
     Double Precision :: sum_squares ! Sum of squares of values inside polygon
     Integer :: dummy ! Dummy variable
     Integer :: dummy1 ! Dummy variable for "MA_GA05A"
     Integer :: dummy2 ! Dummy variable for "MA_GA05A"
     Integer :: dummy3 ! Dummy variable for "MA_GA05A"
     Integer :: int_value ! Dummy value for "IO_SET_VARIABLE"
     Integer :: len_string ! Dummy value for "IO_SET_VARIABLE"
     Integer :: line ! Loop variable for lines of output
     Integer :: num_coordinates ! Number of user defined coordinates in polygon
     Integer :: num_message ! Number of lines of text to output
     Integer :: num_pixels ! Number of pixels in polygon region
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room in symbol store
!      2 = No more room for character strings
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for X-direction pixels
     Integer :: x_maximum ! X-pixel of data maximum
     Integer :: xmaxpix ! The maximum X-pixel number for the search region
     Integer :: xminpix ! The minimum X-pixel number for the search region
     Integer :: y ! Loop variable for Y-direction pixels
     Integer :: y_maximum ! Y-pixel of data maximum
     Integer :: ymaxpix ! The maximum Y-pixel number for the search region
     Integer :: yminpix ! The minimum Y-pixel number for the search region
     Logical :: log_value ! Dummy value for "IO_SET_VARIABLE"
     Real :: inside ! Greater than zero indicates point is within the polygon,
!      zero if on the boundary, otherwise negative
     Real :: maximum ! Maximum data value found within the polygon
     Real :: minimum ! Minimum data value found within the polygon
     Real :: xmaximum ! The maximum X-data coordinate for the search region
     Real :: xmaxmes ! Maximum X-coordinate of message region
     Real :: xminimum ! The minimum X-data coordinate for the search region
     Real :: xminmes ! Minimum X-coordinate of message region
     Real :: xposition ! X-data-corrdinate of pixel
     Real :: ymaximum ! The maximum Y-data coordinate for the search region
     Real :: ymaxmes ! Maximum Y-coordinate of message region
     Real :: yminimum ! The minimum Y-data coordinate for the search region
     Real :: yminmes ! Minimum Y-coordinate of message region
     Real :: yposition ! Y-data-corrdinate of pixel
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(Max_message) ! User output text
     Integer*2, Allocatable :: INDP(:, :) ! Work array "INDP(2, Max_cross)"
     Real, Allocatable :: WORK(:) ! Work array for "MA_GA05A" "WORK(Max_cross*4)
     Real :: X_COORDINATES(Max_coordinates) ! X-coordinates of user defined 
!      polygon
     Real :: Y_COORDINATES(Max_coordinates) ! Y-coordinates of user defined 
!      polygon
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GSTATISTICS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GSTATISTICS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Find out current message region
        Call GS_INQ_MESSAGE (xminmes, yminmes, xmaxmes, ymaxmes, status)
 
!     Input points to define 2-D polygon
        num_coordinates = 0
        MESSAGE(1) = 'Click on points to define an arbitrary polygon'
        MESSAGE(2) = 'region, in which various statistics will be'
        MESSAGE(3) = 'calculated.'
        MESSAGE(4) = 'No coordinates input will give the full ROI,'
        MESSAGE(5) = 'and 2 coordinates will defined a rectangle.'
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'ENTER COORDINATES TO DEFINE POLYGON REGION', 5, MESSAGE, .True., &
          Max_coordinates - 1, num_coordinates, X_COORDINATES, Y_COORDINATES, &
          status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Check that a polygon has been input
        If (num_coordinates .Le. 1) Then

!        Define a rectangle from the full ROI
           xminimum = Real(xstrelm) - 0.5
           yminimum = Real(ystrelm) - 0.5
           xmaximum = Real(xendelm) - 0.5
           ymaximum = Real(yendelm) - 0.5
           X_COORDINATES(1) = xminimum
           Y_COORDINATES(1) = yminimum
           X_COORDINATES(2) = xminimum
           Y_COORDINATES(2) = ymaximum
           X_COORDINATES(3) = xmaximum
           Y_COORDINATES(3) = ymaximum
           X_COORDINATES(4) = xmaximum
           Y_COORDINATES(4) = yminimum
           num_coordinates = 4

        Else If (num_coordinates .Eq. 2) Then
 
!        Define a rectangle from the two input coordinates
           xminimum = Min (X_COORDINATES(1), X_COORDINATES(2))
           yminimum = Min (Y_COORDINATES(1), Y_COORDINATES(2))
           xmaximum = Max (X_COORDINATES(1), X_COORDINATES(2))
           ymaximum = Max (Y_COORDINATES(1), Y_COORDINATES(2))
           X_COORDINATES(1) = xminimum
           Y_COORDINATES(1) = yminimum
           X_COORDINATES(2) = xminimum
           Y_COORDINATES(2) = ymaximum
           X_COORDINATES(3) = xmaximum
           Y_COORDINATES(3) = ymaximum
           X_COORDINATES(4) = xmaximum
           Y_COORDINATES(4) = yminimum
           num_coordinates = 4
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Display user defined region as shaded region
 
!     Make regions closed, by specifying end point as the start point
        X_COORDINATES(num_coordinates + 1) = X_COORDINATES(1)
        Y_COORDINATES(num_coordinates + 1) = Y_COORDINATES(1)
 
!     Draw line around polygon
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_red, status)
        Call LG_POLYLINE (num_coordinates + 1, X_COORDINATES, Y_COORDINATES, &
          status)
 
        Call GS_FPROMPT (1, 1, 'PLEASE WAIT: CALCULATING POLYGON STATISTICS', &
          status)
 
!     Update workstation
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set-up arrays for determining points within polygon
 
!     Find points inside dead zone polygon
 
!     Allocate memory for "MA_GA05A" arrays
        Allocate (WORK(Max_cross * 4), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GSTATISTICS ' // Version)
           Return
        End If
        Allocate (INDP(2, Max_cross * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_GSTATISTICS ' // Version)
           Return
        End If

!     Specify polygon region in form for checking on whether a point is inside 
!     or outside the dead zone
        Call MA_GA05A (num_coordinates + 1, 1, WORK, Max_cross * 4, &
          INDP, Max_cross, X_COORDINATES, Y_COORDINATES, DUMMY1, DUMMY2, DUMMY3)
 
!     Find maximum and minimum X/Y values to define search region
        Call MA_RMINMAX (Max_coordinates, 1, X_COORDINATES, 1, 1, &
          num_coordinates, 1, xminimum, xmaximum, status)
        Call MA_RMINMAX (Max_coordinates, 1, Y_COORDINATES, 1, 1, &
          num_coordinates, 1, yminimum, ymaximum, status)
 
!     Convert data coordinate limits to pixel numbers
        Call GS_CAL_WCTOPIX (xmaxdat, xendelm, X_AXIS, xminimum, xminpix, &
          status)
        Call GS_CAL_WCTOPIX (xmaxdat, xendelm, X_AXIS, xmaximum, xmaxpix, &
          status)
        Call GS_CAL_WCTOPIX (ymaxdat, yendelm, Y_AXIS, yminimum, yminpix, &
          status)
        Call GS_CAL_WCTOPIX (ymaxdat, yendelm, Y_AXIS, ymaximum, ymaxpix, &
          status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Within defined search region, find all points within defined polygon
        num_pixels = 0
        sum = 0.0d0
        sum_squares = 0.0d0
        minimum = 1.7e38
        maximum = -1.7e38
        Do y = yminpix, ymaxpix
           yposition = Y_AXIS(y)
 
           Do x = xminpix, xmaxpix
 
!           Define X/Y position
              xposition = X_AXIS(x)
 
!           Test if point is within the polygon
              Call MA_GA05B (xposition, yposition, inside, &
                num_coordinates + 1, 1, WORK, Max_cross * 4, INDP, Max_cross, &
                X_COORDINATES, Y_COORDINATES, dummy1, dummy2, dummy3)
 
              If (inside .Ge. 0.0 .And. .Not. MASK(x, y)) Then
                 num_pixels = num_pixels + 1
                 sum = sum + DATA(x, y)
                 sum_squares = sum_squares + DATA(x, y) * DATA(x, y)
 
!              Set minimum/maximum values
                 minimum = Min(minimum, DATA(x, y))

                 If (DATA(x, y) .Gt. maximum) Then
                    maximum = DATA(x, y)
                    x_maximum = x
                    y_maximum = y
                 End If
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate average
        If (num_pixels .Ge. 1) Then
           mean = sum / Dble(num_pixels)
        End If
 
!     Calculate standard deviation (n - 1).
        If (num_pixels .Ge. 2) Then
           sigma = Sqrt((sum_squares - Dble(num_pixels) * mean * mean) / &
             Dble(num_pixels - 1))
        Else
           sigma = mean
        End If
 
!     Create text output
        Write (MESSAGE(1), '(''Extremes of polygon (X/Y min, X/Y max) = '')')
        Write (MESSAGE(2), '(4f10.2)') xminimum, yminimum, xmaximum, ymaximum
        Write (MESSAGE(3), '(''Number of pixels inside polygon = '', i8)') &
          num_pixels
        If (num_pixels .Gt. 1) Then
           Write (MESSAGE(4), '(''Smallest value = '', 1pg12.5, ' // &
             ''' Largest value = '', 1pg12.5)') minimum, maximum
           Write (MESSAGE(5), &
             '(''Largest value first found at pixel ('', i5, '','' i5, '')'')' &
             ) x_maximum, y_maximum
           Write (MESSAGE(6), '(''Total intensity = '', ' // &
             '1pg12.5, '' (square root = '', 1pg12.5, '')'')') sum, &
             Sqrt(Abs(sum))
           Write (MESSAGE(7), '(''Average intensity = '', 1pg12.5)') mean
           Write (MESSAGE(8), '(''Root Mean Square (RMS) ' // &
             'value = '', 1pg12.5)') Sqrt(sum_squares / Dble(num_pixels))
           Write (MESSAGE(9), '(''Standard Deviation = '', ' // &
             '1pg12.5, '' Gain (?) = '', 1pg12.5)') sigma, sigma**2 / mean
           Write (MESSAGE(10), '(''Integrated intensity minus ' // &
             'previous average = '', 1pg12.5)') sum - &
             previous_mean * Dble(num_pixels)
           Write (MESSAGE(11), '(''Square root of above = '',1pg12.5)') &
             Sqrt(Abs(sum - previous_mean * Dble(num_pixels)))
           num_message = 11
        Else
           num_message = 3
        End If
 
!     Output text to terminal window (only so not log-file)
        Write (*, '(''INFO: '', a)') Trim(MESSAGE(1))
        Do line = 2, num_message
           Write (*, '(''      '', a)') Trim(MESSAGE(line))
        End Do
 
!     Output text to user graphically
        Call GS_SET_MESSAGE (xminmes, yminmes, xmaxmes, Max(yminmes + 0.1, &
          Min(0.3, ymaxmes)), status)
        Call GS_MESSAGE (Max_message, num_message, MESSAGE, status)
 
!     Store the results as program symbols
        Call IO_SET_VARIABLE ('##MINIMUM', 'r', int_value, log_value, minimum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MAXIMUM', 'r', int_value, log_value, maximum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MEAN', 'r', int_value, log_value, Real(mean), &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##MINIMUM', 'r', int_value, log_value, minimum, &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##RMS', 'r', int_value, log_value, &
          Real(Sqrt(sum_squares/Dble(num_pixels))), len_string, string_value, &
          retstat, status)
        Call IO_SET_VARIABLE ('##SIGMA', 'r', int_value, log_value, &
          Real(sigma), len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##TOTAL', 'r', int_value, log_value, Real(sum), &
          len_string, string_value, retstat, status)
        Call IO_SET_VARIABLE ('##INTEG_MINUS_BACK', 'r', int_value, log_value, &
          Real(sum - previous_mean * Dble(num_pixels)), len_string, &
          string_value, retstat, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Free memory used for "MA_GA05A" arrays
        Deallocate (WORK)
        Deallocate (INDP)
 
!     Save average value
        previous_mean = mean
 
!     Reset message region
        Call GS_SET_MESSAGE (xminmes, yminmes, xmaxmes, ymaxmes, status)
 
     End If
 
     End Subroutine F2D_GSTATISTICS
!********1*********2*********3*********4*********5*********6*********7*********8

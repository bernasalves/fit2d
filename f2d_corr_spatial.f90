!********1*********2*********3*********4*********5*********6*********7**
 
!  ************************
!  *                      *
!  * f2d_corr_spatial.f90 *
!  *                      *
!  ************************
 
!+ F2D_CORR_SPATIAL - FIT 2-D CORRect SPATIAL distortion
     Subroutine F2D_CORR_SPATIAL (fitpack, gui, x_minimum, y_minimum, &
       x_maximum, y_maximum, x_cor_size, y_cor_size, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, y_rows, overload_value, MXAXIS, MYAXIS, MDATA, &
       mxstrelm, mystrelm, mxendelm, myendelm, status)
!  Description:
!    Allocates work space for call to "F2D_CORR5_SPATIAL" or "F2D_CORR6_SPATIAL"
!    which do the real work.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.13 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    23-Feb-1999: V0.12 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.11 Change to use IO internal database routines (Hammersley)
!    16-Dec-1996: V0.10 Avoid open strings crossing lines (Hammersley)
!    30-Aug-1996: V0.9 Use "MA_CAL_2DNCUBSPLINE" to perform the spline 
!      interpolation regardless of "fitpack" variable (Hammersley)
!    20-Aug-1996: V0.8 Option of using FITPACK routine "BISPEV" instead of NAG
!      routine "E02DFF" (Hammersley)
!    22-Mar-1996: V0.7 Obtain overload pixel value from internal data-base 
!      (Hammersley)
!    26-Feb-1996: V0.6 Option of GUI interface (Hammersley)
!    24-Feb-1996: V0.5 Option of triangle overlap area re-binning (Hammersley)
!    25-Jan-1996: V0.4 Add extra user information (Hammersley)
!    16-Nov-1994: V0.3 Input over-loaded pixel value (Hammersley)
!    22-Jun-1994: V0.2 Extra axis values are interpolated if the output axis 
!      is larger than the input axis (Hammersley)
!    30-Apr-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: fitpack ! Ignored, now FITPACK routines are
!      always used
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Real, Intent(IN) :: x_minimum ! Minimum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_minimum ! Minimum Y-value applicable to spline
!      interpolation
     Real, Intent(IN) :: x_maximum ! Maximum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_maximum ! Maximum Y-value applicable to spline
!      interpolation
     Real, Intent(IN) :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Intent(IN) :: y_cor_size ! Size of corrected pixel in metres in
!      Y-direction
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots) ! X-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_MU(x_ynumknots) ! Y-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots) ! X-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_MU(y_ynumknots) ! Y-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
!      Coefficients of spline function for Y-distortion function
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(IN) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End Y-element of region to be fitted
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region to be fitted
!  Import/Export:
     Integer, Intent(INOUT) :: y_rows ! The absolute value is the second
!      dimension of temporary arrays, number of rows of the array for which the
!      distortions are calculated in one go. The sign determines the type of 
!      re-binning algorithm which is applied: positive means the centre of the 
!      edges of each distorted pixel are calculated and over-lapped areas are 
!      approximated by orthogonally aligned edges.
!      Negative means the corner positions will be calculated and
!      straight lines are "drawn" between the corners to provide
!      continuous quadlaterals. The intensity is re-binned
!      according to the over-lapped areas of these polygons.
     Real, Intent(INOUT) :: overload_value ! Value above which pixels are
!      considered to be over-loaded
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
     Integer, Intent(INOUT) :: mxstrelm ! Starting X-element of memory ROI
     Integer, Intent(INOUT) :: mystrelm ! Starting Y-element of memory ROI
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory ROI
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory ROI
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.13' ! Version number
!  Local Variables:
     Integer :: db_stat ! Data Store status return variable
     Integer :: maxwork ! First dimension size of temporary arrays
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable
     Integer :: x_low ! Lower X-pixel limit of spline region
     Integer :: x_up ! Upper X-pixel limit of spline  region
     Integer :: y ! Loop variable
     Integer :: y_low ! Lower Y-pixel limit of spline region
     Integer :: y_up ! Upper Y-pixel limit of spline region
     Logical :: continue ! .True., whilst input is not good
     Real :: step ! The axis value step between adjacent pixel positions
     Real :: temp ! Temporary storage variable
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(23) ! User messages
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic work array used to store 
!      X-coordinates of edges of pixels
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic work array used to store 
!      Y-coordinates of edges of pixels
     Real, Allocatable :: X_DISTORTION(:, :) ! X-distortion values
     Real, Allocatable :: Y_DISTORTION(:, :) ! Y-distortion values
!  Internal Functions:
!  External Functions:
     Character(Len = 20), External :: Io_rtoc ! Convert real value to string
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CORR_SPATIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CORR_SPATIAL ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CORR_SPATIAL ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Output message telling user of the corrected pixel sizes
        Write (MESSAGE(1), '(''INFO: The corrected pixel ' // &
          'dimension in X is '', f12.4, '' microns'')') x_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The corrected pixel ' // &
          'dimension in Y is '', f12.4, '' microns'')') y_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Call IO_WRITE (' ', status)
 
!     Find limits of valid pixel region (may be smaller than required ROI region
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_minimum, temp, status)
        x_low = Int(temp) + 1
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_maximum, temp, status)
        x_up = Int(temp)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_minimum, temp, status)
        y_low = Int(temp) + 1
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_maximum, temp, status)
        y_up = Int(temp)
 
!     Set re-binning region to be the lesser of the ROI and the valid region
        If (xstrelm .Lt. x_low .Or. ystrelm .Lt. y_low .Or. xendelm .Gt. x_up &
          .Or. yendelm .Gt. y_up) Then
           Call IO_WRITE ('WARNING: Region Of Interest ' // &
             '(ROI) extends outside valid region of', status)
           Call IO_WRITE ('spline. Pixels outside the ' // &
             'valid region will be ignored.', status)
           Call IO_WRITE ('INFO: The valid region of ' // &
             'the spline correction function is:', status)
           Write (message, '(''      ('', f8.2, '', '', f8.2, ' // &
             ''') to ('', f8.2, '', '', f8.2, '')'')') x_minimum, y_minimum, &
             x_maximum, y_maximum
           Call IO_WRITE (message, status)
 
        End If
 
!     Set Intersection of ROI and valid spline region
        x_low = Max(x_low, xstrelm)
        y_low = Max(y_low, ystrelm)
        x_up = Min(x_up, xendelm)
        y_up = Min(y_up, yendelm)
 
!     Obtain overloaded intensity value from internal data-base
        Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
          status)
 
        If (.Not. gui) Then
 
!        Overloaded pixel value
           MESSAGE(1) = 'In order to avoid over-loaded pixels ' // &
             'being re-binned and their intensity'
           MESSAGE(2) = 'spread out to an undetermined value, ' // &
             'you can enter a "over-loaded" pixel'
           MESSAGE(3) = 'value. All input pixels which have ' // &
             'this value or more, will cause one or'
           MESSAGE(4) = 'more output pixels to be incremented ' // &
             'by the value regardless of the normal'
           MESSAGE(5) = 'proportional are re-binning algorithm. ' // &
             'Thus over-loaded pixels in the'
           MESSAGE(6) = 'output image can be easily identified ' // &
             'and ignored.'
           MESSAGE(7) = '(This can be turned-off by entering a ' // &
             'very large value.)'
           Call IO_INPR (.True., 0.0, 1.7e38, .True., &
             'OVER-LOADED PIXEL VALUE', 7, MESSAGE, 1, &
             'Enter a real value within given range', overload_value, status)
 
           If (status .Eq. St_escapevalue) Then
              Return
           End If
 
!        Save overloaded intensity value in internal data-base
           Call IO_SET_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
             status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Find out number of rows for which the distortions are to be
!     calculated in one go
        MESSAGE(1) = 'For efficiencies sake many rows of the '// &
          'distortion functions can be calculated'
        MESSAGE(2) = 'in one call to the subroutine which '// &
          'performs the task. However, to store'
        MESSAGE(3) = 'the results extra memory is required. '// &
          '(The memory is allocated automatically'
        MESSAGE(4) = 'from the machines virtual memory and is '// &
          'de-allocated after the spatial'
        MESSAGE(5) = 'distortion correction is finished and the '// &
          'storage is no longer necessary.)'
        MESSAGE(6) = 'Thus, the value entered here choses a '// &
          'compromise between using more memory'
        MESSAGE(7) = 'and taking longer to calculate all the '// &
          'necessary distortion values. If the'
        MESSAGE(8) = 'machine has plenty of available memory '// &
          '(virtual) a larger number such as 100'
        MESSAGE(9) = 'may be more efficient. However, if the '// &
          'machine is short of virtual memory'
        MESSAGE(10) = 'e.g. you have already received an error '// &
          'message that memory allocated has'
        MESSAGE(11) = 'failed, then a smaller value will be '// 'appropriate.'
        MESSAGE(12) = '   NOTE: Too big a number may be '// &
          'counter-productive as it may cause'
        MESSAGE(13) = 'excessive page faulting.'
        MESSAGE(14) = '   The  sign determines  the  type  of ' // &
          're-binning algorithm which  is '
        MESSAGE(15) = 'applied:  positive means the centre ' // &
          'of  the edges  of  each distorted'
        MESSAGE(16) = 'pixel  are  calculated  and  ' // &
          'over-lapped areas  are  approximated  by '
        MESSAGE(17) = 'orthogonally aligned edges.'
        MESSAGE(18) = '   Negative  means  the  corner  ' // &
          'positions  will  be  calculated  and'
        MESSAGE(19) = 'straight lines  are "drawn" between ' // &
          'the corners to provide continuous'
        MESSAGE(20) = 'quadrilaterals. The intensity is ' // &
          're-binned according to the over-lapped'
        MESSAGE(22) = 'areas of these polygons.'
        MESSAGE(23) = 'This takes about three times longer !'
 
        If (.Not. gui) Then
 
           continue = .True.
           Do While (continue)
              Call IO_INPI (.True., -(yendelm - ystrelm + 2), &
                yendelm - ystrelm + 2, .True., &
                'NUMBER OF ROWS OF DISTORTION FUNCTIONS TO ' // &
                'CALCULATED IN A BLOCK', 23, MESSAGE, 1, &
                'Enter an integer within given range', y_rows, status)
 
              If (Abs(y_rows) .Lt. 2) Then
                 Call IO_WRITE ('WARNING: At least 2 rows ' // &
                   'must be calculated together i.e. >= -+2', status)
              Else
                 continue = .False.
              End If
 
           End Do
 
        End If
 
!     Obtain dynamic array space for internal arrays
        maxwork = xendelm - xstrelm + 2
        Allocate (X_DISTORTION(maxwork,  Abs(y_rows)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_DISTORTION(maxwork,  Abs(y_rows)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR_SPATIAL ' // Version)
           Return
        End If
        Allocate (X_COORDINATES(maxwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(Abs(y_rows)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR_SPATIAL ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set output ROI
        mxstrelm = 1
        mystrelm = 1
        mxendelm = xmaxdat
        myendelm = ymaxdat
 
!     Initialise all pixels in output region of memory
        Call MA_RVALUE (xmaxdat, ymaxdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, 0.0, MDATA, status)
 
!     Perform spatial correction
        If (y_rows .Gt. 0) Then
 
!        Orthogonality approximation re-binning
           Call F2D_CORR5_SPATIAL ( xstrelm, ystrelm, xendelm, yendelm, &
             overload_value, x_low, y_low, x_up, y_up, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, &
             Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
             maxwork, Abs(y_rows), X_COORDINATES, Y_COORDINATES, &
             X_DISTORTION, Y_DISTORTION, MDATA, status)
 
        Else
 
!        Distorted quadrilateral over-lap area re-binning
           Call F2D_CORR6_SPATIAL ( xstrelm, ystrelm, xendelm, yendelm, &
             overload_value, x_low, y_low, x_up, y_up, x_xnumknots, &
             x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, &
             Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
             maxwork, Abs(y_rows), X_COORDINATES, Y_COORDINATES, &
             X_DISTORTION, Y_DISTORTION, MDATA, status)
 
        End If
 
!     Free dynamic array space
        Deallocate (X_DISTORTION)
        Deallocate (Y_DISTORTION)
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Create X-axis values
        Do x = 1, xendelm
           MXAXIS(x) = XAXIS(x)
        End Do
 
!     If the output axis extends beyond the input axis the extra
!     elements must be interpolated
        If (mxendelm .Gt. xendelm) Then
 
           step = XAXIS(xendelm) - XAXIS(xendelm - 1)
           Do x = xendelm + 1, mxendelm
              MXAXIS(x) = XAXIS(xendelm) + step * (x - xendelm)
           End Do
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Create X-axis values
        Do y = 1, yendelm
           MYAXIS(y) = YAXIS(y)
        End Do
 
!     If the output axis extends beyond the input axis the extra
!     elements must be interpolated
        If (myendelm .Gt. yendelm) Then
 
           step = YAXIS(yendelm) - YAXIS(yendelm - 1)
           Do y = yendelm + 1, myendelm
              MYAXIS(y) = YAXIS(yendelm) + step * (y - yendelm)
           End Do
 
        End If
 
!     Output user information
        If (.Not. gui) Then
           Call IO_WRITE ('NOTE: Corrected data is in the ' // &
             '"memory" array. Use "EXCHANGE" in the', status)
           Call IO_WRITE ('      main menu to transfer to ' // &
             'the working current data array.', status)
        End If
 
     End If
 
     End Subroutine F2D_CORR_SPATIAL
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_corr6_spatial.f90 *
!  *                       *
!  *************************
 
!+ F2D_CORR6_SPATIAL - FIT 2-D CORRect SPATIAL distortion using triangular base
!  re-binning
     Subroutine F2D_CORR6_SPATIAL (xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, x_low, y_low, x_up, y_up, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, maxxwork, y_rows, &
       X_COORDINATES, Y_COORDINATES, X_DISTORTION, Y_DISTORTION, MDATA, &
       status)
!  Description:
!    Corrects spatial distortions by calculating corrected position of input 
!    pixels from two bi-cubic spline functions. The corners of each pixel are 
!    transformed to the corrected grid and the intensity of the input pixel 
!    is distributed all the covered output pixels in proportion to the covered 
!    area of the approximate quadrilateral.
!  Method:
!    "MA_CAL_2DNCUBSPLINE" is used to calculate the transformed positions of a
!    block of a varable number of lines of pixels at the time. The corrected 
!    position for corners of each pixel are calculated, and two triangles are 
!    formed. "MA_TRIREBIN" is used to distribute the intensity of the triangles.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.8 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    26-Feb-2004: V0.7 Correct output region when input region axis values
!      do not start at zero (Hammersley)
!    16-Dec-1996: V0.6 Avoid open strings crossing lines (Hammersley)
!    03-Oct-1996: V0.5 Correct mistake affecting very first pixel (Hammersley)
!    30-Aug-1996: V0.4 Rename from "F2D_CORR4_SPATIAL" and use 
!      "MA_CAL_2DNCUBSPLINE" to perform the spline interpolation (Hammersley)
!    20-Aug-1996: V0.3 Option of using FITPACK routine "BISPEV" instead of NAG
!      routine "E02DFF" (Hammersley)
!    14-Apr-1996: V0.2 Add special treatment for over-loaded pixels, and allow 
!      output into the whole of the output array (Hammersley)
!    24-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region of interest
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region of interest
     Integer, Intent(IN) :: xendelm ! End X-element of region of interest
     Integer, Intent(IN) :: yendelm ! End Y-element of region of interest
     Real, Intent(IN) :: overload_value ! Value above which pixels are
!      considered to be over-loaded. This value is added to all overlapped 
!      pixels
     Integer, Intent(IN) :: x_low ! Lower X-pixel limit of spline region
     Integer, Intent(IN) :: y_low ! Lower Y-pixel limit of spline region
     Integer, Intent(IN) :: x_up ! Upper X-pixel limit of spline  region
     Integer, Intent(IN) :: y_up ! Upper Y-pixel limit of spline region
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
     Integer, Intent(IN) :: maxxwork ! Dimension of work arrays, must be at
!      least "xendelm - xstrelm + 2"
     Integer, Intent(IN) :: y_rows ! Second dimension of work arrays; number
!      of rows of the distortion to be calculated in one block
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxxwork) ! Used to store
!      X-coordinates for edges of pixels
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows) ! Used to store
!      Y-coordinates for edges of pixels
     Real, Intent(INOUT) :: X_DISTORTION(maxxwork * y_rows) ! Work array for
!      X-distortion values
     Real, Intent(INOUT) :: Y_DISTORTION(maxxwork * y_rows) ! Work array for
!      Y-distortion values
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: cal_rows ! The number of rows to distortion values to
!      be calculated in one call to "MA_CAL_2DNCUBSPLINE"
     Integer :: l_index ! Index into lower element of distortion arrays
     Integer :: lower ! The row within the distortion arrays which contains 
!      the lower edge corners
     Integer stat ! Status return variable for "Allocate"
     Integer :: u_index ! Index into upper element of distortion arrays
     Integer :: x ! Loop variable for X-direction
     Integer :: xsub ! Loop variable for X-direction
     Integer :: x_num_warnings ! Number of warning messages in X-direction
     Integer :: xmax_work ! Dimension size of "X_WORK"
     Integer :: xnumelm ! Number of input elements in X-direction
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_calculated ! Last calculated Y-pixel number for block of
!      calculated distortion values
     Integer :: y_lbase ! Base index into distortion arrays for lower Y-rows
     Integer :: y_num_warnings ! Number of warning messages in Y-direction
     Integer :: y_start ! Starting Y-pixel number for block of
!      calculated distortion values
     Integer :: y_ubase ! Base index into distortion arrays for upper Y-rows
     Integer :: y_b ! Loop variable for Y-direction
     Integer :: y_b_base ! Base index into distortion array at "y_b"
     Integer :: ysub ! Loop variable for Y-direction
     Integer :: ymax_work ! Dimension size of "Y_WORK"
     Integer :: ynumelm ! Number of input elements in Y-direction
     Real :: height ! Height of tirangle from "(xi3, yi3)" and line from
!      "(xi1, yi1)" to "(xi2, yi2)"
     Real :: intensity ! Value of an input pixel
     Real :: time_cpu_end ! CPU time at end of process
     Real :: time_cpu_str ! CPU time at start of process
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
     Real :: area1 ! Area of "lower" triangle of quadrilateral
     Real :: area2 ! Area of "upper" triangle of quadrilateral
     Real :: x_half ! The average spacing from centre of a pixel to its
!      edges in the X-direction
     Real :: x1, x2, x3, x4 ! X-coordinates of corners of quadrilateral
     Real :: xi1, yi1, xi2, yi2, xi3, yi3 ! Permuted triangle vertices
     Real :: xmax ! Maximum X-coordinate for a re-binned output pixel
     Real :: xmin ! Minimum X-coordinate for a re-binned output pixel
     Real :: y1, y2, y3, y4 ! Y-coordinates of corners of quadrilateral
!      edges in the Y-direction
     Real :: ymax ! Maximum Y-coordinate for a re-binned output pixel
     Real :: ymin ! Minimum Y-coordinate for a re-binned output pixel
     Real :: y_half ! The average spacing from centre of a pixel to its
     Real :: y_dash ! Y-coordinate of intersection of vertical from
!      "(xi3, yi3)" and line between "(xi1, yi1)" and "(xi2, yi2)"
!  Local Arrays:
     Integer, Allocatable :: X_IWORK(:) ! Dynamic work array
     Real, Allocatable :: X_WORK(:) ! Dynamic work array
     Integer, Allocatable :: Y_IWORK(:) ! Dynamic work array
     Real, Allocatable :: Y_WORK(:) ! Dynamic work array
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CORR6_SPATIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CORR6_SPATIAL ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably
!  defined
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
        Call ST_SAVE ('Subroutine F2D_CORR6_SPATIAL ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Initialise variables
        x_half = (XAXIS(xendelm) - XAXIS(xstrelm)) / &
          (2.0 * Real(xendelm - xstrelm))
        y_half = (YAXIS(yendelm) - YAXIS(ystrelm)) / &
          (2.0 * Real(yendelm - ystrelm))
 
!     Obtain dynamic array space for internal arrays
        xnumelm = x_up - x_low + 2
        ynumelm = y_up - y_low + 2
 
!     Calculate size of work arrays for "MA_CAL_2DNCUBSPLINE"
        xmax_work = 4 * (x_up + 1)
        ymax_work = 4 * y_rows
        Allocate (X_WORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR6_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_WORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR6_SPATIAL ' // Version)
           Return
        End If
        Allocate (X_IWORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR6_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_IWORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR6_SPATIAL ' // Version)
           Return
        End If

!     Set X-coordinates of input pixel edges (which never vary)
        Do x = 0, xnumelm - 2
           X_COORDINATES(x + 1) = XAXIS(x + x_low) - x_half
        End Do
        X_COORDINATES(xnumelm) = XAXIS(x_up) + x_half
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(10f8.1)')
!     :       (X_COORDINATES(x), x = 1, 32)
!     Write (*, '(''...'')')
!     Write (*, '(10f8.1)')
!     :       (X_COORDINATES(x), x = xnumelm-32, xnumelm)
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Loop through input data, one row at a time, and calculate
!     output pixel positions, and add appropriate elements of output array
        Call IO_WRITE ('INFO: Starting to correct data for spatial ' // &
          'distortion', status)
        x_num_warnings = 0
        y_num_warnings = 0
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu_str, status)
 
        y_start = y_low - y_rows + 1
        y_calculated = y_low
        Do y = y_low, y_up
 
!        Calculate new block of distortion values if necessary
           If (y .Ge. y_calculated) Then
 
!           Number of rows to calculate distortion
              cal_rows = Min(y_rows, y_up + 2 - y_calculated)
 
!           (NOTE: THE FIRST ROW OF EACH BLOCK IS THE LAST ROW OF THE
!           PREVIOUS BLOCK, SO THIS COULD BE MORE EFFICIENT)
 
!           Set Y-coordinates of input pixel edges
              Do y_b = 0, cal_rows - 2
                 Y_COORDINATES(y_b + 1) = YAXIS(y_b + y_calculated) - y_half
              End Do
              Y_COORDINATES(cal_rows) = YAXIS(cal_rows + y_calculated - 2) + &
                y_half
 
!           Calculate distortions at the corners of all the pixels
              Call MA_CAL_2DNCUBSPLINE (1, x_xnumknots, x_xnumknots, X_LAMBDA, &
                x_ynumknots, x_ynumknots, X_MU, &
                (x_xnumknots - 4) * (x_ynumknots - 4), X_COEFFS, maxxwork, &
                X_COORDINATES, y_rows, Y_COORDINATES, 1, 1, xnumelm, cal_rows, &
                xmax_work, ymax_work, X_DISTORTION, X_WORK, &
                Y_WORK, X_IWORK, Y_IWORK, status)
 
              Call MA_CAL_2DNCUBSPLINE (1, y_xnumknots, y_xnumknots, Y_LAMBDA, &
                y_ynumknots, y_ynumknots, Y_MU, &
                (y_xnumknots - 4) * (y_ynumknots - 4), Y_COEFFS, maxxwork, &
                X_COORDINATES, y_rows, Y_COORDINATES, 1, 1, xnumelm, cal_rows, &
                xmax_work, ymax_work, Y_DISTORTION, X_WORK, &
                Y_WORK, X_IWORK, Y_IWORK, status)
 
!           Add original pixel coordinates to distortion arrays to
!           obtain distorted output positions.
!           (26-Feb-04: Subtract start of axis for offset ROI's)
              y_b_base = 0
              Do y_b = 1, cal_rows
 
                 l_index = y_b_base
                 Do x = 1, xnumelm
                    l_index = l_index + 1
!                 X_DISTORTION(l_index) = X_DISTORTION(l_index) +
!                 :                   X_COORDINATES(x)
!                 Y_DISTORTION(l_index) = Y_DISTORTION(l_index) +
!                 :                   Y_COORDINATES(y_b)
                    X_DISTORTION(l_index) = X_DISTORTION(l_index) + &
                      X_COORDINATES(x) - XAXIS(1) + 0.5
                    Y_DISTORTION(l_index) = Y_DISTORTION(l_index) + &
                      Y_COORDINATES(y_b) - YAXIS(1) + 0.5
                 End Do
                 y_b_base = y_b_base + maxxwork
 
              End Do
 
!           Update last calculated row
              y_start = y_start + y_rows - 1
              y_calculated = y_calculated + y_rows - 1
 
           End If
 
!        The arrays "X_DISTORTION" and "Y_DISTORTION" now contain
!        the distortion for the corners of the pixels from row
!        "y_start" to "y_calculated". The first row is the lower
!        edge of "y_start" and the second row is the upper edge
!        of "y_start" and the lower edge of "y_start+1". The first
!        element in the x-direction is the left corner of the first
!        pixel, and the next element is the right edge.
 
!        Calculate row within distortion arrays which corresponds
!        to the lower edge of the pixels
           lower = y - y_start + 1
           y_ubase = lower * maxxwork
           y_lbase = y_ubase - maxxwork
 
!        Calculate initial "right" edge coordinates: really the left edge
           x2 = X_DISTORTION(y_lbase + 1)
           y2 = Y_DISTORTION(y_lbase + 1)
           x4 = X_DISTORTION(y_ubase + 1)
           y4 = Y_DISTORTION(y_ubase + 1)
 
!        Loop over row of input pixels, calculating coverage of
!        output pixels
           l_index = y_lbase + 1
           u_index = y_ubase + 1
           Do x = 1, xnumelm - 1
              intensity = DATA(x + x_low - 1, y)
 
!           Coordinates of four corners of the quadrilateral
              l_index = l_index + 1
              u_index = u_index + 1
              x1 = x2
              y1 = y2
              x2 = X_DISTORTION(l_index)
              y2 = Y_DISTORTION(l_index)
              x3 = x4
              y3 = y4
              x4 = X_DISTORTION(u_index)
              y4 = Y_DISTORTION(u_index)
 
              If (intensity .Ge. overload_value) Then
 
!              Special treatment of over-loaded pixels to ensure that output
!              pixels are definitely over-loaded even though intensity tends to 
!              be spread out by the re-binning process. The full intensity is 
!              added to ALL affected pixels
                 xmin = Min(x1, x2, x3, x4)
                 ymin = Min(y1, y2, y3, y4)
                 xmax = Max(x1, x2, x3, x4)
                 ymax = Max(y1, y2, y3, y4)
 
                 Do ysub = Max(1, Int(ymin) + 1), Min(ymaxdat, Int(ymax) + 1)
 
                    Do xsub = Max(1, Int(xmin) + 1), Min(xmaxdat, Int(xmax) + &
                      1)
                       MDATA(xsub, ysub) = MDATA(xsub, ysub) + intensity
                    End Do
 
                 End Do
 
              Else
 
!              Calculate areas of the output triangles
 
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD
!              Call MA_CAL_TRIAREA (x1, y1, x2, y2, x3, y3,
!              :                area1, status)
!              Call MA_CAL_TRIAREA (x4, y4, x2, y2, x3, y3,
!              :                area2, status)
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD
 
!**INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSER
!              Check for trivial cases
                 If (Abs((x2 - x1) / x1) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x3 - x1) * (y2 - y1))
                 Else If (Abs((x3 - x1) / x1) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x2 - x1) * (y3 - y1))
                 Else If (Abs((x3 - x2) / x2) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x1 - x3) * (y3 - y2))
                 Else If (Abs((y2 - y1) / y1) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x2 - x1) * (y3 - y1))
                 Else If (Abs((y3 - y1) / y1) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x3 - x1) * (y2 - y1))
                 Else If (Abs((y3 - y2) / y2) .Lt. 1.0e-5) Then
                    area1 = Abs(0.5 * (x3 - x2) * (y3 - y1))
                 Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''x1, y1 = '', 2f6.2)') x1, y1
!                 Write (*, '(''x2, y2 = '', 2f6.2)') x2, y2
!                 Write (*, '(''x3, y3 = '', 2f6.2)') x3, y3
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Swap if necessary, so that "(xi3, yi3)" is the middle
!                 X-coordinate
                    If (x3 .Gt. x1) Then
 
                       If (x3 .Lt. x2) Then
                          xi1 = x1
                          yi1 = y1
                          xi2 = x2
                          yi2 = y2
                          xi3 = x3
                          yi3 = y3
                       Else
 
                          If (x1 .Lt. x2) Then
                             xi1 = x1
                             yi1 = y1
                             xi2 = x3
                             yi2 = y3
                             xi3 = x2
                             yi3 = y2
                          Else
                             xi1 = x3
                             yi1 = y3
                             xi2 = x2
                             yi2 = y2
                             xi3 = x1
                             yi3 = y1
                          End If
 
                       End If
 
                    Else
 
                       If (x3 .Gt. x2) Then
                          xi1 = x1
                          yi1 = y1
                          xi2 = x2
                          yi2 = y2
                          xi3 = x3
                          yi3 = y3
                       Else
 
                          If (x2 .Lt. x1) Then
                             xi1 = x1
                             yi1 = y1
                             xi2 = x3
                             yi2 = y3
                             xi3 = x2
                             yi3 = y2
                          Else
                             xi1 = x3
                             yi1 = y3
                             xi2 = x2
                             yi2 = y2
                             xi3 = x1
                             yi3 = y1
                          End If
 
                       End If
 
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''xi1, yi1 = '', 2f6.2)') xi1, yi1
!                 Write (*, '(''xi2, yi2 = '', 2f6.2)') xi2, yi2
!                 Write (*, '(''xi3, yi3 = '', 2f6.2)') xi3, yi3
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!                 Calculate point on line 1-2 directly below "(xi3, yi3)"
                    y_dash = yi1 + (xi3 - xi1) * (yi2 - yi1) / (xi2 - xi1)
                    height = (yi3 - y_dash)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''y_dash = '', f6.2)') y_dash
!                 Write (*, '(''height = '', f6.2)') height
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Calculate area of interior triangle
                    area1 = Abs(0.5 * (xi2 - xi1) * height)
 
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!              Check for trivial cases
                 If (Abs((x2 - x4) / x4) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x3 - x4) * (y2 - y4))
                 Else If (Abs((x3 - x4) / x4) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x2 - x4) * (y3 - y4))
                 Else If (Abs((x3 - x2) / x2) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x4 - x3) * (y3 - y2))
                 Else If (Abs((y2 - y4) / y4) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x2 - x4) * (y3 - y4))
                 Else If (Abs((y3 - y4) / y4) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x3 - x4) * (y2 - y4))
                 Else If (Abs((y3 - y2) / y2) .Lt. 1.0e-5) Then
                    area2 = Abs(0.5 * (x3 - x2) * (y3 - y4))
                 Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''x4, y4 = '', 2f6.2)') x4, y4
!                 Write (*, '(''x2, y2 = '', 2f6.2)') x2, y2
!                 Write (*, '(''x3, y3 = '', 2f6.2)') x3, y3
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Swap if necessary, so that "(xi3, yi3)" is the middle
!                 X-coordinate
                    If (x3 .Gt. x4) Then
 
                       If (x3 .Lt. x2) Then
                          xi1 = x4
                          yi1 = y4
                          xi2 = x2
                          yi2 = y2
                          xi3 = x3
                          yi3 = y3
                       Else
 
                          If (x4 .Lt. x2) Then
                             xi1 = x4
                             yi1 = y4
                             xi2 = x3
                             yi2 = y3
                             xi3 = x2
                             yi3 = y2
                          Else
                             xi1 = x3
                             yi1 = y3
                             xi2 = x2
                             yi2 = y2
                             xi3 = x4
                             yi3 = y4
                          End If
 
                       End If
 
                    Else
 
                       If (x3 .Gt. x2) Then
                          xi1 = x4
                          yi1 = y4
                          xi2 = x2
                          yi2 = y2
                          xi3 = x3
                          yi3 = y3
                       Else
 
                          If (x2 .Lt. x4) Then
                             xi1 = x4
                             yi1 = y4
                             xi2 = x3
                             yi2 = y3
                             xi3 = x2
                             yi3 = y2
                          Else
                             xi1 = x3
                             yi1 = y3
                             xi2 = x2
                             yi2 = y2
                             xi3 = x4
                             yi3 = y4
                          End If
 
                       End If
 
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''xi1, yi1 = '', 2f6.2)') xi1, yi1
!                 Write (*, '(''xi2, yi2 = '', 2f6.2)') xi2, yi2
!                 Write (*, '(''xi3, yi3 = '', 2f6.2)') xi3, yi3
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!                 Calculate point on line 1-2 directly below "(xi3, yi3)"
                    y_dash = yi1 + (xi3 - xi1) * (yi2 - yi1) / (xi2 - xi1)
                    height = (yi3 - y_dash)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''y_dash = '', f6.2)') y_dash
!                 Write (*, '(''height = '', f6.2)') height
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!                 Calculate area of interior triangle
                    area2 = Abs(0.5 * (xi2 - xi1) * height)
 
                 End If
 
!**INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSER
 
!              Check that distortion is not a point discontinuity
                 If (area1 + area2 .Gt. 1.0e-6) Then
 
!                 Distribute intensity according to fractional covered area
                    Call MA_TRIREBIN (x1, y1, x2, y2, x3, y3, intensity / &
                      (area1 + area2), area1, xmaxdat, ymaxdat, 1, 1, xmaxdat, &
                      ymaxdat, x_num_warnings, y_num_warnings, MDATA, status)
                    Call MA_TRIREBIN (x4, y4, x2, y2, x3, y3, intensity / &
                      (area1 + area2), area2, xmaxdat, ymaxdat, 1, 1, xmaxdat, &
                      ymaxdat, x_num_warnings, y_num_warnings, MDATA, status)
                 End If
 
              End If
 
           End Do
 
!        Check for problems
           If (status .Ne. St_goodvalue) Then
              Return
 
!           Output progress
           Else If (Mod(y - y_low + 1, 200) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows re-binned = '', ' // &
                'i6, '' ('',i3, ''%)'')') y - y_low + 1, &
                Int(100.0 * Real(y - y_low + 1) / Real(y_up - y_low + 1))
              Call IO_WRITE (message, status)
 
           End If
 
        End Do
 
!     Time for operation
        Call IO_TIMES (timend, time_cpu_end, status)
        Write (message, '(''INFO: Time for correction = '', ' // &
          'f12.2, '' seconds'')') timend - timstr
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: CPU Time for correction = '', ' // &
          'f12.2, '' seconds'')') time_cpu_end - time_cpu_str
        Call IO_WRITE (message, status)
 
!     Output message if pixels have been ignored owing to re-binning
!     over more than three output pixels
        If (x_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' triangles ' // &
             'have been ignored, because they require'')') x_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than three output pixels (X-direction).', status)
        End If
 
        If (y_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' triangles ' // &
             'have been ignored, because they require'')') y_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than three output pixels (Y-direction).', status)
        End If
 
!     Free dynamic array space
        Deallocate (X_WORK)
        Deallocate (X_IWORK)
        Deallocate (Y_WORK)
        Deallocate (Y_IWORK)
 
     End If
 
     End Subroutine F2D_CORR6_SPATIAL
!********1*********2*********3*********4*********5*********6*********7**
 
 

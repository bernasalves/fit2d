!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_fit2dtest.f90 *
!  *                   *
!  *********************
 
!+ F2D_FIT2DTEST: FIT 2-D: FIT 2-D spline TEST values
     Subroutine F2D_FIT2DTEST (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, xmaxknots, ymaxknots, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
!  Description:
!    Calculate value of spline at all measured points and outputs worst mis-fit
!  Keywords:
!    Fit.Grid, Mask~Grid.Fit~Function, Spline~Interpolation
!  Method:
!    Uses FITPACK routine "BISPEV" to evaluate spline
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.4 Avoid open strings crossing lines (Hammersley)
!    02-Sep-1996: V0.3 Change to use "MA_CAL_NBCUBSPLINE" (Hammersley)
!    20-Aug-1996: V0.2 Convert to use FITPACK routine "BISPEV" (Hammersley)
!    09-Nov-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in grid horizontally
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in grid vertically
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of grid
!      peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of grid
!      peak centres
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks) ! X-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks) ! Y-distortion at
!      each peak (The distortion is measured as true position minus measured 
!      position)
     Integer, Intent(IN) :: xmaxknots ! Maximum number of knots for X-direction
     Integer, Intent(IN) :: ymaxknots ! Maximum number of knots for Y-direction
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(xmaxknots) ! X-Positions of spline "knots" for
!      X-distortion function
     Real, Intent(IN) :: X_MU(ymaxknots) ! Y-Positions of spline "knots" for 
!      X-distortion function
     Real, Intent(IN) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(xmaxknots) ! X-Positions of spline "knots" for
!      Y-distortion function
     Real, Intent(IN) :: Y_MU(ymaxknots) ! Y-Positions of spline "knots" for 
!      Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for Y-distortion function
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Real :: spline_value ! Value of spline at calculated point
     Real :: x_coordinate ! X-coordinate for calculation of spline value
     Real :: y_coordinate ! Y-coordinate for calculation of spline value
     Integer :: x ! Loop variable for X-direction
     Integer :: x_xhigh ! X-number of grid hole with highest X-discrepency
     Integer :: x_xlow ! X-number of grid hole with lowest X-discrepency
     Integer :: x_yhigh ! Y-number of grid hole with highest X-discrepency
     Integer :: x_ylow ! Y-number of grid hole with lowest X-discrepency
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_xhigh ! X-number of grid hole with highest Y-discrepency
     Integer :: y_xlow ! X-number of grid hole with lowest Y-discrepency
     Integer :: y_yhigh ! Y-number of grid hole with highest Y-discrepency
     Integer :: y_ylow ! Y-number of grid hole with lowest Y-discrepency
     Real :: misfit ! Difference between calculated spline value and
!      value measured at a grid hole position
     Real :: x_highest ! Largest positive discrepancy in X-distortion
     Real :: x_lowest ! Largest negative discrepancy in X-distortion
     Real :: y_highest ! Largest positive discrepancy in Y-distortion
     Real :: y_lowest ! Largest negative discrepancy in Y-distortion
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FIT2DTEST'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FIT2DTEST ' // Version)
        Return
     End If
 
     If (xmax_peaks .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymax_peaks .Lt. 1) Then
        status = St_bad_dim2
     Else If (xnum_peaks .Lt. 1 .Or. xnum_peaks .Gt. xmax_peaks) Then
        status = St_bad_adr1
     Else If (ynum_peaks .Lt. 1 .Or. ynum_peaks .Gt. ymax_peaks) Then
        status = St_bad_adr2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_FIT2DTEST ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input arguments appear to be correct
 
!     Initialise worst case values
        x_highest = -1.7e38
        x_lowest = 1.7e38
        y_highest = -1.7e38
        y_lowest = 1.7e38
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_FIT2DTEST: Before loop'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Do y = 1, ynum_peaks
 
           Do x = 1, xnum_peaks
 
              If (X_PEAKS(x, y) .Gt. -1.7e38) Then
 
!              Peak exist, calculate value of spline at this point
                 x_coordinate = X_PEAKS(x, y)
                 y_coordinate = Y_PEAKS(x, y)
 
!              X-distortion
                 Call MA_CAL_NBCUBSPLINE ( x_xnumknots, x_xnumknots, X_LAMBDA, &
                   x_ynumknots, x_ynumknots, X_MU, (x_xnumknots - 4) * &
                   (x_ynumknots - 4), X_COEFFS, x_coordinate, y_coordinate, &
                   spline_value, status)
 
                 misfit = spline_value - X_DISTORTION(x, y)
 
                 If (misfit .Gt. x_highest) Then
                    x_highest = misfit
                    x_xhigh = x
                    x_yhigh = y
                 End If
 
                 If (misfit .Lt. x_lowest) Then
                    x_lowest = misfit
                    x_xlow = x
                    x_ylow = y
                 End If
 
!              Y-distortion
                 Call MA_CAL_NBCUBSPLINE ( y_xnumknots, y_xnumknots, Y_LAMBDA, &
                   y_ynumknots, y_ynumknots, Y_MU, (y_xnumknots - 4) * &
                   (y_ynumknots - 4), Y_COEFFS, x_coordinate, y_coordinate, &
                   spline_value, status)
 
                 misfit = spline_value - Y_DISTORTION(x, y)
 
                 If (misfit .Gt. y_highest) Then
                    y_highest = misfit
                    y_xhigh = x
                    y_yhigh = y
                 End If
 
                 If (misfit .Lt. y_lowest) Then
                    y_lowest = misfit
                    y_xlow = x
                    y_ylow = y
                 End If
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Output results to user
        Call IO_WRITE ('INFO: Worst case fit discrepancies ' // &
          ' in pixels (spline value - measured)', status)
        Write (message, '(''INFO: Lowest X-distortion =  '', ' // &
          'e12.5,'' at grid position (X/Y) '', 2i4)') x_lowest, x_xlow, x_ylow
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Highest X-distortion = '', ' // &
          'e12.5,'' at grid position (X/Y) '', 2i4)') x_highest, x_xhigh, &
          x_yhigh
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Lowest Y-distortion =  '', ' // &
          'e12.5,'' at grid position (X/Y) '', 2i4)') y_lowest, y_xlow, y_ylow
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Highest Y-distortion = '', ' // &
          'e12.5,'' at grid position (X/Y) '', 2i4)') y_highest, y_xhigh, &
          y_yhigh
        Call IO_WRITE (message, status)
 
     End If
 
     End Subroutine F2D_FIT2DTEST
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

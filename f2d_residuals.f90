!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_residuals.f90 *
!  *                   *
!  *********************
 
!+ F2D_RESIDUALS - FIT 2-D calculate distortion function RESIDUALS
     Subroutine F2D_RESIDUALS (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, status)
!  Description:
!    Calculates the residual between the 2-D fitted distortion spline function 
!    in the measured data points for the X or Y distortion values. The result is
!    output in the memory.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    02-Sep-1996: V0.2 Change to use "MA_CAL_NBCUBSPLINE" (Hammersley)
!    22-Jun-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS"
!      and "Y_PEAKS"
     Integer, Intent(IN) :: xnum_peaks ! Number of peaks in grid
!      horizontally
     Integer, Intent(IN) :: ynum_peaks ! Number of peaks in grid vertically
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of
!      grid peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of
!      grid peak centres
     Real, Intent(IN) :: X_DISTORTION(xmax_peaks, ymax_peaks)
!      X-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
     Real, Intent(IN) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!      Y-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
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
!  Import/Export:
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Start X-element of memory data region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Start Y-element of memory data region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title for output array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: spline_value ! Value of spline at calculated point
     Real :: x_coordinate ! X-coordinate for calculation of spline value
     Real :: y_coordinate ! Y-coordinate for calculation of spline value
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
     Logical :: x_display ! .True., if the distortion for the X-direction is to
!      be calculated, otherwise the distortion for the Y-direction is calculated
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_RESIDUALS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_RESIDUALS ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_RESIDUALS ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire if the distortion function is to be calcualted for the X or 
!     Y-distortions
        Call IO_INPL (.True., 0, 1, .True., 'CALCULATE X-RESIDUALS', 1, &
          'Enter "YES" for X-distortion fit residuals, "NO" for Y-distortion', &
          1, 'Enter "YES" or "NO"', x_display, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        Do y = 1, ynum_peaks
 
           Do x = 1, xnum_peaks
 
              If (X_PEAKS(x, y) .Gt. -1.7e38) Then
 
!              Peak exist, calculate value of spline at this point
                 x_coordinate = X_PEAKS(x, y)
                 y_coordinate = Y_PEAKS(x, y)
 
                 If (x_display) Then
 
                    Call MA_CAL_NBCUBSPLINE ( x_xnumknots, x_xnumknots, &
                      X_LAMBDA, x_ynumknots, x_ynumknots, X_MU, &
                      (x_xnumknots - 4) * (x_ynumknots - 4), X_COEFFS, &
                      x_coordinate, y_coordinate, spline_value, status)
 
                    MDATA(x, y) = spline_value - X_DISTORTION(x, y)
 
                 Else
 
                    Call MA_CAL_NBCUBSPLINE ( y_xnumknots, y_xnumknots, &
                      Y_LAMBDA, y_ynumknots, y_ynumknots, Y_MU, &
                      (y_xnumknots - 4) * (y_ynumknots - 4), Y_COEFFS, &
                      x_coordinate, y_coordinate, spline_value, status)
 
                    MDATA(x, y) = spline_value - Y_DISTORTION(x, y)
 
                 End If
 
              Else
 
!              Peak missing, set residual to zero
                 MDATA(x, y) = 0.0
 
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set memory ROI
        mxstrelm = 1
        mystrelm = 1
        mxendelm = xnum_peaks
        myendelm = ynum_peaks
 
!     Create title
        If (x_display) Then
           mtitle = 'X-DISTORTION FIT RESIDUALS'
        Else
           mtitle = 'Y-DISTORTION FIT RESIDUALS'
        End If
 
!     Create axis values
        Do x = 1, mxendelm
           MXAXIS(x) = Real(x)
        End Do
        Do y = 1, myendelm
           MYAXIS(y) = Real(y)
        End Do
 
     End If
 
     End Subroutine F2D_RESIDUALS
!********1*********2*********3*********4*********5*********6*********7*********8
 

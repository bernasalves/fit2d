!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_fit2dgrid.f90 *
!  *                   *
!  *********************
 
!+ F2D_FIT2DGRID: FIT 2-D GRID peak centres
     Subroutine F2D_FIT2DGRID (xmax_peaks, ymax_peaks, xnum_peaks, &
       ynum_peaks, closeness, xmin, ymin, xmax, ymax, xmaxknots, ymaxknots, &
       X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, spatial_exist, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
!  Description:
!    Calculates an approximation function to allow calculation of interpolation 
!    values at all positions from a number of discrete coordinates.
!  Keywords:
!    Fit.Grid, Mask~Grid.Fit~Function, Spline~Interpolation
!  Method:
!    Sets up arrays for FITPACK routine "SURFIT"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Apr-2006: V0.13 Remove old code and use Fortran-90 dynamically allocated
!      arrays (Hammersley)
!    16-Dec-1996: V0.12 Avoid open strings crossing lines (Hammersley)
!    04-Oct-1996: V0.11 Allow negative solutions from "SURFIT" to be stored as 
!      interpolating splines (Hammersley)
!    13-Sep-1996: V0.10 Add explanation for "SURFIT" status failure values 
!      (Hammersley)
!    03-Sep-1996: V0.9 Comment out code for NAG based routines (Hammersley)
!    20-Aug-1996: V0.8 Add option of using "SURFIT" to perform surface fitting 
!      (Hammersley)
!    14-Apr-1996: V0.7 Set spatial distortion to be undefined if the fitting 
!      fails (Hammersley)
!    19-Dec-1994: V0.6 Allow a larger minimum number of knot point for small 
!      grids (Hammersley)
!    21-Jun-1994: V0.5 Make sure that enough memory is allocated for NAG routine
!      E02DDF, which has previously caused a problem owing to rank deficient 
!      problems needing extra memory (Hammersley)
!    05-May-1994: V0.4 Reduce maximum number of knots to reduce memory 
!      requirements (Hammersley)
!    27-Apr-1994: V0.3 Investigate crash following fail by NAG routine. Return 
!      if there is a problem with NAG (Hammersley)
!    09-Nov-1993: V0.2 Calculate worst fit of spline to measured points 
!      (Hammersley)
!    03-Nov-1993: V0.1 Original, based on "F2D_FITGRID" (Hammersley)
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
     Real, Intent(IN) :: closeness ! Maximum AVERAGE required discrepancy
!      between input peak positions and spline values
     Real, Intent(IN) :: xmin ! Minimum X-value for which spline function
!      should be valid
     Real, Intent(IN) :: ymin ! Minimum Y-value for which spline function
!      should be valid
     Real, Intent(IN) :: xmax ! Maximum X-value for which spline function
!      should be valid
     Real, Intent(IN) :: ymax ! Maximum Y-value for which spline function
!      should be valid
     Integer, Intent(IN) :: xmaxknots ! Maximum number of knots for X-direction
     Integer, Intent(IN) :: ymaxknots ! Maximum number of knots for Y-direction
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
!  Export:
     Logical, Intent(OUT) :: spatial_exist ! .True., if the spatial distortion
!      spline function has been fitted O.K.
     Integer, Intent(OUT) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(OUT) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(OUT) :: X_LAMBDA(xmaxknots) ! X-Positions of spline "knots" 
!      for X-distortion function
     Real, Intent(OUT) :: X_MU(ymaxknots) ! Y-Positions of spline "knots" for 
!      X-distortion function
     Real, Intent(OUT) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(OUT) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(OUT) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(OUT) :: Y_LAMBDA(xmaxknots) ! X-Positions of spline "knots" 
!      for Y-distortion function
     Real, Intent(OUT) :: Y_MU(ymaxknots) ! Y-Positions of spline "knots" for 
!      Y-distortion function
     Real, Intent(OUT) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for Y-distortion function
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.13' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: b1 ! "SURFIT" variable for calculating work space
     Integer :: b2 ! "SURFIT" variable for calculating work space
     Integer :: bx ! "SURFIT" variable for calculating work space
     Integer :: by ! "SURFIT" variable for calculating work space
     Integer :: ier_x ! "SURFIT" status return variable for X-distortion
     Integer :: ier_y ! "SURFIT" status return variable for Y-distortion
     Integer :: maxcoeffs ! Maximum number of spline coefficients which may be 
!      stored
     Integer :: maxiwork ! Dimension size for array "IWORK"
     Integer :: maxknots ! Maximum of "xknots" and "yknots"
     Integer :: maxwork ! Dimension size for array "WORK"
     Integer :: maxwork2 ! Dimension size for array "WORK2"
     Integer :: n ! "SURFIT" variable for calculating work space
     Integer :: num_peaks ! Number of peaks to be fitted including two false 
!      peaks used to define area
     Integer :: max_peaks ! This is "xnum_peaks * ynum_peaks + 2", which is the
!      maximum number of peaks which may need to be fitted (two false peaks are 
!      added)
     Integer stat ! Status return variable for "Allocate"
     Integer :: u ! "SURFIT" variable for calculating work space
     Integer :: v ! "SURFIT" variable for calculating work space
     Integer :: w ! "SURFIT" variable for calculating work space
     Integer :: x ! Loop variable for X-direction 
     Integer :: x_numcoeffs ! Number of spline coefficients for X-distortion 
!      function
     Integer :: xknots ! Starting number of knots for X-direction
     Integer :: y ! Loop variable for Y-direction 
     Integer :: y_numcoeffs ! Number of spline coefficients for Y-distortion 
!      function
     Integer :: yknots ! Starting number of knots for Y-direction
     Real :: chi_squared ! Calculated sum of square residuals
     Real :: smoothness ! Required sum of square residuals
!  Local Arrays:
     Integer, Allocatable :: IWORK(:) ! Dynamic work array
     Real, Allocatable :: WEIGHTS(:) ! Dynamic array for fitted point weights
     Real, Allocatable :: WORK(:) ! Dynamic work array
     Real, Allocatable :: WORK2(:) ! Dynamic work array
     Real, Allocatable :: X_COORDINATES(:) ! X-coordinates of fit points
     Real, Allocatable :: X_VALUES(:) ! Values of X-distortion fit points
     Real, Allocatable :: Y_COORDINATES(:) ! Y-coordinates of fit points
     Real, Allocatable :: Y_VALUES(:) ! Values of Y-distortion fit points
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered: F2D_FIT2DGRID'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input arguments appear to be correct
 
!     Initialise variables
        max_peaks = Max(16, xnum_peaks * ynum_peaks + 2)
        xknots = Max(Min(xmaxknots, xnum_peaks, xnum_peaks / 2 + 8), 8)
        yknots = Max(Min(ymaxknots, ynum_peaks, ynum_peaks / 2 + 8), 8)
        maxknots = Max (xknots, yknots)
        maxcoeffs = (xknots - 4) * (yknots - 4)
 
        u = xknots - 4
        v = yknots - 4
        w = 4
        n = Max(xknots, yknots)
        bx = 3 * v + 3 + 1
        by = 3 * u + 3 + 1
        If (bx .Le. by) Then
           b1 = bx
           b2 = b1 + v - 3
        Else
           b1 = by
           b2 = b1 + u - 3
        End If
        maxwork = u * v * (2 + b1 + b2) + 2 * (u + v + w * (max_peaks + n) + n &
          - 3 - 3) + b2 + 1
        maxwork2 = u * v * (b2 + 1) + b2
        maxiwork = max_peaks + (xknots - 7) * (yknots - 7)
 
!     Obtain dynamic array space for work arrays
        Allocate (X_COORDINATES(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (X_VALUES(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (Y_VALUES(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (WEIGHTS(max_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (WORK(maxwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (WORK2(maxwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
        Allocate (IWORK(maxiwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FIT2DGRID ' // Version)
           Return
        End If
 
!     Check that the memory has been allocated O.K.
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Transfer 2-D peak coordinates arrays to 1-D single precision arrays, 
!     ignoring missing peaks.
        num_peaks = 0
        Do y = 1, ynum_peaks
 
           Do x = 1, xnum_peaks
 
              If (X_PEAKS(x, y) .Gt. -1.7e38) Then
                 num_peaks = num_peaks + 1
                 X_COORDINATES(num_peaks) = X_PEAKS(x, y)
                 Y_COORDINATES(num_peaks) = Y_PEAKS(x, y)
                 X_VALUES(num_peaks) = X_DISTORTION(x, y)
                 Y_VALUES(num_peaks) = Y_DISTORTION(x, y)
                 WEIGHTS(num_peaks) = 1.0
              End If
 
           End Do
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_FIT2DGRID: Before SURFIT'')')
!     Write (*, '(''xknots, yknots = '', 2i8)') xknots, yknots
!     Write (*, '(''num_peaks = '', i)') num_peaks
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Inform user
        Call IO_WRITE ('INFO: Fitting X-Distortion ', status)
 
!     Calculate approximating spline
        smoothness = closeness**2 * Real(num_peaks)
        Call SURFIT (0, num_peaks, X_COORDINATES, Y_COORDINATES, &
          X_VALUES, WEIGHTS, xmin, xmax, ymin, ymax, 3, 3, &
          smoothness, xknots, yknots, maxknots, 1.0e-6, x_xnumknots, &
          X_LAMBDA, x_ynumknots, X_MU, X_COEFFS, &
          chi_squared, WORK, maxwork, WORK2, maxwork2, IWORK, maxiwork, ier_x)
 
!     Number of spline coefficients
        x_numcoeffs = (x_xnumknots - 4) * (x_ynumknots - 4)
 
        If (ier_x .Gt. 0) Then
           Call IO_WRITE ('WARNING: Spline fit to ' // &
             'X-distortion function failed:', status)
 
           If (ier_x .Eq. 1) Then
              Call IO_WRITE ('         Too many spline knots required.', status)
           Else If (ier_x .Ge. 2 .And. ier_x .Le. 5) Then
              Call IO_WRITE ('         Required RMS discrepency too small', &
                status)
           Else If (ier_x .Eq. 10) Then
              Call IO_WRITE ('         Invalid calling arguments', status)
           Else If (ier_x .Gt. 10) Then
              Call IO_WRITE ('         Work space not big enough', status)
           End If
 
        Else
 
           If (ier_x .Eq. -1) Then
              Call IO_WRITE ( 'INFO: An interpolating spline has been ' // &
                'calculated for the X-distortion', status)
           Else If (ier_x .Eq. -2) Then
              Call IO_WRITE ( 'INFO: A weighted least-squares polynomial has ' &
                // 'been calculated for the', status)
              Call IO_WRITE ('      X-distortion', status)
           Else If (ier_x .Lt. -2) Then
              Call IO_WRITE ('INFO: A minimum norm least-' // &
                'squares spline from a (numerically) rank', status)
              Call IO_WRITE ('      deficient system has ' // &
                'been calculated for the X-distortion', status)
              Write (message, '(''      The rank deficiency = '', i6)') &
                (x_xnumknots - 4) * (x_ynumknots - 4) + ier_x
              Call IO_WRITE (message, status)
           End If
 
!        Output results
           Call IO_WRITE ('INFO: X-Distortion Spline', status)
           Write (message, &
             '(''INFO: Number of horizontal/ vertical true knot ' // &
             'points = '', 2i5)') x_xnumknots - 6, x_ynumknots - 6
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Average position discrepancy = '', 1pe12.5)') &
             Sqrt(chi_squared / Real(num_peaks))
           Call IO_WRITE (message, status)
        End If
 
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate Y-distortion function
 
!     Inform user
        Call IO_WRITE ('INFO: Fitting Y-Distortion ', status)
 
!     Calculate approximating spline
        smoothness = closeness**2 * Real(num_peaks)
        Call SURFIT (0, num_peaks, X_COORDINATES, Y_COORDINATES, &
          Y_VALUES, WEIGHTS, xmin, xmax, ymin, ymax, 3, 3, &
          smoothness, xknots, yknots, maxknots, 1.0e-6, y_xnumknots, &
          Y_LAMBDA, y_ynumknots, Y_MU, Y_COEFFS, &
          chi_squared, WORK, maxwork, WORK2, maxwork2, IWORK, maxiwork, ier_y)
 
!     Number of spline coefficients
        y_numcoeffs = (y_xnumknots - 4) * (y_ynumknots - 4)
 
        If (ier_y .Gt. 0) Then
           Call IO_WRITE ('WARNING: Spline fit to ' // &
             'Y-distortion function failed', status)
 
           If (ier_y .Eq. 1) Then
              Call IO_WRITE ('         Too many spline knots required.', status)
           Else If (ier_y .Ge. 2 .And. ier_y .Le. 5) Then
              Call IO_WRITE ('         Required RMS discrepency too small', &
                status)
           Else If (ier_y .Eq. 10) Then
              Call IO_WRITE ('         Invalid calling arguments', status)
           Else If (ier_y .Gt. 10) Then
              Call IO_WRITE ('         Work space not big enough', status)
           End If
 
        Else
 
           If (ier_y .Eq. -1) Then
              Call IO_WRITE ( 'INFO: An interpolating spline has been ' // &
                'calculated for the Y-distortion', status)
           Else If (ier_y .Eq. -2) Then
              Call IO_WRITE ( 'INFO: A weighted least-squares polynomial has ' &
                // 'been calculated for the', status)
              Call IO_WRITE ('      Y-distortion', status)
           Else If (ier_y .Lt. -2) Then
              Call IO_WRITE ('INFO: A minimum norm least-' // &
                'squares spline from a (numerically) rank', status)
              Call IO_WRITE ('      deficient system has ' // &
                'been calculated for the Y-distortion', status)
              Write (message, '(''      The rank deficiency = '', i6)') &
                (y_xnumknots - 4) * (y_ynumknots - 4) + ier_y
              Call IO_WRITE (message, status)
           End If
 
!        Output results
           Call IO_WRITE ('INFO: Y-Distortion Spline', status)
           Write (message, &
             '(''INFO: Number of horizontal/ vertical true knot ' // &
             'points = '', 2i5)') y_xnumknots - 6, y_ynumknots - 6
           Call IO_WRITE (message, status)
           Write (message, &
             '(''INFO: Average position discrepancy = '', 1pe12.5)') &
             Sqrt(chi_squared / Real(num_peaks))
           Call IO_WRITE (message, status)
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check that the splines have been fitted O.K.
        If (ier_x .Le. 0 .And. ier_y .Le. 0) Then
 
!        Set return status of fit
           spatial_exist = .True.
 
!        Calculate value of spline at all measured points
           Call F2D_FIT2DTEST (xmax_peaks, ymax_peaks, xnum_peaks, ynum_peaks, &
             X_PEAKS, Y_PEAKS, X_DISTORTION, Y_DISTORTION, xmaxknots, &
             ymaxknots, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
             y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
 
        Else
 
!        Set the spatial distortion to be undefined
           spatial_exist = .False.
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Free dynamic array space
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
        Deallocate (X_VALUES)
        Deallocate (Y_VALUES)
        Deallocate (WEIGHTS)
        Deallocate (WORK)
        Deallocate (IWORK)
        Deallocate (WORK2)
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''END OF F2D_FIT2DGRID'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_FIT2DGRID
!********1*********2*********3*********4*********5*********6*********7*********8

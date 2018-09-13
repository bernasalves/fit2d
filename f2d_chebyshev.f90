!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_chebyshev.f90 *
!  *                   *
!  *********************
 
!+ F2D_CHEBYSHEV - FIT 2-D CHEBYSHEV polynomial
     Subroutine F2D_CHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, FIT, status)
!  Description:
!    Fits data with a 2-D Chebyshev polynomial, does not fit where "MASK(x,y)" 
!    is ".True." The approximated surface is returned in "FIT", at all "(x, y)"
!    positions.
!  Keywords:
!    Surface~Interpolation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    01-Dec-1996: V0.4 Remove data fitting code to "F2D_FITCHEBYSHEV"
!      (Hammersley)
!    27-Nov-1996: V0.3 Convert to using "MA_2DCHEBYSHEV" (Hammersley)
!    03-Jan-1996: V0.2 Changes for IBM AIX "xlf" compiler: Doesn't like "g" 
!      format descriptor without a width qualifier (Hammersley)
!    01-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., where the
!      data is masked, i.e. is not to be fitted
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: FIT(xmaxdat, ymaxdat) ! The fitted surface at all
!      points in the ROI
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! Contains user messages
     Integer :: num_fit ! Number of elements in fit
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = Bad status, not enough independent data points in the X-direction 
!          for the required X-order polynomial fit for one or more rows
!      2 = Bad status, not enough independent data points in the X-direction 
!          for the required Y-order polynomial fit for one or more columns
     Integer stat ! Status return variable for "Allocate"
     Integer :: total ! Total number data points
     Integer :: x ! Array element position for X-direction
     Integer :: x_total ! Total number data points in X-direction
     Integer :: x_order ! Order of the polynomial in X
     Integer :: y ! Array element position for Y-direction
     Integer :: y_total ! Total number data points in Y-direction
     Integer :: y_order ! Order of the polynomial in Y
     Real :: sum_square ! Sum of squares of residuals at fitting points
     Real :: x_maximum ! Maximum X-coordinate
     Real :: x_minimum ! Minimum X-coordinate
     Real :: y_maximum ! Maximum Y-coordinate
     Real :: y_minimum ! Minimum Y-coordinate
!  Local Arrays:
     Real, Allocatable :: COEFFICIENTS(:, :) ! Dynamic array for Chebyshev 
!      coefficients of the fit function
     Real, Allocatable :: COEFFS_1D(:, :) ! Dynamic work array to hold 1-D 
!      X-coefficients for each row
     Real, Allocatable :: X_COEFFS(:) ! Dynamic work array
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CHEBYSHEV ' // Version)
        Return
     End If
 
!  Check that the input variables are reasonable
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CHEBYSHEV ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Initialise variable
        x_total = xendelm - xstrelm + 1
        y_total = yendelm - ystrelm + 1
        total = x_total * y_total
 
!     Enter order of polynomial
        x_order = 1
        y_order = 1
        Call IO_INPI (.True., 0, x_total - 1, .True., 'POLYNOMIAL X ORDER', 1, &
          'Order of polynomial function in X-direction', 1, &
          'Must be within specified bounds', x_order, status)
        Call IO_INPI (.True., 0, y_total - 1, .True., 'POLYNOMIAL Y ORDER', 1, &
          'Order of polynomial function in Y-direction', 1, &
          'Must be within specified bounds', y_order, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate array sizes
        Allocate (COEFFICIENTS(x_order + 1, y_order + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CHEBYSHEV ' // Version)
           Return
        End If
 
!     Fit data with 2-D Chebyshev polynomial
        Call F2D_FITCHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, X_AXIS, Y_AXIS, MASK, x_order + 1, y_order + 1, &
          x_order, y_order, retstat, x_minimum, y_minimum, x_maximum, &
          y_maximum, COEFFICIENTS, status)
 
!     Check status
        If (retstat .Eq. 0) Then
 
           Call IO_WRITE ('INFO: 2-D Chebyshev polynomial ' // &
             'coefficients calculated', status)
 
        Else If (retstat .Eq. 1) Then
 
!        Not enough data points in X-direction
           Call IO_WRITE ('WARNING: The polynomial could ' // &
             'not be fitted in the X-direction owing', status)
           Call IO_WRITE ('         to insufficient number ' // &
             'of data points', status)
           Return
 
        Else If (retstat .Eq. 2) Then
 
!        Not enough data points in X-direction
           Call IO_WRITE ('WARNING: The polynomial could ' // &
             'not be fitted in the X-direction owing', status)
           Call IO_WRITE ('         to insufficient number ' // &
             'of data points', status)
           Return
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''2-D polynomial coefficients calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Allocate work arrays
        Allocate (X_COEFFS(x_order + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CHEBYSHEV ' // Version)
           Return
        End If
        Allocate (COEFFS_1D(x_order + 1, (yendelm - ystrelm + 1)), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CHEBYSHEV ' // Version)
           Return
        End If

!     Coefficients calculated; calculate surface
        Call MA_CAL_2DCHEBYSHEV (.False., x_minimum, y_minimum, x_maximum, &
          y_maximum, x_order + 1, y_order + 1, COEFFICIENTS, x_order, &
          y_order, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
          x_order + 1, yendelm - ystrelm + 1, X_AXIS, Y_AXIS, FIT, &
          COEFFS_1D, X_COEFFS, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Returned from MA_CAL_2DCHEBYSHEV'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Check status
        If (status .Eq. St_goodvalue) Then
 
!        Calculate residual fit statistic
           sum_square = 0.0
           num_fit = 0
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (.Not. MASK(x, y)) Then
                    num_fit = num_fit + 1
                    sum_square = sum_square + (DATA(x, y) - FIT(x, y))**2
                 End If
 
              End Do
 
           End Do
 
           Write (message, '(''INFO: RMS residual = '', g14.5)') &
             Sqrt(sum_square / Real(num_fit))
           Call IO_WRITE (message, status)
 
        End If
 
!     Free dynamic arrays
        Deallocate (COEFFICIENTS)
        Deallocate (COEFFS_1D)
        Deallocate (X_COEFFS)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End of F2D_CHEBYSHEV'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End If
 
     End Subroutine F2D_CHEBYSHEV
!********1*********2*********3*********4*********5*********6*********7*********8

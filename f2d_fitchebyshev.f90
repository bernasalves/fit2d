!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  *  f2d_fitchebyshev.f90 *
!  *                       *
!  *************************
 
!+ F2D_FITCHEBYSHEV - FIT 2-D CHEBYSHEV polynomial
     Subroutine F2D_FITCHEBYSHEV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, xmax_coefficients, &
       ymax_coefficients, x_order, y_order, retstat, x_minimum, y_minimum, &
       x_maximum, y_maximum, COEFFICIENTS, status)
!  Description:
!    Fits data with a 2-D Chebyshev polynomial, does not fit where "MASK(x, y)"
!    is .True. The polynomial coefficients are returned in "COEFFICIENTS".
!  Keywords:
!    Surface~Interpolation, Chebyshev~Polynomial.2-D.Fitting,
!    2-D.Chebyshev~Polynomial.Fitting, Polynomial~2-D.Chebyshev.Fitting,
!    Fitting.Chebyshev~Polynomial.2-D
!  Method:
!    Call to "MA_2DCHEBYSHEV" after setting up appropriate arrays.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Mar-2006: V0.3 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Apr-1999: V0.2 Investigating problem getting sensible values when mask 
!      is present. Change routine to only fit un-masked points instead of giving
!      them a weight of zero. (Hammersley)
!    01-Dec-1996: V0.1 Original, based on "F2D_CHEBYSHEV" (Hammersley)
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
     Integer, Intent(IN) :: xmax_coefficients ! SECOND dimension of
!      "COEFFICIENTS" array
     Integer, Intent(IN) :: ymax_coefficients ! FIRST dimension of
!      "COEFFICIENTS" array
     Integer, Intent(IN) :: x_order ! Order of the fitted polynomial in the
!      X-direction
     Integer, Intent(IN) :: y_order ! Order of the fitted polynomial in the
!      Y-direction
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status variable:
!      0 = Good status
!      1 = Bad status, not enough independent data points in the
!          X-direction for the required X-order polynomial fit for
!          one or more of the rows
!      2 = Bad status, not enough independent data points in the
!          X-direction for the required Y-order polynomial fit for
!          one or more of the columns
     Real, Intent(OUT) :: x_minimum ! "x_minimum" and "x_maximum" set the range
!       over which the Chebyshev (-1.0, +1.0) interval is mapped in the 
!       X-direction
     Real, Intent(OUT) :: y_minimum ! "y_minimum" and "y_maximum" set the range
!      over which the Chebyshev (-1.0, +1.0) interval is mapped in the 
!      Y-direction
     Real, Intent(OUT) :: x_maximum ! "x_minimum" and "x_maximum" set the range 
!      over which the Chebyshev (-1.0, +1.0) interval is mapped in the 
!      X-direction
     Real, Intent(OUT) :: y_maximum ! "y_minimum" and "y_maximum" set the range 
!      over which the Chebyshev (-1.0, +1.0) interval is mapped in the 
!      Y-direction
     Real, Intent(OUT) :: COEFFICIENTS(ymax_coefficients, xmax_coefficients)
!      The fitted 2-D polynomial coefficients. NOTE: the coefficients are 
!      stored in the reverse sense to normal storage "(y_order, x_order)". This 
!      is because it is more efficient for "MA_CAL_2DCHEBYSHEV" to have the 
!      coefficients this way round.
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable for "MA_2DCHEBYSHEV"
     Integer :: num_rows ! Number of rows of coordinates to fit
     Integer :: num_work ! Dimension of work array
     Integer stat ! Status return variable for "Allocate"
     Integer :: total ! Total number data points
     Integer :: x_total ! Total number data points in X-direction
     Integer :: y_total ! Total number data points in Y-direction
!  Local Arrays:
     Real, Allocatable :: XNUM(:) ! Dynamic array for number of X-values in each
!      y(i) = y line
     Real, Allocatable :: XVALUES(:) ! Dynamic array for X-coordinate of each 
!      and every data value to be fitted
     Real, Allocatable :: YVALUES(:) ! Dynamic array for Y-coordinate of each 
!      of the rows to be fitted
     Real, Allocatable :: ZVALUES(:) ! Dynamic array for Z-coordinate 
!      (intensity) of each and every data value to be fitted
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_FITCHEBYSHEV'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Initialise variable
        x_total = xendelm - xstrelm + 1
        y_total = yendelm - ystrelm + 1
        total = x_total * y_total
 
!     Calculate array sizes
        num_work = total + 2 * Max(x_total, y_total) + 2 * y_total * &
          (x_order + 2) + 5 * (1 + Max(x_order, y_order))
 
!     Create dynamic arrays
        Allocate (XNUM(y_total + 1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
           Return
        End If
        Allocate (XVALUES(total), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
           Return
        End If
        Allocate (YVALUES(y_total), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
           Return
        End If
        Allocate (ZVALUES(total), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_FITCHEBYSHEV ' // Version)
           Return
        End If

!     Transfer the data values to 1-D arrays
        Call F2D_SUBFITCHEBY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, X_AXIS, Y_AXIS, MASK, total, y_total, num_rows, &
          XNUM, XVALUES, YVALUES, ZVALUES)
 
!     Fit 2-D Chebyshev polynomial to coordinates
        Call MA_2DCHEBYSHEV (.False., 0, y_total, num_rows, XNUM, &
          YVALUES, total, XVALUES, ZVALUES, dummy, &
          xmax_coefficients, ymax_coefficients, x_order, y_order, retstat, &
          x_minimum, y_minimum, x_maximum, y_maximum, COEFFICIENTS, status)
 
!     Free dynamic work arrays
        Deallocate (XNUM)
        Deallocate (XVALUES)
        Deallocate (YVALUES)
        Deallocate (ZVALUES)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End of F2D_FITCHEBYSHEV'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End If
 
     End Subroutine F2D_FITCHEBYSHEV
!********1*********2*********3*********4*********5*********6*********7*********8
     Subroutine F2D_SUBFITCHEBY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, MASK, total, y_total, num_rows, XNUM, &
       XVALUES, YVALUES, ZVALUES)
!  Description:
!    Transfers 2-D data values to 1-D arrays for 2-D polynomial fitting
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    01-Mar-1993: V0.1 Original (Hammersley)
!    27-Nov-1996: V0.2 Convert to "MA_2DCHEBYSHEV" (Hammersley)
!    14-Apr-1999: V0.3 Re-define values to be stored (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
!  Include 'st_symbols.inc' ! Symbolic 'status' constants
!  Import:
     Integer :: xmaxdat ! Dimension size in X-direction for data arrays
     Integer :: ymaxdat ! Dimension size in Y-direction for data arrays
     Integer :: xstrelm ! The X-pixel number for the start of the ROI
     Integer :: ystrelm ! The Y-pixel number for the start of the ROI
     Integer :: xendelm ! The X-pixel number for the end of the ROI
     Integer :: yendelm ! The Y-pixel number for the end of the ROI
     Real :: DATA(xmaxdat, ymaxdat) ! The data values
     Real :: X_AXIS(xmaxdat) ! X-axis values
     Real :: Y_AXIS(ymaxdat) ! Y-axis values
     Logical*1 :: MASK(xmaxdat, ymaxdat) ! .True., where the data is
!      masked, i.e. is not to be fitted
     Integer :: total ! Dimension size of data-point arrays
     Integer :: y_total ! Dimension size of y-line arrays
!  Import/Export:
!  Export:
     Integer :: num_rows ! Number of rows of coordinates to fit
     Integer :: XNUM(y_total + 1) ! Number of coordinates in each row
     Real :: XVALUES(total) ! X-coordinates
     Real :: YVALUES(y_total) ! Y-coordinates of each row
     Real :: ZVALUES(total) ! Intensity values
!  Local Variables:
     Integer :: coordinate ! Element counter
     Integer :: x ! Loop variable for the X-direction
     Integer :: y ! Loop variable for the Y-direction
     Logical :: values_set ! .True., if a value is unmasked within a row
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_SUBFITCHEBY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Transfer elements
     coordinate = 0
     num_rows = 1
     Do y = ystrelm, yendelm
 
        values_set = .False.
        XNUM(num_rows) = 0
 
        Do x = xstrelm, xendelm
 
           If (.Not. MASK(x, y)) Then
              coordinate = coordinate + 1
              XNUM(num_rows) = XNUM(num_rows) + 1
              XVALUES(coordinate) = X_AXIS(x)
              ZVALUES(coordinate) = DATA(x, y)
              values_set = .True.
           End If
 
        End Do
 
        If (values_set) Then
           YVALUES(num_rows) = Y_AXIS(y)
           num_rows = num_rows + 1
        End If
 
     End Do
 
!  Reset last row
     num_rows = num_rows - 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''num_rows = '', i6)') num_rows
!  Do y = 1, num_rows
!  Write (*, '(''Y-coordinate, number points = '', f12.5, i6)')
!  :       YVALUES(y), XNUM(y)
!  End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_SUBFITCHEBY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

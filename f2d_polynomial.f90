!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_polynomial.f90 *
!  *                    *
!  **********************
 
!+ F2D_POLYNOMIAL: POLYNOMIAL fit to coordinates
     Subroutine F2D_POLYNOMIAL (lim_order, max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, max_order, WEIGHTS, COEFFICIENTS, &
       RESIDUALS, lower_range, upper_range, order, POLYNOMIAL, status)
!  Description:
!    Fits polynomial to coordinates
!  Keywords:
!    Polynomial.Fit
!  Method:
!    Fitting is performed by "MA_POLYNOMIAL".
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.8 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    16-Dec-1996: V0.7 Avoid open strings crossing lines (Hammersley)
!    26-Nov-1996: V0.6 Changes to argument list of "MA_POLYNOMIAL" (Hammersley)
!    16-Nov-1996: V0.5 Convert to use "MA_POLYNOMIAL", and hence to single 
!      precision (Hammersley)
!    10-Nov-1994: V0.4 Add limit to maximum order (Hammersley)
!    31-Oct-1994: V0.3 Find lowest residual value for default polynomial order
!      (Hammersley)
!    28-Jun-1994: V0.2 Output graphical display of input data and fitted 
!      function (Hammersley)
!    02-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: lim_order ! If greater than zero this is the
!      maximum order
     Integer, Intent(IN) :: max_coordinates ! Dimension of coordinate arrays
     Integer, Intent(IN) :: num_coordinates ! Number of input coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates) ! X-coordinates
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates) ! Y-coordinates
     Integer, Intent(IN) :: max_order ! Maximum order of polynomial which
!      may be fitted
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: WEIGHTS(max_coordinates) ! Weights for each point
     Real, Intent(OUT) :: COEFFICIENTS(max_order + 1, max_order + 1)
!      Chebychev polynomial coefficients for the differenet orders of fitted 
!      polynomial
     Real, Intent(OUT) :: RESIDUALS(max_order + 1) ! Residuals from different 
!      fitted curves
     Real, Intent(OUT) :: lower_range ! Lower limit of calculated polynomial
     Real, Intent(OUT) :: upper_range ! Upper limit of calculated polynomial
     Integer, Intent(OUT) :: order ! Order of fitted polynomial
     Real, Intent(OUT) :: POLYNOMIAL(max_order + 1) ! Coefficients of polynomial
!      of required order
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Unit_out = 9 ! I/O unit number for output
!  Local Variables:
     Character(Len = 80) :: file_name = 'poly.dat' ! Name of output file
     Character(Len = 80) :: message ! User messages
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: retstat ! "MA_POLYNOMIAL" return status variable
     Integer :: orderplus1 ! Maximum order + 1 to fit
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for polynomial order
     Real :: lowest_residual ! Lowest residual value
     Real :: x_maximum ! Maximum X-coordinate in defined range
     Real :: x_minimum ! Minimum X-coordinate in defined range
!  Local Arrays:
     Real, Allocatable :: WORK1(:) ! Dynamic work array
     Real, Allocatable :: WORK2(:) ! Dynamic work array
     Real, Allocatable :: X_NORM(:) ! Dynamic work array
     Real, Allocatable :: X3(:) ! Dynamic work array
     Real, Allocatable :: Y_WEIGHTS(:) ! Dynamic work array
!  Internal Functions:
!  External Functions:
!  Local Data:
!    Save:
!    Save file_name
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
     Else If (max_coordinates .Le. 0 .Or. max_order .Le. 0) Then
        status = St_bad_dim1
     Else If (num_coordinates .Gt. max_coordinates ) Then
        status = St_bad_adr1
     Else If (num_coordinates .Le. 1) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
     Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''inside F2D_POLYNOMIAL'')')
!     Do coordinate = 1, num_coordinates
!     Write (*, '(2(1pe12.5))') X_COORDINATES(coordinate),
!     :         Y_COORDINATES(coordinate)
!     End Do
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Set range of polynomial
        lower_range = X_COORDINATES(1)
        upper_range = X_COORDINATES(num_coordinates)
 
!     Obtain dynamic array space
        orderplus1 = Min(max_order + 1, num_coordinates - 1)
 
        If (lim_order .Gt. 0) Then
           orderplus1 = Min(orderplus1, lim_order)
        End If
 
        Allocate (X_NORM(num_coordinates), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
           Return
        End If
        Allocate (Y_WEIGHTS(num_coordinates), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
           Return
        End If
        Allocate (X3(num_coordinates), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
           Return
        End If
        Allocate (WORK1(orderplus1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
           Return
        End If
        Allocate (WORK2(orderplus1), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYNOMIAL ' // Version)
           Return
        End If
 
!     Set up weights array for fitting
        Do coordinate = 1, num_coordinates
           WEIGHTS(coordinate) = 1.0 ! /
!        :          Sqrt(Y_COORDINATES(coordinate))
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Do coordinate = 1, num_coordinates
!     Write (*, '(''X, Y, W = '', 3f12.5)')
!     :          X_COORDINATES(coordinate), Y_COORDINATES(coordinate),
!     :          WEIGHTS(coordinate)
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Fit polynomial to data points
        Call MA_POLYNOMIAL (.False., .True., 0, max_coordinates, &
          num_coordinates, X_COORDINATES, Y_COORDINATES, WEIGHTS, max_order + &
          1, max_order + 1, orderplus1 - 1, num_coordinates, max_order + 1, &
          x_minimum, x_maximum, retstat, COEFFICIENTS, RESIDUALS, &
          X_NORM, Y_WEIGHTS, X3, WORK1, WORK2, status)
 
!     Output results
        Do order = 0, Min(30, orderplus1 - 1)
           Write (message, '(''Order = '', i3, '' residual = '',1pe12.5)') &
             order, RESIDUALS(order + 1)
           Call IO_WRITE (message, status)
        End Do
 
!     Free dynamic array space
        Deallocate (X_NORM)
        Deallocate (Y_WEIGHTS)
        Deallocate (X3)
        Deallocate (WORK1)
        Deallocate (WORK2)
 
!     Find lowest residual value to present as the default value
        lowest_residual = 1.7e38
        Do x = 0, orderplus1 - 1
 
           If (RESIDUALS(x + 1) .Lt. lowest_residual) Then
              order = x
              lowest_residual = RESIDUALS(x + 1)
           End If
 
        End Do
 
!     Enter order of polynomial
        Call IO_INPI (.True., 0, orderplus1 - 1, .True., &
          'ORDER OF POLYNOMIAL', 1, &
          'Enter order of polynomial for fitting scan', 1, &
          'Value must be within given range', order, status)

!     Check for "user escape"
        If (status .Eq. St_escapevalue) Then
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Create graphical display of fitted function and allow user to
!     confirm or change order of polynomial
        Call F2D_POLYFIT (max_coordinates, num_coordinates, X_COORDINATES, &
          Y_COORDINATES, max_order, orderplus1, COEFFICIENTS, order, &
          POLYNOMIAL, status)
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''End of F2D_POLYNOMIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Subroutine F2D_POLYNOMIAL
!********1*********2*********3*********4*********5*********6*********7*********8

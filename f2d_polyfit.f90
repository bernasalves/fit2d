!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_polyfit.f90 *
!  *                 *
!  *******************
 
!+ F2D_POLYFIT: POLYFIT fit to coordinates
     Subroutine F2D_POLYFIT (max_coordinates, num_coordinates, X_COORDINATES, &
       Y_COORDINATES, max_order, orderplus1, COEFFICIENTS, order, POLYNOMIAL, &
       status)
!  Description:
!    Creates examples of different orders of the polynomial fit and
!    displays them to the user for choice of suitable order of polynomial.
!  Keywords:
!    Polynomial.Fit.Display, Display.Polynomial.Fit
!  Method:
!    Fills dynamically allocated arrays and uses "GS_XYGRAPH"
!    to display the data and the fitted curve
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.8 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    26-Nov-1996: V0.7 Change of row/column order for "COEFFICIENTS"
!      (Hammersley)
!    16-Nov-1996: V0.6 Convert to single precision (Hammersley)
!    25-Jan-1996: V0.5 Force graphics output (Hammersley)
!    28-Aug-1995: V0.4 Allow escape from polynomial order loop (Hammersley)
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    01-Jul-1994: V0.2 Calculate minimum and maximum Y-data values (Hammersley)
!    28-Jun-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: max_coordinates ! Dimension of coordinate arrays
     Integer, Intent(IN) :: num_coordinates ! Number of input coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates) ! X-coordinates
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates) ! Y-coordinates
     Integer, Intent(IN) :: max_order ! Maximum order of polynomial which
!      may be fitted
     Integer, Intent(IN) :: orderplus1 ! The order + 1 for which the
!      coefficients have been calculated
     Real, Intent(IN) :: COEFFICIENTS(max_order + 1, max_order + 1)
!      Chebyshev polynomial coefficients for the different orders of
!      fitted polynomial
!  Import/Export:
     Integer, Intent(INOUT) :: order ! Order of fitted polynomial
     Real, Intent(INOUT) :: POLYNOMIAL(max_order + 1) ! Coefficients for the
!      order of polynomial selected by the user
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Integer stat ! Status return variable for "Allocate"
     Logical :: accept ! .True., if the order of the polynomial is
!      accepted, .False., if the order is to be changed
     Logical :: resize ! .True., if the graphics "terminal" window has
!      been re-sized
     Logical :: xmaxautoddr ! .True., if the X-maximum of the data
!      display region is to be automatically controlled
     Logical :: xminautoddr ! .True., if the X-minimum of the data
!      display region is to be automatically controlled
     Logical :: ymaxautoddr ! .True., if the Y-maximum of the data
!      display region is to be automatically controlled
     Logical :: yminautoddr ! .True., if the Y-minimum of the data
!      display region is to be automatically controlled
     Integer :: max_curve ! Number of coordinates to be calculated for
!      fitted curve
     Integer :: temp_status ! Temporary version of status return variable
     Real :: xmaxddr ! The maximum X-coordinate of the data display region
     Real :: xminddr ! The minimum X-coordinate of the data display region
     Real :: y_minimum ! Lowest Y-value in data
     Real :: y_maximum ! Highest Y-value in data
     Real :: ymaxddr ! The maximum Y-coordinate of the data display region
     Real :: yminddr ! The minimum Y-coordinate of the data display region
!  Local Arrays:
     Integer :: END_COORDINATES(2) ! Upper coordinates of display range
     Integer :: START_COORDINATES(2) ! Lower coordinates of display range
     Real, Allocatable :: X_PLOT(:) ! Dynamic array 
     Real, Allocatable :: Y_PLOT(:) ! Dynamic array 
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POLYFIT ' // Version)
     Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''inside F2D_POLYFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate number of points to calculate on fitted curve
        max_curve = (num_coordinates - 1) * 4 + 1
 
!     Obtain dynamic array space
        Allocate (X_PLOT(max_curve * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYFIT ' // Version)
           Return
        End If
        Allocate (Y_PLOT(max_curve * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_POLYFIT ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop showing different fits until the user is happy
        accept = .False.
        Do While (.Not. accept)
 
!        Transfer Real data values to single precision
!        display arrays, and create fitted curve values
           Call F2D_POLYFIT2 (max_coordinates, num_coordinates, X_COORDINATES, &
             Y_COORDINATES, max_order, COEFFICIENTS, order, max_curve, &
             POLYNOMIAL, y_minimum, y_maximum, X_PLOT, Y_PLOT, status)
 
!        Set curve 1 to be markers only, and 2 to line only
           Call GS_SET_CURVESTYLE (1, .False., .True., .False., status)
           Call GS_SET_CURVESTYLE (2, .True., .False., .False., status)
 
!        Check for re-sizing of graphics window
           Call LG_INP_RESIZE (Gs_wkid_terminal, resize, status)
 
!        Save current graphics display settings
           Call GS_INQ_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, &
             ymaxautoddr, status)
           Call GS_INQ_DDR (xminddr, yminddr, xmaxddr, ymaxddr, status)
 
!        Set graph display region to cover the data range in the
!        Y-axis
           Call GS_SET_AUTODDR (.True., .False., .True., .False., status)
           Call GS_SET_DDR (0.0, y_minimum - (y_maximum - y_minimum) / 20.0, &
             1.0, y_maximum + (y_maximum - y_minimum) / 20.0, status)
 
!        Output graphic of data points and fit
           START_COORDINATES(1) = 1
           END_COORDINATES(1) = num_coordinates
           START_COORDINATES(2) = 1
           END_COORDINATES(2) = max_curve
           Call GS_XYGRAPH (max_curve, 2, 2, START_COORDINATES, &
             END_COORDINATES, X_PLOT, Y_PLOT, &
             'Polynomial Fit to Data', 'X-axis', 'Fitted Values', status)
 
!        Force graphics update
           Call GS_UPDATE (status)
 
!        Restore previous data display region values
           Call GS_SET_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, &
             ymaxautoddr, status)
           Call GS_SET_DDR (xminddr, yminddr, xmaxddr, ymaxddr, status)
 
!        Accept fit or try anthor order
           Call IO_INPL (.True., 0, 1, .True., &
             'ACCEPT FIT ("NO" TO TRY ANOTHER ORDER)', 1, &
             'Enter "YES" to accept this fit, "NO" to try ' // &
             'another order of fit', 1, 'Answer "YES", or "NO"', accept, &
             status)
 
           If (.Not. accept) Then
 
!           Enter new order of polynomial
              Call IO_INPI (.True., 0, orderplus1 - 1, .True., &
                'ORDER OF POLYNOMIAL', 1, &
                'Enter order of polynomial for fitting scan', 1, &
                'Value must be within given range', order, status)
           End If
 
!        Check for "user escape"
           If (status .Eq. St_escapevalue) Then
              accept = .True.
           End If
 
        End Do
 
!     Free dynamic array space, even if user escape was issued
        temp_status = status
        If (temp_status .Eq. St_escapevalue) Then
           temp_status = St_goodvalue
        End If
        Deallocate (X_PLOT)
        Deallocate (Y_PLOT)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End of F2D_POLYFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End If
 
     End Subroutine F2D_POLYFIT
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!+ F2D_POLYFIT2: POLYFIT (2) fit to coordinates
     Subroutine F2D_POLYFIT2 (max_coordinates, num_coordinates, X_COORDINATES, &
       Y_COORDINATES, max_order, COEFFICIENTS, order, max_curve, POLYNOMIAL, &
       y_minimum, y_maximum, X_PLOT, Y_PLOT, status)
!  Description:
!    Slave routine for 'F2D_POLYFIT', transfers double
!    precision values to single precision array and calculates
!    polynomial fit
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    01-Dec-1996: V0.5 Extra argument for "MA_CAL_POLNOMIAL" (Hammersley)
!    26-Nov-1996: V0.4 Change of row/column order for "COEFFICIENTS"
!      (Hammersley)
!    16-Nov-1996: V0.3 Convert to use "MA_CAL_POLYNOMIAL" (Hammersley)
!    01-Jul-1994: V0.2 Calculate minimum and maximum Y-data values (Hammersley)
!    28-Jun-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
!    Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_coordinates ! Dimension size of coordinate
!      arrays
     Integer, Intent(IN) :: num_coordinates ! Number of input coordinates
     Real, Intent(IN) :: X_COORDINATES(max_coordinates) ! X-coordinates
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates) ! Y-coordinates
     Integer, Intent(IN) :: max_order ! Maximum order of polynomial which
!      may be fitted
     Real, Intent(IN) :: COEFFICIENTS(max_order + 1, max_order + 1)
!      Chebyshev polynomial coefficients for the different orders of fitted 
!      polynomial
     Integer, Intent(IN) :: order ! Order of fitted polynomial
     Integer, Intent(IN) :: max_curve ! Dimension of "X_PLOT(2, max_curve)"
!      and "Y_PLOT"
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: POLYNOMIAL(max_order+1) ! Coefficients for the
!      order of polynomial to be calculated (work array)
     Real, Intent(OUT) :: y_minimum ! Lowest Y-value in data
     Real, Intent(OUT) :: y_maximum ! Highest Y-value in data
     Real, Intent(OUT) :: X_PLOT(max_curve, 2) ! X-values of coordinates to
!      be plotted
     Real, Intent(OUT) :: Y_PLOT(max_curve, 2) ! Y-values of coordinates to
!      be plotted
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Real :: lower_range ! Lowest limit of polynomial defined range
     Real :: upper_range ! Upper limit of polynomial defined range
     Real :: x_value ! X-coordinate for calculating value of polynomial function
     Real :: y_value ! Y-value of polynomial function
     Integer :: coeff ! Loop variable for coefficients
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: element ! Array element within "X_COORDINATES" array
     Integer :: offset ! Fraction between "X_COORDINATES" elements
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Initialise variables
     y_minimum = 1.7e38
     y_maximum = -1.7e38
 
!  Set polynomial data value range
     lower_range = X_COORDINATES(1)
     upper_range = X_COORDINATES(num_coordinates)
 
!  Transfer data coordinate values to first curve
     Do coordinate = 1, num_coordinates
        X_PLOT(coordinate, 1) = Real(X_COORDINATES(coordinate))
        Y_PLOT(coordinate, 1) = Real(Y_COORDINATES(coordinate))
 
!     Set minimum and maximum
        y_minimum = Min(y_minimum, Real(Y_COORDINATES(coordinate)))
        y_maximum = Max(y_maximum, Real(Y_COORDINATES(coordinate)))
 
     End Do
 
!  Transfer coefficients of required order
     Do coeff = 1, order + 1
        POLYNOMIAL(coeff) = COEFFICIENTS(coeff, order + 1)
     End Do
 
!  Calculate values of fitted curve, with values at every X-coordinate value and
!  at equal spaces inbetween coordinate values
     Do coordinate = 1, max_curve
 
!     Calculate X-coordinate
        element = (coordinate - 1) / 4
        offset = coordinate-1 - element * 4
        x_value = ((4 - offset) * X_COORDINATES(element + 1) + offset * &
          X_COORDINATES(element + 2)) / 4.0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''coordinate, element, x_value = '', 2i6, f8.3)')
!     :          coordinate, element, x_value
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate value of polynomial at angle
        Call MA_CAL_POLYNOMIAL (.False., lower_range, upper_range, order + 1, &
          order, POLYNOMIAL, 1, 1, x_value, y_value, status)
 
        X_PLOT(coordinate, 2) = Real(x_value)
        Y_PLOT(coordinate, 2) = Real(y_value)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''y_value = '', f8.3)') y_value
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
     End Do
 
     End Subroutine F2D_POLYFIT2
!********1*********2*********3*********4*********5*********6*********7*********8

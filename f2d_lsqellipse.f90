!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_lsqellipse.f90 *
!  *                    *
!  **********************
 
!+ F2D_LSQELLIPSE: Least SQuares ELLIPSE parameters
     Subroutine F2D_LSQELLIPSE (max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, x_centre, y_centre, radius1, radius2, &
       angle1, radial_error, status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Ellipse.Centre/Radius, Least~Squares.Ellipse.Centre/Radius
!  Method:
!    Non-linear least squares iterative solution
!  Deficiencies:
!    The number of coordinates the pointers to the coordinate arrays must be 
!    passed through "COMMON" to the objective function "F2D_FUNELLIPSE"
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.5 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    22-Oct-1996: V0.4 Change to use "MA_MODELFIT" (Hammersley)
!    21-Jul-1995: V0.3 Find initial ellipse estimates by fitting a
!      circle (which appears to be more robust) (Hammersley)
!    20-Jan-1995: V0.2 Avoid possibility of "radial_error" being
!      calculated as NaN owing to rounding errors leading to the
!      square root of a negative number being calculated (Hammersley)
!    17-Aug-1994: V0.1 Original, based on "F2D_LSQCIRCLE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: Max_coordinates ! Dimension of coordinate arrays
     Integer, Intent(IN) :: num_coordinates ! Number of input coordinates
!      (must be greater than 4 and not more than "Max_coordinates")
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates) ! X-coordinates of peaks
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates) ! Y-coordinates of peaks
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_centre ! X-coordinate of best fit beam centre
     Real, Intent(OUT) :: y_centre ! Y-coordinate of best fit beam centre
     Real, Intent(OUT) :: radius1 ! Longer of the two axes, radius of best fit 
!      ellipse
     Real, Intent(OUT) :: radius2 ! Shorter axis radius of best fit ellipse
     Real, Intent(OUT) :: angle1 ! Orientation angle of longer axis of best fit
!      ellipse
     Real, Intent(OUT) :: radial_error ! Estimated average coordinate radial
!      position error from best fit parameters. If only 5 coordinates have been
!      entered, this is set to -1.7e38
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
     Integer, Parameter :: Max_control = 1 ! Dimension for control parameter 
!      array
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: dummy ! Dummy variable for unused array references
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_control ! Number of control parameters
     Integer :: num_fun_calls ! Number of function calls used
     Integer :: num_variables ! Number of fit variables
     Integer :: par ! Loop variable for parameters
     Integer :: retstat ! Return status variable:
!      -10 = No variable parameters to optimise
!       -3 = The cosine of the angle between "RESIDUALS" and any
!            column of the Jacobian is at most "Abs(tolerence)"
!       -2 = Condition of "0" and "-1" are both true
!       -1 = Actual and predicted relative reductions in the sum
!            of squares are at most "tolerence"
!        0 = Good status: Relative error between two iterations
!            is "tolerence" or below
!        1 = More variable parameters than unmasked data values
!        2 = Not enough available memory
!        3 = Improper input parameters
!        4 = Number of calls to "CAL_MODEL" has reached or
!            exceeded "max_fun_calls"
!        5 = "tolerence" is too small. No further reduction in the
!            the sum of squares is possible
!        6 = "tolerence" is too small. No further improvement in
!            the approximate solution "PARAMS" is possible
!        7 = "tolerence" is too small. "RESIDUALS" is orthogonal to
!            the columns of the Jacobian (to machine precision)
     Integer stat ! Status return variable for "Allocate"
     Real :: fepsilon ! Estimated error in calculating function
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares of mis-fit distances
     Real :: temp ! Temporary variable
     Real :: tolerence ! Tolerence on termination
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(5) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(5) ! Parameter array
!      (1) = x_centre
!      (2) = y_centre
!      (3) = radius1
!      (4) = radius2
!      (5) = angle1
     Real :: SCALE_FACTORS(5) ! Scale factors array for the parameter
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     External F2D_FUNELLIPSE
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQELLIPSE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_coordinates .Le. 0) Then
        status = St_bad_dim1
     Else If (num_coordinates .Lt. 5 .Or. num_coordinates .Gt. &
       max_coordinates) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQELLIPSE ' // Version)
     Else
 
!     Initial centre and radius estimates from best fit circle
        Call F2D_LSQCIRCLE (max_coordinates, num_coordinates, X_COORDINATES, &
          Y_COORDINATES, x_centre, y_centre, radius1, radial_error, status)
 
        radius2 = radius1 * 0.99
        angle1 = 0.0 ! Start at zero degrees
 
!     Transfer coordinate variables through common
        Do coordinate  = 1, num_coordinates
           F2D_X_COORDINATES(coordinate) = X_COORDINATES(coordinate)
           F2D_Y_COORDINATES(coordinate) = Y_COORDINATES(coordinate)
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Non-linear least squares fitting of centre major and minor axes
!     and orientation
        num_variables = 5
 
!     Set initial values of variables
        PARAMS(1) = x_centre
        PARAMS(2) = y_centre
        PARAMS(3) = radius1
        PARAMS(4) = radius2
        PARAMS(5) = angle1
        Do par = 1, 5
           CONSTRAINTS(par) = .False.
           SCALE_FACTORS(par) = radius1 / 100.0
        End Do
        SCALE_FACTORS(5) = 0.05
 
!     Set up variables for "MA_MODELFIT"
        maxfun = 100
        tolerence = 1.0e-5
        step_bound = 100.0
        fepsilon = 5.0e-7
        num_control = 0
 
!     Obtain dynamic array space for model function
        Allocate (MODEL(num_coordinates), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_LSQELLIPSE ' // Version)
           Return
        End If
 
!     Perform minimisation
        Call MA_MODELFIT (experiment, 0, F2D_FUNELLIPSE, &
          .False., .False., .False., &
          num_coordinates, 1, dummy, dummy, dummy, 1, 1, num_coordinates, 1, &
          1, maxfun, tolerence, step_bound, fepsilon, Max_control, &
          num_control, CONTROL, 5, 5, CONSTRAINTS, SCALE_FACTORS, &
          num_coordinates, 1, .False., PARAMS, MODEL, retstat, &
          num_fun_calls, sumsq, status)
 
        If (retstat .Ge. 1) Then
           Call IO_WRITE ('WARNING: Problem fitting the ellipse', status)
        End If
 
!     Free dynamic array space
        Deallocate (MODEL)
 
!     Set output variables
        x_centre = PARAMS(1)
        y_centre = PARAMS(2)
        radius1 = PARAMS(3)
        radius2 = PARAMS(4)
        angle1 = PARAMS(5)
 
!     'sumsq' should strictly always be positive, but owing to
!     rounding errors it can go negative, which means that the
!     the sum of squares is really about zero
        If (sumsq .Gt. 0.0) Then
           If (num_coordinates .Gt. 5) Then
              radial_error = Sqrt(sumsq / Real(num_coordinates - 5))
           Else
              radial_error = -1.7e38
           End If
 
        Else
           radial_error = 0.0
        End If
 
!     Set radius 1 to be the longer of the two axes
        If (radius2 .Gt. radius1) Then
           temp = radius1
           radius1 = radius2
           radius2 = temp
           angle1 = angle1 + Pi / 2.0
        End If
 
!     Set angle to be between +-Pi/2.0
        If (angle1 .Gt. Pi / 2.0) Then
 
           Do While (angle1 .Gt. Pi / 2.0)
              angle1 = angle1 - Pi
           End Do
 
        Else If (angle1 .Lt. -Pi / 2.0) Then
 
           Do While (angle1 .Lt. -Pi / 2.0)
              angle1 = angle1 + Pi
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_LSQELLIPSE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 

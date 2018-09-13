!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_lsqcircle.f90 *
!  *                   *
!  *********************
 
!+ F2D_LSQCIRCLE: Least SQuares CIRCLE parameters
     Subroutine F2D_LSQCIRCLE (max_coordinates, num_coordinates, &
       X_COORDINATES, Y_COORDINATES, x_centre, y_centre, radius, radial_error, &
       status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Circle.Centre/Radius, Least~Squares.Circle.Centre/Radius
!  Method:
!    Non-linear least squares iterative solution
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.6 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.5 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    22-Oct-1996: V0.4 "MA_MODELFIT" returns number of function calls 
!      (Hammersley)
!    19-Oct-1996: V0.3 Convert to using "MA_MODELFIT" (Hammersley)
!    26-Jan-1995: V0.2 Change of names of common coordinate arrays (Hammersley)
!    16-Aug-1994: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
!     Use MA_LIB
!  Use GS_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc'
!  Import:
     Integer, Intent(IN) :: Max_coordinates ! Dimension size of coordinate
!      arrays
     Integer, Intent(IN) :: num_coordinates ! Number of input coordinates
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates) ! X-coordinates of peaks
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates) ! Y-coordinates of peaks
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_centre ! X-coordinate of best fit beam centre
     Real, Intent(OUT) :: y_centre ! Y-coordinate of best fit beam centre
     Real, Intent(OUT) :: radius ! Best fit radius
     Real, Intent(OUT) :: radial_error ! Estimated average coordinate radial
!      position error from best fit parameters
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
     Real :: tolerence ! Tolerence on termination
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(3) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(3) ! Parameter array
     Real :: SCALE_FACTORS(3) ! Scale factors array for the parameter
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     External F2D_FUNCIRCLE
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQCIRCLE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_coordinates .Le. 0) Then
        status = St_bad_dim1
     Else If (num_coordinates .Lt. 3 .Or. num_coordinates .Gt. &
       max_coordinates) Then
        status = St_bad_adr1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQCIRCLE ' // Version)
     Else
 
!     Set "reasonable" initial estimates of centre and radius (centre is set as
!     average coordinate, and an average radius is calculated based on this 
!     centre)
        x_centre = 0.0
        y_centre = 0.0
        Do coordinate = 1, num_coordinates
           x_centre = x_centre + X_COORDINATES(coordinate)
           y_centre = y_centre + Y_COORDINATES(coordinate)
        End Do
        x_centre = x_centre / Real(num_coordinates)
        y_centre = y_centre / Real(num_coordinates)
 
!     Initial radius
        radius = 0.0
        Do coordinate = 1, num_coordinates
           radius = radius + Sqrt( (x_centre - X_COORDINATES(coordinate))**2 + &
             (y_centre - Y_COORDINATES(coordinate))**2)
        End Do
        radius = radius / Real(num_coordinates)
 
!     Transfer coordinate variables through common
        Do coordinate  = 1, num_coordinates
           F2D_X_COORDINATES(coordinate) = X_COORDINATES(coordinate)
           F2D_Y_COORDINATES(coordinate) = Y_COORDINATES(coordinate)
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Non-linear least squares fitting of centre and radius
 
        num_variables = 3
 
!     Set initial values of variables
        PARAMS(1) = x_centre
        PARAMS(2) = y_centre
        PARAMS(3) = radius
        Do par = 1, 3
           CONSTRAINTS(par) = .False.
           SCALE_FACTORS(par) = radius / 100.0
        End Do
 
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
           Call ST_SAVE ('Subroutine F2D_LSQCIRCLE ' // Version)
           Return
        End If
 
!     Perform minimisation
        Call MA_MODELFIT (experiment, 0, F2D_FUNCIRCLE, &
          .False., .False., .False., &
          num_coordinates, 1, dummy, dummy, dummy, 1, 1, num_coordinates, 1, &
          1, maxfun, tolerence, step_bound, fepsilon, Max_control, &
          num_control, CONTROL, 3, 3, CONSTRAINTS, SCALE_FACTORS, &
          num_coordinates, 1, .False., PARAMS, MODEL, retstat, &
          num_fun_calls, sumsq, status)
 
        If (retstat .Ge. 1) Then
           Call IO_WRITE ('WARNING: Problem fitting the circle', status)
        End If
 
!     Free dynamic array space
        Deallocate (MODEL)
 
!     Set output variables
        x_centre = PARAMS(1)
        y_centre = PARAMS(2)
        radius = PARAMS(3)
        radial_error = Sqrt(sumsq / Real(num_coordinates))
 
     End If
 
     End Subroutine F2D_LSQCIRCLE
!********1*********2*********3*********4*********5*********6*********7*********8

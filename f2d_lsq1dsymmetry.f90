!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_lsq1dsymmetry.f90 *
!  *                       *
!  *************************
 
!+ F2D_LSQ1DSYMMETRY: FIT 1-D SYMMETRY
     Subroutine F2D_LSQ1DSYMMETRY (maxdat, strelm, endelm, DATA, MASK, &
       x_symmetry, status)
!  Description:
!    Fits a symmetry point to the data between "strelm" and "endelm" in "DATA".
!  Keywords:
!    Symmetry.1-D, Least~Squares.Symmetry.1-D
!  Method:
!    "F2D_LSQ1DSYMMETRY" is used to calculate the residuals.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.3 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.2 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    26-May-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: maxdat ! Dimension of data array
     Integer, Intent(IN) :: strelm ! First element of region to fit
     Integer, Intent(IN) :: endelm ! Last element of region to fit
     Real, Intent(IN) :: DATA(maxdat) ! Data to fit
     Logical*1, Intent(IN) :: MASK(maxdat) ! .True., if an element is masked
!      off
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_symmetry ! Fitted symmetry point
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_control = 1 ! Dimension for control parameter 
!      array
     Integer, Parameter :: Max_parameters = 1 ! Dimension for parameter array
!  Local Variables:
     Integer :: dummy ! Dummy variable for unused array references
     Integer :: info ! Level of "MA_MODELFIT" user information output:
!      0:    No information
!      >= 1" Output iteration numbers
!      >= 2: Output sum of squared residuals
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_control ! Number of control parameters
     Integer :: num_fun_calls ! Number of function calls used
     Integer :: retstat ! Return status variable:
!      -10 = No variable parameters to optimise
!      -3 = The cosine of the angle between "RESIDUALS" and any
!           column of the Jacobian is at most "Abs(tolerence)"
!      -2 = Condition of "0" and "-1" are both true
!      -1 = Actual and predicted relative reductions in the sum
!           of squares are at most "tolerence"
!       0 = Good status: Relative error between two iterations
!           is "tolerence" or below
!       1 = More variable parameters than unmasked data values
!       2 = Not enough available memory
!       3 = Improper input parameters
!       4 = Number of calls to "CAL_MODEL" has reached or exceeded 
!           "max_fun_calls"
!       5 = "tolerence" is too small. No further reduction in the sum of squares
!             is possible
!       6 = "tolerence" is too small. No further improvement in
!           the approximate solution "PARAMS" is possible
!       7 = "tolerence" is too small. "RESIDUALS" is orthogonal to
!           the columns of the Jacobian (to machine precision)
     Integer :: stat ! Status return variable for "Allocate"
     Real :: fepsilon ! Estimated error in calculating function
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares of mis-fit distances
     Real :: tolerence ! Tolerence on termination
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(Max_parameters) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(Max_parameters) ! Parameter array symmetry centre
     Real :: SCALE_FACTORS(Max_parameters) ! Scale factors array for the 
!      parameters
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     External F2D_FUN1DSYMMETRY
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQ1DSYMMETRY ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_LSQ1DSYMMETRY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the region to be examined is reasonably defined
     If (maxdat .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQ1DSYMMETRY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Define model parameters
        PARAMS(1) = Real(strelm + endelm - 1) / 2.0
 
!     Set scale factor of typical parameter changes
        SCALE_FACTORS(1) = 5.0
 
!     Set constrained parameters
        CONSTRAINTS(1) = .False.
 
!     Set up variables for "MA_MODELFIT"
        maxfun = 300
        tolerence = 1.0e-5
        step_bound = 100.0
        fepsilon = 5.0e-7
        num_control = 0
        info = 0
 
!     Obtain dynamic array space for model function
        Allocate (MODEL(maxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_LSQ1DSYMMETRY ' // Version)
           Return
        End If
 
!     Perform minimisation
        Call MA_MODELFIT (experiment, info, F2D_FUN1DSYMMETRY, &
          .True., .False., .False., &
          maxdat, 1, DATA, MASK, dummy, strelm, 1, endelm, 1, 1, maxfun, &
          tolerence, step_bound, fepsilon, Max_control, num_control, CONTROL, &
          Max_parameters, 1, CONSTRAINTS, SCALE_FACTORS, maxdat, 1, .True., &
          PARAMS, MODEL, retstat, num_fun_calls, sumsq, status)
 
        If (retstat .Ge. 1) Then
           Call IO_WRITE ('WARNING: Problem fitting the symmetry centre', &
             status)
        End If
 
!     Free dynamic array space
        Deallocate (MODEL)
 
!     Set output variables
        x_symmetry = PARAMS(1)
 
     End If
 
     End Subroutine F2D_LSQ1DSYMMETRY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

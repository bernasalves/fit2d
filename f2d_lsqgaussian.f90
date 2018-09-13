!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_lsqgaussian.f90 *
!  *                     *
!  ***********************
 
!+ F2D_LSQGAUSSIAN: Least SQuares GAUSSIAN fitting
     Subroutine F2D_LSQGAUSSIAN (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, x_centre, y_centre, intensity, sigma1, sigma2, angle, &
       background, retstat, chi_squared, status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Fit.2-D~Gaussian, 2-D~Gaussian.Fit, Gaussian.2-D.Fit
!  Method:
!    Non-linear least squares iterative solution
!  Deficiencies:
!    Pointers to the data and work arrays must be passed through
!    "COMMON" to the objective function "F2D_FUNGAUSSIAN"
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.8 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.7 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    10-Feb-1997: V0.6 Change parameter scaling to improve convergence
!      (Hammersley)
!    10-Jan-1997: V0.5 Change parameter scaling (Hammersley)
!    19-Dec-1996: V0.4 Investigating poor centring of Gaussian (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    22-Oct-1996: V0.2 Convert to using "MA_MODELFIT" (Hammersley)
!    06-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!    Include 'f2d_fitgaussian.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
     Real, Intent(INOUT) :: x_centre ! X-coordinate (pixels) of best fit
!      Gaussian centre
     Real, Intent(INOUT) :: y_centre ! Y-coordinate (pixels) of best fit
!      Gaussian centre
     Real, Intent(INOUT) :: intensity ! Maximum intensity of Gaussian
     Real, Intent(INOUT) :: sigma1 ! First axis standard deviation (pixels)
!      of Gaussian
     Real, Intent(INOUT) :: sigma2 ! Second axis standard deviation (pixels)
!      of Gaussian
     Real, Intent(INOUT) :: angle ! Orientation angle of first axis of Gaussian
     Real, Intent(INOUT) :: background ! Background value
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status variable:
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
!       4 = Number of calls to "CAL_MODEL" has reached or
!           exceeded "max_fun_calls"
!       5 = "tolerence" is too small. No further reduction in the
!           the sum of squares is possible
!       6 = "tolerence" is too small. No further improvement in
!           the approximate solution "PARAMS" is possible
!       7 = "tolerence" is too small. "RESIDUALS" is orthogonal to
!           the columns of the Jacobian (to machine precision)
     Real, Intent(OUT) :: chi_squared ! Reduced chi squared value
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_control = 1 ! Dimension for control
!      parameter array
     Integer, Parameter :: Max_parameters = 7 ! Dimension of the variable
!      parameters array
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: dummy ! Dummy variable for unused array references
     Integer :: iter ! Loop variable for fitting iterations
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_control ! Number of control parameters
     Integer :: num_fun_calls ! Number of function calls used
     Integer :: num_variables ! Number of fit variables
     Integer :: par ! Loop variable for parameters
     Integer :: stat ! Status return variable for "Allocate"
     Integer :: xmaxmod ! First dimension size for model array
     Integer :: ymaxmod ! First dimension size for model array
     Real :: fepsilon ! Estimated error in calculating function
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares of mis-fit distances
     Real :: tolerence ! Tolerence on termination
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(Max_parameters) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(Max_parameters) ! Stores the function parameters
!      (1) = x_centre
!      (2) = y_centre
!      (3) = intensity
!      (4) = sigma1
!      (5) = sigma2
!      (6) = angle
!      (7) = background
     Real :: SCALE_FACTORS(Max_parameters) ! Scale factors array for
!      the parameter changes
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     External F2D_FUNGAUSSIAN
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_LSQGAUSSIAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQGAUSSIAN ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQGAUSSIAN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Non-linear least squares fitting of Gaussian position, size,
!     orientation, and background
        num_variables = 7
 
!     Set initial values of variables
        PARAMS(1) = x_centre
        PARAMS(2) = y_centre
        PARAMS(3) = intensity
        PARAMS(4) = sigma1
        PARAMS(5) = sigma2
        PARAMS(6) = angle
        PARAMS(7) = background
 
        Do par = 1, num_variables
           CONSTRAINTS(par) = .False.
        End Do
        SCALE_FACTORS(1) = 0.25
        SCALE_FACTORS(2) = 0.25
        SCALE_FACTORS(3) = Max(1.0, Sqrt(intensity) * 2.0)
        SCALE_FACTORS(4) = Max(0.1, sigma1 / 20.0)
        SCALE_FACTORS(5) = Max(0.1, sigma2 / 20.0)
        SCALE_FACTORS(6) = 0.05
        SCALE_FACTORS(7) = Max(1.0, Sqrt(background) * 3.0)
 
!     Set up variables for "MA_MODELFIT"
        maxfun = 300
        tolerence = 5.0d-5
        step_bound = 100.0
        fepsilon = 5.0e-7
        num_control = 0
        xmaxmod = xendelm - xstrelm + 1
        ymaxmod = yendelm - ystrelm + 1
 
!     Obtain dynamic array space for model function
        Allocate (MODEL(xmaxmod * ymaxmod), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_LSQGAUSSIAN ' // Version)
           Return
        End If
 
!     Perform minimisation
        Do iter = 1, 4
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Before MA_MODELFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Call MA_MODELFIT (experiment, 0, F2D_FUNGAUSSIAN, &
             .False., .False., .True., &
             xmaxdat, ymaxdat, DATA, dummy, dummy, xstrelm, ystrelm, xendelm, &
             yendelm, 1, maxfun, tolerence, step_bound, fepsilon, Max_control, &
             num_control, CONTROL, 7, 7, CONSTRAINTS, SCALE_FACTORS, xmaxmod, &
             ymaxmod, .False., PARAMS, MODEL, retstat, num_fun_calls, &
             sumsq, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Do par = 1, 7
!        Write (*, '(''PARAMETER '', i2, '' = '', g14.7)')
!        :             par, PARAMS(par)
!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
!     Refine only centre
        Do par = 3, num_variables
           CONSTRAINTS(par) = .True.
        End Do

        Call MA_MODELFIT (experiment, 0, F2D_FUNGAUSSIAN, &
          .False., .False., .True., &
          xmaxdat, ymaxdat, DATA, dummy, dummy, xstrelm, ystrelm, xendelm, &
          yendelm, 1, maxfun, tolerence, step_bound, fepsilon, Max_control, &
          num_control, CONTROL, 7, 7, CONSTRAINTS, SCALE_FACTORS, xmaxmod, &
          ymaxmod, .False., PARAMS, MODEL, retstat, num_fun_calls, &
          sumsq, status)
 
!     Free dynamic array space
        Deallocate (MODEL)
 
        If (retstat .Ge. 1) Then
           Call IO_WRITE ('WARNING: Problem fitting the Gaussian', status)
        End If
 
!     Set output variables
        x_centre = PARAMS(1)
        y_centre = PARAMS(2)
        intensity = PARAMS(3)
        sigma1 = PARAMS(4)
        sigma2 = PARAMS(5)
        angle = PARAMS(6)
        background = PARAMS(7)
 
        chi_squared = sumsq / Real((xendelm - xstrelm + 1) * (yendelm - &
          ystrelm + 1))
 
        Write (message, '(''INFO: Number of function calls = '', i6)') &
          num_fun_calls
        Call IO_WRITE (message, status)
 
     End If
 
     End Subroutine F2D_LSQGAUSSIAN
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7**
 
!  ***************************
!  *                         *
!  * f2d_lsqpolarisation.f90 *
!  *                         *
!  ***************************
 
!+ F2D_LSQPOLARISATION: Least SQuare refinement of POLARISATION
!    (Note: coordinates are passed through "f2d_fitrings.inc" common block)
     Subroutine F2D_LSQPOLARISATION (sample_distance, max_rings, num_rings, &
       max_rcoordinates, polarisation_factor, status)
!  Description:
!    Refines the polarisation factor to obtain the minimum least
!    squares difference between the average value for each ring
!    after polaristation correction and each corrected data point.
!  Keywords:
!    Least~Squares.Polarisation.Powder
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    30-Mar-2006: V0.3 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.2 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    09-Jul-1996: V0.1 Original, based on "F2D_FITCALIBRANT" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc' ! Coordinate arrays for fitting
     Include 'f2d_lsqpowder.inc' ! Calibrant Powder ring fitting control 
!      parameters
!  Import:
     Real, Intent(IN) :: sample_distance ! Distance (metres) from sample to
!      detector (beam centre on detector)
     Integer, Intent(IN) :: max_rings ! First Dimension of coordinate arrays
     Integer, Intent(IN) :: num_rings ! Number of powder rings with data
!      points
     Integer, Intent(IN) :: max_rcoordinates ! Second dimension of
!      coordinate arrays
!  Import/Export:
     Real, Intent(INOUT) :: polarisation_factor ! Polarisation factor,
!      defined as (I_h - I_v) / (I_h + I_v), where horizontal should normally
!      correspond to the X-direction of the image
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
     Integer, Parameter :: Max_control = 1 ! Dimension for control
!      parameter array
     Integer, Parameter :: Max_variables = 2 ! Dimension for parameter arrays
!  Local Variables:
     Integer :: coordinate ! Loop variable for ring coordinates
     Integer :: dummy ! Dummy variable for unused array references
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_control ! Number of control parameters
     Integer :: num_fun_calls ! Number of function calls used
     Integer :: num_residuals ! Number of coordinates to fit
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
     Integer :: ring ! Loop variable for powder rings
     Integer stat ! Status return variable for "Allocate"
     Real :: fepsilon ! Estimated error in calculating function
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares of mis-fit distances
     Real :: tolerence ! Tolerence on termination
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(Max_variables) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(Max_variables) ! Parameter array polarisation_factor
     Real :: SCALE_FACTORS(Max_variables) ! Scale factors array for
!      the parameter
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: EXPERIMENT ! Dummy, not used
!  External Functions:
     External F2D_FUNPOLARISATION
!    Common:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQPOLARISATION ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_LSQPOLARISATION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the region to be examined is reasonably defined
     If (max_rings .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQPOLARISATION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate number of data values to fit
        num_residuals = 0
        Do ring = 1, num_rings
           num_residuals = num_residuals + F2D_NUM_RCOORDINATES(ring)
        End Do
 
        If (num_residuals .Le. 0) Then
           Call IO_WRITE ( 'ERROR: There are no coordinates to fit !', status)
           status = St_mod_fit2d + St_bad_other
           Call ST_SAVE ('Subroutine F2D_LSQPOLARISATION ' // Version)
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Do ring = 1, num_rings
 
           Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
 
              Write (*, '(''ring, azimuth, intensity = '', i3, 2g14.5)') ring, &
                F2D_AZIMUTHS(coordinate, ring) * 180.0 / Pi, &
                F2D_RINTENSITIES(coordinate, ring)
 
           End Do
 
        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Non-linear least squares fitting of parameters to the powder rings
!     Set up variables for "MA_MODELFIT"
        maxfun = 300
        tolerence = 1.0e-5
        step_bound = 100.0
        fepsilon = 5.0e-7
        num_control = 0
 
        f2d_num_rings = num_rings
 
        PARAMS(1) = polarisation_factor
        PARAMS(2) = sample_distance
        CONSTRAINTS(1) = .False.
        CONSTRAINTS(2) = .True.
        SCALE_FACTORS(1) = 0.05
 
!     Obtain dynamic array space for model function
        Allocate (MODEL(num_residuals), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_LSQPOLARISATION ' // Version)
           Return
        End If
 
!     Perform minimisation
        Call MA_MODELFIT (EXPERIMENT, 2, F2D_FUNPOLARISATION, &
          .False., .False., .False., &
          num_residuals, 1, dummy, dummy, dummy, 1, 1, num_residuals, 1, 1, &
          maxfun, tolerence, step_bound, fepsilon, Max_control, num_control, &
          CONTROL, 2, 2, CONSTRAINTS, SCALE_FACTORS, num_residuals, 1, &
          .False., PARAMS, MODEL, retstat, num_fun_calls, sumsq, status)
 
        If (retstat .Ge. 1) Then
           Call IO_WRITE ('WARNING: Problem fitting the ' // &
             'polarisation factor', status)
        Else
 
           polarisation_factor = PARAMS(1)
 
        End If
 
!     Free dynamic array space
        Deallocate (MODEL)
 
     End If
 
     End Subroutine F2D_LSQPOLARISATION
!********1*********2*********3*********4*********5*********6*********7**
 
 

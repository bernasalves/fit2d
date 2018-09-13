!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_lsqpowder.f90 *
!  *                   *
!  *********************
 
!+ F2D_LSQPOWDER: Least SQuares POWDER parameters (Note: coordinates are 
!    passed through "f2d_fitrings.inc" common block)
     Subroutine F2D_LSQPOWDER (full_info, max_angles, num_rings, &
       weighted_fitting, detector_gain, refine_beam_centre, &
       refine_sample_distance, refine_tilt, sample_distance, x_beam, y_beam, &
       ANGLE_CONES, tilt_plane_rotation, tilt_angle, radial_error, status)
!  Description:
!    Fitting of beam centre and tilt to one or more powder rings
!  Keywords:
!    Powder.Centre/Radius, Least~Squares.Powder.Centre/Radius
!  Method:
!    Non-linear least squares iterative solution
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    24-Apr-2013: V0.11 Output RMS residual value (Hammersley)
!    30-Mar-2006: V0.10 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.9 Use dummy "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.8 Avoid open strings crossing lines (Hammersley)
!    15-Nov-1996: V0.7 Convert to "MA_MODELFIT" (Hammersley)
!    22-Aug-1996: V0.6 Add option to not fit tilt (Hammersley)
!    19-Feb-1995: V0.5 Choice of weighted fitting or not (Hammersley)
!    27-Jan-1995: V0.4 Coordinates passed through "f2d_fitring.inc" common 
!      block (Hammersley)
!    26-Jan-1995: V0.3 Simultaneous fitting of several powder rings (Hammersley)
!    20-Jan-1995: V0.2 Avoid possibility of "radial_error" being
!      calculated as NaN owing to rounding errors leading to the
!      square root of a negative number being calculated (Hammersley)
!    23-Aug-1994: V0.1 Original, based on "F2D_LSQELLIPSE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitrings.inc' ! Coordinate arrays for fitting
     Include 'f2d_lsqpowder.inc' ! Used to pass control variables to 
!      "F2D_FUNPOWDER"
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by the user
     Integer, Intent(IN) :: max_angles ! Dimension size of "ANGLES" array
     Integer, Intent(IN) :: num_rings ! Number of powder rings for which
!      coordinates are available for fitting
     Logical, Intent(IN) :: weighted_fitting ! .True., if the fitting of
!      tilt and beam centre is to be weighted by the intensities
     Real, Intent(IN) :: detector_gain ! The gain of the detector i.e. the
!      intensity values divided by the gain should produce counts
     Logical, Intent(IN) :: refine_beam_centre ! .True., if the beam centre
!      position is to be refined
     Logical, Intent(IN) :: refine_sample_distance ! .True., if the sample
!      to detector distance is to be refined
     Logical, Intent(IN) :: refine_tilt ! .True., if the detector
!      non-orthogonality is to be refined
!  Import/Export:
     Real, Intent(INOUT) :: sample_distance ! Distance (metres) from sample
!      to detector (beam centre on detector)
     Real, Intent(INOUT) :: x_beam ! Best fit X-coordinate of beam centre
     Real, Intent(INOUT) :: y_beam ! Best fit Y-coordinate of beam centre
     Real, Intent(INOUT) :: ANGLE_CONES(max_angles) ! Opening angles of cones
     Real, Intent(INOUT) :: tilt_plane_rotation ! Angle of rotation for the
!      tilt plane in the Z = 0 plane (radians)
     Real, Intent(INOUT) :: tilt_angle ! Angle of detector tilt in the
!      rotated Y-direction (radians)
!  Export:
     Real, Intent(OUT) :: radial_error ! Estimated average coordinate radial
!      position error from best fit parameters. If only 5 coordinates have 
!      been entered, this is set to -1.7e38
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
     Integer, Parameter :: Max_control = 1 ! Dimension for control
!      parameter array
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: cycle ! Loop variable for fitting cycles
     Integer :: dummy ! Dummy variable for unused array references
     Integer :: info ! Level of "MA_MODELFIT" user information output:
!      0: no information
!      >= 1" output iteration numbers
!      >= 2: output sum of squared residuals
     Integer :: maxfun ! Maximum number of function calls
     Integer :: num_control ! Number of control parameters
     Integer :: num_fun_calls ! Number of function calls used
     Integer :: num_variables ! Number of fit variables
     Integer :: num_residuals ! Number of coordinates to fit
     Integer :: ring ! Loop variable for powder rings
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
!       5 = "tolerence" is too small. No further reduction in the
!           the sum of squares is possible
!       6 = "tolerence" is too small. No further improvement in
!           the approximate solution "PARAMS" is possible
!       7 = "tolerence" is too small. "RESIDUALS" is orthogonal to
!           the columns of the Jacobian (to machine precision)
     Integer stat ! Status return variable for "Allocate"
     Real :: fepsilon ! Estimated error in calculating function
     Real :: reference_weight ! Weighting value of first coordinate on first 
!      ring
     Real :: step_bound ! Maximum iteration step bound
     Real :: sumsq ! Sum of squares of mis-fit distances
     Real :: tolerence ! Tolerence on termination
     Real :: weight ! Used to calculate average weight
!  Local Arrays:
     Integer :: CONTROL(Max_control) ! Control parameters (unused)
     Logical :: CONSTRAINTS(Max_parameters) ! Constraints array
     Real, Allocatable :: MODEL(:) ! Dynamic array to hold model
     Real :: PARAMS(Max_parameters) ! Parameter array
!      x_beam
!      y_beam
!      distance
!      tilt_plane_rotation
!      tilt_angle
!      ANGLE_CONE(1)
!      ANGLE_CONE(2)
!      etc.
     Real :: SCALE_FACTORS(Max_parameters) ! Scale factors array for the 
!      parameters
!  Local Data Structures:
     Type(EXPERIMENTAL_DETAILS) :: experiment ! Dummy, not used
!  External Functions:
     External F2D_FUNPOWDER
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LSQPOWDER ' // Version)
        Return
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_LSQPOWDER'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check that the region to be examined is reasonably defined
     If (max_angles .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_LSQPOWDER ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_beam, y_beam (mm) = '', 2f12.3)')
!     :       x_beam*1000.0, y_beam*1000.0
!     Do ring = 1, num_rings
!     Write (*, '(''Ring '', i2, '' Angle (degrees) = '',
!     :          f12.3)') ring, ANGLE_CONES(ring)*180.0/Pi
!     End Do
!     Write (*, '(''tilt_plane_rotation, tilt_angle = '', 2f12.3)')
!     :       tilt_plane_rotation*180.0/Pi, tilt_angle*180.0/Pi
 
!     Do coordinate = 1, F2D_NUM_RCOORDINATES(1)
!     Write (*, '(i, 2f8.3)') coordinate,
!     :          F2D_X_RCOORDINATES(coordinate, 1)*1000.0,
!     :          F2D_Y_RCOORDINATES(coordinate, 1)*1000.0
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Pass control information to objective function subroutine
        f2d_num_rings = num_rings
        f2d_weighted_fitting = weighted_fitting
        f2d_detector_gain = detector_gain
 
!     Calculate number of data values to fit
        num_residuals = 0
        Do ring = 1, num_rings
           num_residuals = num_residuals + F2D_NUM_RCOORDINATES(ring)
        End Do
 
        If (num_residuals .Le. 0) Then
           Call IO_WRITE ( 'ERROR: There are no coordinates to fit !', status)
           status = St_mod_fit2d + St_bad_other
           Call ST_SAVE ('Subroutine F2D_LSQPOWDER ' // Version)
           Return
        End If
 
!     Define model parameters
        num_variables = 5 + num_rings
        PARAMS(1) = x_beam
        PARAMS(2) = y_beam
        PARAMS(3) = sample_distance
        PARAMS(4) = tilt_plane_rotation
        PARAMS(5) = tilt_angle
 
!     Set scale factor of typical parameter changes
        SCALE_FACTORS(1) = 0.0005
        SCALE_FACTORS(2) = 0.0005
        SCALE_FACTORS(3) = 0.001
        SCALE_FACTORS(4) = 0.35
        SCALE_FACTORS(5) = 0.01
 
!     Set constrained parameters
        CONSTRAINTS(1) = .Not. refine_beam_centre
        CONSTRAINTS(2) = .Not. refine_beam_centre
        CONSTRAINTS(3) = .Not. refine_sample_distance
        CONSTRAINTS(4) = .Not. refine_tilt
        CONSTRAINTS(5) = .Not. refine_tilt
 
!     Diffraction cone angles
        Do ring = 1, num_rings
           PARAMS(5 + ring) = ANGLE_CONES(ring)
           CONSTRAINTS(5 + ring) = .False.
           SCALE_FACTORS(5 + ring) = 0.005
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''num_variables = '', i3)') num_variables
!     Write (*, '(''num_residuals = '', i3)') num_residuals
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Non-linear least squares fitting of parameters to the powder rings
!     Set up variables for "MA_MODELFIT"
        maxfun = 300
        tolerence = 1.0e-5
        step_bound = 100.0
        fepsilon = 5.0e-7
        num_control = 0
        If (full_info) Then
           info = 2
        Else
           info = 0
        End If
 
!     Obtain dynamic array space for model function
        Allocate (MODEL(num_residuals), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_LSQPOWDER ' // Version)
           Return
        End If
 
        Do cycle = 1, 2
 
!        Perform minimisation
           Call MA_MODELFIT (experiment, info, F2D_FUNPOWDER, &
             .False., .False., .False., &
             num_residuals, 1, dummy, dummy, dummy, 1, 1, num_residuals, 1, 1, &
             maxfun, tolerence, step_bound, fepsilon, Max_control, &
             num_control, CONTROL, Max_parameters, num_rings + 5, CONSTRAINTS, &
             SCALE_FACTORS, num_residuals, 1, .False., PARAMS, MODEL, &
             retstat, num_fun_calls, sumsq, status)
 
           If (retstat .Ge. 1) Then
              Call IO_WRITE ('WARNING: Problem fitting the ' // &
                'powder pattern', status)
           End If
 
        End Do
 
!     Free dynamic array space
        Deallocate (MODEL)
 
!     Set output variables
        x_beam = PARAMS(1)
        y_beam = PARAMS(2)
        sample_distance = PARAMS(3)
        tilt_plane_rotation = PARAMS(4)
        tilt_angle = PARAMS(5)
 
!     Diffraction cone angles
        Do ring = 1, num_rings
           ANGLE_CONES(ring) = PARAMS(5 + ring)
        End Do
 
        Write (message, '(''INFO: Number of function calls = '', i8)') &
          num_fun_calls
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: Sum of squares = '', g14.5)') sumsq
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: RMS residual = '', g14.5)') &
          Sqrt(sumsq / num_residuals)
        Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     For weighted fitting we need to calculate the average weight in order to 
!     calculate the radial error from the sum of squared residuals
        If (weighted_fitting) Then
 
           reference_weight = Sqrt(F2D_RINTENSITIES(1, 1) / detector_gain)
 
!        Find average weight
           weight = 0.0
           Do ring = 1, num_rings
 
              Do coordinate = 1, F2D_NUM_RCOORDINATES(ring)
                 weight = weight + Sqrt( F2D_RINTENSITIES(coordinate, ring) / &
                   detector_gain) / reference_weight
              End Do
 
           End Do
           weight = weight / Real(num_residuals)
 
!        "sumsq" should strictly always be positive, but owing to
!        rounding errors it can go negative, which means that the
!        the sum of squares is really about zero
           If (sumsq .Gt. 0.0) Then
 
              If (num_residuals .Gt. num_variables) Then
                 radial_error = Sqrt((sumsq / weight**2) / Dble(num_residuals &
                   - num_variables))
              Else
                 radial_error = -1.7e38
              End If
 
           Else
              radial_error = 0.0
           End If
 
        Else
 
!        "sumsq" should strictly always be positive, but owing to rounding 
!        errors it can go negative, which means that the the sum of squares is 
!        really about zero
           If (sumsq .Gt. 0.0) Then
              If (num_residuals .Gt. num_variables) Then
                 radial_error = Sqrt(sumsq / &
                   Dble(num_residuals - num_variables))
              Else
                 radial_error = -1.7e38
              End If
 
           Else
              radial_error = 0.0
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_LSQPOWDER
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

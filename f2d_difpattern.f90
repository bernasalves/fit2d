!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_difpattern.f90 *
!  *                    *
!  **********************
 
!+ F2D_DIFPATTERN: calculates DIFfraction PATTERN
     Subroutine F2D_DIFPATTERN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       experiment, status)
!  Description:
!    Calculates and displays peaks from a user specified diffraction pattern.
!  Keywords:
!    Diffraction~Pattern
!  Method:
!    Call "F2D_DIFFRACTION"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    31-Mar-2006: V0.15 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    14-Mar-2006: V0.14 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    03-Dec-1997: V0.13 Correct slightly misleading user prompts (Hammersley)
!    25-Nov-1997: V0.12 Add more user help text (Hammersley)
!    16-Dec-1996: V0.11 Avoid open strings crossing lines (Hammersley)
!    03-Jun-1996: V0.10 Optional choice of hardcopy output (Hammersley)
!    11-Feb-1996: V0.9 Changes to "F2D_GEOMETRY" (Hammersley)
!    30-Jan-1996: V0.8 Use DDDR instead of DDR (Hammersley)
!    20-Jun-1995: V0.7 Convert to GS graphics library (Hammersley)
!    24-Jan-1995: V0.6 Input pixel sizes from arguments (Hammersley)
!    14-Feb-1994: V0.5 Output l = 0 predicted centres as a separate set of 
!      markers (Hammersley)
!    10-Jun-1993: V0.4 Calculate reciprocal unit cell angles (Hammersley)
!    06-May-1993: V0.3 Increase input range for phi (Hammersley)
!    23-Apr-1993: V0.2 Overlay possibility (Hammersley)
!    16-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics System constants
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
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.15' ! Version number
!  Local Variables:
     Integer, Save :: h_minimum = -1 ! The minimum h Miller index of the
!      peaks to be determined
     Integer, Save :: h_maximum = 1 ! The maximum h Miller index of the
!      peaks to be determined
     Integer, Save :: k_minimum = -1 ! The minimum k Miller index of the
!      peaks to be determined
     Integer, Save :: k_maximum = 1 ! The maximum k Miller index of the
!      peaks to be determined
     Integer, Save :: l_minimum = -30 ! The minimum l Miller index of the
!      peaks to be determined
     Integer, Save :: l_maximum = 30 ! The maximum l Miller index of the
!      peaks to be determined
     Integer :: max_coordinates ! Dimension size of coordinate arrays
     Integer :: num_coordinates ! Number of peak positions calculated
     Integer :: num_coord2 ! Number of peak positions for l=0 peaks
     Integer stat ! Status return variable for "Allocate"
     Logical :: file_open ! .True., if PS output file open
     Logical, Save :: hardcopy = .True. ! .True., if hardcopy output is
!      required
     Logical, Save :: overlay = .True. ! .True., if an overlayed display is
!      required
     Real, Save :: a_star = 0.02606 ! The length of the a-axis in inverse
!      Angstroms
     Real, Save :: a_theta = 88.34 ! The angle from the Z-axis (fibre) to
!      the a-axis in radians
     Real, Save :: a_phi = 0.0 ! The angle from the X-axis (beam) to the
!      a-axis in the X/Y plane in radians
     Real :: alpha_star ! Reciprocal unit cell angle alpha
     Real, Save :: b_star = 0.03852 ! The length of the b-axis in inverse
!      Angstroms
     Real, Save :: b_theta = 90.275 ! The angle from the Z-axis (fibre) to
!      the b-axis in radians
     Real, Save :: b_phi = 74.417 ! The angle from the X-axis (beam) to the
!      b-axis in the X/Y plane in radians
     Real :: beta_star ! Reciprocal unit cell angle beta
     Real, Save :: c_star = 0.00148 ! The length of the c-axis in inverse
!      Angstroms
     Real, Save :: c_theta = 2.8891 ! The angle from the Z-axis (fibre) to
!      the c-axis in radians
     Real, Save :: c_phi = -0.24 ! The angle from the X-axis (beam) to the
!      c-axis in the X/Y plane in radians
     Real :: gamma_star ! Reciprocal unit cell angle gamma
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(10) ! User information text
     Integer :: END_COORDINATES(2) ! Number of coordinates in curves to be
!      plotted
     Integer :: START_COORDINATES(2) ! Number of coordinates in curves to
!      be plotted
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic work array
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic work array
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DIFPATTERN ' // Version)
     Else
 
!     Attract users attention to usual definition of angles
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('NOTE: The reciprocal cell angles ' // &
          'are not defined in a standard', status)
        Call IO_WRITE ('      alpha star, beta star, gamma ' // &
          'star fashion.', status)
        Call IO_WRITE (' ', status)
 
!     Define general information text
        MESSAGE(1) = 'The diffraction pattern is that of a unit ' // &
          'cell randomly orientated'
        MESSAGE(2) = 'about a single axis i.e. a fibre pattern. ' // &
          'The unit cell must be'
        MESSAGE(3) = 'entered as the reciprocal space unit cell. ' // &
          'The command "UNIT CELL'
        MESSAGE(4) = 'PARAMETERS" may be used to convert between ' // &
          'real space and reciprocal'
        MESSAGE(5) = 'space cell parameters. The orientation of ' // &
          'the unit cell axes wrt to'
        MESSAGE(6) = 'the rotation (fibre) axis are defined ' // &
          'according to a system given'
        MESSAGE(7) = 'in R D B Fraser and T P MacRae, "Unit cell ' // &
          'and Molecular Connectivity'
        MESSAGE(8) = 'in Tendon Collagen", Int. J. Biol. ' // &
          'Macromol., Vol 3, pp 193-200, 1981.'
        MESSAGE(9) = ' '
 
!     Input parameters of unit cell
        MESSAGE(10) = 'Size of A star axis in reciprocal space.'
        Call IO_INPR (.True., 0.0, 100.0, .True., 'A* (INVERSE ANGSTROMS)', &
          10, MESSAGE, 1, 'Value must be within given range', a_star, status)
 
        MESSAGE(10) = 'Size of B star axis in reciprocal space.'
        Call IO_INPR (.True., 0.0, 100.0, .True., 'B* (INVERSE ANGSTROMS)', &
          10, MESSAGE, 1, 'Value must be within given range', b_star, status)
 
        MESSAGE(10) = 'Size of C star axis in reciprocal space.'
        Call IO_INPR (.True., 0.0, 1.0, .True., 'C* (INVERSE ANGSTROMS)', 10, &
          MESSAGE, 1, 'Value must be within given range', c_star, status)
 
!     Theta angles
        MESSAGE(10) = 'Angle from Z-axis (fibre) to A*'
        Call IO_INPR (.True., 0.0, 180.0, .True., 'THETA A* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', a_theta, status)
 
        MESSAGE(10) = 'Angle from Z-axis (fibre) to B*'
        Call IO_INPR (.True., 0.0, 180.0, .True., 'THETA B* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', b_theta, status)
 
        MESSAGE(10) = 'Angle from Z-axis (fibre) to C*'
        Call IO_INPR (.True., 0.0, 180.0, .True., 'THETA C* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', c_theta, status)
 
!     Phi angles
        MESSAGE(10) = 'Angle from X-axis (beam) to A*'
        Call IO_INPR (.True., -360.0, 360.0, .True., 'PHI A* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', a_phi, status)
 
        MESSAGE(10) = 'Angle from X-axis (beam) to B*'
        Call IO_INPR (.True., -360.0, 360.0, .True., 'PHI B* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', b_phi, status)
 
        MESSAGE(10) = 'Angle from X-axis (beam) to C*'
        Call IO_INPR (.True., -360.0, 360.0, .True., 'PHI C* (degrees)', 10, &
          MESSAGE, 1, 'Value must be within given range', c_phi, status)
 
!     Calculate more standard reciprocal unit cell parameters from angles
        alpha_star = Acos( Cos(b_theta * pi / 180.0) * Cos(c_theta * pi / &
          180.0) + Sin(b_theta * pi / 180.0) * Sin(c_theta * pi / 180.0) * &
          Cos(b_phi * pi / 180.0 - c_phi * pi / 180.0))
        beta_star = Acos( Cos(a_theta * pi / 180.0) * Cos(c_theta * pi / &
          180.0) + Sin(a_theta * pi / 180.0) * Sin(c_theta * pi / 180.0) * &
          Cos(a_phi * pi / 180.0 - c_phi * pi / 180.0))
        gamma_star = Acos( Cos(a_theta * pi / 180.0) * Cos(b_theta * pi / &
          180.0) + Sin(a_theta * pi / 180.0) * Sin(b_theta * pi / 180.0) * &
          Cos(a_phi * pi / 180.0 - b_phi * pi / 180.0))
 
!     Output User information
        Write (MESSAGE(1), '(''INFO: alpha star = '', 1pe12.5, ' // &
          ''' degrees'')') alpha_star * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: beta star = '', 1pe12.5, ' // &
          ''' degrees'')') beta_star * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: gamma star = '', 1pe12.5, ' // &
          ''' degrees'')') gamma_star * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
 
!     Orders to calculate
        Call IO_INPI (.True., -1000, 1000, .True., 'MINIMUM h index', 1, &
          'Lowest index in range of h indices to calculate', 1, &
          'Value must be within given range', h_minimum, status)
        Call IO_INPI (.True., h_minimum, 1000, .True., 'MAXIMUM h index', 1, &
          'Highest index in range of h indices to calculate', 1, &
          'Value must be within given range', h_maximum, status)
        Call IO_INPI (.True., -1000, 1000, .True., 'MINIMUM k index', 1, &
          'Lowest index in range of k indices to calculate', 1, &
          'Value must be within given range', k_minimum, status)
        Call IO_INPI (.True., k_minimum, 1000, .True., 'MAXIMUM k index', 1, &
          'Highest index in range of k indices to calculate', 1, &
          'Value must be within given range', k_maximum, status)
        Call IO_INPI (.True., -1000, 1000, .True., 'MINIMUM l index', 1, &
          'Lowest index in range of l indices to calculate', 1, &
          'Value must be within given range', l_minimum, status)
        Call IO_INPI (.True., l_minimum, 1000, .True., 'MAXIMUM l index', 1, &
          'Highest index in range of l indices to calculate', 1, &
          'Value must be within given range', l_maximum, status)
 
!     Create dynamic arrays to contain peak coordinates
        max_coordinates = 2 * (h_maximum - h_minimum + 1) * (k_maximum - &
          k_minimum + 1) * (l_maximum - l_minimum + 1)

        Allocate (X_COORDINATES(max_coordinates * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_DIFPATTERN ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(max_coordinates * 2), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_DIFPATTERN ' // Version)
           Return
        End If

!     Set data display region
        Call GS_SET_DDDR (X_AXIS(xstrelm), Y_AXIS(ystrelm), X_AXIS(xendelm), &
          Y_AXIS(yendelm), status)
 
!     Calculate coordinates
        Call F2D_DIFFRACTION (experiment, a_star, a_theta * Pi &
          / 180.0, a_phi * Pi / 180.0, b_star, b_theta * Pi / 180.0, b_phi * &
          Pi / 180.0, c_star, c_theta * Pi / 180.0, c_phi * Pi / 180.0, &
          h_minimum, h_maximum, k_minimum, k_maximum, l_minimum, l_maximum, &
          max_coordinates, num_coordinates, num_coord2, X_COORDINATES, &
          Y_COORDINATES, status)
 
!     Set curve styles
        Call GS_SET_CURVESTYLE (1, .False., .True., .False., status)
        Call GS_SET_CURVESTYLE (2, .False., .True., .False., status)
 
!     Set number of coordiantes in curves
        START_COORDINATES(1) = 1
        START_COORDINATES(2) = 1
        END_COORDINATES(1) = num_coordinates
        END_COORDINATES(2) = num_coord2
 
!     Display coordinates
        If (num_coordinates .Gt. 0) Then
 
           Call IO_INPL (.True., 0, 1, .True., 'OVERLAY PLOT', 1, &
             'YES for overlay, NO for basic X/Y plot', 1, &
             'Value must be within given range', overlay, status)
 
           If (.Not. overlay) Then
 
              Call GS_XYGRAPH (max_coordinates, 2, 2, START_COORDINATES, &
                END_COORDINATES, X_COORDINATES, Y_COORDINATES, &
                'Predicted Diffraction Pattern', 'X-film', 'Y-film', status)
           Else
 
!           Produce combined plot of active data region and overlay
!           diffraction pattern
 
!           Draw image plot of data
              Call GS_2DIMAGE (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                zlabel, status)
 
!           Draw overlay
              Call GS_XYCURVES (max_coordinates, 2, 2, START_COORDINATES, &
                END_COORDINATES, X_COORDINATES, Y_COORDINATES, &
                X_COORDINATES, Y_COORDINATES, &
                X_COORDINATES, Y_COORDINATES, status)
 
!           Update workstation
              Call GS_UPDATE (status)
 
           End If
 
!        Choice of hardcopy output
           Call IO_INPL (.True., 0, 1, .True., 'HARDCOPY OUTPUT', 1, &
             'YES for saving to file, NO for no output', 1, &
             'Answer "YES" or "NO"', hardcopy, status)
 
           If (hardcopy) Then
 
!           Deactive terminal workstation and activate hardcopy workstation
              Call GS_ON_PRINT (.False., file_open, status)
 
              If (file_open) Then
 
                 If (.Not. overlay) Then
 
                    Call GS_XYGRAPH (max_coordinates, 2, 2, START_COORDINATES, &
                      END_COORDINATES, X_COORDINATES, &
                      Y_COORDINATES, 'Predicted Diffraction Pattern', &
                      'X-film', 'Y-film', status)
                 Else
 
!                 Produce combined plot of active data region and
!                 overlay diffraction pattern
 
!                 Draw image plot of data
                    Call GS_2DIMAGE (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, &
                      xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                      ylabel, zlabel, status)
 
!                 Draw overlay
                    Call GS_XYCURVES (max_coordinates, 2, 2, &
                      START_COORDINATES, END_COORDINATES, &
                      X_COORDINATES, Y_COORDINATES, &
                      X_COORDINATES, Y_COORDINATES, &
                      X_COORDINATES, Y_COORDINATES, status)
 
                 End If
 
!              Deactive hardcopy workstation and activate terminal
!              workstation
                 Call GS_OFF_PRINT (status)
 
                 Call IO_WRITE ( 'INFO: Finished writing graphics file', &
                   status)
 
              End If
 
           End If
 
        End If
 
!     Free dynamic memory
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
 
     End If
 
     End Subroutine F2D_DIFPATTERN
!********1*********2*********3*********4*********5*********6*********7*********8
 

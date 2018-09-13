!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_reciprocal.f90 *
!  *                    *
!  **********************
 
!+ F2D_RECIPROCAL: calculates RECIPROCAL cell parameters
     Subroutine F2D_RECIPROCAL (status)
!  Description:
!    Calculates and displays reciprocal cell parameters
!  Keywords:
!    Unit~Cell.Parameters, Parameters.Unit~Cell
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    30-Aug-1993: V0.2 Allow conversion in both directions (Hammersley)
!    11-Jun-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Logical, Save :: real_to_reciprocal = .True. ! .True., if real cell
!      parameters are to be converted to reciprocal cell parameters, otherwise
!      reciprocal cell parameters are converted to real cell parameters
     Double Precision, Save :: a = 86.5d0 ! The length of the a-axis in
!      Angstroms
     Double Precision, Save :: a_star = 2.605d-2 ! The reciprocal space
!      length of the a-axis in inverse Angstroms
     Double Precision, Save :: alpha = 94.6d0 ! Unit cell inter-axes angle
!      (radians)
     Double Precision, Save :: alpha_star = 89.515d0 ! Reciprocal space
!      cell inter-axes angle (radians)
     Double Precision, Save :: b = 86.5d0 ! The length of the b-axis in
!      Angstroms
     Double Precision, Save :: b_star = 3.865d-2 ! The reciprocal space
!      length of the b-axis in inverse Angstroms
     Double Precision, Save :: beta = 88.05d0 ! Unit cell inter-axes angle
!      (radians)
     Double Precision, Save :: beta_star = 85.45d0 ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision, Save :: c = 2690.0d0 ! The length of the c-axis in
!      Angstroms
     Double Precision, Save :: c_star = 1.495d-3 ! The reciprocal space
!      length of the a-axis in inverse Angstroms
     Double Precision, Save :: gamma = 120.0d0 ! Unit cell inter-axes angle
!      (radians)
     Double Precision, Save :: gamma_star = 74.5d0 ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision :: volume ! Volume of unit cell in cubic Angstroms
     Double Precision :: volume_star ! Volume of reciprocal space unit cell
!      in cubic Inverse Angstroms
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(3) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RECIPROCAL ' // Version)
     Else
 
!     Real to reciprocal or reciprocal to real
        MESSAGE(1) = '"YES" to convert from real unit cell ' // &
          'parameters to reciprocal space'
        MESSAGE(2) = 'cell parameters, "NO" to convert from ' // &
          'reciprocal space cell paramters'
        MESSAGE(3) = 'to real space unit cell parameters'
        Call IO_INPL (.True., 0, 1, .True., 'REAL TO RECIPROCAL', 3, MESSAGE, &
          1, 'Enter "YES" or "NO"', real_to_reciprocal, status)
 
        If (real_to_reciprocal) Then
 
!        Input parameters of real unit cell
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'A axis length (ANGSTROMS)', 1, 'Length of A axis of unit cell', &
             1, 'Value must be within given range', a, status)
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'B axis length (ANGSTROMS)', 1, 'Length of B axis of unit cell', &
             1, 'Value must be within given range', b, status)
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'C axis length (ANGSTROMS)', 1, 'Length of C axis of unit cell', &
             1, 'Value must be within given range', c, status)
 
!        Unit cell inter-axial angles
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., 'ALPHA (degrees)', 1, &
             'Alpha Angle', 1, 'Value must be within given range', alpha, &
             status)
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., 'BETA (degrees)', 1, &
             'Beta Angle', 1, 'Value must be within given range', beta, &
             status)
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., 'GAMMA (degrees)', 1, &
             'Gamma Angle', 1, 'Value must be within given range', gamma, &
             status)
 
!        Convert angles to radians
           alpha = alpha * Pi_d / 180.0d0
           beta = beta * Pi_d / 180.0d0
           gamma = gamma * Pi_d / 180.0d0
 
!        Convert unit cell parameters to reciprocal cell parameters
           Call F2D_UNIT2RECIPROCAL ( a, b, c, alpha, beta, gamma, volume, &
             a_star, b_star, c_star, alpha_star, beta_star, gamma_star, &
             status)
 
!        Convert angles to degrees
           alpha = alpha * 180.0d0 / Pi_d
           beta = beta * 180.0d0 / Pi_d
           gamma = gamma * 180.0d0 / Pi_d
           alpha_star = alpha_star * 180.0d0 / Pi_d
           beta_star = beta_star * 180.0d0 / Pi_d
           gamma_star = gamma_star * 180.0d0 / Pi_d
 
!        Output User information
           Write (MESSAGE(1), '(''INFO: Unit cell volume = '',' // &
             '1pe12.5, '' cubic Angstroms'')') volume
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: a star = '', 1pe12.5,' // &
             ''' Inverse Angstroms'')') a_star
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: b star = '', 1pe12.5,' // &
             ''' Inverse Angstroms'')') b_star
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: c star = '', 1pe12.5,' // &
             ''' Inverse Angstroms'')') c_star
           Call IO_WRITE (MESSAGE(1), status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
           Write (MESSAGE(1), '(''INFO: alpha star = '',' // &
             '1pe12.5, '' degrees'')') alpha_star
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: beta star = '',' // &
             '1pe12.5, '' degrees'')') beta_star
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: gamma star = '',' // &
             '1pe12.5, '' degrees'')') gamma_star
           Call IO_WRITE (MESSAGE(1), status)
 
        Else
 
!        Input parameters of reciprocal unit cell
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'A star axis length (Inverse ANGSTROMS)', 1, &
             'Length of A star axis of reciprocal unit cell', 1, &
             'Value must be within given range', a_star, status)
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'B star axis length (Inverse ANGSTROMS)', 1, &
             'Length of B star axis of reciprocal unit cell', 1, &
             'Value must be within given range', b_star, status)
           Call IO_INPD (.True., 0.0d0, 1000000.0d0, .True., &
             'C star axis length (Inverse ANGSTROMS)', 1, &
             'Length of C star axis of reciprocal unit cell', 1, &
             'Value must be within given range', c_star, status)
 
!        Reciprocal cell inter-axial angles
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., &
             'ALPHA STAR (degrees)', 1, 'Alpha Star Angle', 1, &
             'Value must be within given range', alpha_star, status)
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., &
             'BETA STAR (degrees)', 1, 'Beta Star Angle', 1, &
             'Value must be within given range', beta_star, status)
           Call IO_INPD (.True., 0.0d0, 180.0d0, .True., &
             'GAMMA STAR (degrees)', 1, 'Gamma Star Angle', 1, &
             'Value must be within given range', gamma_star, status)
 
!        Convert angles to radians
           alpha_star = alpha_star * Pi_d / 180.0d0
           beta_star = beta_star * Pi_d / 180.0d0
           gamma_star = gamma_star * Pi_d / 180.0d0
 
!        Convert reciprocal cell parameters to unit cell parameters
           Call F2D_RECIP2UNIT (a_star, b_star, c_star, alpha_star, beta_star, &
             gamma_star, volume_star, a, b, c, alpha, beta, gamma, status)
 
!        Convert angles to degrees
           alpha = alpha * 180.0d0 / Pi_d
           beta = beta * 180.0d0 / Pi_d
           gamma = gamma * 180.0d0 / Pi_d
           alpha_star = alpha_star * 180.0d0 / Pi_d
           beta_star = beta_star * 180.0d0 / Pi_d
           gamma_star = gamma_star * 180.0d0 / Pi_d
 
!        Output results
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
           Write (MESSAGE(1), '(''INFO: Reciprocal cell ' // &
             'volume = '', 1pe12.5, '' cubic Inverse Angstroms'')') &
             volume_star
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: a = '', 1pe12.5, '' Angstroms'')') a
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: b = '', 1pe12.5, '' Angstroms'')') b
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: c = '', 1pe12.5, '' Angstroms'')') c
           Call IO_WRITE (MESSAGE(1), status)
 
           Write (MESSAGE(1), '(''INFO: alpha = '', 1pe12.5,' // &
             ''' degrees'')') alpha
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: beta = '', 1pe12.5,' // &
             ''' degrees'')') beta
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: gamma = '', 1pe12.5,' // &
             ''' degrees'')') gamma
           Call IO_WRITE (MESSAGE(1), status)
 
        End If
 
     End If
 
     End Subroutine F2D_RECIPROCAL
!********1*********2*********3*********4*********5*********6*********7**

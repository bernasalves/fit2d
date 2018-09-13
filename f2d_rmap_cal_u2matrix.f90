!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************************
!  *                           *
!  * f2d_rmap_cal_u2matrix.f90 *
!  *                           *
!  *****************************
 
!+ F2D_RMAP_CAL_U2MATRIX: Reciprocal MAP CALculate U MATRIX
     Subroutine F2D_RMAP_CAL_U2MATRIX (B_MATRIX, h_1, k_1, l_1, &
       two_theta_1, phi_1, chi_1, omega_1, h_2, k_2, l_2, two_theta_2, phi_2, &
       chi_2, omega_2, experiment, status)
!  Description:
!    Calculate U matrix elements from two reflections given B matrix
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Apr-2006: V0.3 Convert angles to double precision (Hammersley)
!    10-Apr-2006: V0.2 Output diagnostics (Hammersley)
!    06-Apr-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3) ! B matrix of crystal orientation
!      (Angstroms)
     Integer, Intent(IN) :: h_1 ! h index of primary reflection
     Integer, Intent(IN) :: k_1 ! k index of primary reflection
     Integer, Intent(IN) :: l_1 ! l index of primary reflection
     Double Precision, Intent(IN) :: two_theta_1 ! Two theta angle of primary 
!      reflection
     Double Precision, Intent(IN) :: phi_1 ! Diffractometer phi angle for 1st 
!      reflection
     Double Precision, Intent(IN) :: chi_1 ! Diffractometer chi angle for 1st 
!      reflection
     Double Precision, Intent(IN) :: omega_1 ! Diffractometer omega angle for 
!      1st reflection
     Integer, Intent(IN) :: h_2 ! h index of secondary reflection
     Integer, Intent(IN) :: k_2 ! k index of secondary reflection
     Integer, Intent(IN) :: l_2 ! l index of secondary reflection
     Double Precision, Intent(IN) :: two_theta_2 ! Two theta angle of secondary 
!      reflection
     Double Precision, Intent(IN) :: phi_2 ! Diffractometer phi angle for 2nd 
!      reflection
     Double Precision, Intent(IN) :: chi_2 ! Diffractometer chi angle for 2nd 
!      reflection
     Double Precision, Intent(IN) :: omega_2 ! Diffractometer omega angle for 
!      2nd reflection
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
!  Local Arrays:
     Real U1P(3) ! Unit vector in the phi-system for primary reflection
     Real U2P(3) ! Unit vector in the phi-system for secondary reflection
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_U2MATRIX ' // Version)
     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate unit vector in phi-axis system for primary reflection
        Call F2D_RMAP_CAL_U_PHI (experiment%wavelength, B_MATRIX, &
          h_1, k_1, l_1, two_theta_1, phi_1, chi_1, omega_1, U1P, status)

!     Calculate unit vector in phi-axis system for secondary reflection
        Call F2D_RMAP_CAL_U_PHI (experiment%wavelength, B_MATRIX, &
          h_2, k_2, l_2, two_theta_2, phi_2, chi_2, omega_2,  U2P, status)

!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output diagnostics to the user
!        Call IO_WRITE ( 'U1P:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (U1P(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE ( 'U2P:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (U2P(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE (' ', status)
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate UB matrix from two scattering vectors and reflection indices
        Call F2D_RMAP_CAL_U3MATRIX (B_MATRIX, h_1, k_1, l_1, U1P, &
          h_2, k_2, l_2, U2P, experiment, status)

     End If

     End Subroutine F2D_RMAP_CAL_U2MATRIX
!********1*********2*********3*********4*********5*********6*********7*********8
!+ F2D_RMAP_CAL_U_PHI: Reciprocal MAP CALculate U PHI
     Subroutine F2D_RMAP_CAL_U_PHI (wavelength, B_MATRIX, h, k, l, &
       two_theta_r, phi_r, chi_r, omega_r, U_PHI, status)
!  Description:
!    Calculate U_phi for a given set of diffractometer angles
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Apr-2006: V0.2 Angles input in radians (Hammersley)
!    10-Apr-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: wavelength ! Radiation wavelength in metres
     Real, Intent(IN) :: B_MATRIX (3, 3) ! B matrix of crystal orientation
!      (Angstroms)
     Integer, Intent(IN) :: h ! h index of reflection
     Integer, Intent(IN) :: k ! k index of reflection
     Integer, Intent(IN) :: l ! l index of reflection
     Double Precision, Intent(IN) :: two_theta_r ! Detector two-theta angle for 
!      reflection
     Double Precision, Intent(IN) :: phi_r ! Phi angle for reflection
     Double Precision, Intent(IN) :: chi_r ! Chi angle for reflection
     Double Precision, Intent(IN) :: omega_r ! Omega angle for reflection
!  Import/Export:
     Real U_PHI(3) ! Unit vector in the phi-system
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Real d_spacing ! D-spacing on reflection
     Real chi ! Chi angle (radians)
     Real omega ! Omega angle (radians)
     Real phi ! Phi angle (radians)
     Real sign_tth ! Sign of two-theta angle
     Real theta ! Theta angle of reflection (radians)
!  Local Arrays:
     Real HA(3) ! Index vector
     Real HB(3) ! Cartesian vector
!  External Functions:
!  Local Data:
!  Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_U2MATRIX ' // Version)
     Else

        HA(1) = Real(h)
        HA(2) = Real(k)
        HA(3) = Real(l)

!     Calculate Bragg angle
        Call F2D_GMPRD (B_MATRIX, HA, HB, 3, 3, 1)
        d_spacing = Sqrt(HB(1) * HB(1) + HB(2) * HB(2) + HB(3) * HB(3))
        theta = Asin(d_spacing * (wavelength * 1.0e10) / 2.0)

!     Calculate sign of two-theta angle
        If (two_theta_r .Ge. 0.0) Then
           sign_tth = 1.0
        Else
           sign_tth = -1.0
        End If

!     Calculate beam-line "omega", "chi", "phi"
        omega = sign_tth * omega_r - theta
        chi = sign_tth * chi_r
        phi = phi_r + (sign_tth - 1.0) * (Pi / 2.0)
 
        U_PHI(1) = Cos(omega) * Cos(chi) * Cos(phi) - Sin(omega) * Sin(phi)
        U_PHI(2) = Cos(omega) * Cos(chi) * Sin(phi) + Sin(omega) * Cos(phi)
        U_PHI(3) = Cos(omega) * Sin(chi)
 
     End If

     End Subroutine F2D_RMAP_CAL_U_PHI
!********1*********2*********3*********4*********5*********6*********7*********8

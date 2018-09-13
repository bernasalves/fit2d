!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************************
!  *                           *
!  * f2d_rmap_cal_ubmatrix.f90 *
!  *                           *
!  *****************************
 
!+ F2D_RMAP_CAL_UBMATRIX: Reciprocal MAP input UB MATRIX
     Subroutine F2D_RMAP_CAL_UBMATRIX (experiment, status)
!  Description:
!    Calculate UB matrix elements from input of unit cell and diffractometer
!    angles of two reflections.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Jun-2006: V0.5 Changes to "F2D_GUI_REFLECTIONS" (Hammersley)
!    19-Apr-2006: V0.4 Changes to "F2D_GUI_REFLECTIONS" (Hammersley)
!    30-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    21-Jul-2005: V0.2 Changes to call to "F2D_RMAP_CAL_UMATRIX" (Hammersley)
!    09-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: h1 = 1 ! h index of primary reflection
     Integer :: h2 = 2 ! h index of secondary reflection
     Integer :: k1 = 1 ! k index of primary reflection
     Integer :: k2 = 2 ! k index of secondary reflection
     Integer :: l1 = 1 ! l index of primary reflection
     Integer :: l2 = 1 ! l index of secondary reflection
     Double Precision :: chi_1 = -88.8867 * Pi_d / 180.0 ! Chi angle (radians) 
!      of diffractometer for primary reflection
     Double Precision :: chi_2 = -88.9536 * Pi_d / 180.0 ! Chi angle (radians) 
!      of diffractometer for secondary reflection
     Double Precision :: omega_1 = 24.3999 * Pi_d / 180.0 ! Omega angle 
!      (radians) of diffractometer for primary reflection
     Double Precision :: omega_2 = 30.4021 * Pi_d / 180.0 ! Omega angle 
!      (radians) of diffractometer for secondary reflection
     Double Precision :: phi_1 = 0.0 ! Phi angle (radians) of diffractometer for
!      primary reflection
     Double Precision :: phi_2 = 0.0 ! Phi angle (radians) of diffractometer for
!      secondary  reflection
     Double Precision :: two_theta_1 = 49.7484 * Pi_d / 180.0 ! Two theta angle 
!      (radians) of primary refection
     Double Precision :: two_theta_2 = 93.5294 * Pi_d / 180.0 ! Two theta angle 
!      (radians) of secondary refection
!  Local Arrays:
     Real :: B_MATRIX (3, 3) ! B matrix of crystal unit cell
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_UBMATRIX ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Input unit cell parameters
        Call F2D_GUI_UNITCELL (experiment, status)
 
!     Calculate B matrix from unit cell parameters
        Call F2D_RMAP_CAL_BMATRIX (experiment, B_MATRIX, status)
 
!     Input diffractometer angles of two reflections
        Call F2D_GUI_REFLECTION (.False., 1, 'ENTER PRIMARY REFLECTION', &
          h1, k1, l1, experiment, status)
        two_theta_1 = experiment%two_theta
        phi_1 = experiment%phi_start
        chi_1 = experiment%chi_start
        omega_1 = experiment%omega_start
        
        Call F2D_GUI_REFLECTION (.False., 2, 'ENTER SECONDARY REFLECTION', &
          h2, k2, l2, experiment, status)
        two_theta_2 = experiment%two_theta
        phi_2 = experiment%phi_start
        chi_2 = experiment%chi_start
        omega_2 = experiment%omega_start
 
!     Calculate U matrix from reflections
!        Call F2D_RMAP_CAL_UMATRIX (B_MATRIX, h1, k1, l1, &
!          two_theta_1, phi_1, chi_1, omega_1, h2, k2, l2, two_theta_2, phi_2, &
!          chi_2, omega_2, experiment, status)

!     Experimental version
        Call F2D_RMAP_CAL_U2MATRIX (B_MATRIX, h1, k1, l1, &
          two_theta_1, phi_1, chi_1, omega_1, h2, k2, l2, two_theta_2, phi_2, &
          chi_2, omega_2, experiment, status)

     End If
 
     End Subroutine F2D_RMAP_CAL_UBMATRIX
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

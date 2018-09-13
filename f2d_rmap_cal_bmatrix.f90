!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_rmap_cal_bmatrix.f90 *
!  *                          *
!  ****************************
 
!+ F2D_RMAP_CAL_BMATRIX: Reciprocal MAP  CALculate B MATRIX from unit cell
     Subroutine F2D_RMAP_CAL_BMATRIX (experiment, B_MATRIX, status)
!  Description:
!    Calculate B matrix elements from unit cell parameters
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    30-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Aug-2005: V0.2 Convert to double precision (Hammersley)
!    09-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: B_MATRIX (3, 3) ! B matrix of crystal orientation
!      (Angstroms)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
     Double Precision :: a_star ! a* vector length of reciprocal space cell
     Double Precision :: alpha_radians ! alpha angle in radians
     Double Precision :: b_star ! b* vector length of reciprocal space cell
     Double Precision :: beta_radians ! beta angle in radians
     Double Precision :: beta_star ! beta* angle in radians of reciprocal cell
     Double Precision :: c_star ! c* vector length of reciprocal space cell
     Double Precision :: gamma_radians ! gamma angle in radians
     Double Precision :: gamma_star ! gamma* angle in radians of reciprocal cell
     Double Precision :: s ! Temporary sum
     Double Precision :: volume ! Volume of unit cell
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_BMATRIX ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        alpha_radians = experiment%cell_alpha * Pi_d / 180.0
        beta_radians = experiment%cell_beta  * Pi_d / 180.0
        gamma_radians = experiment%cell_gamma  * Pi_d / 180.0
        s = 0.5 * (alpha_radians + beta_radians + gamma_radians)
        volume = Sin(s) * Sin(s - alpha_radians) * Sin(s - beta_radians) * &
          sin(s - gamma_radians)
        volume = 2.0 * experiment%cell_length_a * experiment%cell_length_b * &
          experiment%cell_length_c * Sqrt(volume)
        beta_star = (Cos(alpha_radians) * Cos(gamma_radians) - &
          Cos(beta_radians))
        beta_star = Acos(beta_star / (Sin(gamma_radians) * &
          Sin(alpha_radians)))
        gamma_star = (Cos(alpha_radians) * Cos(beta_radians) - &
          Cos(gamma_radians))
        gamma_star = Acos(gamma_star / (Sin(beta_radians) * &
          Sin(alpha_radians)))
        a_star = experiment%cell_length_b * experiment%cell_length_c * &
          Sin(alpha_radians) / volume
        b_star = experiment%cell_length_a * experiment%cell_length_c * &
          Sin(beta_radians) / volume
        c_star = experiment%cell_length_a * experiment%cell_length_b * &
          Sin(gamma_radians) / volume
 
        B_MATRIX(1, 1) = a_star
        B_MATRIX(1, 2) = b_star * Cos(gamma_star)
        B_MATRIX(1, 3) = c_star * Cos(beta_star)
        B_MATRIX(2, 1) = 0.0
        B_MATRIX(2, 2) = b_star * Sin(gamma_star)
        B_MATRIX(2, 3) = -c_star * Sin(beta_star) * Cos(alpha_radians)
        B_MATRIX(3, 1) = 0.0
        B_MATRIX(3, 2) = 0.0
        B_MATRIX(3, 3) = 1.0 / experiment%cell_length_c
 
!     Output result to user
        Call IO_WRITE ('INFO: unit cell B matrix:', status)
        Do i = 1, 3
           Write (message, '(''     '', 3(g12.5, 1x))') &
             (B_MATRIX(i, j), j = 1, 3)
           Call IO_WRITE (message, status)
        End Do
        Call IO_WRITE (' ', status)
 
     End If
 
     End Subroutine F2D_RMAP_CAL_BMATRIX
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

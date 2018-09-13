!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_recip2unit.f90 *
!  *                    *
!  **********************
 
!+ F2D_RECIP2UNIT: converts RECIProcal space cell parameters to real space UNIT
!  cell parameters
     Subroutine F2D_RECIP2UNIT (a_star, b_star, c_star, alpha_star, beta_star, &
       gamma_star, volume_star, a, b, c, alpha, beta, gamma, status)
!  Description:
!    Converts standard reciprocal space cell parameters to real space unit cell 
!    parameters.
!  Keywords:
!    Unit~Cell.Parameters, Parameters.Unit~Cell
!  Method:
!    See IUCr tables Vol 3 (?) page 106
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Aug-1993: V0.1 Original, from "F2D_UNIT2RECIPROCAL" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: a_star ! The reciprocal space length of
!      the a-axis in inverse Angstroms
     Double Precision, Intent(IN) :: b_star ! The reciprocal space length of
!      the b-axis in inverse Angstroms
     Double Precision, Intent(IN) :: c_star ! The reciprocal space length of
!      the a-axis in inverse Angstroms
     Double Precision, Intent(IN) :: alpha_star ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision, Intent(IN) :: beta_star ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision, Intent(IN) :: gamma_star ! Reciprocal space cell
!      inter-axes angle (radians)
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: volume_star ! The volume of the
!      reciprocal space unit cell
     Double Precision, Intent(OUT) :: a ! The length of the a-axis in Angstroms
     Double Precision, Intent(OUT) :: b ! The length of the b-axis in Angstroms
     Double Precision, Intent(OUT) :: c ! The length of the c-axis in Angstroms
     Double Precision, Intent(OUT) :: alpha ! Unit cell inter-axes angle 
!      (radians)
     Double Precision, Intent(OUT) :: beta ! Unit cell inter-axes angle
!      (radians)
     Double Precision, Intent(OUT) :: gamma ! Unit cell inter-axes angle
!      (radians)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RECIP2UNIT ' // Version)
     Else
 
!     Calculate volume
        volume_star = a_star * b_star * c_star * Sqrt(1.0d0 - &
          Cos(alpha_star)**2 - Cos(beta_star)**2 - Cos(gamma_star)**2 + 2.0d0 &
          * Cos(alpha_star) * Cos(beta_star) * Cos(gamma_star))
 
!     Calculate real space unit cell dimensions
        a = (b_star * c_star * Sin(alpha_star)) / volume_star
        b = (c_star * a_star * Sin(beta_star)) / volume_star
        c = (a_star * b_star * Sin(gamma_star)) / volume_star
 
!     Calculate real space unit cell inter-axes angles
        alpha = Acos ((Cos(beta_star) * Cos(gamma_star) - Cos(alpha_star)) / &
          (Sin(beta_star) * Sin(gamma_star)) )
        beta = Acos ((Cos(gamma_star) * Cos(alpha_star) - Cos(beta_star)) / &
          (Sin(gamma_star) * Sin(alpha_star)) )
        gamma = Acos ((Cos(alpha_star) * Cos(beta_star) - Cos(gamma_star)) / &
          (Sin(alpha_star) * Sin(beta_star)) )
 
     End If
 
     End Subroutine F2D_RECIP2UNIT
!********1*********2*********3*********4*********5*********6*********7*********8

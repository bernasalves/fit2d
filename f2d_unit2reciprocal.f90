!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_unit2reciprocal.f90 *
!  *                         *
!  ***************************
 
!+ F2D_UNIT2RECIPROCAL: converts UNIT cell parameters to RECIPROCAL space 
!  parameters
     Subroutine F2D_UNIT2RECIPROCAL (a, b, c, alpha, beta, gamma, volume, &
       a_star, b_star, c_star, alpha_star, beta_star, gamma_star, status)
!  Description:
!    Converts standard real space cell parameters to reciprocal space cell 
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
!    11-Jun-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Double Precision, Intent(IN) :: a ! The length of the a-axis in Angstroms
     Double Precision, Intent(IN) :: b ! The length of the b-axis in Angstroms
     Double Precision, Intent(IN) :: c ! The length of the c-axis in Angstroms
     Double Precision, Intent(IN) :: alpha ! Unit cell inter-axes angle 
!      (radians)
     Double Precision, Intent(IN) :: beta ! Unit cell inter-axes angle
!      (radians)
     Double Precision, Intent(IN) :: gamma ! Unit cell inter-axes angle
!      (radians)
!  Import/Export:
!  Export:
     Double Precision, Intent(OUT) :: volume ! Volume of unit cell in cubic
!      Angstroms
     Double Precision, Intent(OUT) :: a_star ! The reciprocal space length of 
!      the a-axis in inverse Angstroms
     Double Precision, Intent(OUT) :: b_star ! The reciprocal space length of 
!      the b-axis in inverse Angstroms
     Double Precision, Intent(OUT) :: c_star ! The reciprocal space length of
!      the a-axis in inverse Angstroms
     Double Precision, Intent(OUT) :: alpha_star ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision, Intent(OUT) :: beta_star ! Reciprocal space cell
!      inter-axes angle (radians)
     Double Precision, Intent(OUT) :: gamma_star ! Reciprocal space cell
!      inter-axes angle (radians)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Double Precision :: s ! Intermediate calculation variable
!    ("2s" = "alpha" + "beta" + "gamma")
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_UNIT2RECIPROCAL ' // Version)
     Else
 
!     Calculate volume
        s = (alpha + beta + gamma) / 2.0d0
        volume = 2.0d0 * a * b * c * &
          Sqrt( Sin(s) * Sin(s - alpha) * Sin(s - beta) * Sin(s - gamma))
 
!     Calculate reciprocal space cell dimensions
        a_star = (b * c * Sin(alpha)) / volume
        b_star = (c * a * Sin(beta)) / volume
        c_star = (a * b * Sin(gamma)) / volume
 
!     Calculate reciprocal space cell inter-axes angles
        alpha_star = Acos ((Cos(beta) * Cos(gamma) - Cos(alpha)) / (Sin(beta) &
          * Sin(gamma)) )
        beta_star = Acos ((Cos(gamma) * Cos(alpha) - Cos(beta)) / (Sin(gamma) &
          * Sin(alpha)) )
        gamma_star = Acos ((Cos(alpha) * Cos(beta) - Cos(gamma)) / (Sin(alpha) &
          * Sin(beta)) )
 
     End If
 
     End Subroutine F2D_UNIT2RECIPROCAL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

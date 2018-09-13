!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_draw_lattice.f90 *
!  *                      *
!  ************************
 
!+ F2D_DRAW_LATTICE - FIT 2-D DRAW LATTICE
     Subroutine F2D_DRAW_LATTICE (experiment, status)
!  Description:
!    Draws reciprocal space lattice
!  Keywords:
!    Draw.Lattice, Lattice.Draw
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    02-May-2008: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! GS constants
     Include 'io.inc' ! Data structure definitions
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Double Precision :: a_star ! Reciprocal space a lattice spacing
     Double Precision :: alpha_star ! Reciprocal space alpha angle
     Double Precision :: b_star ! Reciprocal space b lattice spacing
     Double Precision :: beta_star ! Reciprocal space beta angle
     Double Precision :: c_star ! Reciprocal space c lattice spacing
     Double Precision :: gamma_star ! Reciprocal space gamma angle
     Double Precision :: volume ! Unit cell volume 
     Integer h ! Loop index for h direction
     Integer k ! Loop index for k direction
     Integer l ! Loop index for l direction
     Real :: x1 ! First X-coordinate of lattice line 
     Real :: x2 ! Second X-coordinate of lattice line 
     Real :: y1 ! First Y-coordinate of lattice line 
     Real :: y2 ! Second Y-coordinate of lattice line 
     Real :: z1 ! First Z-coordinate of lattice line 
     Real :: z2 ! Second Z-coordinate of lattice line 
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_LATTICE ' // Version)
        Return
 
     Else
 
!     Calculate reciprocal cell
        Call F2D_UNIT2RECIPROCAL (Dble(experiment%cell_length_a), &
          Dble(experiment%cell_length_b), Dble(experiment%cell_length_c), &
          Dble(experiment%cell_alpha), Dble(experiment%cell_beta), &
          Dble(experiment%cell_gamma), volume, a_star, b_star, c_star, &
          alpha_star, beta_star, gamma_star, status)

!     Draw lattice lines
        Do l = -5, 5

           Do k = -5, 5

!           Calculate positions at end of lines
              x1 = -5 * a_star + Cos(beta_star) * b_star * Real(k) &
                + Cos(gamma_star) * c_star * Real(l)
              y1 = - sin(beta_star) * b_star * Real(k) &
                + Cos(gamma_star) * c_star * Real(l)
              z1 = Cos(beta_star) * b_star * Real(k) - &
                Sin(gamma_star) * c_star * Real(l)
              x2 = 5 * a_star + Cos(beta_star) * b_star * Real(k) &
                + Cos(gamma_star) * c_star * Real(l)
              y2 = y1
              z2 = z1

!           Draw line
              Call GS_3LINE (x1, y1, z1, x2, y2, z2, status)

           End Do

        End Do

     End If
 
     End Subroutine F2D_DRAW_LATTICE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

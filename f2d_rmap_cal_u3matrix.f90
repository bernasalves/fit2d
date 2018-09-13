!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************************
!  *                           *
!  * f2d_rmap_cal_u3matrix.f90 *
!  *                           *
!  *****************************
 
!+ F2D_RMAP_CAL_U3MATRIX: Reciprocal MAP CALculate U MATRIX
     Subroutine F2D_RMAP_CAL_U3MATRIX (B_MATRIX, h_1, k_1, l_1, Z1, &
       h_2, k_2, l_2, Z2, experiment, status)
!  Description:
!    Calculate U matrix elements from two reflections cartesian coordinates
!    given B matrix
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Apr-2006: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: Z1(3) ! Scattering vector of primary reflection
     Integer, Intent(IN) :: h_2 ! h index of secondary reflection
     Integer, Intent(IN) :: k_2 ! k index of secondary reflection
     Integer, Intent(IN) :: l_2 ! l index of secondary reflection
     Real, Intent(IN) :: Z2(3) ! Scattering vector of secondary reflection
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
!  Local Arrays:
     Real H1(3) ! Primary reflection scattering vector
     Real H1C(3) ! Primary reflection scattering vector in cartesian system
     Real H2(3) ! Secondary reflection scattering vector
     Real H2C(3) ! Secondary reflection scattering vector in cartesian system
     Real H3C(3, 3) ! Orthogonal vectors in cartesian system
     Real H3CT(3, 3) ! Transpose of "H3C"
     Real U_MATRIX(3, 3) ! "U" orientation matrix
     Real Z3(3, 3) ! Orthogonal scattering vectors in rotated cartesian system
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_U3MATRIX ' // Version)
     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set-up scattering vector matrices
        H1(1) = Real(h_1)
        H1(2) = Real(k_1)
        H1(3) = Real(l_1)
        H2(1) = Real(h_2)
        H2(2) = Real(k_2)
        H2(3) = Real(l_2)

!     Calculate scattering vectors in the crystal cartesian system
        Call F2D_GMPRD (B_MATRIX, H1, H1C, 3, 3, 1)
        Call F2D_GMPRD (B_MATRIX, H2, H2C, 3, 3, 1)

!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output diagnostics to the user
!        Call IO_WRITE ( 'H1C:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (H1C(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE ( 'H2C:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (H2C(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE (' ', status)
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate a third vector orthogonal to "H1C" and "H2C"
        Call TR1P (H1C, H2C, H3C)
        Call F2D_GMTRA (H3C, H3CT, 3, 3)

!     Calculate a third vector orthogonal to "Z1" and "Z2"
        Call TR1P (Z1, Z2, Z3)

!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output diagnostics to the user
!        Call IO_WRITE ( 'Z3:', status)
!        Do i = 1, 3
!           Write (message, '(''     '', 3(g12.5, 1x))') (Z3(i, j), j = 1, 3)
!           Call IO_WRITE (message, status)
!        End Do
!        Call IO_WRITE (' ', status)
!        Call IO_WRITE ( 'H3CT:', status)
!        Do i = 1, 3
!           Write (message, '(''     '', 3(g12.5, 1x))') (H3CT(i, j), j = 1, 3)
!           Call IO_WRITE (message, status)
!        End Do
!        Call IO_WRITE (' ', status)
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate UB matrix, by multiplying phi system vectors with transpose of 
!     cartesian vectors
        Call F2D_GMPRD (Z3, H3CT, U_MATRIX, 3, 3, 3)
 
!     Calculate UB matrix
        Call F2D_GMPRD (U_MATRIX, B_MATRIX, experiment%UB_MATRIX, 3, 3, 3)
 
!     Output result to user
        Call IO_WRITE ('INFO: Diffractometer UB matrix (Busing and Levy):', &
          status)
        Do i = 1, 3
           Write (message, '(''     '', 3(g12.5, 1x))') &
             (experiment%UB_MATRIX(i, j), j = 1, 3)
           Call IO_WRITE (message, status)
        End Do
        Call IO_WRITE (' ', status)
 
     End If
 
     End Subroutine F2D_RMAP_CAL_U3MATRIX
!********1*********2*********3*********4*********5*********6*********7*********8

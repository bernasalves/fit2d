!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************************
!  *                          *
!  * f2d_rmap_cal_umatrix.f90 *
!  *                          *
!  ****************************
 
!+ F2D_RMAP_CAL_UMATRIX: Reciprocal MAP CALculate U MATRIX
     Subroutine F2D_RMAP_CAL_UMATRIX (B_MATRIX, h_1, k_1, l_1, &
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
!    10-Apr-2006: V0.5 Output diagnostics (Hammersley)
!    30-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    21-Jul-2005: V0.3 Try to use rafin code (Hammersley)
!    12-Jun-2005: V0.2 Continue implementation (Hammersley)
!    10-Jun-2005: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: two_theta_1 ! Two theta angle of primary reflection
     Real, Intent(IN) :: phi_1 ! Diffractometer phi angle for 1st reflection
     Real, Intent(IN) :: chi_1 ! Diffractometer chi angle for 1st reflection
     Real, Intent(IN) :: omega_1 ! Diffractometer omega angle for 1st reflection
     Integer, Intent(IN) :: h_2 ! h index of secondary reflection
     Integer, Intent(IN) :: k_2 ! k index of secondary reflection
     Integer, Intent(IN) :: l_2 ! l index of secondary reflection
     Real, Intent(IN) :: two_theta_2 ! Two theta angle of secondary reflection
     Real, Intent(IN) :: phi_2 ! Diffractometer phi angle for 2nd reflection
     Real, Intent(IN) :: chi_2 ! Diffractometer chi angle for 2nd reflection
     Real, Intent(IN) :: omega_2 ! Diffractometer omega angle for 2nd reflection
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
     Real :: cos_chi ! Cos of chi
     Real :: cos_phi ! Cos of phi
     Real :: cos_omega ! Cos of omega
     Real :: degrees ! Dummy variable for "Radians" function
     Real :: dotpr ! Dot product of vectors
     Real :: sin_chi ! Sin of chi
     Real :: sin_phi ! Sin of phi
     Real :: sin_omega ! Sin of omega
!  Local Arrays:
     Integer :: ISIGN(2) ! Sign of two-theta angles
     Real :: A4COBS(4, 2) ! Eular angles of reflections
     Real :: H(3, 2) ! HKL of orientation reflections
     Real :: VAB(3, 2)
!  External Functions:
!  Local Data:
!    Internal Functions:
     Real :: Radians ! Convert angles in degrees to radians
     Radians(degrees) = degrees * 3.1415926 / 180.0
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_UMATRIX ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set-up matrices
        H(1, 1) = h_1
        H(2, 1) = k_1
        H(3, 1) = l_1
        H(1, 2) = h_2
        H(2, 2) = k_2
        H(3, 2) = l_2
        A4COBS(1, 1) = Radians(two_theta_1)
        A4COBS(2, 1) = Radians(omega_1)
        A4COBS(3, 1) = Radians(chi_1)
        A4COBS(4, 1) = Radians(phi_1)
        A4COBS(1, 2) = Radians(two_theta_2)
        A4COBS(2, 2) = Radians(omega_2)
        A4COBS(3, 2) = Radians(chi_2)
        A4COBS(4, 2) = Radians(phi_2)
        ISIGN(1) = Sign(1.0, two_theta_1)
        ISIGN(2) = Sign(1.0, two_theta_2)
 
!     Calculate vectors [UB]H(i)
        Call F2D_RMAP_CAL_UBH (experiment%wavelength, B_MATRIX, H, A4COBS, &
          ISIGN, VAB)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''VAB = '', 6g12.5)') VAB
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate UB matrix
        Call F2D_RMAP_CAL_UBSUB (B_MATRIX, H, VAB, experiment%UB_MATRIX, status)
 
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
 
     End Subroutine F2D_RMAP_CAL_UMATRIX
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * F2D_RMAP_CAL_UBH *
!  *                  *
!  ********************
 
!+ F2D_RMAP_CAL_UBH:
     Subroutine F2D_RMAP_CAL_UBH (wavelength, B_MATRIX, H, A4COBS, ISIGN, VAB)
!  Description:
!    Calculate  2 vectors "VAB" from 2 reflections
!    (1.0 / wavelength) * [VAB(i)] = [UB] * [H(i)]
!    [VAB(i)] = t[PHI]t [CHI]t [OMEGA] [Hi]
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    21-Jul-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: wavelength ! Wavelength of radiation in metres
     Real, Intent(IN) :: B_MATRIX (3, 3) ! B matrix of crystal orientation
!      (Angstroms)
     Real, Intent(IN) :: H(3, 2) ! HKL of orientation reflections
     Real, Intent(IN) :: A4COBS(4, 2) ! Euler angles of reflections
     Integer, Intent(IN) :: ISIGN(2) ! Sign of 2-theta values
!  Export:
     Real, Intent(OUT) :: VAB(3, 2) ! Reflection vectors
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
     Integer :: k ! Loop variable
     Real :: hkld ! Distance to reflection
     Real :: omebl, chi, phi
     Real :: theta
!  Local Arrays:
     Real :: CS(3), SN(3), ANG(3)
     Real :: HA(3), HB(3)
!  External Functions:
!  Local Data:
!    Internal Functions:
     Equivalence (ANG(1), omebl), (ANG(2), chi), (ANG(3), phi)
!--------1---------2---------3---------4---------5---------6---------7---------8
 
     Do i = 1, 2
 
        j = i
        Do k = 1, 3
           HA(k) = H(k, j)
        End Do
 
!     Write (*, '(''i = '', i3, '' j = '', i3)') i, j
 
!     Calculate Bragg angle
        Call F2D_GMPRD (B_MATRIX, HA, HB, 3, 3, 1)
        hkld = Sqrt(HB(1) * HB(1) + HB(2) * HB(2) + HB(3) * HB(3))
        theta = Asin(hkld * (wavelength * 1.0e10) / 2.0)
 
!     Write (*, '(''hkld = '', g12.5, '' theta = '', g12.5)') hkld, theta
 
!     Calculate "omega"(B-L) "chi" "phi"
        omebl = ISIGN(j) * A4COBS(2, j) - theta
        chi = ISIGN(j) * A4COBS(3, j)
        phi = A4COBS(4, j) + (ISIGN(j) - 1) * (Pi / 2.0)
 
!     Write (*, '(''omebl = '', g12.5, '' chi = '', g12.5, ' // &
!       '' phi = '', g12.5)') omebl, chi, phi
 
        Do k = 1, 3
           CS(k)  = Cos(ANG(k))
           SN(k)  = Sin(ANG(k))
        End Do
 
        VAB(1, i) = CS(1) * CS(2) * CS(3) - SN(1) * SN(3)
        VAB(2, i) = CS(1) * CS(2) * SN(3) + SN(1) * CS(3)
        VAB(3, i) = CS(1) * SN(2)
 
     End Do
 
     End Subroutine F2D_RMAP_CAL_UBH
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * F2D_RMAP_CAL_UBSUB *
!  *                    *
!  **********************
 
!+ F2D_RMAP_CAL_UBSUB: CALculate UB matrix
     Subroutine F2D_RMAP_CAL_UBSUB (B_MATRIX, H12, H12O, UB_MATRIX, status)
!  Description:
!    Calculate UB matrix elements using Busing and Levy method
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    21-Jun-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Real, Intent(IN) :: B_MATRIX (3, 3) ! B matrix of crystal orientation
!      (Angstroms)
     Real, Intent(IN) :: H12(3, 2)
     Real, Intent(IN) :: H12O(3, 2)
!      H1C : Direction h, k, l in ref. reciprocal orthogonal to the crystal
!      H1O :     "       "     "  diffractometer reference frame
!      H1O = UB * H1C
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: UB_MATRIX (3, 3) ! UB matrix of crystal orientation 
!      (Angstroms)
!  Status:
    Integer status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
!  Local Arrays:
     Real :: H1(3), H2(3), H1C(3), H2C(3), H1O(3), H2O(3)
     Real :: TR1PC(3, 3), TTR1PC(3, 3), TR1PO(3, 3)
     Real :: U(3, 3)
!  External Functions:
!  Local Data:
!    Internal Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
     Do i = 1, 3
        H1(I) = H12(I, 1)
        H2(I) = H12(I, 2)
        H1O(I) = H12O(I, 1)
        H2O(I) = H12O(I, 2)
     End Do
 
!  Calculate "H1C" and "H2C"
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
 
!  Define a third orthogonal vector
     Call TR1P (H1C, H2C, TR1PC)
     Call F2D_GMTRA (TR1PC, TTR1PC, 3, 3)

!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output diagnostics to the user
!        Call IO_WRITE ( 'H1O:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (H1O(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE ( 'H2O:', status)
!        Write (message, '(''     '', 3(g12.5, 1x))') (H2O(i), i = 1, 3)
!        Call IO_WRITE (message, status)
!        Call IO_WRITE (' ', status)
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Define a vector orthogonal to H1O and H2O
     Call TR1P (H1O, H2O, TR1PO)

!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output diagnostics to the user
!        Call IO_WRITE ('TR1PO:', status)
!        Do i = 1, 3
!           Write (message, '(''     '', 3(g12.5, 1x))') (TR1PO(i, j), j = 1, 3)
!           Call IO_WRITE (message, status)
!        End Do
!        Call IO_WRITE (' ', status)
!        Call IO_WRITE ( 'TTR1PC:', status)
!        Do i = 1, 3
!           Write (message, '(''     '', 3(g12.5, 1x))') (TTR1PC(i, j),j = 1, 3)
!           Call IO_WRITE (message, status)
!        End Do
!        Call IO_WRITE (' ', status)
!***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Calculate U matrix
     Call F2D_GMPRD (TR1PO, TTR1PC, U, 3, 3, 3)
 
!  Calculate UB matrix
     Call F2D_GMPRD (U, B_MATRIX, UB_MATRIX, 3, 3, 3)
 
     End Subroutine F2D_RMAP_CAL_UBSUB 
!********1*********2*********3*********4*********5*********6*********7*********8
!+ NORMV: Normalise vector
     Subroutine NORMV (V1, V1N, V1M)
 
!  Import:
     Real, Intent(IN) :: V1(3) ! Input vector
!  Export
     Real, Intent(OUT) :: V1N(3) ! Normalise vector
     Real, Intent(OUT) :: V1M ! Length of input vector
!--------1---------2---------3---------4---------5---------6---------7--

     V1M = Sqrt(V1(1) * V1(1) + V1(2) * V1(2) + V1(3) * V1(3))
 
     If (V1M .Gt. 0.0) Then
 
        Do i = 1, 3
           V1N(i) = V1(i) / V1M
        End Do
 
     Else
 
        Do i = 1, 3
           V1N(i) = 0.0
           V1M = 0.0
        End Do
 
     End If
 
     End Subroutine NORMV
!********1*********2*********3*********4*********5*********6*********7*********8
!+ PRODV: Vector product
     Subroutine PRODV (V1, V2, V3)
 
!  Import:
     Real, Intent(IN) :: V1(3) ! Input vector
     Real, Intent(IN) :: V2(3) ! Input vector
!  Export
     Real, Intent(OUT) :: V3(3) ! Product of "V1" and "V2"
!--------1---------2---------3---------4---------5---------6---------7--

     Do i = 1, 3
        J = Mod(i, 3) + 1
        K = Mod(j, 3) + 1
        V3(i) = V1(j) * V2(k) - V1(K) * V2(J)
     End Do
 
     End Subroutine PRODV 
!********1*********2*********3*********4*********5*********6*********7*********8
!+ TR1P:  
     Subroutine TR1P (X, Y, XYZ)
 
! Description: 
!  Given vectors X and Y calculate XYZ orthogonal so that:
!  XYZ(1,J)//X(J) XYZ(2,J)//(X(J).Y(J).X(J)) XYZ(3,J)//(X(J).Y(J))

!  Import:
     Real, Intent(IN) :: X(3) ! Input vector
     Real, Intent(IN) :: Y(3) ! Input vector
!  Export
     Real, Intent(OUT) :: XYZ(3, 3) ! Orthogonal vector
!-----------------------------------------------------------------------
!
     Real :: Z(3, 3), XX(3), YY(3), ZZ(3)
!
 
!  Calculate vector T3C
     Call PRODV (X, Y, Z)
     Call NORMV (Z, ZZ, VN)
!  Call MA_VECTOR_NORMALISE (3, 3, Z, status)
 
!  Calculate vector T1C
     Call NORMV (X, XX, VN)

!  Call MA_VECTOR_NORMALISE (3, 3, X, status)
 
!  Calculate vector T2C
     Call PRODV (ZZ, XX, YY)
 
!  Calculate matrix TC
     Do i = 1, 3
        XYZ(i, 1) = XX(i)
        XYZ(i, 2) = YY(i)
        XYZ(i, 3) = ZZ(i)
     End Do
 
     End Subroutine TR1P
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

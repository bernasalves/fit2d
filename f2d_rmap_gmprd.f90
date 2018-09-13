!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_rmap_gmprd.f90 *
!  *                    *
!  **********************
 
!+ F2D_RMAP_GMPRD - Fit 2-D: General Matrix PRoDuct and other routines
!  Description:
!    Garry MacIntyres code for reciprocal map transformation.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    10-Mar-2006: V0.4 Input diffractometer angles in double precision
!    13-May-2005: V0.3 Code removed, to reduce subroutine calls in inner
!      loops, calls placed in "F2D_RMAP_TRANSFORM" (Hammersley)
!    20-Apr-2005: V0.2 Renamed (Hammersley)
!    **-***-****: V0.1 Original (Garry McIntyre)
!  Type Definitions:
!    Implicit None
!  Global Constants:
!  Import:
!  Import/Export:
!  Status:
!  Local Constants:
!    Character*(5) Version ! Version number for subroutine
!    Parameter (Version = 'V0.3')
!  Local Variables:
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
     SUBROUTINE F2D_GMPRD (A, B, R, N, M, L)
!-----------------------------------------------------------------------
     DIMENSION A(1), B(1), R(1)
!***********************************************************************
!  *                                                                     *
!  *    SUBROUTINE F2D_GMPRD                                             *
!  *                                                                     *
!  *    PURPOSE                                                          *
!  *    MULTIPLY TWO GENERAL MATRICES TO FORM A RESULTANT GENERAL        *
!  *    MATRIX                                                           *
!  *                                                                     *
!  *    USAGE                                                            *
!  *    CALL F2D_GMPRD(A,B,R,N,M,L)                                      *
!  *                                                                     *
!  *    DESCRIPTION OF PARAMETERS                                        *
!  *    A - NAME OF FIRST INPUT MATRIX                                   *
!  *    B - NAME OF SECOND INPUT MATRIX                                  *
!  *    R - NAME OF OUTPUT MATRIX                                        *
!  *    N - NUMBER OF ROWS IN A                                          *
!  *    M - NUMBER OF COLUMNS IN A AND ROWS IN B                         *
!  *    L - NUMBER OF COLUMNS IN B                                       *
!  *                                                                     *
!  *    REMARKS                                                          *
!  *    ALL MATRICES MUST BE STORED AS GENERAL MATRICES                  *
!  *    MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A              *
!  *    MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX B              *
!  *    NUMBER OF COLUMNS OF MATRIX A MUST BE EQUAL TO NUMBER OF ROW     *
!  *    OF MATRIX B                                                      *
!  *                                                                     *
!  *    SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                    *
!  *    NONE                                                             *
!  *                                                                     *
!  *    METHOD                                                           *
!  *    THE M BY L MATRIX B IS PREMULTIPLIED BY THE N BY M MATRIX A      *
!  *    AND THE RESULT IS STORED IN THE N BY L MATRIX R.                 *
!  *                                                                     *
!***********************************************************************
     IR = 0
     IK = -M
     DO 10 K = 1, L
        IK = IK + M
        DO 10 J = 1, N
           IR = IR + 1
           JI = J - N
           IB = IK
           R(IR) = 0
           DO 10 I = 1, M
              JI = JI + N
              IB = IB + 1
              10            R(IR) = R(IR) + A(JI) * B(IB)
              RETURN
              END
!-----------------------------------------------------------------------
              SUBROUTINE F2D_GMTRA (A, R, N, M)
!-----------------------------------------------------------------------
              DIMENSION A(1), R(1)
!***********************************************************************
!           *
!             *
!           *    SUBROUTINE F2D_GMTRA
!             *
!           *
!             *
!           *    PURPOSE
!             *
!           *    TRANSPOSE A GENERAL MATRIX
!             *
!           *
!             *
!           *    USAGE
!             *
!           *    CALL F2D_GMTRA(A,R,N,M)
!             *
!           *
!             *
!           *    DESCRIPTION OF PARAMETERS
!             *
!           *    A - NAME OF MATRIX TO BE TRANSPOSED
!             *
!           *    R - NAME OF RESULTANT MATRIX
!             *
!           *    N - NUMBER OF ROWS OF A AND COLUMNS OF R
!             *
!           *    M - NUMBER OF COLUMNS OF A AND ROWS OF R
!             *
!           *
!             *
!           *    REMARKS
!             *
!           *    MATRIX R CANNOT BE IN THE SAME LOCATION AS MATRIX A
!             *
!           *    MATRICES A AND R MUST BE STORED AS GENERAL MATRICES
!             *
!           *
!             *
!           *    SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!             *
!           *    NOME
!             *
!           *
!             *
!           *    METHOD
!             *
!           *    TRANSPOSE N BY M MATRIX A TO FORM M BY N MATRIX R
!             *
!           *
!             *
!***********************************************************************
              IR = 0
              DO 10 I = 1, N
                 IJ = I - N
                 DO 10 J = 1, M
                    IJ = IJ + N
                    IR = IR + 1
                    10         R(IR) = A(IJ)
                    RETURN
                    END
!-----------------------------------------------------------------------
                    SUBROUTINE F2D_PHIMAT (phi, DUM)
!-----------------------------------------------------------------------
                    Double Precision :: phi ! Phi (or omega) crystal rotation 
!                     angle (radians)
                    Real :: DUM(3, 3)
!---- BUSING AND LEVY CONVENTION ROTATION MATRIX FOR PHI OR OMEGA
 
                    Do i = 1, 3
                       Do j = 1, 3
                          DUM(i, j) = 0.0
                       End Do
                    End Do
 
                    DUM(1, 1) = Cos(phi)
                    DUM(1, 2) = Sin(phi)
                    DUM(2, 1) = -DUM(1, 2)
                    DUM(2, 2) = DUM(1, 1)
                    DUM(3, 3) = 1.0
                    Return
                    End
!-----------------------------------------------------------------------
                    Subroutine F2D_CHIMAT (chi, DUM)
!-----------------------------------------------------------------------
                    Double Precision :: chi ! Chi crystal rotation angle 
!                     (radians)
                    Real :: DUM(3, 3)
!                 COMMON /ASPIE/ PI,PIR
!---- BUSING AND LEVY CONVENTION ROTATION MATRIX FOR "chi"
 
                    DUM(1, 1) = Cos(chi)
                    DUM(1, 2) = 0.0
                    DUM(1, 3) = Sin(chi)
                    DUM(2, 1) = 0.0
                    DUM(2, 2) = 1.0
                    DUM(2, 3) = 0.0
                    DUM(3, 1) = -DUM(1, 3)
                    DUM(3, 2) = 0.0
                    DUM(3, 3) = DUM(1, 1)
 
                    Return
                    End
!-----------------------------------------------------------------------
                    SUBROUTINE F2D_NORMAL (V, IERR)
!-----------------------------------------------------------------------
                    DIMENSION V(3)
!---- NORMALISE VECTOR V
                    D = (V(1) * V(1) + V(2) * V(2) + V(3) * V(3))
                    If (D .Gt. 0.0) GOTO 10
                    IERR = -1
                    GOTO 30
                    10 D = SQRT(D)
                    DO 20 I = 1, 3
                       20      V(I) = V(I) / D
                       IERR = 0
                       30 RETURN
                       END
!-----------------------------------------------------------------------
                       SUBROUTINE F2D_VPRODT (V1, V2, V3)
!-----------------------------------------------------------------------
                       DIMENSION V1(3), V2(3), V3(3)
                       V3(1) = V1(2) * V2(3) - V1(3) * V2(2)
                       V3(2) = V1(3) * V2(1) - V1(1) * V2(3)
                       V3(3) = V1(1) * V2(2) - V1(2) * V2(1)
                       RETURN
                       END
 
!                    SUBROUTINE F2D_Z1FRMD (WAVE, CH, PH, GA, OM, NU, Z1)
!-----------------------------------------------------------------------
!                    DIMENSION Z1(3),Z3(3)
!                    REAL NU
!***********************************************************************
!                    *
!                               *
!                    *  CALCULATE DIFFRACTION VECTOR Z1 FROM CH, PH, GA, OM, NU
!                               *
!                    *
!                               *
!***********************************************************************
!                    CALL F2D_Z1FRNB (WAVE, Z3, GA, OM, NU)
!                    CALL F2D_Z1FRZ3 (Z1, CH, PH, Z3)
!                    RETURN
!                    END
!-----------------------------------------------------------------------
!                    SUBROUTINE F2D_Z1FRNB (WAVE, Z3, GA, OM, NU)
!-----------------------------------------------------------------------
!                    DIMENSION Z3(3), Z4(3), DUM1(3, 3), DUM2(3, 3)
!                    REAL NU
!***********************************************************************
!                    *
!                               *
!                    *  CALCULATE DIFFRACTION VECTOR Z3 FROM GA, OM, NU,
!                    ASSUMING CH=PH=0  *
!                    *
!                               *
!***********************************************************************
!                    CALL F2D_Z4FRGN (WAVE, Z4, GA, NU)
!                    CALL F2D_PHIMAT (OM, DUM1)
!                    CALL F2D_GMTRA (DUM1, DUM2, 3, 3)
!                    CALL F2D_GMPRD (DUM2, Z4, Z3, 3, 3, 1)
!                    RETURN
!                    END
!-----------------------------------------------------------------------
!                    SUBROUTINE F2D_Z1FRZ3 (Z1, CH, PH, Z3)
!-----------------------------------------------------------------------
!                    DIMENSION Z1(3), Z2(3), Z3(3), DUM1(3, 3), DUM2(3, 3)
!---- CALCULATE Z1 = [PHI]T.[CHI]T.Z3
!                    CALL F2D_CHIMAT (CH, DUM1)
!                    CALL F2D_GMTRA (DUM1, DUM2, 3, 3)
!                    CALL F2D_GMPRD (DUM2, Z3, Z2, 3, 3, 1)
!                    CALL F2D_Z1FRZ2 (Z1, PH, Z2)
!                    RETURN
!                    END
!-----------------------------------------------------------------------
!                    SUBROUTINE F2D_Z1FRZ2 (Z1, PH, Z2)
!-----------------------------------------------------------------------
!                    DIMENSION Z1(3), Z2(3), DUM1(3, 3), DUM2(3, 3)
!---- CALCULATE Z1 = [PHI]T.Z2
!                    CALL F2D_PHIMAT (PH, DUM1)
!                    CALL F2D_GMTRA (DUM1, DUM2, 3, 3)
!                    CALL F2D_GMPRD (DUM2, Z2, Z1, 3, 3, 1)
!                    RETURN
!                    END
!-----------------------------------------------------------------------
!                    SUBROUTINE F2D_Z4FRGN (WAVE, Z4, GA, NU)
!-----------------------------------------------------------------------
!                    DIMENSION Z4(3)
!                    COMMON /ASPIE/  PI,PIR
!                    REAL NU,NUR
!---- CALCULATES DIFFRACTION VECTOR IN LAB SYSTEM FROM GA AND NU
!                    PIR = 180.0 / 3.1415926
!                    GAR = GA / PIR
!                    NUR = NU / PIR
!                    Z4(1) = ( SIN(GAR) * COS(NUR)    ) / WAVE
!                    Z4(2) = ( COS(GAR) * COS(NUR) - 1.0 ) / WAVE
!                    Z4(3) = ( SIN(NUR)             ) / WAVE
!                    RETURN
!                    END
 

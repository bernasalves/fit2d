!********1*********2*********3*********4*********5*********6*********7**
 
!  ****************
!  *              *
!  * f2d_cdiv.f90 *
!  *              *
!  ****************
 
!+ F2D_CDIV - Fit 2-D Constant Division
     Subroutine F2D_CDIV (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, status)
!  Description:
!    Divides "DATA(xmaxdat,ymaxdat)" by a constant in the region
!    "(xstrelm, ystrelm") to "(xendelm , yendelm)". (Same to "VARIANCES" if 
!    "variances_exist").
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    21-Dec-1988: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Erros array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real, Save :: value = 1.0 ! Value to divide
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CDIV ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CDIV ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input value to divide
        Call IO_INPR (.False., 0.0, 0.0, .True., 'DIVISION CONSTANT', 1, &
          'Enter real value to divide the region of interest', 1, &
          'Enter real number', value, status)
 
        If (value .Eq. 0.0) Then
 
           Call IO_WRITE ( 'WARNING: Not possible to divide by zero', status)
 
        Else If (value .Ne. 1.0) Then
 
!        Multiply by inverse of constant
           value = 1.0 / value
           Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, value, DATA, status)
 
           If (variances_exist) Then
              Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, value**2, VARIANCES, status)
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_CDIV
!********1*********2*********3*********4*********5*********6*********7**
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_maskinvert.f90 *
!  *                    *
!  **********************
 
!+ F2D_MASKINVERT - INVERT MASK
     Subroutine F2D_MASKINVERT (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
       ystrelm, xendelm, yendelm, MASK, status)
!  Description:
!    Grow the mask, by adding masked elements to all elements who touch
!    presently masked pixels by their edges.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    17-Mar-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of elements in first dimension
     Integer, Intent(IN) :: ynumdat ! Number of elements in second dimension
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that an element is masked-off from all operations which take
!      into account masking
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKINVERT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKINVERT ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
              MASK(x, y) = .Not. MASK(x, y)
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_MASKINVERT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

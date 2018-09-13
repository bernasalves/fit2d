!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_pixelregion.f90 *
!  *                     *
!  ***********************
 
!+ F2D_PIXELREGION - Fit 2-D PIXEL REGION definition
     Subroutine F2D_PIXELREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
       ystrelm, xendelm, yendelm, status)
!  Description:
!    Allows user to re-defined region of interest by specifying pixel limit 
!    values
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    08-Oct-1996: V0.2 Improve user text (Hammersley)
!    24-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(INOUT) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(INOUT) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(INOUT) :: yendelm ! Defines Y-end of region of interest
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PIXELREGION ' // Version)
        Return
     End If
 
!  Check that input variables are reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PIXELREGION ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input new pixel limits
        Call IO_INPI (.True., 1, xnumdat, .True., 'X-LOWER LIMIT', 1, &
          'Enter integer to define lower X-limit of region of interest', &
          1, 'Enter integer within given range', xstrelm, status)
        Call IO_INPI (.True., 1, ynumdat, .True., 'Y-LOWER LIMIT', 1, &
          'Enter integer to define lower Y-limit of region of interest', &
          1, 'Enter integer within given range', ystrelm, status)
        Call IO_INPI (.True., xstrelm, xnumdat, .True., 'X-UPPER LIMIT', 1, &
          'Enter integer to define upper X-limit of region of interest', &
          1, 'Enter integer within given range', xendelm, status)
        Call IO_INPI (.True., ystrelm, ynumdat, .True., 'Y-UPPER LIMIT', 1, &
          'Enter integer to define upper Y-limit of region of interest', &
          1, 'Enter integer within given range', yendelm, status)
 
     End If
 
     End Subroutine F2D_PIXELREGION
!********1*********2*********3*********4*********5*********6*********7*********8
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_pixel_value.f90 *
!  *                     *
!  ***********************
 
!+ F2D_PIXEL_VALUE - Fit 2-D set pixel value
     Subroutine F2D_PIXEL_VALUE (xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       status)
!  Description:
!    Allows user to specify a pixel and to set its value.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Aug-1993: V0.2 Remember value of multiplying constant (Hammersley)
!    21-Dec-1988: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of defined elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of defined elements in Y-direction
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! X-element to set
     Integer :: y ! Y-element to set
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PIXEL_VALUE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PIXEL_VALUE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input indices of pixel
        Call IO_INPI (.True., 1, xnumdat, .True., 'X-INDICE OF PIXEL', 1, &
          'Enter the X (horizontal) direction indice of the pixel to set', 1, &
          'Enter integer within given range', x, status)
        Call IO_INPI (.True., 1, ynumdat, .True., 'Y-INDICE OF PIXEL', 1, &
          'Enter the Y (vertical) direction indice of the pixel to set', &
          1, 'Enter integer within given range', y, status)
 
!     Set pixel value
        Call IO_INPR (.False., 0.0, 0.0, .True., 'PIXEL VALUE TO SET', 1, &
          'Enter value for the pixel to set', 1, 'Enter real number', &
          DATA(x, y), status)
 
     End If
 
     End Subroutine F2D_PIXEL_VALUE
!********1*********2*********3*********4*********5*********6*********7*********8

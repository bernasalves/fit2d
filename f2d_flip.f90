!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_flip.f90 *
!  *              *
!  ****************
 
!+ F2D_FLIP - Fit 2-D  FLIP data horizontally or vertically
     Subroutine F2D_FLIP (variances_exist, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, VARIANCES, status)
!  Description:
!    Flips "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)" to 
!    "(xendelm, yendelm)" either horizontally or vertically.
!  Method:
!    Uses "MA_RFLIP" to perform the flip.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    13-Dec-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: variances_exist ! .True., if variance estimates
!      exist
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat,ymaxdat) ! Data array to be flipped
     Real, Intent(INOUT) :: VARIANCES(xmaxdat,ymaxdat) ! Data variances to
!      be flipped
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: version = 'V0.1' ! Version number
!  Local Variables:
     Logical, Save :: left_right = .True. ! .True., if the reflection is in
!      the vertical plane i.e. left to right
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 !  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FLIP ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_FLIP ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input direction of flip
        Call IO_INPL (.True., 0, 1, .True., &
          'FLIP LEFT TO RIGHT ("NO" = TOP/BOTTOM)', 1, &
          '"YES" for reflection in vertical plane, "NO" for ' // &
          'reflection in horizontal ', 1, 'Enter real number', left_right, &
          status)
 
!     Flip data in horizontal or vertical plane
        Call MA_RFLIP (left_right, xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, DATA, status)
 
        If (variances_exist) Then
 
!        Flip variances in horizontal or vertical plane
           Call MA_RFLIP (left_right, xmaxdat, ymaxdat, xstrelm, ystrelm, &
             xendelm, yendelm, VARIANCES, status)
 
        End If
 
     End If
 
     End Subroutine F2D_FLIP
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

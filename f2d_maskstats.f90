!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_maskstats.f90 *
!  *                   *
!  *********************
 
!+ F2D_MASKSTATS - MASK STATisiticS
     Subroutine F2D_MASKSTATS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, MASK, status)
!  Description:
!    Calculates percentage of un-masked elements and outputs the result to the 
!    user.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    04-Feb-1996: V0.1 Original (Hammersley)
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
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that an element is masked-off from all operations which take
!      into account masking
!  Import/Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User message
     Integer :: num_mask ! Number of masked elements in ROI
     Integer :: num_total ! Total number of elements in ROI
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKSTATS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKSTATS ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
        num_total = (xendelm - xstrelm + 1) * (yendelm - ystrelm + 1)
 
!     Calculate number of masked of elements
        num_mask = 0
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
              If (MASK(x, y)) Then
                 num_mask = num_mask + 1
              End If
 
           End Do
 
        End Do
 
!     Output information
        Write (message, '(''INFO: Total number of elements in active ' // &
          'data region = '', i8)') num_total
        Call IO_WRITE (message, status)
        Write (message, '(''      Number of masked elements = '', i8)') &
          num_mask
        Call IO_WRITE (message, status)
        Write (message, '(''      Number of unmasked elements = '', i8)') &
          num_total - num_mask
        Call IO_WRITE (message, status)
        Write (message, '(''      Percentage of masked elements = '', f11.8)') &
          (Real(num_mask) / Real(num_total)) * 100.0
        Call IO_WRITE (message, status)
 
     End If
 
     End Subroutine F2D_MASKSTATS
!********1*********2*********3*********4*********5*********6*********7*********8

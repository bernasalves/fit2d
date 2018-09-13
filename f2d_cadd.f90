!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_cadd.f90 *
!  *              *
!  ****************
 
!+ F2D_CADD - Fit 2-D Constant Addition
     Subroutine F2D_CADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
!  Description:
!    Adds a constant to "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, 
!    ystrelm)" to "(xendelm, yendelm)".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Feb-2006: V0.2 Change user message (Hammersley)
!    21-Dec-1993: V0.1 Original (Hammersley)
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
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array to be added to
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real, Save :: value = 1.0 ! Value to add
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CADD ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_CADD ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input value to add
        Call IO_INPR (.False., 0.0, 1.0, .True., 'ADDITION CONSTANT', 1, &
          'Enter real value to add to region of interest', 1, &
          'Enter real number', value, status)
 
        If (value .Ne. 0.0) Then
 
!        Add constant
           Call MA_RCADD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, value, DATA, status)
 
        End If
 
     End If
 
     End Subroutine F2D_CADD
!********1*********2*********3*********4*********5*********6*********7*********8
 

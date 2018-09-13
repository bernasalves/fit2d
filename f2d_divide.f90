!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_divide.f90 *
!  *                *
!  ******************
 
!+ F2D_DIVIDE - Fit 2-D DIVIDE current data by memory
     Subroutine F2D_DIVIDE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MDATA, MVARIANCES, MASK, DATA, VARIANCES, &
       status)
!  Description:
!    Divides "DATA(xmaxdat, ymaxdat)" by "MDATA(xmaxdat, ymaxdat)" in the region
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)". (Error propagation if 
!    "variances_exist").
!  Method:
!    Uses "MA_DIVIDE" for data divided by the memory, and Fortran code for 
!    error propagation.
!
!    If z = x/y then
!
!    dz**2 =~ dx**2/y**2 + x**2dy**2/y**4
!
!    dz**2 =~ ( dx**2 + z**2dy**2 ) / y**2
!
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.2 Include "MASK" and masking (Hammersley)
!    28-Apr-1993: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: MDATA(xmaxdat, ymaxdat) ! Memory data
     Real, Intent(IN) :: MVARIANCES(xmaxdat, ymaxdat) ! Memory variance array
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Mask array
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array, the output
!      elements are "DATA(x,y) / MDATA(x,y)"
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array, on
!      output contains estimated variances from division operation, given
!      the input values (See above for formula)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_DIVIDE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DIVIDE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_DIVIDE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
        Call MA_MRDIVIDE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
          xendelm, yendelm, MDATA, MASK, xmaxdat, ymaxdat, DATA, status)

        If (variances_exist) Then
 
!        Perform error propagation
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (.Not. MASK(x, y)) Then

                    If (MDATA(x, y) .Eq. 0.0) Then
                       VARIANCES(x, y) = -1.7014117e38
                       status = St_bad_divide0
                    Else
                       VARIANCES(x, y) = (VARIANCES(x, y) + DATA(x, y)**2 * &
                         MVARIANCES(x, y)) / MDATA(x, y)**2
                    End If
 
                 End If

              End Do
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_DIVIDE
!********1*********2*********3*********4*********5*********6*********7*********8

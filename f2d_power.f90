!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_power.f90 *
!  *               *
!  *****************
 
!+ F2D_POWER - Fit 2-D take array elements to specified power
     Subroutine F2D_POWER (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, MASK, DATA, VARIANCES, status)
!  Description:
!    Exponents elements of  "DATA(xmaxdat,ymaxdat)" by specified power in the 
!    region "(xstrelm, ystrelm)" to "(xendelm, yendelm)".
!  Method:
!  Deficiencies:
!    No error propagation at present
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.3 Include "MASK", and use masking (Hammersley)
!    04-Sep-1996: V0.2 GUI option (Hammersley)
!    25-Jan-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be used 
!      for user prompts, messages and input
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Errors array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: retstat ! Subroutine return status:
!      0 = Good status
!      1 = Attempt to take square root of negative number for one
!          or more array elements
!      2 = Attempt to divide by zero for one or more numbers
     Real, Save :: power = 2.0 ! Value
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(2) ! User help text
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POWER ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_POWER ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input power
        If (gui) Then
 
           Call GS_INPR (.False., 0.0, 0.0, .True., 'ENTER POWER', 1, &
             'Enter power by which data elements are to be exponented', &
             1, 'Enter real number', power, status)
        Else
           Call IO_INPR (.False., 0.0, 0.0, .True., 'ENTER POWER', 1, &
             'Enter power by which data elements are to be exponented', &
             1, 'Enter real number', power, status)
        End If
 
!     Take elements to specified power
        Call MA_POWER (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
          power, DATA, retstat, status)
 
        If (retstat .Eq. 1) Then
 
           If (gui) Then
 
              MESSAGE(1) = 'ATTEMPT TO TAKE SQUARE ROOT ON ONE OR MORE'
              MESSAGE(2) = 'NEGATIVE NUMBERS. VALUES SET TO 0.0'
              Call GS_FWARNING (2, 2, MESSAGE, status)
 
           Else
              Call IO_WRITE ('WARNING: Attempt to take ' // &
                'square root on one or more', status)
              Call IO_WRITE ('         negative numbers. ' // &
                'values set to 0.0', status)
           End If
 
        Else If (retstat .Eq. 2) Then
 
           If (gui) Then
 
              MESSAGE(1) = 'ATTEMPT TO DIVIDE BY ZERO.'
              MESSAGE(2) = 'VALUES SET TO 1.7*10**38'
              Call GS_FWARNING (2, 2, MESSAGE, status)
 
           Else
              Call IO_WRITE ('WARNING: Attempt to divide by zero.', &
                status)
              Call IO_WRITE ('         Values set to 1.7e38', status)
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_POWER
!********1*********2*********3*********4*********5*********6*********7*********8

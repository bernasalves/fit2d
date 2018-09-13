!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_logarithm.f90 *
!  *                   *
!  *********************
 
!+ F2D_LOGARITHM - Fit 2-D LOGARITHM (base 10)
     Subroutine F2D_LOGARITHM (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, variances_exist, MASK, DATA, VARIANCES, status)
!  Description:
!    Takes logarithm of elements of "DATA(xmaxdat, ymaxdat)" in the region 
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)". (Error propagation in 
!    "VARIANCES" if "variances_exist").
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.3 Include "MASK", and use masking (Hammersley)
!    04-Sep-1996: V0.2 Allow GUI option (Hammersley)
!    21-Dec-1988: V0.1 Original (Hammersley)
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
     Real, Intent(INOUT) :: VARIANCES(xmaxdat, ymaxdat) ! Variances array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: threshold ! Lower threshold, given if logarithm of zero or negative
!      numbers occurs
!  Local Arrays:
!  Local Data:
!  External Functions:
     Integer, External :: St_errorcode ! Gives error code from "status" value
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_LOGARITHM ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_LOGARITHM ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
        If (variances_exist) Then
 
!        Error propagation first as we need to calculate dy**2 = dx**2/x**2
           Do y = ystrelm, yendelm
 
              Do x = xstrelm, xendelm
 
                 If (.Not. MASK(x, y)) Then

                    If (DATA(x, y) .Eq. 0.0) Then
                       VARIANCES(x, y) = 38.0
                    Else
                       VARIANCES(x, y) = VARIANCES(x, y) / DATA(x, y)**2
                    End If
 
                 End If

              End Do
 
           End Do
 
!        Check for divide by zero
           If (St_errorcode(status) .Eq. St_bad_divide0) Then
              Call ST_DEF_SYSTEM (status)
           End If
 
        End If
 
!     Take logarithm of region of interest
        Call MA_LOGARITHM (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, status)
 
!     Check status value for log of zero or negative
        If (St_errorcode(status) .Eq. St_bad_log0) Then
           Call ST_DEF_SYSTEM (status)
 
!        Set lower threshold value
           threshold = -10.0
           If (gui) Then
 
              Call GS_INPR (.False., 0.0, 0.0, .True., 'LOWER THRESHOLD', 1, &
                'Log(0) or Log(-) has been encountered. ' // &
                'Enter value for lower threshold', 1, 'Enter real value', &
                threshold, status)
 
           Else
 
              Call IO_INPR (.False., 0.0, 0.0, .True., 'LOWER THRESHOLD', 1, &
                'Log(0) or Log(-) has been encountered. ' // &
                'Enter value for lower threshold', 1, 'Enter real value', &
                threshold, status)
 
           End If
 
           Call MA_THRESHOLD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, .True., .False., threshold, 0.0, DATA, status)
 
           If (variances_exist) Then
 
!           Set limits in variances for zero data values
              Call MA_THRESHOLD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, .True., .True., 0.0, Abs(threshold),  VARIANCES, &
                status)
 
           End If
 
        End If
 
        If (variances_exist) Then
 
!        Multiple error values by constant to account for log(10)
!        instead of log(e)
           Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, 0.4343, VARIANCES, status)

        End If
 
     End If
 
     End Subroutine F2D_LOGARITHM
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  *  f2d_threshold.f90  *
!  *                     *
!  ***********************
 
!+ F2D_THRESHOLD - Fit 2-D THRESHOLDing
     Subroutine F2D_THRESHOLD (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, MASK, DATA, status)
!  Description:
!    Takes threshold of elements of "DATA(xmaxdat,ymaxdat)" in the
!    region "(xstrelm, ystrelm)" to "(xendelm, yendelm)".
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Dec-2014: V0.3 Use "MASK" and masking (Hammersley)
!    04-Sep-1996: V0.2 Option of GUI (Hammersley)
!    22-Dec-1988: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Real, Save :: lower = -1.7e38 ! Lower threshold
     Real, Save :: upper = 1.7e38 ! Upper threshold
!  Local Arrays:
!  Local Data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_THRESHOLD ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_THRESHOLD ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Set lower threshold value
        If (gui) Then
 
           Call GS_INPR (.True., -1.7e38, 1.7e38, .True., &
             'MINIMUM THRESHOLD VALUE', 1, &
             'Values below this value are set to this threshold value', &
             1, 'Enter real value', lower, status)
 
           If (upper .Lt. lower) Then
              upper = lower + 1.0
           End If
 
           Call GS_INPR (.True., lower, 1.7e38, .True., &
             'MAXIMUM THRESHOLD VALUE', 1, &
             'Values above this value are set to this threshold value', &
             1, 'Must be greater of equal to the lower threshold', upper, &
             status)
        Else
 
           Call IO_INPR (.True., -1.7e38, 1.7e38, .True., &
             'MINIMUM THRESHOLD VALUE', 1, &
             'Values below this value are set to this threshold value', &
             1, 'Enter real value', lower, status)
 
           If (upper .Lt. lower) Then
              upper = lower + 1.0
           End If
 
           Call IO_INPR (.True., lower, 1.7e38, .True., &
             'MAXIMUM THRESHOLD VALUE', 1, &
             'Values above this value are set to this threshold value', &
             1, 'Must be greater of equal to the lower threshold', upper, &
             status)
 
        End If
 
!     Apply thresholding
        Call MA_MTHRESHOLD (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, .True., .True., lower, upper, MASK, DATA, status)
 
     End If
 
     End Subroutine F2D_THRESHOLD
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

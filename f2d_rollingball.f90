!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_rollingball.f90 *
!  *                     *
!  ***********************
 
!+ F2D_ROLLINGBALL - Fit 2-D ROLLING BALL filter
     Subroutine F2D_ROLLINGBALL (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
!  Description:
!    Rolling ball filter of "DATA(xmaxdat, ymaxdat)" in the region 
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)" with a choice of filter size.
!  Method:
!    Uses "MA_ROLLINGBALL".
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Sep-2006: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be filtered
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Filtered data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer, Save :: x_rectangle = 51 ! Size of filter window in X-direction
     Integer, Save :: x_size = 31 ! Diameter of ball in X-direction
     Integer, Save :: y_rectangle = 51 ! Size of filter window in Y-direction
     Integer, Save :: y_size = 31 ! Diameter of ball in Y-direction
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ROLLINGBALL ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ROLLINGBALL ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        If (gui) Then
 
           Call GS_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'DIAMETER OF "RUGBY BALL" IN X-DIRECTION (PIXELS)', 1, &
             'Size of ball in X-direction', 1, &
             'Enter integer within given range', x_size, status)
           Call GS_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'DIAMETER OF "RUGBY BALL" IN Y-DIRECTION (PIXELS)', 1, &
             'Size of ball in X-direction', 1, &
             'Enter integer within given range', y_size, status)
           Call GS_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'WIDTH OF FILTER IN X-DIRECTION (PIXELS)', 1, &
             'Width of filter in X-direction', 1, &
             'Enter integer within given range', x_rectangle, status)
           Call GS_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'WIDTH OF FILTER IN Y-DIRECTION (PIXELS)', 1, &
             'Width of filter in Y-direction', 1, &
             'Enter integer within given range', y_rectangle, status)

        Else
 
           Call IO_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'DIAMETER OF "RUGBY BALL" IN X-DIRECTION (PIXELS)', 1, &
             'Size of ball in X-direction', 1, &
             'Enter integer within given range', x_size, status)
           Call IO_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'DIAMETER OF "RUGBY BALL" IN Y-DIRECTION (PIXELS)', 1, &
             'Size of ball in X-direction', 1, &
             'Enter integer within given range', y_size, status)
           Call IO_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'WIDTH OF FILTER IN X-DIRECTION (PIXELS)', 1, &
             'Width of filter in X-direction', 1, &
             'Enter integer within given range', x_rectangle, status)
           Call IO_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'WIDTH OF FILTER IN Y-DIRECTION (PIXELS)', 1, &
             'Width of filter in Y-direction', 1, &
             'Enter integer within given range', y_rectangle, status)
 
        End If
 
!     Filter input array ROI (output in memory)
        Call MA_ROLLINGBALL (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
          xendelm, yendelm, x_rectangle, y_rectangle, x_size, y_size, &
          xmaxdat, ymaxdat, MDATA, status)

     End If
 
     End Subroutine F2D_ROLLINGBALL
!********1*********2*********3*********4*********5*********6*********7*********8

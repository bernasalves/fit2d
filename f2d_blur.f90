!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_blur.f90 *
!  *              *
!  ****************
 
!+ F2D_BLUR - Fit 2-D BLURring
     Subroutine F2D_BLUR (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, variances_exist, DATA, VARIANCES, MDATA, MVARIANCES, status)
!  Description:
!    Blurs "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)" to 
!    "(xendelm, yendelm)" by a top hat function, whose size in number of pixels
!    is interactively specified. The result is output in "MDATA". If a variance
!    array exists it is also blurred.
!  Method:
!    Uses "MA_TOPHATCON".
!  Deficiencies:
!    If error propagation is carried out the variances at the edge
!    elements will be slightly too low owing to edge effects. The
!    variance array should ideally by divided by n**2 where n is the
!    number of pixels which are averaged to produce an output value.
!    In the middle n is a constant, but at the edge of the region the
!    value varies. For the data array the normalisation constant is
!    correct, but for the variances it is applied once correctly, but
!    must be applied again; the second time n is used instead of n - 1
!    or n - 2 etc.
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Sep-1996: V0.3 GUI option (Hammersley)
!    27-May-1993: V0.2 Output in memory, and blurring of variance array if it 
!      exists (Hammersley)
!    21-Dec-1988: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of active data region
     Integer, Intent(IN) :: xendelm ! Defines X-end of active data region
     Integer, Intent(IN) :: yendelm ! Defines Y-end of active data region
     Logical, Intent(IN) :: variances_exist ! .True., if variance arrays
!      exist and error propagation is to be carried out
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be blurred
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array to be
!      blurred
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Blurred data array
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Blurred variance
!      array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer, Save :: xnumblur = 2 ! Blurring size in the X-direction in pixels
     Integer, Save :: ynumblur = 2 ! Blurring size in the Y-direction in pixels
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_BLUR ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_BLUR ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input number of pixels by which to blur in each direction
        If (gui) Then
           Call GS_INPI (.True., 1, xmaxdat, .True., 'X BLUR SIZE', 1, &
             'Number of pixels in "top-hat" in X-direction', 1, &
             'Number of pixels must be postive', xnumblur, status)
           Call GS_INPI (.True., 1, ymaxdat, .True., 'Y BLUR SIZE', 1, &
             'Number of pixels in "top-hat" in Y-direction', 1, &
             'Number of pixels must be postive', ynumblur, status)
        Else
           Call IO_INPI (.True., 1, xmaxdat, .True., 'X BLUR SIZE', 1, &
             'Number of pixels in "top-hat" in X-direction', 1, &
             'Number of pixels must be postive', xnumblur, status)
           Call IO_INPI (.True., 1, ymaxdat, .True., 'Y BLUR SIZE', 1, &
             'Number of pixels in "top-hat" in Y-direction', 1, &
             'Number of pixels must be postive', ynumblur, status)
        End If
 
!     Perform top-hat convolution
        Call MA_TOPHATCON (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, xnumblur, ynumblur, .True., xmaxdat, ymaxdat, MDATA, &
          status)
 
!     Error propagation if required
        If (variances_exist) Then
 
           Call MA_TOPHATCON (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
             xendelm, yendelm, xnumblur, ynumblur, .True., xmaxdat, ymaxdat, &
             MVARIANCES, status)
 
!        Need to divide the variance values by the normalisation factor
           Call MA_RCMULT (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
             yendelm, 1.0 / Real(xnumblur * ynumblur), MVARIANCES, status)
 
        End If
 
     End If
 
     End Subroutine F2D_BLUR
!********1*********2*********3*********4*********5*********6*********7*********8
 

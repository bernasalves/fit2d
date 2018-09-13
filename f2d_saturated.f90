!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_saturated.f90 *
!  *                   *
!  *********************
 
!+ F2D_SATURATED - FIT 2-D number of SATURATED pixels
     Subroutine F2D_SATURATED (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, status)
!  Description:
!    Number of pixels above a threshold value.
!  Keywords:
!    Pixel.Information, Coordinate.Values
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Mar-2004: V0.4 Use "GS_FPROMPT" to output message (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    21-Jan-1996: V0.2 Inquire GUI prompt region (Hammersley)
!    19-Jul-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: num_pixels ! Number of pixels above threshold
     Real, Save :: threshold_value = 65534.0 ! Threshold limit
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(1) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SATURATED ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_SATURATED ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Enter threshold value
        Call GS_INPR (.False., 0.0, 0.0, .True., &
          'THRESHOLD VALUE (INCLUSIVE)', 1, &
          'Enter threshold value for counting pixels', 1, 'Enter valid real', &
          threshold_value, status)
 
        Call MA_SATURATED (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, threshold_value, num_pixels, status)
 
        Write (MESSAGE(1), '(''INFO: Number of pixels equal ' // &
          'and above threshold value = '', i12)') num_pixels
 
        Call GS_FPROMPT (1, 1, MESSAGE, status)
        Call IO_WRITE (MESSAGE(1), status)
 
     End If
 
     End Subroutine F2D_SATURATED
!********1*********2*********3*********4*********5*********6*********7*********8

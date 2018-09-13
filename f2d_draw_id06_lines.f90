!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_draw_id06_lines.f90 *
!  *                         *
!  ***************************
 
!+ F2D_DRAW_ID06_LINES: DRAW un-tilted ID06 LINES
     Subroutine F2D_DRAW_ID06_LINES (experiment, max_angles, num_cali_rings, &
       D_SPACINGS, status)
!  Description:
!    Draws position of un-tilted diffraction lines.
!  Keywords:
!    Cake.Draw, Draw.Cake
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    02-May-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! GS constants
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(IN) :: max_angles ! Dimension of "D_SPACINGS" array
     Integer, Intent(IN) :: num_cali_rings ! Number of defined rings
     Real, Intent(IN) :: D_SPACINGS(max_angles) ! Calibrant diffraction peak
!      d-spacings in order of distance in metres
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real :: distance ! Distance in metres from beam centre to line
     Integer :: line ! Loop variable for lines
     Real :: pixel_pos ! Pixel position (X) of line
     Real :: two_theta ! Two theta angle of line
     Real :: xmaxdddr ! X-maximum of displayed data display region
     Real :: xmindddr ! X-minimum of displayed data display region
     Real :: ymaxdddr ! Y-maximum of displayed data display region
     Real :: ymindddr ! Y-minimum of displayed data display region
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_ID06_LINES ' // Version)
        Return
     Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''f2d_draw_id06_lines'')')
!     Write (*, '(''x_beam, y_beam (pixels) = '', 2g14.5)')
!     :       x_beam, y_beam
!     Write (*, '(''start/end, azimuth (degrees) = '', 2g14.5)')
!     :       start_azimuth * 180.0 / Pi , end_azimuth * 180.0 / Pi
!     Write (*, '(''inner/outer limit = '', 2g14.5)')
!     :       inner_limit, outer_limit
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Inquire current displayed data display region
        Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
 
!     Set clipping window to displayed graph position
        Call LG_CLIPWINDOW (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
        Call LG_CLIP (.True., status)
 
!     Set red solid line style
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_red, status)
 
        Do line = 1, num_cali_rings
           
!        Calculate 2-theta angle
           two_theta = 2.0 * Asin(experiment%wavelength / &
             (2.0 * D_SPACINGS(line)))

!        Calculate distance on orthoganal detector
           distance = experiment%detector_distance * Tan(two_theta)

!        Calculate pixel position
           pixel_pos = experiment%x_beam + distance / experiment%x_pixel_size

!        Draw line
           Call GS_LINE (pixel_pos, ymindddr, pixel_pos, ymaxdddr, status)

        End Do

!     Turn-off clipping
        Call LG_CLIP (.False., status)
 
     End If
 
     End Subroutine F2D_DRAW_ID06_LINES
!********1*********2*********3*********4*********5*********6*********7*********8

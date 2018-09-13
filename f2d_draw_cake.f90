!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_draw_cake.f90 *
!  *                   *
!  *********************
 
!+ F2D_DRAW_CAKE: DRAW CAKE region
     Subroutine F2D_DRAW_CAKE (undraw, experiment, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, status)
!  Description:
!    Draws outline of cake
!  Keywords:
!    Cake.Draw, Draw.Cake
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    05-Feb-1996: V0.3 Option to undraw old region (Hammersley)
!    03-Feb-1996: V0.2 Take account of non-square pixels (Hammersley)
!    02-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! GS constants
!  Import:
     Logical, Intent(IN) :: undraw ! .True. if previous region is to be
!      undrawn
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: start_azimuth ! Angle of azimuth of start of region
!      in radians
     Real, Intent(IN) :: end_azimuth ! Angle of azimuth of end of region in
!      radians
     Real, Intent(IN) :: inner_limit ! Inner radius in metres
     Real, Intent(IN) :: outer_limit ! Outer radius in metres
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Real, Save :: old_end ! Previous end azimuth
     Real, Save :: old_inner ! Previous inner limit
     Real, Save :: old_outer ! Previous outer limit
     Real, Save :: old_start ! Previous start azimuth
     Real, Save :: old_x_beam ! Previous X beam coordinate
     Real, Save :: old_x_pixel ! Previous X pixel size
     Real, Save :: old_y_beam ! Previous Y beam coordinate
     Real, Save :: old_y_pixel ! Previous Y pixel size
     Real, Save :: x1_end ! X-coordinate at the end of a line
     Real, Save :: x1_start ! X-coordinate at the start of a line
     Real, Save :: x2_end ! X-coordinate at the end of a line
     Real, Save :: x2_start ! X-coordinate at the start of a line
     Real :: xmaxdddr ! X-maximum of displayed data display region
     Real :: xmindddr ! X-minimum of displayed data display region
     Real, Save :: y1_end ! Y-coordinate at the end of a line
     Real, Save :: y1_start ! Y-coordinate at the start of a line
     Real, Save :: y2_end ! Y-coordinate at the end of a line
     Real, Save :: y2_start ! Y-coordinate at the start of a line
     Real :: ymaxdddr ! Y-maximum of displayed data display region
     Real :: ymindddr ! Y-minimum of displayed data display region
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_CAKE ' // Version)
        Return
     Else
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''f2d_draw_cake'')')
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
 
!     Undraw if required
        If (undraw) Then
 
!        Set inverse video solid line style
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
 
!        Start azimuth
           Call GS_LINE (x1_start, y1_start, x1_end, y1_end, status)
 
!        End azimuth
           If (Abs(old_end - 2.0 * Pi - old_start) .Gt. 1.0e-4) Then
              Call GS_LINE (x2_start, y2_start, x2_end, y2_end, status)
           End If
 
!        Inner radius
           If (old_inner .Gt. 0.0) Then
              Call GS_ARCELLIPSE (old_x_beam, old_y_beam, old_inner / &
                old_x_pixel, old_inner / old_y_pixel, old_start, old_end, &
                .True., .False., 100, status)
           End If
 
!        Outer radius
           Call GS_ARCELLIPSE (old_x_beam, old_y_beam, old_outer / &
             old_x_pixel, old_outer / old_y_pixel, old_start, old_end, .True., &
             .False., 100, status)
 
        End If
 
!     Save drawing variables
        old_x_pixel = experiment%x_pixel_size
        old_y_pixel = experiment%y_pixel_size
        old_x_beam = experiment%x_beam
        old_y_beam = experiment%y_beam
        old_inner = inner_limit
        old_outer = outer_limit
        old_start = start_azimuth
        old_end = end_azimuth
 
!     Display beam centre on image
        Call GS_MARK (experiment%x_beam, experiment%y_beam, 4.0, status)
 
!     Set inverse video solid line style
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
 
!     Line at start azimuth
        x1_start = experiment%x_beam + inner_limit / &
          experiment%x_pixel_size * Cos(start_azimuth)
        y1_start = experiment%y_beam + inner_limit / &
          experiment%y_pixel_size  * Sin(start_azimuth)
        x1_end = experiment%x_beam + outer_limit / &
          experiment%x_pixel_size * Cos(start_azimuth)
        y1_end = experiment%y_beam + outer_limit / &
          experiment%y_pixel_size * Sin(start_azimuth)
        Call GS_LINE (x1_start, y1_start, x1_end, y1_end, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_start, y_start, x_end, y_end = '',
!     :       4g14.5)') x_start, y_start, x_end, y_end
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (Abs(end_azimuth - 2.0 * Pi - start_azimuth) .Gt. 1.0e-4) Then
 
!        Line at end azimuth
           x2_start = experiment%x_beam + inner_limit / &
             experiment%x_pixel_size * Cos(end_azimuth)
           y2_start = experiment%y_beam + inner_limit / &
             experiment%y_pixel_size * Sin(end_azimuth)
           x2_end = experiment%x_beam + outer_limit / &
             experiment%x_pixel_size * Cos(end_azimuth)
           y2_end = experiment%y_beam + outer_limit / &
             experiment%y_pixel_size * Sin(end_azimuth)
           Call GS_LINE (x2_start, y2_start, x2_end, y2_end, status)
 
        End If
 
!     *DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_start, y_start, x_end, y_end = '',
!     :       4g14.5)') x_start, y_start, x_end, y_end
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        If (inner_limit .Gt. 0.0) Then
 
!        Draw inner circle
           Call GS_ARCELLIPSE (experiment%x_beam, experiment%y_beam, &
             inner_limit / experiment%x_pixel_size, &
             inner_limit / experiment%y_pixel_size, &
             start_azimuth, end_azimuth, .True., .False., 100, status)
 
        End If
 
!     Draw outer circle
        Call GS_ARCELLIPSE (experiment%x_beam, experiment%y_beam, &
          outer_limit / experiment%x_pixel_size, &
          outer_limit / experiment%y_pixel_size, &
          start_azimuth, end_azimuth, .True., .False., 100, status)
 
!     Turn-off clipping
        Call LG_CLIP (.False., status)
 
     End If
 
     End Subroutine F2D_DRAW_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_draw_peaks.f90 *
!  *                    *
!  **********************
 
!+ F2D_DRAW_PEAKS - FIT 2-D DRAW PEAKS
     Subroutine F2D_DRAW_PEAKS (draw_bad_weak, max_peaks, num_peaks, PEAKS, &
       status)
!  Description:
!    Draws overlay of peak positions
!  Keywords:
!    Draw.Peaks, Peaks.Draw
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    15-Nov-2006: V0.3 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    26-Jun-1998: V0.2 Don't subtract 0.5 pixels, as the centres are already 
!      corrected (Hammersley)
!    08-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! GS constants
     Include 'io.inc' ! Data structure definitions
!  Import:
     Logical, Intent(IN) :: draw_bad_weak ! .True., if bad unsaturated "peaks" 
!      are to be drawn
     Integer, Intent(IN) :: max_peaks ! Dimension of peak arrays
     Integer, Intent(IN) :: num_peaks ! Total peaks found, including problem
!      peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks) ! Peak results
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: peak ! Loop variable for peaks
     Real :: x_peak ! X-coordinate of peak
     Real :: xmax_dddr ! X-maximum drawn display region
     Real :: xmin_dddr ! X-minimum drawn display region
     Real :: y_peak ! Y-coordinate of peak
     Real :: ymax_dddr ! Y-maximum drawn display region
     Real :: ymin_dddr ! Y-minimum drawn display region
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DRAW_PEAKS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (max_peaks .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_DRAW_PEAKS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Find current displayed region
        Call GS_INQ_DDDR (xmin_dddr, ymin_dddr, xmax_dddr, ymax_dddr, status)
 
!     Set marker style
        Call GS_MARKERSTYLE (5, 0.5, Gs_red, status)
        Call LG_MARKERWIDTH (0.5, status)
        Do peak = 1, num_peaks
 
           x_peak = PEAKS(peak)%x_centre
           y_peak = PEAKS(peak)%y_centre
 
!        Check that the peak is within the display region
           If (x_peak .Ge. xmin_dddr .And. x_peak .Le. xmax_dddr .And. &
             y_peak .Ge. ymin_dddr .And. y_peak .Le. ymax_dddr) Then
 
              If (PEAKS(peak)%status .Eq. 0) Then
 
!              Normal peak
                 Call LG_POLYMARKER (1, x_peak, y_peak, status)
 
              Else If (PEAKS(peak)%status .Eq. 1) Then
 
!              Saturated: Draw yellow cross
                 Call GS_MARKERSTYLE (5, 0.5, Gs_yellow, status)
                 Call LG_POLYMARKER (1, x_peak, y_peak, status)
                 Call GS_MARKERSTYLE (5, 0.5, Gs_red, status)
 
              Else If (PEAKS(peak)%status .Eq. 10) Then
 
                 If (draw_bad_weak) Then
 
!                 "Bad" peak, unsaturated: red asterisk
                    Call GS_MARKERSTYLE (3, 0.5, Gs_red, status)
                    Call LG_POLYMARKER (1, x_peak, y_peak, status)
                    Call GS_MARKERSTYLE (5, 0.5, Gs_red, status)

                 End If
 
              Else
 
!              "Bad" peak, saturated: yellow asterisk
                 Call GS_MARKERSTYLE (3, 0.5, Gs_yellow, status)
                 Call LG_POLYMARKER (1, x_peak, y_peak, status)
                 Call GS_MARKERSTYLE (5, 0.5, Gs_red, status)
 
              End If
 
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_DRAW_PEAKS
!********1*********2*********3*********4*********5*********6*********7**
 
 

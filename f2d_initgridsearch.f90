!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_initgridsearch.f90 *
!  *                        *
!  **************************
 
!+ F2D_CALIBRATEGRID: CALIBRATES GRID peak centres
     Subroutine F2D_INITGRIDSEARCH (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
       num_display, x_start, y_start, x_axis1, y_axis1, x_axis2, y_axis2, &
       status)
!  Description:
!    Displays part of image, inputs three starting peaks to define
!    initial estimates of grid search start peak and grid vectors
!  Keywords:
!    Peak.Search, Search.Peaks
!  Method:
!    The following tasks are undertaken:
!
!    1  Display section of image, maximum of "num_display*num_display" pixels 
!       centred on centre of current ROI
!    2  Input graphically three peaks to define a start peak and two vectors of 
!       the grid search
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Aug-1996: V0.11 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.10 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    14-Nov-1995: V0.9 "Spy-glass" output during coordinate input (Hammersley)
!    12-Sep-1995: V0.8 Obtain GUI prompt region (Hammersley)
!    16-Aug-1995: V0.7 Remove debugging "write" statements (Hammersley)
!    20-Jun-1995: V0.6 Convert to use GS graphics library (Hammersley)
!    25-Jan-1995: V0.5 Use "GR_SI_MARK" to produce double cross marking first 
!      peak (Hammersley)
!    14-Dec-1994: V0.4 Make sure that the initial horizontal vector is to the 
!      right and that the initial vectical vector is upwards (Hammersley)
!    24-Mar-1994: V0.3 Size of displayed region now an input argument 
!      (Hammersley)
!    12-Nov-1993: V0.2 Change "MA_DC2PIXEL" to "MA_DC2PC" (Hammersley)
!    26-Oct-1993: V0.1 Original, based on "F2D_CALIBRATEGRID" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the
!      ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the
!      ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: num_display ! Number of pixels to display
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_start ! X-centre of starting peak in pixel
!      coordinates
     Real, Intent(OUT) :: y_start ! Y-centre of starting peak in pixel
!      coordinates
     Real, Intent(OUT) :: x_axis1 ! X-centre of peak along axis 1 from
!      starting peak
     Real, Intent(OUT) :: y_axis1 ! Y-centre of peak along axis 1 from
!      starting peak
     Real, Intent(OUT) :: x_axis2 ! X-centre of peak along axis 2 from
!      starting peak
     Real, Intent(OUT) :: y_axis2 ! Y-centre of peak along axis 2 from
!      starting peak
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: dummy ! Dummy variable
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User help text
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INITGRIDSEARCH ' // Version)
     Else
 
!     Display data, "num_display * num_display" maximum size centred
!     on centre of ROI
        Call GS_2DIMAGE (xmaxdat, ymaxdat, DATA, XAXIS, YAXIS, Max(xstrelm, &
          (xstrelm + xendelm) / 2 - num_display / 2), Max(ystrelm, (ystrelm + &
          yendelm) / 2 - num_display / 2), Min(xendelm, (xstrelm + xendelm) / &
          2 + num_display / 2), Min(yendelm, (ystrelm + yendelm) / 2 + &
          num_display / 2), title, xlabel, ylabel, zlabel, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input centre of peak
        num_coordinates = 1
        MESSAGE(1) = 'Click on peak to define starting point for the'
        MESSAGE(2) = 'peak search. It is best to choose a reasonably'
        MESSAGE(3) = 'central peak, by it must be positioned such'
        MESSAGE(4) = 'that the row and column do not contain any'
        MESSAGE(5) = 'missing peaks, and neither does the row above,'
        MESSAGE(6) = 'nor the column to the left.'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          Max(xstrelm, (xstrelm + xendelm) / 2 - num_display / 2), &
          Max(ystrelm, (ystrelm + yendelm) / 2 - num_display / 2), &
          Min(xendelm, (xstrelm + xendelm) / 2 + num_display / 2), &
          Min(yendelm, (ystrelm + yendelm) / 2 + num_display / 2), DATA, &
          dummy, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
          'Click on centre of starting peak', 6, MESSAGE, .False., 1, &
          num_coordinates, x_start, y_start, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_start, y_start = '', 2f)') x_start, y_start
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Display coordinate
        Call GS_MARK (x_start, y_start, 1.0, status)
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input centre of next horizontal peak (to right)
        num_coordinates = 1
        MESSAGE(1) = 'Click on next peak to the right of the starting'
        MESSAGE(2) = 'peak. This defines a horizontal search vector.'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          Max(xstrelm, (xstrelm + xendelm) / 2 - num_display / 2), &
          Max(ystrelm, (ystrelm + yendelm) / 2 - num_display / 2), &
          Min(xendelm, (xstrelm + xendelm) / 2 + num_display / 2), &
          Min(yendelm, (ystrelm + yendelm) / 2 + num_display / 2), DATA, &
          dummy, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
          'Click on centre of next peak right horizontally', 2, MESSAGE, &
          .False., 1, num_coordinates, x_axis1, y_axis1, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_axis1, y_axis1 = '', 2f)') x_axis1, y_axis1
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Display arrow
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_white, status)
        Call GS_ARROW (x_axis1, y_axis1, x_start, y_start, 0, 1.0, 1.0, &
          status)
 
!     Display arrow
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input centre of next vertical peak (upwards)
        num_coordinates = 1
        MESSAGE(1) = 'Click on next peak above the starting peak.'
        MESSAGE(2) = 'This defines a vertical search vector.'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          Max(xstrelm, (xstrelm + xendelm) / 2 - num_display / 2), &
          Max(ystrelm, (ystrelm + yendelm) / 2 - num_display / 2), &
          Min(xendelm, (xstrelm + xendelm) / 2 + num_display / 2), &
          Min(yendelm, (ystrelm + yendelm) / 2 + num_display / 2), DATA, &
          dummy, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, &
          'Click on next peak vertically upwards (left most)', 2, MESSAGE, &
          .False., 1, num_coordinates, x_axis2, y_axis2, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_axis2, y_axis2 = '', 2f)') x_axis2, y_axis2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Display arrow
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_white, status)
        Call GS_ARROW (x_axis2, y_axis2, x_start, y_start, 0, 1.0, 1.0, &
          status)
 
!     User message
        Call GS_FPROMPT (1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', status)
 
!     Update screen
        Call GS_UPDATE (status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Convert input coordinates to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_start, x_start, status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_start, y_start, status)
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_axis1, x_axis1, status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_axis1, y_axis1, status)
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_axis2, x_axis2, status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_axis2, y_axis2, status)
 
     End If
 
     End Subroutine F2D_INITGRIDSEARCH
!********1*********2*********3*********4*********5*********6*********7*********8

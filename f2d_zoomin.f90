!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_zoomin.f90 *
!  *                *
!  ******************
 
!+ F2D_ZOOMIN - FIT 2-D or 1-D ZOOM-IN by graphical input
     Subroutine F2D_ZOOMIN (xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
       Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, ystrelm, xendelm, &
       yendelm, status)
!  Description:
!    1. Inputs two coordinates and calculates new region
!    2. Redraws region
!  Keywords:
!    Region~Of~Interest.Graphical~Input, Graphical~Input.Region~Of~Interest
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Mar-1997: V0.17 Output results for the user (Hammersley)
!    17-Dec-1996: V0.16 Correct problem with zoom-in on single column 
!      (Hammersley)
!    14-Dec-1996: V0.15 Rename from "F2D_REGION" and allow zooming to a 
!      single line (Hammersley)
!    13-Dec-1996: V0.14 Set region of interest for 1-D data (Hammersley)
!    08-Oct-1996: V0.13 Use "GS_INP_XYREGION" for 1-D situation (Hammersley)
!    10-Sep-1996: V0.12 Cater for 1-D logarithmic plot zoomings (Hammersley)
!    26-Aug-1996: V0.11 Improve help text and treat graphical "CANCEL" input
!      (Hammersley)
!    23-Aug-1996: V0.10 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    08-Mar-1996: V0.9 Treat 1-D column and row cases (Hammersley)
!    29-Jan-1996: V0.8 Further working around suspected HP bug (Hammersley)
!    22-Jan-1996: V0.7 Add more protection against weirdo graphics bug. I'M 
!      ALMOST SURE THAT THERE IS BUG IN THE HP FORTRAN COMPILER ! I'VE ADDED 
!      THE VARIABLES "ix1, ix2, iy1, iy2" WHICH USED TO BE CONVERTED TO INTEGERS
!      IN THE Min() and Max() CALLS. THIS IS PROBABLY WHERE THE ERROR WAS 
!      COMING. I'VE SEEN A PROBLEM OF THIS SORT BEFORE (Hammersley)
!    04-Nov-1995: V0.6 Try to find possible source of "ystrelm" greater than 
!      "yendelm" problem (Hammersley)
!    26-Oct-1995: V0.5 Use spy-glass output during coordinate input (Hammersley)
!    27-Sep-1995: V0.4 Stop the possibility that the region is only one pixel
!      wide or high (Hammersley)
!    21-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    12-Nov-1993: V0.2 Change "MA_DC2PIXEL" to "MA_DC2PIXC" (Hammersley)
!    20-Aug-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The image data
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.17' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer :: ix1 ! X-pixel number of first coordinate
     Integer :: ix2 ! X-pixel number of second coordinate
     Integer :: iy1 ! Y-pixel number of first coordinate
     Integer :: iy2 ! Y-pixel number of second coordinate
     Integer :: num_coordinates ! Number of returned coordinates
     Real :: x1 ! First X-pixel coordinate of limit
     Real :: x2 ! Second X-pixel coordinate of limit
     Real :: y1 ! First Y-pixel coordinate of limit
     Real :: y2 ! Second Y-pixel coordinate of limit
     Real :: xp1 ! First X-pixel coordinate of limit
     Real :: xp2 ! Second X-pixel coordinate of limit
     Real :: yp1 ! First Y-pixel coordinate of limit
     Real :: yp2 ! Second Y-pixel coordinate of limit
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User help text
     Real :: X_COORDINATES(2) ! X-coordinates of input limits
     Real :: Y_COORDINATES(2) ! Y-coordinates of input limits
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ZOOMIN ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ZOOMIN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
        If (xendelm .Eq. xstrelm .Or. yendelm .Eq. ystrelm) Then
 
!        1-D column or row situation
           Call GS_INP_XYREGION (status)
 
!        Find data display region
           Call GS_INQ_DDR (x1, y1, x2, y2, status)
 
           If (xendelm .Ne. xstrelm) Then
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x1, xp1, status)
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x2, xp2, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''xp1, xp2 = '', 2f10.3)') xp1, xp2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert to integers
              xstrelm = Int(xp1) + 1
              xendelm = Int(xp2) + 1
              xstrelm = Max(1, xstrelm)
              xendelm = Min(xnumdat, xendelm)
 
              Write (MESSAGE(1), &
                '(''INFO: Zoom pixel region = '', i6, '': '', i6)') xstrelm, &
                xendelm
              Call IO_WRITE (MESSAGE(1), status)
 
           Else
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, x1, yp1, status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, x2, yp2, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''y1, y2 = '', 2f10.3)') y1, y2
!           Write (*, '(''yp1, yp2 = '', 2f10.3)') yp1, yp2
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Convert to integers
              ystrelm = Int(yp1) + 1
              yendelm = Int(yp2) + 1
              ystrelm = Max(1, ystrelm)
              yendelm = Min(ynumdat, yendelm)
 
              Write (MESSAGE(1), &
                '(''INFO: Zoom pixel region = '', i6, '': '', i6)') ystrelm, &
                yendelm
              Call IO_WRITE (MESSAGE(1), status)
 
           End If
 
        Else
 
!        2-D situation
 
!        Input 2 coordinates for limits of new ROI
           num_coordinates = 2
           MESSAGE(1) = 'Click on two coordinates to define required'
           MESSAGE(2) = 'zoom in region. Clicking just outside the'
           MESSAGE(3) = 'image region is equivalent to clicking on'
           MESSAGE(4) = 'the very edge.'
           Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, &
             title, xlabel, ylabel, zlabel, &
             'CLICK ON OPPOSITE CORNERS OF NEW REGION', 4, MESSAGE, .False., &
             2, num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
              Return
           Else If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_1, y_1 = '', 2f12.3)')
!        :          X_COORDINATES(1), Y_COORDINATES(1)
!        Write (*, '(''x_2, y_2 = '', 2f12.3)')
!        :          X_COORDINATES(2), Y_COORDINATES(2)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Convert data coordinates to pixel coordinates
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(1), x1, &
             status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(1), y1, &
             status)
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(2), x2, &
             status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(2), y2, &
             status)
 
!        Convert to integers
           ix1 = Int(x1) + 1
           ix2 = Int(x2) + 1
           iy1 = Int(y1) + 1
           iy2 = Int(y2) + 1
 
!        Set new ROI limits
           xstrelm = Min(ix1, ix2)
           ystrelm = Min(iy1, iy2)
           xendelm = Max(ix1, ix2)
           yendelm = Max(iy1, iy2)
 
!        Check HP hasn't screwed up
           If (xstrelm .Gt. xendelm) Then
              Call IO_WRITE ('INFO: !!!!! CORRECTING ' // &
                'COMPILER BUG, PLEASE REPORT THIS INCIDENT', status)
              Call IO_ISWAP (xstrelm, xendelm, status)
           End If
 
           If (ystrelm .Gt. yendelm) Then
              Call IO_WRITE ('INFO: !!!!! CORRECTING ' // &
                'COMPILER BUG, PLEASE REPORT THIS INCIDENT', status)
              Call IO_ISWAP (ystrelm, yendelm, status)
           End If
 
!        Check that ROI limits are allowable
           xstrelm = Max(1, xstrelm)
           ystrelm = Max(1, ystrelm)
           xendelm = Min(xnumdat, xendelm)
           yendelm = Min(ynumdat, yendelm)
 
!        Make doubly sure that values are within valid ranges
           If (xstrelm .Lt. 1) Then
              xstrelm = 1
           End If
 
           If (xendelm .Gt. xnumdat) Then
              xendelm = xnumdat
           End If
 
           If (ystrelm .Lt. 1) Then
              ystrelm = 1
           End If
 
           If (yendelm .Gt. ynumdat) Then
              yendelm = ynumdat
           End If
 
!        Re-check correct order
           If (xstrelm .Gt. xendelm) Then
 
              If (xendelm .Lt. xnumdat) Then
                 xstrelm = xnumdat - 1
                 xendelm = xnumdat
              Else If (xstrelm .Ge. 2) Then
                 xstrelm = 1
                 xendelm = 2
              End If
 
           End If
 
           If (ystrelm .Gt. yendelm) Then
 
              If (yendelm .Lt. ynumdat) Then
                 ystrelm = ynumdat - 1
                 yendelm = ynumdat
              Else If (ystrelm .Gt. 1) Then
                 ystrelm = 1
                 yendelm = 2
              End If
 
           End If
 
           Write (MESSAGE(1), '(''INFO: Zoom pixel region = ('', i6, '', '', ' &
             // 'i6, '') to ('', i6, '', '', i6, '')'')') xstrelm, ystrelm, &
             xendelm, yendelm
           Call IO_WRITE (MESSAGE(1), status)
 
 
        End If
 
     End If
 
     End Subroutine F2D_ZOOMIN
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

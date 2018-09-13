!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_distance.f90 *
!  *                  *
!  ********************
 
!+ F2D_DISTANCE - FIT2D: calculate DISTANCE between two points
     Subroutine F2D_DISTANCE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, &
       zlabel, x_pixel_size, y_pixel_size, status)
!  Description:
!    Graphical input of two points for which the distance is calculated.
!  Keywords:
!    Distance.Calculation, Calculation.Distance
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    20-May-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Real, Intent(IN) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User message
     Integer :: dummy ! Dummy variable
     Integer :: num_coordinates ! Number of returned coordinates
     Real :: distance ! Distance between two points
     Real :: xstrco ! X-coordinate of first point
     Real :: ystrco ! Y-coordinate of first point
     Real :: xendco ! X-coordinate of second point
     Real :: yendco ! Y-coordinate of second point
!  Local Arrays:
     Real :: X_COORDINATES(2) ! X-coordinates of input limits
     Real :: Y_COORDINATES(2) ! Y-coordinates of input limits
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DISTANCE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_DISTANCE ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
        Call IO_WRITE (' ', status)
 
!     Input 2 coordinates for calculating distance
        num_coordinates = 2
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'ENTER POINTS FOR DISTANCE CALCULATION', 1, &
          'Click on 2 X/Y coordinates', .True., 2, num_coordinates, &
          X_COORDINATES, Y_COORDINATES, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Convert data coordinates to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(1), xstrco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(1), ystrco, &
          status)
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(2), xendco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(2), yendco, &
          status)
 
!     Calculate distance in pixels
        distance = Sqrt((xendco - xstrco)**2 + (yendco - ystrco)**2)
        Write (message, '(''INFO: Distance between coordinates = '', f9.2, ' &
          // ''' pixels'')') distance
        Call IO_WRITE (message, status)
 
!     Calculate distance in metres
        distance = Sqrt(((xendco - xstrco) * x_pixel_size)**2 + &
          ((yendco - ystrco) * y_pixel_size)**2)
        Write (message, '(''INFO: Distance between coordinates = '', g14.7, ' &
          // ''' millimetres'')') distance * 1.0e3
        Call IO_WRITE (message, status)
        Call IO_WRITE (' ', status)
 
     End If
 
     End Subroutine F2D_DISTANCE
!********1*********2*********3*********4*********5*********6*********7*********8

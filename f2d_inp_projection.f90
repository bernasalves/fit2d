!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_inp_projection.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_PROJECTION - FIT2D: INPut PROJECTION region
     Subroutine F2D_INP_PROJECTION (mask_data, gisaxs, x_beam, y_beam, &
       xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, &
       DATA, MASK, title, xlabel, ylabel, zlabel, x_pixel_size, y_pixel_size, &
       X_REGION, Y_REGION, status)
!  Description:
!    Graphical input of two points defining a line, and two more
!    points defining the width of the region to projection onto the
!    line.
!  Keywords:
!    Input.Projection, Projection.Input
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    25-Feb-2004: V0.5 Correct number of coordinates being input (Hammersley)
!    25-May-1998: V0.4 Make sure projection region is drawn in the
!      correct position (Hammersley)
!    16-Apr-1998: V0.3 Don't reset 'status' if "CANCEL" is issued (Hammersley)
!    15-Apr-1998: V0.2 Option of GISAXS projection input (Hammersley)
!    31-Mar-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels
     Logical, Intent(IN) :: gisaxs ! .True., if the projection is for
!      grazing incidence work, in which case it is centred on the beam centre
     Real, Intent(IN) :: x_beam ! X-pixel coordinate of the beam centre
     Real, Intent(IN) :: y_beam ! Y-pixel coordinate of the beam centre
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! Defines starting X-point of ROI
     Integer, Intent(IN) :: ystrelm ! Defines starting Y-point of ROI
     Integer, Intent(IN) :: xendelm ! Defines end X-point of ROI
     Integer, Intent(IN) :: yendelm ! Defines end Y-point of ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
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
     Real, Intent(OUT) :: X_REGION(5) ! X-coordinates of projection region
!      (fifth point used for drawing the region)
     Real, Intent(OUT) :: Y_REGION(5) ! X-coordinates of projection region
!      (fifth point used for drawing the region)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: num_coordinates ! Number of returned coordinates
     Logical :: region_ok ! .True., when the user is satisfied that the
!      graphical region is defined O.K.
     Real :: x_vector ! X-component of vector from line to point along
!      perpendicular
     Real :: x1 ! X-coordinate of first point defining region
     Real :: x2 ! X-coordinate of second point defining region
     Real :: xendco ! X-coordinate of end point of slice
     Real :: xinter ! X-coordinate of intersection
     Real :: xstrco ! X-coordinate of starting point of slice
     Real :: y_vector ! Y-component of vector from line to point along
!      perpendicular
     Real :: y1 ! Y-coordinate of first point defining region
     Real :: y2 ! Y-coordinate of second point defining region
     Real :: yendco ! Y-coordinate of end point of slice
     Real :: yinter ! Y-coordinate of intersection
     Real :: ystrco ! Y-coordinate of starting point of slice
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User messages
     Real :: X_COORDINATES(2) ! X-coordinates of input limits
     Real :: Y_COORDINATES(2) ! Y-coordinates of input limits
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_INP_PROJECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_PROJECTION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_INP_PROJECTION ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        region_ok = .False.
 
        Do While (.Not. region_ok)
 
           If (gisaxs) Then
 
!           Display beam centre on image
              Call GS_MARK (x_beam, y_beam, 2.0, status)
 
!           Turn on "rubber-banding"
              Call LG_SET_RUBBERBAND ( Gs_wkid_terminal, 1, x_beam, y_beam, &
                status)
 
              num_coordinates = 2
              MESSAGE(1) = 'Click on a coordinates to define the ' // &
                'end of the '
              MESSAGE(2) = 'projection line of the 1-D scan.'
              Call GS_INPS_FCOORDINATES (mask_data, .True., xmaxdat, ymaxdat, &
                xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, &
                Y_AXIS, title, xlabel, ylabel, zlabel, &
                'ENTER END OF PROJECTION LINE (ONE COORDINATE)', 2, MESSAGE, &
                .True., 2, num_coordinates, X_COORDINATES, Y_COORDINATES, &
                status)
 
!           Turn off "rubber-banding"
              Call LG_SET_RUBBERBAND ( Gs_wkid_terminal, 0, x_beam, y_beam, &
                status)
 
              xendco = X_COORDINATES(1)
              yendco = Y_COORDINATES(1)
 
!           Calculate start coordinate
              xstrco = x_beam - (xendco - x_beam)
              ystrco = y_beam - (yendco - y_beam)
 
!           Draw projection line
              Call GS_LINESTYLE (Lg_solid, 1.0, Gs_red, status)
              Call GS_LINE (xstrco, ystrco, xendco, yendco, status)
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xstrco, xstrco, &
                status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, ystrco, ystrco, &
                status)
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, xendco, xendco, &
                status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, yendco, yendco, &
                status)
           Else
 
!           Input 2 coordinates for limits of the projection line
              num_coordinates = 2
              MESSAGE(1) = 'Click on two coordinates to define ' // &
                'the projection'
              MESSAGE(2) = 'line i.e. the line to form the ' // &
                'X-axis of the'
              MESSAGE(3) = '1-D output.'
              Call GS_INPS_FCOORDINATES (mask_data, .True., xmaxdat, ymaxdat, &
                xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, &
                Y_AXIS, title, xlabel, ylabel, zlabel, &
                'ENTER ENDS OF PROJECTION LINE (TWO COORDINATES)', 3, MESSAGE, &
                .True., 2, num_coordinates, X_COORDINATES, Y_COORDINATES, &
                status)
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(1), &
                xstrco, status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(1), &
                ystrco, status)
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(2), &
                xendco, status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(2), &
                yendco, status)
           End If
 
!        Check for user escape
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Input 2 coordinates for limits of the region to project
           num_coordinates = 2
           MESSAGE(1) = 'Click on two coordinates to define the extent'
           MESSAGE(2) = 'of the region to be projected onto the line.'
           MESSAGE(3) = 'These two coordinates should normally define'
           MESSAGE(4) = 'a line roughly perpendicular to the projection'
           MESSAGE(5) = 'line.'
           Call GS_INPS_FCOORDINATES (mask_data, .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, &
             title, xlabel, ylabel, zlabel, &
             'ENTER LIMITS OF PROJECTION REGION', 5, MESSAGE, .True., 2, &
             num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
!        Check for user escape
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Convert data coordinates to pixel coordinates
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(1), x1, &
             status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(1), y1, &
             status)
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_COORDINATES(2), x2, &
             status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_COORDINATES(2), y2, &
             status)
 
!        Calculate perpendicular vector from the line to the first point
           Call MA_PERINTERSECT (xstrco, ystrco, xendco, yendco, x1, y1, &
             xinter, yinter, status)
           x_vector = x1 - xinter
           y_vector = y1 - yinter
 
!        Calculate two of the points defining the projection region
           X_REGION(1) = xstrco + x_vector
           Y_REGION(1) = ystrco + y_vector
           X_REGION(2) = xendco + x_vector
           Y_REGION(2) = yendco + y_vector
 
!        Calculate perpendicular vector from the line to the second point
           Call MA_PERINTERSECT (xstrco, ystrco, xendco, yendco, x2, y2, &
             xinter, yinter, status)
           x_vector = x2 - xinter
           y_vector = y2 - yinter
 
!        Calculate the two remaining points defining the projection region
           X_REGION(3) = xendco + x_vector
           Y_REGION(3) = yendco + y_vector
           X_REGION(4) = xstrco + x_vector
           Y_REGION(4) = ystrco + y_vector
           X_REGION(5) = X_REGION(1)
           Y_REGION(5) = Y_REGION(1)
 
!        Convert pixel coordinates to data coordinates for display
           Do coordinate = 1, 5
 
              Call MA_PC2DC (xmaxdat, xendelm, X_AXIS, X_REGION(coordinate), &
                X_REGION(coordinate), status)
              Call MA_PC2DC (ymaxdat, yendelm, Y_AXIS, Y_REGION(coordinate), &
                Y_REGION(coordinate), status)
 
           End Do
 
!        Draw parallelogram defining projection region
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_red, status)
           Call LG_POLYLINE (5, X_REGION, Y_REGION, status)
 
!        Re-convert data coordinates to pixel coordinates
           Do coordinate = 1, 5
 
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, X_REGION(coordinate), &
                X_REGION(coordinate), status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, Y_REGION(coordinate), &
                Y_REGION(coordinate), status)
 
           End Do
 
!        User message in GUI prompt region
           MESSAGE(1) = 'A rectangle has been drawn around the'
           MESSAGE(2) = 'projection region.'
           MESSAGE(3) = 'Press the "YES" button to proceed, or the'
           MESSAGE(4) = '"NO" button to re-define the region'
           Call GS_INPL (.True., 0, 1, .True., &
             'IS THE DEFINED PROJECTION REGION O.K.', 4, MESSAGE, 1, &
             'Enter "YES" or "NO" only', region_ok, status)
 
 
           If (.Not. region_ok) Then
 
!           Re-draw image
              Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
 
           End If
 
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_INP_PROJECTION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

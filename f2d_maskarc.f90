!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_maskarc.f90 *
!  *                 *
!  *******************
 
!+ F2D_MASKARC - FIT 2-D: MASK ARC
     Subroutine F2D_MASKARC (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, xstrelm, &
       ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, MASK, status)
!  Description:
!    The user is requested to define an arc region which defines masked-off 
!    pixels in the image, i.e. the areas which will be ignored.
!  Keywords:
!    Mask.Arc.Input, Arc.Mask.Definition, Define.Mask, Input.Arc.Mask
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Aug-1996: V0.3 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.2 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    10-Dec-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to display
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be fitted, .True. = masked/bad data point, not 
!      to be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for the X-direction
     Integer :: y ! Loop variable for the Y-direction
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: retstat ! Return status variable from "MA_ARCSLICE":
!      0 = good status
!      1 = Array not large enough for whole slice
     Integer :: turning_direction ! Sense of rotation defined by the three 
!      points:
!        1 = clockwise
!        0 = The three points are on a straight line (within rounding errors)
!       -1 = Anti-clockwise
     Logical :: zero_crossing ! .True., if the arc crosses the zero radian angle
     Real :: angle ! Angle of a search pixel relative to the arc centre
     Real, Save :: arc_width = 9.0 ! Width in pixels of masking arc
     Real :: end_angle ! End angle of arc
     Real :: max_sqr_radius ! Square of maximum radius for masked pixel
     Real :: mid_angle ! Angle of middle point
     Real :: min_sqr_radius ! Square of minimum radius for masked pixel
     Real :: radius ! Radius of arc
     Real :: sqr_radius ! Square of radius of a pixel
     Real :: start_angle ! Starting angle of arc
     Real :: temp ! Temporary storage
     Real :: x_max ! Maximum X-pixel coordinate for search region
     Real :: x_min ! Minimum X-pixel coordinate for search region
     Real :: xcentre ! X-coordinate of centre of arc
     Real :: xendco ! X-coordinate of end point of slice
     Real :: xmidco ! X-coordinate of a point on the arc slice
     Real :: xstrco ! X-coordinate of starting point of slice
     Real :: y_max ! Maximum Y-pixel coordinate for search region
     Real :: y_min ! Minimum Y-pixel coordinate for search region
     Real :: ycentre ! Y-coordinate of centre of arc
     Real :: yendco ! Y-coordinate of end point of slice
     Real :: ymidco ! Y-coordinate of a point on the arc slice
     Real :: ystrco ! Y-coordinate of starting point of slice
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User help text
     Real :: X_COORDINATES(3) ! X-coordinates of points on arc
     Real :: Y_COORDINATES(3) ! Y-coordinates of points on arc
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKARC ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKARC ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input 3 coordinates for limits and curvature of arc
        num_coordinates = 3
        MESSAGE(1) = 'In order to define an arc, three coordinates must'
        MESSAGE(2) = 'be input in the following order: starting point,'
        MESSAGE(3) = 'a point on the arc, and the end point. Note that'
        MESSAGE(4) = '(This option can be used to define a straight'
        MESSAGE(5) = 'line !) '
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, YAXIS, title, xlabel, &
          ylabel, zlabel, 'ENTER ARC THREE COORDINATES (START, MIDDLE, END)', &
          5, MESSAGE, .True., 3, num_coordinates, X_COORDINATES, &
          Y_COORDINATES, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Convert data coordinates to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, X_COORDINATES(1), xstrco, &
          status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, Y_COORDINATES(1), ystrco, &
          status)
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, X_COORDINATES(2), xmidco, &
          status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, Y_COORDINATES(2), ymidco, &
          status)
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, X_COORDINATES(3), xendco, &
          status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, Y_COORDINATES(3), yendco, &
          status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xstrco, ystrco = '', 2g)') xstrco, ystrco
!     Write (*, '(''xmidco, ymidco = '', 2g)') xmidco, ymidco
!     Write (*, '(''xendco, yendco = '', 2g)') xendco, yendco
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate centre of arc
        Call MA_CAL_CIRCENTRE (xstrco, ystrco, xmidco, ymidco, xendco, yendco, &
          retstat, xcentre, ycentre, radius, status)
 
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('WARNING: Arc centre cannot be calculated', status)
           Return
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xcentre, ycentre, radius = '', 3g)')
!     :       xcentre, ycentre, radius
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Enter width of arc to mask
        Call GS_INPR (.True., 0.5, 100.0, .True., 'WIDTH OF ARC (PIXELS)', 1, &
          'Enter number of pixels for width of masking arc', 1, &
          'Enter integer within given range', arc_width, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate start and end angle of arc
        start_angle = Atan2 (ystrco - ycentre, xstrco - xcentre)
        mid_angle = Atan2 (ymidco - ycentre, xmidco - xcentre)
        end_angle = Atan2 (yendco - ycentre, xendco - xcentre)
 
!     Make sure that the angles are between 0.0 and 2 Pi
        If (start_angle .Lt. 0.0) Then
           start_angle = start_angle + 2.0 * Pi
        End If
        If (mid_angle .Lt. 0.0) Then
           mid_angle = mid_angle + 2.0 * Pi
        End If
        If (end_angle .Lt. 0.0) Then
           end_angle = end_angle + 2.0 * Pi
        End If
 
!     Find out if the sense of rotation is clockwise or anticlockwise
        Call MA_CLOCKWISE (xstrco, ystrco, xmidco, ymidco, xendco, yendco, &
          retstat, turning_direction, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''turning_direction = '', i)')
!     :       turning_direction
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Arrange angles in anti-clockwise direction
        If (turning_direction .Eq. 1) Then
 
!        Swap starting and ending angle
           temp = start_angle
           start_angle = end_angle
           end_angle = temp
 
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''start, end angles = '', 2g)')
!     :       start_angle*180.0/Pi, end_angle*180.0/Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Work out if the arc crosses over the zero radian boundary
        zero_crossing = start_angle .Gt. end_angle
        If (zero_crossing) Then
           start_angle = start_angle - 2.0 * Pi
        End If
 
!     Calculate rectangular search region for masked-off pixels
        x_min = Min(xstrco, xmidco, xendco)
        y_min = Min(ystrco, ymidco, yendco)
        x_max = Max(xstrco, xmidco, xendco)
        y_max = Max(ystrco, ymidco, yendco)
 
        If (start_angle .Le. 0.0 .And. end_angle .Ge. 0.0) Then
           x_max = Max(x_max, xcentre + radius)
        End If
 
        If ((start_angle .Le. Pi / 2.0 .And. end_angle .Ge. Pi / 2.0) .Or. &
          (zero_crossing .And. start_angle .Le. -1.5 * Pi)) Then
           y_max = Max(y_max, ycentre + radius)
        End If
 
        If ((start_angle .Le. Pi .And. end_angle .Ge. Pi) .Or. (zero_crossing &
          .And. start_angle .Le. -Pi)) Then
           x_min = Min(x_min, xcentre - radius)
        End If
 
        If ((start_angle .Le. 1.5 * Pi .And. end_angle .Ge. 1.5 * Pi) .Or. &
          (zero_crossing .And. start_angle .Le. -Pi / 2.0)) Then
           y_min = Min(y_min, ycentre - radius)
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_min, y_min, x_max, y_max = '', 4g)')
!     :       x_min, y_min, x_max, y_max
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate minium and maximum squared radial limits
        min_sqr_radius = (radius - arc_width / 2.0)**2
        max_sqr_radius = (radius + arc_width / 2.0)**2
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''min_sqr_radius, max_sqr_radius = '', 2g)')
!     :       min_sqr_radius, max_sqr_radius
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop over search region
        Do y = Max(ystrelm, Int(y_min - arc_width / 2.0) + 1), Min(yendelm, &
          Int(y_max + arc_width / 2.0) + 1)
 
           Do x = Max(xstrelm, Int(x_min - arc_width / 2.0) + 1), Min(xendelm, &
             Int(x_max + arc_width / 2.0) + 1)
 
!           Calculate square of radius from arc centre
              sqr_radius = (Real(x) - 0.5 - xcentre)**2 + &
                (Real(y) - 0.5 - ycentre)**2
 
              If (sqr_radius .Ge. min_sqr_radius .And. sqr_radius .Le. &
                max_sqr_radius ) Then
 
!              Calculate angle of pixel
                 angle = Atan2 (Real(y) - 0.5 - ycentre, &
                   Real(x) - 0.5 - xcentre)
                 If (angle .Lt. 0.0) Then
                    angle = angle + 2.0 * Pi
                 End If
 
!              Check for angular range, including special case for
!              "zero crossing" arcs
                 If (((angle .Ge. start_angle .And. angle .Le. end_angle)) &
                   .Or. (zero_crossing .And. angle - 2.0 * Pi .Ge. start_angle &
                   .And. angle - 2.0 * Pi .Le. end_angle)) Then
 
!                 Mask pixel
                    MASK(x, y) = .True.
 
                 End If
 
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_MASKARC
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

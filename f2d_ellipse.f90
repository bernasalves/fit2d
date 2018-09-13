!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_ellipse.f90 *
!  *                 *
!  *******************
 
!+ F2D_ELLIPSE: Output ellipse to graphics
     Subroutine F2D_ELLIPSE (x_pixel_size, y_pixel_size, x_ellipse, y_ellipse, &
       radius1, radius2, angle1, colour, status)
!  Description:
!    Draws an ellipse centred at "(x_ellipse, y_ellipse)", of radii "radius1" 
!    and "radius2", with the first radius rotation by "angle1" anti-clockwise 
!    from the X-axis. All ellipse parameters are in metre units, or radians. The
!    ellipse is drawn on a pixel array, where the pixel sizes are "x_pixel_size"
!    and "y_pixel_size".
!  Keywords:
!    Draw.Ellipse
!  Method:
!    An equivalent ellipse in pixel coordinate parameters is calculated.
!  Deficiencies:
!    The ellipse is not drawn rotated at present
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    11-Jun-1996: V0.4 Output of rotated ellipsed (Hammersley)
!    14-Nov-1995: V0.3 Correct error in X-centre scaling (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    29-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the X-direction
!      (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the Y-direction
!      (metres)
     Real, Intent(IN) :: x_ellipse ! X-coordinate of centre of fitted ellipse 
!      (metres)
     Real, Intent(IN) :: y_ellipse ! Y-coordinate of centre of fitted ellipse 
!      (metres)
     Real, Intent(IN) :: radius1 ! First radius of ellipse (metres)
     Real, Intent(IN) :: radius2 ! Second radius of ellipse (metres)
     Real, Intent(IN) :: angle1 ! Orientation angle of first axis of best fit 
!      ellipse
     Integer, Intent(IN) :: colour ! Colour index used for the ellipse
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Real :: pc_angle1 ! The angle of the ellipse when the pixel size has been 
!      taken into account
     Real :: pc_radius1 ! First radius of ellipse (pixels)
     Real :: pc_radius2 ! Second radius of ellipse (pixels)
     Real :: pc_x_ellipse ! X-coordinate of centre of fitted ellipse (pixels)
     Real :: pc_y_ellipse ! Y-coordinate of centre of fitted ellipse (pixels)
     Real :: x_pc ! X-pixel coordinate
     Real :: y_pc ! X-pixel coordinate
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ELLIPSE ' // Version)
     Else
 
!     Convert metres units to pixel coordinates and angle
        pc_x_ellipse = x_ellipse / x_pixel_size
        pc_y_ellipse = y_ellipse / y_pixel_size
        x_pc = Cos(angle1) * radius1 / x_pixel_size
        y_pc = Sin(angle1) * radius1 / y_pixel_size
        pc_angle1 = Atan2(y_pc, x_pc)
        pc_radius1 = Sqrt(x_pc**2 + y_pc**2)
        x_pc = Cos(angle1 + Pi / 2.0) * radius2 / x_pixel_size
        y_pc = Sin(angle1 + Pi / 2.0) * radius2 / y_pixel_size
        pc_radius2 = Sqrt(x_pc**2 + y_pc**2)
 
!     Set graphics attributes, solid normal line
        Call GS_LINESTYLE (1, 1.0, colour, status)
 
!     Draw ellipse
        Call GS_RELLIPSE (pc_x_ellipse, pc_y_ellipse, pc_radius1, pc_radius2, &
          pc_angle1, .True., .False., 199, status)
 
     End If
 
     End Subroutine F2D_ELLIPSE
!********1*********2*********3*********4*********5*********6*********7*********8

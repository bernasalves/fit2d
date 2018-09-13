!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_cal_projection.f90 *
!  *                        *
!  **************************
 
!+ F2D_CAL_PROJECTION - Fit2D: CALculate PROJECTION
     Subroutine F2D_CAL_PROJECTION (distance, x_pixel_size, y_pixel_size, &
       x_centre, y_centre, x_up, y_up, longitude, latitude, x_pc, y_pc, &
       status)
!  Description:
!    Calculates the projected pixel coordinate of a "reflection" at angle 
!    "longitude" in the X/Y plane, and latitude from the X/Y plane. Where the 
!    X-axis is the beam direction. Y-axis is in the plane of the "synchrotron".
!
!    The detector is a flat detector, at "distance" metres from the sample. The
!    beam centre is at pixel coordinate "(x_centre, y_centre)" on the detector. 
!    The size of each "pixel" unit on the detector is "x_pixel_size" and 
!    "y_pixel_size" in the X and Y detector direction. The orientation of the 
!    detector is set by the projection of the Z-axis onto the detector.
!    "(x_up, y_up)" is the projected unit vector Z-axis onto the detector. 
!    (See Book 12, page 141.)
!  Keywords:
!    Geometry.Spherical, Projection.Spherical
!  Method:
!    Basic trigonometry
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
!  Import:
     Real, Intent(IN) :: distance ! Sample to detector distance (metres)
     Real, Intent(IN) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Real, Intent(IN) :: x_centre ! The pixel coordinate for the centre of
!      the beam in the X-direction
     Real, Intent(IN) :: y_centre ! The pixel coordinate for the centre of
!      the beam in the Y-direction
     Real, Intent(IN) :: x_up ! X-component of "UP" unit vector on the
!      detector. This is used for rotation, so must be a unit vector
     Real, Intent(IN) :: y_up ! Y-component of "UP" unit vector on the
!      detector. This is used for rotation, so must be a unit vector
     Real, Intent(IN) :: longitude ! Rotation angle in radians from the
!      X-axis (beam) in the X/Y plane of the "reflection".
     Real, Intent(IN) :: latitude ! Angle from the X/Y plane to the
!      "reflection" in radians
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: x_pc ! X-coordinate of projected "reflection" on
!      the detector
     Real, Intent(OUT) :: y_pc ! Y-coordinate of projected "reflection" on
!      the detector
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real :: ddist ! Distance from sample to equator longitude intersection
     Real :: x_detector ! X-component of projected vector on detector
     Real :: x_projection ! X-component of projected vector on ideal X/Y surface
     Real :: y_detector ! Y-component of projected vector on detector
     Real :: y_projection ! Y-component of projected vector on ideal X/Y surface
!  Local Arrays:
!    Local data:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAL_PROJECTION ' // Version)
        Return
     End If
 
!  Check that the work array is large enough to produce the data
     If (distance .Le. 0.0 .Or. x_pixel_size .Le. 0.0) Then
        status = St_bad_real1
     Else If (y_pixel_size .Le. 0.0) Then
        status = St_bad_real2
     Else If (longitude .Lt. -Pi * 0.5 .Or. longitude .Gt. Pi * 0.5) Then
        status = St_bad_real1
     Else If (latitude .Lt. -Pi * 0.5 .Or. latitude .Gt. Pi * 0.5) Then
        status = St_bad_real2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CAL_PROJECTION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate projected distances in ideal X/Y frame
        x_projection = -Tan(longitude) * distance
        ddist = distance/ Cos(longitude)
        y_projection = Tan(latitude) * ddist
 
!     Rotate to detector X/Y coordinate system
        x_detector = y_up * x_projection + x_up * y_projection
        y_detector = y_up * y_projection - x_up * x_projection
 
!     Calculate pixel coordinates
        x_pc = x_centre + (x_detector / x_pixel_size)
        y_pc = y_centre + (y_detector / y_pixel_size)
 
     End If
 
     End Subroutine F2D_CAL_PROJECTION
!********1*********2*********3*********4*********5*********6*********7*********8

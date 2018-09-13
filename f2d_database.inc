!*******1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_database.inc *
!  *                  *
!  ********************
 
!+ FIT2D DATA-BASE
 
!  Description:
!    Declarations of constants. variables and arrays, which form an
!    internal "data-base" of "fit2d",
!  Keywords:
!    fit2d.Data-base,  Data-base.fit2d
!  Method:
!    Common blocks
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    24-Jan-1995: V0.2 Remove saving pixel sizes, and add detector tilt angles 
!      (Hammersley)
!    10-Mar-1989: V0.1 Original (Hammersley)
!  Global Constants:
!    Global Variables:
     Logical :: f2d_geometry_defined ! .True., if experiment geometry
!    has been defined
     Real :: f2d_x_centre ! X-coordinate of centre of the beam
     Real :: f2d_y_centre ! Y-coordinate of centre of the beam
     Real :: f2d_distance ! Sampe to detector distance (metres)
     Real :: f2d_wavelength ! Wavelength of radiation in metres
     Real :: f2d_tilt_plane_rotation ! Rotation angle of tilt plane
!    anti-clockwise from the X-axis (radians)
     Real :: f2d_tilt_angle ! Tilt angle of detector wrt to the beam
!    in the tilt plane (radians)
     Real :: f2d_detector_rotation ! Angle of rotation from ideal detector
!    X-axis (laboratory Y-axis) TO X-axis of actual detector i.e.
!    scanned film, or image plate (radians, positive angle is
!    anti-clockwise rotation)
 
!  Common block for data data-base
     Common / F2D_DATABASE / f2d_geometry_defined, f2d_x_centre, &
       f2d_y_centre, f2d_distance, f2d_wavelength, f2d_tilt_plane_rotation, &
       f2d_tilt_angle, f2d_detector_rotation
 
!--------1---------2---------3---------4---------5---------6---------7--
 
!  End of f2d_database.inc
!********1*********2*********3*********4*********5*********6*********7**

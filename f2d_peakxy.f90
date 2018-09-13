!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_peakxy.f90 *
!  *                *
!  ******************
 
!+ F2D_PEAKXY: calculate PEAK detector X, Y pixel coordinates for h, k, l index
     Subroutine F2D_PEAKXY (experiment, a_star, a_theta, a_phi, &
       b_star, b_theta, c_star, b_phi, c_theta, c_phi, h, k, l, minus, &
       intersect, x_pixel, y_pixel, status)
!  Description:
!    Calculates (X, Y) detector pixel coordinate for the peak
!    centre of the peak with Miller index (h, k, l). The reciprocal
!    space unit cell is defined by the three axes a, b, c. Of lengths
!    "a_star", "b_star", "c_star", and of angle to the Z-axis (fibre)
!    of "a_theta", "b_theta", "c_theta", and of angle to the X-axis
!    (beam) of "a_phi", "b_phi", "c_phi".
!  Keywords:
!    Peak.Coordinates.(X,Y), hkl~Reflection.Coordinates.(X,Y)
!  Method:
!    "F2D_PEAKRZ" is used to calculate (R, Z) coordinates of the
!    peak. These are then converted to an (x, y, z) position on the
!    normalised Ewald sphere, and this position is projected onto the
!    flat detector and converted to pixel coordinates on the
!    rotated detector. (See 12:166) for further details.)
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.5 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    24-Nov-1997: V0.4 Checking lack of off equator peaks for 90-90-90 degree 
!      cell (Hammersley)
!    15-Feb-1994: V0.3 Produce negative "R" coordinates if required (Hammersley)
!    23-Apr-1993: V0.2 Convert to pixel coordinates (Hammersley)
!    22-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: a_star ! The length of the a-axis in inverse Angstroms
     Real, Intent(IN) :: a_theta ! The angle from the Z-axis (fibre) to the
!      a-axis in radians
     Real, Intent(IN) :: a_phi ! The angle from the X-axis (beam) to the
!      a-axis in the X/Y plane in radians
     Real, Intent(IN) :: b_star ! The length of the b-axis in inverse
!      Angstroms
     Real, Intent(IN) :: b_theta ! The angle from the Z-axis (fibre) to the
!      b-axis in radians
     Real, Intent(IN) :: b_phi ! The angle from the X-axis (beam) to the
!      b-axis in the X/Y plane in radians
     Real, Intent(IN) :: c_star ! The length of the c-axis in inverse Angstroms
     Real, Intent(IN) :: c_theta ! The angle from the Z-axis (fibre) to the
!      c-axis in radians
     Real, Intent(IN) :: c_phi ! The angle from the X-axis (beam) to the
!      c-axis in the X/Y plane in radians
     Integer, Intent(IN) :: h ! The h Miller index of the peak to be determined
     Integer, Intent(IN) :: k ! The k Miller index of the peak to be determined
     Integer, Intent(IN) :: l ! The l Miller index of the peak to be determined
     Logical, Intent(IN) :: minus ! .True., if the -R coordinate is to be
!      produced
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: intersect ! .True., if the reflection
!      intersects with the Ewald sphere i.e. can exist on the film
     Real, Intent(OUT) :: x_pixel ! X-pixel coordinate of reflection on detector
     Real, Intent(OUT) :: y_pixel ! Y-pixel coordinate of reflection on detector
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Real :: cos_theta ! Cosine of detector rotation angle
     Real :: radius_squared ! The square of the radius of the small
!      circle on the normalised Ewalded sphere at height "z_ewald"
     Real :: r_coordinate ! R cylindrical polar coordinate of peak
     Real :: sin_theta ! Sine of detector rotation angle
     Real :: x_ewald ! X-coordinate on normalised Ewald sphere
     Real :: x_metres ! Ideal X-axis distance from beam centre in metres
     Real :: y_ewald ! Y-coordinate on normalised Ewald sphere
     Real :: y_metres ! Ideal Y-axis distance from beam centre in metres
     Real :: z_coordinate ! Z cylindrical polar coordinate of peak
     Real :: z_ewald ! Z-coordinate on normalised Ewald sphere
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKXY ' // Version)
     Else
 
!     Initialise variables
        intersect = .False.
 
!     Calculate (R, Z) coordinate of peak
        Call F2D_PEAKRZ (a_star, a_theta, a_phi, b_star, b_theta, c_star, &
          b_phi, c_theta, c_phi, h, k, l, r_coordinate, z_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''h, k, l = '', 3i)') h, k, l
!     Write (*, '(''(R, Z) = '', 2g)') r_coordinate, z_coordinate
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Normalise to unity radius Ewald sphere
        r_coordinate = r_coordinate * experiment%wavelength * 1.0e10
        z_ewald = z_coordinate * experiment%wavelength * 1.0e10
 
!     Check Z coordinate is within range of unit sphere
        If (z_ewald .Le. 1.0 .And. z_ewald .Ge. -1.0) Then
 
!        Calculate radius of small circle at height z
           radius_squared = 1.0 - z_ewald**2
 
!        Calculate x-coordinate relative to Ewald sphere
           x_ewald = (radius_squared - r_coordinate**2 + 1.0 ) / 2.0
 
!        Produce -x coordinate if required
           If (minus) Then
              x_ewald = -x_ewald
           End If
 
           If (x_ewald .Le. 1.0 .And. x_ewald .Ge. -1.0) Then
 
!           Calculate x-coordinate relative to Ewald sphere
              y_ewald = 1.0 - z_ewald**2 - x_ewald**2
 
!           Check solution to equation is not complex
              If (y_ewald .Ge. 0.0) Then
                 y_ewald = Sqrt(y_ewald)
 
!              Calculate projection onto flat screen at x_ewald = 1
                 intersect = .True.
                 x_metres = (y_ewald / x_ewald) * experiment%detector_distance
                 y_metres = (z_ewald / x_ewald) * experiment%detector_distance
 
!              Convert ideal detector X/Y distances to rotated
!              detector pixel coordinates
                 cos_theta = Cos(-experiment%detector_rotation)
                 sin_theta = Sin(-experiment%detector_rotation)
                 x_pixel = experiment%x_beam + (x_metres * cos_theta - &
                   y_metres * sin_theta) / experiment%x_pixel_size
                 y_pixel = experiment%y_beam + (x_metres * sin_theta + &
                   y_metres * cos_theta) / experiment%y_pixel_size
 
              End If
 
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_PEAKXY
!********1*********2*********3*********4*********5*********6*********7*********8
 

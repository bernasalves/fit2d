!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_peakrz.f90 *
!  *                *
!  ******************
 
!+ F2D_PEAKRZ: calculate PEAK R, Z coordinate for h, k, l index
     Subroutine F2D_PEAKRZ (a_star, a_theta, a_phi, b_star, b_theta, c_star, &
       b_phi, c_theta, c_phi, h, k, l, r_coordinate, z_coordinate, status)
!  Description:
!    Calculates (R, Z) reciprocal space coordinate for the peak centre of the
!    peak with Miller index (h, k, l). The reciprocal space unit cell is defined
!    by the three axes a, b, c. Of lengths "a_star", "b_star", "c_star", and of 
!    angle to the Z-axis (fibre) of "a_theta", "b_theta", "c_theta", and of 
!    angle to the X-axis (beam) of "a_phi", "b_phi", "c_phi".
!  Keywords:
!    Peak.Coordinates.(R,Z), hkl~Reflection.Coordinates.(R,Z)
!  Method:
!    See Fraser R D B and MacRae T P, Int. J. Biol. Macromol, Vol 3, 1981
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: a_star ! The length of the a-axis in inverse Angstroms
     Real, Intent(IN) :: a_theta ! The angle from the Z-axis (fibre) to the
!      a-axis in radians
     Real, Intent(IN) :: a_phi ! The angle from the X-axis (beam) to the a-axis
!      in the X/Y plane in radians
     Real, Intent(IN) :: b_star ! The length of the b-axis in inverse Angstroms
     Real, Intent(IN) :: b_theta ! The angle from the Z-axis (fibre) to the
!      b-axis in radians
     Real, Intent(IN) :: b_phi ! The angle from the X-axis (beam) to the b-axis
!      in the X/Y plane in radians
     Real, Intent(IN) :: c_star ! The length of the c-axis in inverse Angstroms
     Real, Intent(IN) :: c_theta ! The angle from the Z-axis (fibre) to the
!      c-axis in radians
     Real, Intent(IN) :: c_phi ! The angle from the X-axis (beam) to the c-axis
!      in the X/Y plane in radians
     Integer, Intent(IN) :: h ! The h Miller index of the peak to be determined
     Integer, Intent(IN) :: k ! The k Miller index of the peak to be determined
     Integer, Intent(IN) :: l ! The l Miller index of the peak to be determined
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: r_coordinate ! R-coordinate of reflection
     Real, Intent(OUT) :: z_coordinate ! Z-coordinate of reflection
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real :: cos_a_phi ! Cosine of "a_phi"
     Real :: cos_a_theta ! Cosine of "a_theta"
     Real :: cos_b_phi ! Cosine of "b_phi"
     Real :: cos_b_theta ! Cosine of "b_theta"
     Real :: cos_c_phi ! Cosine of "c_phi"
     Real :: cos_c_theta ! Cosine of "c_theta"
     Real :: sin_a_phi ! Sine of "a_phi"
     Real :: sin_a_theta ! Sin of "a_theta"
     Real :: sin_b_phi ! Sine of "b_phi"
     Real :: sin_b_theta ! Sin of "b_theta"
     Real :: sin_c_phi ! Sine of "c_phi"
     Real :: sin_c_theta ! Sin of "c_theta"
     Real :: term1 ! First term of "r_coordinate" calculation
     Real :: term2 ! Second term of "r_coordinate" calculation
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKRZ ' // Version)
     Else
 
!     Calculate triagonomic values
        cos_a_phi = Cos(a_phi)
        cos_a_theta = Cos(a_theta)
        sin_a_phi = Sin(a_phi)
        sin_a_theta = Sin(a_theta)
        cos_b_phi = Cos(b_phi)
        cos_b_theta = Cos(b_theta)
        sin_b_phi = Sin(b_phi)
        sin_b_theta = Sin(b_theta)
        cos_c_phi = Cos(c_phi)
        cos_c_theta = Cos(c_theta)
        sin_c_phi = Sin(c_phi)
        sin_c_theta = Sin(c_theta)
 
!     Calculate the terms which make "r_coordinate"
        term1 = Real(h) * a_star * sin_a_theta * Cos_a_phi + Real(k) * b_star &
          * sin_b_theta * Cos_b_phi + Real(l) * c_star * sin_c_theta * &
          Cos_c_phi
        term2 = Real(h) * a_star * sin_a_theta * Sin_a_phi + Real(k) * b_star &
          * sin_b_theta * Sin_b_phi + Real(l) * c_star * sin_c_theta * &
          Sin_c_phi
 
!     Calculate (R, Z) coordinates
        r_coordinate = Sqrt(term1**2 + term2**2)
        z_coordinate = Real(h) * a_star * cos_a_theta + Real(k) * b_star * &
          cos_b_theta + Real(l) * c_star * cos_c_theta
 
     End If
 
     End Subroutine F2D_PEAKRZ
!********1*********2*********3*********4*********5*********6*********7*********8

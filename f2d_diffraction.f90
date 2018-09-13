!********1*********2*********3*********4*********5*********6*********7**
 
!  ***********************
!  *                     *
!  * f2d_diffraction.f90 *
!  *                     *
!  ***********************
 
!+ F2D_DIFFRACTION: calculates diffraction pattern for a range
!    of h, k, l, values
     Subroutine F2D_DIFFRACTION (EXPERIMENT, a_star, a_theta, &
       a_phi, b_star, b_theta, b_phi, c_star, c_theta, c_phi, h_minimum, &
       h_maximum, k_minimum, k_maximum, l_minimum, l_maximum, max_coordinates, &
       num_coordinates, num_coord2, X_COORDINATES, Y_COORDINATES, status)
!  Description:
!    Calculates (X, Y) flat "film" coordinates for the peak
!    centres of the peaks with Miller indices (h, k, l) from
!    '(h_minimum, k_minimum, l_minimum)' to '
!    '(h_maximum, k_maximum, l_maximum)'. The reciprocal
!    space unit cell is defined by the three axes a, b, c. Of lengths
!    'a_star', 'b_star', 'c_star', and of angle to the Z-axis (fibre)
!    of 'a_theta', 'b_theta', 'c_theta', and of angle to the X-axis
!    (beam) of 'a_phi', 'b_phi', 'c_phi'.
!  Keywords:
!    Peak.Coordinates.(X,Y), hkl~Reflection.Coordinates.(X,Y)
!  Method:
!    See Fraser R D B and MacRae T P, Int. J. Biol. Macromol, Vol 3,
!    1981
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    14-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    30-Jan-1996: V0.8 Use DDDR instead of DDR (Hammersley)
!    20-Jun-1995: V0.7 Convert to GS graphics library (Hammersley)
!    24-Jan-1995: V0.6 Input pixels sizes through argument list (Hammersley)
!    15-Feb-1994: V0.5 Produce negative R coordinates (Hammersley)
!    14-Feb-1994: V0.4 Create second curve of l = 0 reflections (Hammersley)
!    27-Aug-1993: V0.3 Only create coordinates if within current
!      data display region (Hammersley)
!    23-Apr-1993: V0.2 Convert to Pixel Coordinates (Hammersley)
!    16-Apr-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
     Real, Intent(IN) :: a_star ! The length of the a-axis in inverse
!      Angstroms
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
     Real, Intent(IN) :: c_star ! The length of the c-axis in inverse
!      Angstroms
     Real, Intent(IN) :: c_theta ! The angle from the Z-axis (fibre) to the
!      c-axis in radians
     Real, Intent(IN) :: c_phi ! The angle from the X-axis (beam) to the
!      c-axis in the X/Y plane in radians
     Integer, Intent(IN) :: h_minimum ! The minimum h Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: h_maximum ! The maximum h Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: k_minimum ! The minimum k Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: k_maximum ! The maximum k Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: l_minimum ! The minimum l Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: l_maximum ! The maximum l Miller index of the
!      peaks to be determined
     Integer, Intent(IN) :: max_coordinates ! Dimension size of coordinate
!      arrays
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_coordinates ! Number of peak positions
!      calculated
     Integer, Intent(OUT) :: num_coord2 ! Number of peak positions in second
!      curve
     Real, Intent(OUT) :: X_COORDINATES(max_coordinates, 2)
!      X-coordinates of reflections
     Real, Intent(OUT) :: Y_COORDINATES(max_coordinates, 2)
!      Y-coordinates of reflections
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Integer :: h ! h Miller index for a peak
     Integer :: i ! Loop variable
     Integer :: k ! k Miller index for a peak
     Integer :: l ! l Miller index for a peak
     Logical :: intersect ! .True., if a peak reflection intersects
!      the Ewald sphere i.e. exists
     Logical :: minus ! .True., if the negative R coordinate is required
     Real :: x_pixel ! X-pixel coordinate on detector
     Real :: xmaxdddr ! The maximum X-coordinate for the data display
!    region
     Real :: xmindddr ! The minimum X-coordinate for the data display
!    region
     Real :: y_pixel ! Y-pixel coordinate on detector
     Real :: ymaxdddr ! The maximum Y-coordinate for the data display
!    region
     Real :: ymindddr ! The minimum Y-coordinate for the data display
!    region
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DIFFRACTION ' // Version)
     Else
 
!     Inquire current data display region
        Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
 
!     Loop through reflections
        num_coordinates = 0
        num_coord2 = 0
        Do h = h_minimum, h_maximum
 
           Do k = k_minimum, k_maximum
 
              Do l = l_minimum, l_maximum
 
                 Do i = 1, 2
 
                    minus = i .Eq. 2
 
!                 Calculate peak position
                    Call F2D_PEAKXY (EXPERIMENT, &
                      a_star, a_theta, a_phi, b_star, &
                      b_theta, c_star, b_phi, c_theta, c_phi, h, k, l, minus, &
                      intersect, x_pixel, y_pixel, status)
 
                    If (intersect) Then
 
                       If (x_pixel .Ge. xmindddr .And. x_pixel .Le. xmaxdddr &
                         .And. y_pixel .Ge. ymindddr .And. y_pixel .Le. &
                         ymaxdddr) Then
 
                          num_coordinates = num_coordinates + 1
                          X_COORDINATES(num_coordinates, 1) = x_pixel
                          Y_COORDINATES(num_coordinates, 1) = y_pixel
 
!                       Second curve of l=0 reflections only
                          If (l .Eq. 0) Then
                             num_coord2 = num_coord2 + 1
                             X_COORDINATES(num_coord2, 2) = x_pixel
                             Y_COORDINATES(num_coord2, 2) = y_pixel
                          End If
 
                       End If
 
                    End If
 
                 End Do
 
              End Do
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_DIFFRACTION
!********1*********2*********3*********4*********5*********6*********7**

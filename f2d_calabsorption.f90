!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_calabsorption.f90 *
!  *                       *
!  *************************
 
!+ F2D_CALABSORPTION: CALculate ABORPTION profile for XRII/CCD system
     Subroutine F2D_CALABSORPTION ( radius_detect, width_detect, &
       absorb_detect, radius_protect, width_protect, absorb_protect, &
       source_distance, x_pixel_size, max_pixels, PROFILE, status)
!  Description:
!  Keywords:
!    Calculate.Profile
!  Method:
!    1. For each angle calculate intersection of ray from source position to
!       detection position.
!    2. The equation of the line (ray) can now be calculated.
!    3. The intersection points of the ray with the other inner and outer 
!       spheres can be calculated by solving the two simultaneous equations.
!    4. From the intersection positions the path length through the
!       two different layers may be calculated.
!    5. The absorption in the two different layers may be calculated
!    to give the fractional absorption in the detection layer.
!    6. The radial correction profile may be calculated by
!       normalising to the on-axis absorption.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    10-Apr-1994: V0.2 Include effect of extra off-axis absorption (Hammersley)
!    03-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: radius_detect ! Outer radius of detection surface
     Real, Intent(IN) :: width_detect ! Radial width of detection layer in
!      metres
     Real, Intent(IN) :: absorb_detect ! Absorption coefficient of the
!      detective layer
     Real, Intent(IN) :: radius_protect ! Outer radius of protection layer
     Real, Intent(IN) :: absorb_protect ! Absorption coefficient of the
!      protective layer
     Real, Intent(IN) :: width_protect ! Radial width of protection layer in
!      metres
     Real, Intent(IN) :: source_distance ! Distance from source to outer
!      radius of protective layer
     Real, Intent(IN) :: x_pixel_size ! Size of pixel in metres in X-direction
     Integer, Intent(IN) :: max_pixels ! Dimension size of pixel correction
!      array
!  Import/Export:
!  Export:
     Real, Intent(INOUT) :: PROFILE(0: max_pixels) ! The theoretical response
!      to an isotropic source, radially pixel by pixel
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: max_position ! The highest pixel number at 90 degees
!      position on spheric surface
     Integer :: pixel ! Loop variable for pixels
     Integer :: solutions ! Number of solution to intersection of a
!      line with a circle
     Logical :: defined ! .True., if a line equation is defined
     Real :: intercept ! Intercept with Y-axis of a line
     Real :: path_detect ! The path length of X-rays through the
!      detection layer (metres)
     Real :: off_axis_angle ! Off-axis angle of pixel
     Real :: slope ! Slope of a line
     Real :: step_angle ! The change in angle for one pixel
     Real :: x_detect_inner ! The X-coordinate of the inner detection
!      intersection point
     Real :: x_detect_outer ! The X-coordinate of the outer detection
!      intersection point
     Real :: x1, x2 ! X-coordinates of intersection of line with a circle
     Real :: y_detect_inner ! The Y-coordinate of the inner detection
!      intersection point
     Real :: y_detect_outer ! The Y-coordinate of the outer detection
!      intersection point
     Real :: y1, y2 ! Y-coordinates of intersection of line with a circle
!  Local Arrays:
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALABSORPTION ' // Version)
     Else If (max_pixels .Le. 0) Then
        status = St_bad_dim1
        Call ST_SAVE ('Subroutine F2D_CALABSORPTION ' // Version)
     Else
 
!     Initialise variables
 
!     Calculate maximum calculable pixel position (angle <= 90)
        step_angle = x_pixel_size / radius_detect
        max_position = Int(Pi / step_angle)
 
!     Loop through pixels
        Do pixel = 0, Min(max_pixels, max_position)
 
!        Off-axis angle for pixel
           off_axis_angle = Real(pixel) * step_angle
 
!        Detection outer radius position
           x_detect_outer = radius_detect * Cos(off_axis_angle)
           y_detect_outer = radius_detect * Sin(off_axis_angle)
 
!        Equation of line from "(x_detect_outer, y_detect_outer)"
!        to source position
           Call MA_SLOPE (x_detect_outer, y_detect_outer, radius_protect + &
             source_distance, 0.0, defined, slope, intercept, status)
 
           If (.Not. defined) Then
              Call IO_WRITE ('ERROR: The source/detector ' // &
                'geometry is impossible, the source cannot', status)
              Call IO_WRITE ('       be on the surface of ' // &
                'the detection sphere', status)
              Return
           End If
 
!        Find intersection point of the line with the inner
!        spheres by solving the two simultaneous equations. (The
!        line and the circle of radius "radius_detect - width_detect")
           Call MA_LINECIRCLE (slope, intercept, radius_detect - width_detect, &
             solutions, x1, y1, x2, y2, status)
 
!        Of the 2 possible solutions we want the solution with x
!        between 0.0 and "radius_detect"
           If (solutions .Eq. 2) Then
 
              If (x1 .Ge. 0.0) Then
                 x_detect_inner = x1
                 y_detect_inner = y1
              Else
                 x_detect_inner = x2
                 y_detect_inner = y2
              End If
 
           Else If (solutions .Eq. 1) Then
              x_detect_inner = x1
              y_detect_inner = y1
 
           Else
              Call IO_WRITE ('ERROR: The detector cannot ' // &
                'be hit by the X-ray !', status)
              Return
           End If
 
!        Calculate path length in detection layer
           path_detect = Sqrt((x_detect_inner - x_detect_outer)**2 + &
             (y_detect_inner - y_detect_outer)**2 )
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_detect_inner, y_detect_inner = '',
!        :          2f10.5)') x_detect_inner, y_detect_inner
!        Write (*, '(''x_detect_outer, y_detect_outer = '',
!        :          2f10.5)') x_detect_outer, y_detect_outer
!        Write (*, '(''Path length = '', 1pe12.5)') path_detect
!        Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        End Do
 
     End If
 
     End Subroutine F2D_CALABSORPTION
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_polar_cartesian.f90 *
!  *                         *
!  ***************************
 
!+ F2D_POLAR_CARTESIAN -  convert polar coordinates to cartesian coordinates
     Subroutine F2D_POLAR_CARTESIAN (max_rings, max_coordinates, &
       num_rings, NUM_COORDINATES, x_beam, x_pixel_size, AZIMUTHS, &
       X_COORDINATES, Y_COORDINATES, X_CARTESIAN, Y_CARTESIAN, status)
!  Description:
!    Convert polar coordinates to cartesian coordinates.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    06-Jun-2014: V0.2 Separate output arrays (Hammersley)
!    06-Mar-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: max_rings ! Dimension size of coordinates
!      arrays
     Integer, Intent(IN) :: max_coordinates ! Dimension size of coordinates
!      arrays
     Integer, Intent(IN) :: num_rings ! Number of coordinates to convert
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings) ! Number of coordinates 
!      to convert in each ring
     Real, Intent(IN) :: x_beam ! X-coordinate of beam centre
     Real, Intent(IN) :: x_pixel_size ! Pixel size in X-direction
     Real, Intent(IN) :: AZIMUTHS(max_coordinates, max_rings) ! Azimuth angles 
!      in radians of the data points
     Real, Intent(IN) :: X_COORDINATES(max_coordinates, max_rings)
!      X-coordinates of coordinates "on" the powder ring (metres)
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates, max_rings) 
!      Y-coordinates of coordinates "on" the powder ring (metres)
!  Import/Export:
     Real, Intent(OUT) :: X_CARTESIAN(max_coordinates, max_rings)
!      X-coordinates of coordinates "on" the powder ring (metres)
     Real, Intent(OUT) :: Y_CARTESIAN(max_coordinates, max_rings) 
!      Y-coordinates of coordinates "on" the powder ring (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: angle ! Angle of coordinate in radians 
     Integer :: coordinate ! Loop variable for coordinates
     Real :: distance ! Distance in metres between coordinate and beam centre
     Integer :: ring ! Loop variable for rings
     Real :: x_centre ! X-coordinate of beam centre in metres 
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_POLAR_CARTESIAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_POLAR_CARTESIAN ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_rings .Lt. 0 .Or. max_coordinates .Le. 0) Then
        status = St_bad_dim1
     Else If (num_rings .Gt. max_rings) Then
        status = St_bad_int1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_POLAR_CARTESIAN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        x_centre = x_beam * x_pixel_size

        Do ring = 1, num_rings
           
           Do coordinate = 1, NUM_COORDINATES(ring)
 
              distance = (X_COORDINATES(coordinate, ring) - x_beam) * &
                x_pixel_size
              angle = AZIMUTHS(coordinate, ring)
              X_CARTESIAN(coordinate, ring) = x_centre + Cos(angle) * distance
              Y_CARTESIAN(coordinate, ring) = Sin(angle) * distance

           End Do

        End Do

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End F2D_POLAR_CARTESIAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_POLAR_CARTESIAN
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

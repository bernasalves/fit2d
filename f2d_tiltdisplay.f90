!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_tiltdisplay.f90 *
!  *                     *
!  ***********************
 
!+ F2D_TILTDISPLAY -  TILT DISPLAY centre of gravity coordinates
     Subroutine F2D_TILTDISPLAY (x_pixel_size, y_pixel_size, max_coordinates, &
       max_rings, num_rings, NUM_COORDINATES, X_COORDINATES, Y_COORDINATES, &
       status)
!  Description:
!    Draws coordinates positions.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    27-Jan-1995: V0.2 Display coordinates of several rings
!    21-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the X-direction
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the Y-direction
     Integer, Intent(IN) :: max_coordinates ! First dimension of coordinate 
!      arrays
     Integer, Intent(IN) :: max_rings ! Second dimension of coordinate arrays
     Integer, Intent(IN) :: num_rings ! Number of rings for which coordinates 
!      are defined
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings) ! Number of coordinates 
!      in each ring
     Real, Intent(IN) :: X_COORDINATES(Max_coordinates, Max_rings)
!      X-coordinates of peaks
     Real, Intent(IN) :: Y_COORDINATES(Max_coordinates, Max_rings)
!      Y-coordinates of peaks
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: ring ! Loop variable for rings
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''ENTERED F2D_TILTDISPLAY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TILTDISPLAY ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_coordinates .Le. 0) Then
        status = St_bad_dim1
     Else If (max_rings .Le. 0) Then
        status = St_bad_dim2
     Else If (num_rings .Lt. 1 .Or. num_rings .Gt. max_rings) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_TILTDISPLAY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        Do ring = 1, num_rings
 
           Do coordinate = 1, NUM_COORDINATES(ring)
 
!           Draw horizontal white crosses and black diagonal cross
              Call GS_MARK ( X_COORDINATES(coordinate, ring) / x_pixel_size, &
                Y_COORDINATES(coordinate, ring) / y_pixel_size, 1.0, status)
 
           End Do
 
        End Do
 
!     Display coordinates
        Call GS_UPDATE (status)
 
     End If
 
     End Subroutine F2D_TILTDISPLAY
!********1*********2*********3*********4*********5*********6*********7*********8

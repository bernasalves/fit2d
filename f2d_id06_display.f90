!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_id06_display.f90 *
!  *                      *
!  ************************
 
!+ F2D_ID06_DISPLAY - ID06 calibrant DISPLAY
     Subroutine F2D_ID06_DISPLAY (max_coordinates, max_rings, num_rings, &
       NUM_COORDINATES, X_COORDINATES, Y_COORDINATES, status)
!  Description:
!    Calibrates sample to detector distance, wavelength, beam centre,
!    and detector non-orthoganality using a high quality calibrant
!    calibration pattern.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Mar-2014: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: max_coordinates ! Dimension of coordinate arrays
     Integer, Intent(IN) :: max_rings ! Dimension of coordinate arrays
     Integer, Intent(IN) :: num_rings ! Number of rings to display
     Integer, Intent(IN) :: NUM_COORDINATES(max_rings) ! Number of coordinates 
!      for each ring
     Real, Intent(IN) :: X_COORDINATES(max_coordinates, max_rings) 
!      X-coordinate of coordinates to display
     Real, Intent(IN) :: Y_COORDINATES(max_coordinates, max_rings) 
!      X-coordinate of coordinates to display
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Data:
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: ring ! Loop variable for rings 
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_DISPLAY ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_rings .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_ID06_DISPLAY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_ID06_DISPLAY'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Do ring = 1, num_rings

           Do coordinate = 1, NUM_COORDINATES(ring)
 
!           Draw horizontal white crosses and black diagonal cross
              Call GS_MARK (X_COORDINATES(coordinate, ring), &
                Y_COORDINATES(coordinate, ring), 1.0, status)
 
           End Do
 
        End Do

!     Display coordinates
        Call GS_UPDATE (status)

     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ID06_DISPLAY: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ID06_DISPLAY
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

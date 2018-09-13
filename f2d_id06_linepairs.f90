!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_id06_linepairs.f90 *
!  *                        *
!  **************************
 
!+ F2D_ID06_LINEPAIRS - ID06 calibrant DISPLAY
     Subroutine F2D_ID06_LINEPAIRS (max_coordinates, max_rings, num_rings, &
       NUM_COORDINATES, X_POLAR, AZIMUTHS, &
       num_pairs, X_PAIRS, PAIR_DIFS, AZI_PAIRS, min_pos, max_pos, status)
!  Description:
!    Finds pairs of line coordinates with the same azimuths.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Sep-2014: V0.2 use outer ring for maximum / minimum azimuth (Hammersley)
!    14-Aug-2014: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: X_POLAR(max_coordinates, max_rings) 
!      X-coordinate of coordinates to display
     Real, Intent(IN) :: AZIMUTHS(max_coordinates, max_rings) 
!      X-coordinate of coordinates to display
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_pairs ! Number of found pairs
     Real, Intent(OUT) :: X_PAIRS(max_coordinates) ! X-coordinates of found pairs
     Real, Intent(OUT) :: PAIR_DIFS(max_coordinates) ! Distance between found 
!      pair positions on lines
     Real, Intent(OUT) :: AZI_PAIRS(max_coordinates) ! Azimuths of found pairs
     Integer, Intent(OUT) :: min_pos ! Position of minimum pair difference
     Integer, Intent(OUT) :: max_pos ! Position of maximum pair difference
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Data:
!  Local Variables:
     Real :: azimuth2 ! Azimuth of 2nd line coordinate
     Integer :: coord1 ! Loop variable for coordinates on first line
     Integer :: coord2 ! Loop variable for coordinates on second line
     Real :: min_dif ! Running minimum found difference between line coordinates
     Real :: max_dif ! Running maximum found difference between line coordinates
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ID06_LINEPAIRS ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (max_coordinates .Le. 0) Then
        status = St_bad_dim1
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_ID06_LINEPAIRS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_ID06_LINEPAIRS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Initialis min max search variables
        min_pos = -1
        min_dif = 1.0e30
        max_pos = -1
        max_dif = -1.0e30
        coord1 = 1
        coord2 = 1
        num_pairs = 0

        Do
           azimuth2 = AZIMUTHS(coord2, 2)

           If (AZIMUTHS(coord1, 1) .Eq. azimuth2) Then

              num_pairs = num_pairs + 1
              X_PAIRS(num_pairs) = X_POLAR(coord1, 1)
              PAIR_DIFS(num_pairs) = Abs(X_POLAR(coord2, 2) - X_POLAR(coord1, 1))
              AZI_PAIRS(num_pairs) = AZIMUTHS(coord1, 1)

              If (X_POLAR(coord2, 2) .Lt. min_dif) Then
                 min_pos = num_pairs
                 min_dif = X_POLAR(coord2, 2)
              End If

              If (X_POLAR(coord2, 2) .Gt. max_dif) Then
                 max_pos = num_pairs
                 max_dif = X_POLAR(coord2, 2)
              End If
              
              coord1 = coord1 + 1
              coord2 = coord2 + 1

           Else If (AZIMUTHS(coord1, 1) .Gt. azimuth2) Then

              coord2 = coord2 + 1

           Else If (AZIMUTHS(coord1, 1) .Lt. azimuth2) Then

              coord1 = coord1 + 1

           End If

           If (coord1 .Gt. NUM_COORDINATES(1) .Or. &
             coord2 .Gt. NUM_COORDINATES(2)) Then

              Exit

           End If

        End Do
   
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ID06_LINEPAIRS: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ID06_LINEPAIRS
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

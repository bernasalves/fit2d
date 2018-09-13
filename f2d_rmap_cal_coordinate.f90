!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************************
!  *                             *
!  * f2d_rmap_cal_coordinate.f90 *
!  *                             *
!  *******************************
 
!+ F2D_RMAP_CAL_COORDINATE - Calculate coordinates position in reciprocal space
     Subroutine F2D_RMAP_CAL_COORDINATE (x_coordinate, y_coordinate, &
       experiment, status)
!  Description:
!    Calculate reciprocal space position of a given detector coordinate
!  Keywords:
!    Reciprocal~Space.Transformation, Transformation.Reciprocal~Space
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    18-Apr-2006: V0.6 Cartesian coordinate calculated by 
!      "F2D_RMAP_CAL_CARTESIAN" (Hammersley)
!    30-Mar-2006: V0.5 Remove unnecessary subroutine arguments (Hammersley)
!    10-Mar-2006: V0.4 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    04-Nov-2005: V0.3 Option to mirror diffraction image (Hammersley)
!    27-Oct-2005: V0.2 Change detector angle definition (Hammersley)
!    17-Oct-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointers to program arrays
!  Import:
     Real, Intent(IN) :: x_coordinate ! X-pixel coordinate on detector
     Real, Intent(IN) :: y_coordinate ! Y-pixel coordinate on detector
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: i ! Loop variable
     Integer :: j ! Loop variable
!  Local Arrays:
     Real :: Z(3) ! Scattering vector of a pixel position
     Real :: HKL(3) ! Detector position converted to HKL
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_COORDINATE ' // Version)
        Return
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_RMAP_CAL_COORDINATE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.

!     Calculate cartesian reciprocal space coordinates of detector position 
        Call F2D_RMAP_CAL_CARTESIAN (1, x_coordinate, y_coordinate, &
          experiment, Z, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Do i = 1, 3
!           Write (message, '(''     '', 3(g12.5, 1x))') &
!             (experiment%INV_UB(i, j), j = 1, 3)
!           Call IO_WRITE (message, status)
!        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Multiple by inverse of UB matrix
        Call F2D_GMPRD (experiment%INV_UB, Z, HKL, 3, 3, 1)
 
        Write (message, '(''hkl = '', 3g12.4)') HKL(1), HKL(2), HKL(3)
        Call IO_WRITE (message, status)
  
     End If
 
     End Subroutine F2D_RMAP_CAL_COORDINATE
!********1*********2*********3*********4*********5*********6*********7*********8
 

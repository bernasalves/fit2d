!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***************************
!  *                         *
!  * f2d_rmap_initialise.f90 *
!  *                         *
!  ***************************
 
!+ F2D_RMAP_INITIALISE: Reciprocal MAP INITIALISE
     Subroutine F2D_RMAP_INITIALISE (xmaxmap, ymaxmap, zmaxmap, MAP, &
       NORMALISE, status)
!  Description:
!    Initialisation of reciprocal map and normalisation array
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    19-Apr-2005: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxmap ! First dimension of reciprocal map
     Integer, Intent(INOUT) :: ymaxmap ! Second dimension of reciprocal map
     Integer, Intent(INOUT) :: zmaxmap ! Third dimension of reciprocal map
!  Export:
     Real, Intent(OUT) :: MAP(xmaxmap, ymaxmap, zmaxmap) ! Reciprocal map array
     Real, Intent(OUT) :: NORMALISE(xmaxmap, ymaxmap, zmaxmap)
!      Normalisation array
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for first dimension
     Integer :: y ! Loop variable for second dimension
     Integer :: z ! Loop variable for third dimension
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RMAP_INITIALISE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inform user, as this may take some time
        Call GS_FPROMPT (1, 1, 'Initialising reciprocal map', status)
        Call GS_UPDATE (status)
 
!     Initialise map
        Do z = 1, zmaxmap
 
           Do y = 1, ymaxmap
 
              Do x = 1, xmaxmap
                 MAP(x, y, z) = 0.0
              End Do
 
           End Do
 
        End Do
 
!     Inform user, as this may take some time
        Call GS_FPROMPT (1, 1, 'Initialising normalisation array', status)
        Call GS_UPDATE (status)
 
!     Initialise normalisation array
        Do z = 1, zmaxmap
 
           Do y = 1, ymaxmap
 
              Do x = 1, xmaxmap
                 NORMALISE(x, y, z) = 0.0
              End Do
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_RMAP_INITIALISE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

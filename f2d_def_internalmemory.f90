!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************************
!  *                            *
!  * f2d_def_internalmemory.f90 *
!  *                            *
!  ******************************
 
!+ F2D_DEF_INTERNALMEMORY - Fit2D: DEFault INTERNAL MEMORY REGION
     Subroutine F2D_DEF_INTERNALMEMORY (status)
!  Description:
!    Sets internal memories to default state (un-defined).
!  Method:
!  Usage:
!    Limited
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Feb-2004: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Error status return definitions
     Include 'f2d_internal.inc' ! Internal memory data-base
!  Import:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DEF_INTERNALMEMORY ' // Version)
     Else
 
!     Set no defined internal memories
        num_store = 0
 
     End If
 
     End Subroutine F2D_DEF_INTERNALMEMORY
!********1*********2*********3*********4*********5*********6*********7*********8
 

!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_internal.inc *
!  *                  *
!  ********************
 
!+ F2D_INTERNAL - FIT2D: INTERNAL memory
!  Description:
!    This file contains the definitions of the "COMMON" blocks used
!    to implement a hidden database for variables used to control
!    the drawing of the graphs.
!  Method:
!    Named common is used to pass information
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    26-Feb-2004: V0.1 Original (Hammersley)
!  Type Definitions:
!  Global Constants:
     Integer, Parameter :: Max_store = 10 ! Dimension size of arrays to
!      sotre ADR's
!    Variables:
     Integer :: num_store ! Number of stored ADR's
!    Arrays:
     Character(Len = 132) :: TITLES(Max_store) ! Titles of ADR's
     Integer :: pDATA(Max_store) ! Pointers to stored data arrays of ADR's
     Integer :: pVARIANCES(Max_store) ! Pointers to stored variance
!    arrays of ADRs
     Integer :: X_ENDELM(Max_store) ! X-end element of ADR's
     Integer :: X_STRELM(Max_store) ! X-starting element of ADR's
     Integer :: Y_ENDELM(Max_store) ! Y-end element of ADR's
     Integer :: Y_STRELM(Max_store) ! Y-starting element of ADR's
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Internal Memory Character string data-base
     Common / F2D_INTERNALCDB / TITLES
 
!  Internal Memory data-base
     Common / F2D_INTERNALDB / num_store, pDATA, pVARIANCES, X_ENDELM, &
       X_STRELM, Y_ENDELM, Y_STRELM
 
!  The internal memory is initialised with "F2D_DEF_INTERNALMEMORY"
 
!  The internal memory may be filled and recored with "F2D_INTERNALMEMORY"
 
!  Once defined "F2D_BLOCKCOPY" may be used to copy block of data to and
!  from internal memories
 
!  End of Include file F2D_INTERNAL
!********1*********2*********3*********4*********5*********6*********7**
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_freearrays.f90 *
!  *                    *
!  **********************
 
!+ F2D_FREEARRAYS - FIT 2-D ARRAYS: free array dynamic memory
     Subroutine F2D_FREEARRAYS (memory_exist, variances_exist, mask_exist, &
       shared_memory, memory_id, pDATA, pXAXIS, pYAXIS, pVARIANCES, pMASK, &
       pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES, results, status)
!  Description:
!    Allocate dynamic memory for main program arrays
!  Keywords:
!    Arrays
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Dec-2014: V0.2 Fortran allocation for result vectors (Hammersley)
!    13-Nov-2003: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io.inc' ! Data structure declarations
!  Import:
     Logical, Intent(IN) :: memory_exist ! .True. if memory array exists
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(IN) :: mask_exist ! .True. if mask array exists
     Logical, Intent(IN) :: shared_memory ! .True., if shared memory is to used
     Integer, Intent(IN) :: memory_id ! Identifier of shared memory (if used)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: pDATA ! Pointer to dynamic array "DATA"
     Integer, Intent(OUT) :: pXAXIS ! Pointer to dynamic array "XAXIS"
     Integer, Intent(OUT) :: pYAXIS ! Pointer to dynamic array "YAXIS"
     Integer, Intent(OUT) :: pVARIANCES ! Pointer to dynamic array "VARIANCES"
     Integer, Intent(OUT) :: pMASK ! Pointer to dynamic array "MASK"
     Integer, Intent(OUT) :: pMDATA ! Pointer to dynamic array "MDATA"
     Integer, Intent(OUT) :: pMXAXIS ! Pointer to dynamic array "MXAXIS"
     Integer, Intent(OUT) :: pMYAXIS ! Pointer to dynamic array "MYAXIS"
     Integer, Intent(OUT) :: pMVARIANCES ! Pointer to dynamic array "MVARIANCES"
!     Integer, Intent(OUT) :: pVECTORS ! Point to dynamic array "VECTORS" used
!      to store vectors of 1-D values
     Type(RESULT_VECTORS), Intent(INOUT) :: results ! Result vectors
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Integer :: retstat ! Return status variable
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FREEARRAYS ' // Version)
        Return
     End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_FREEARRAYS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Arguments would appear to be reasonable, go ahead.
     If (shared_memory) Then
 
!     Detach from shared memory
        Call IO_DETACHC (pDATA)
 
     Else
 
        Call IO_FREE (pDATA, status)
!     Write (*, '(''Data freed'')')
        Call IO_FREE (pXAXIS, status)
!     Write (*, '(''Xaxis freed'')')
        Call IO_FREE (pYAXIS, status)
!     Write (*, '(''Yaxis freed'')')
 
!     Write (*, '(''Current freed'')')
 
        If (memory_exist) Then
           Call IO_FREE (pMDATA, status)
           Call IO_FREE (pMXAXIS, status)
           Call IO_FREE (pMYAXIS, status)
        End If
!     Write (*, '(''Memory freed'')')
 
        If (variances_exist) Then
           Call IO_FREE (pVARIANCES, status)
 
           If (memory_exist) Then
              Call IO_FREE (pMVARIANCES, status)
           End If
 
        End If
 
        If (mask_exist) Then
           Call IO_FREE (pMASK, status)
        End If
 
     End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_FREEARRAYS: Before vectors'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Free result vectors arrays
!     Call IO_FREE (pVECTORS, status)
     Deallocate (results%VECTORS)
     Deallocate (results%STARTS)
     Deallocate (results%ENDS)
     Deallocate (results%TITLES)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''End: F2D_FREEARRAYS'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_FREEARRAYS
!********1*********2*********3*********4*********5*********6*********7*********8

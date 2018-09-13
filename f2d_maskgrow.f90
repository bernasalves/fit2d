!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_maskgrow.f90 *
!  *                  *
!  ********************
 
!+ F2D_MASKGROW - "GROW" MASK
     Subroutine F2D_MASKGROW (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
!  Description:
!    Grow the mask, by adding masked elements to all elements who touch
!    presently masked pixels by their edges.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.3 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    23-Feb-2005: V0.2 Continue implementation (Hammersley)
!    18-Feb-2005: V0.1 Original, based on "FTGrowMask" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xnumdat ! Number of elements in first dimension
     Integer, Intent(IN) :: ynumdat ! Number of elements in second dimension
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      means that an element is masked-off from all operations which take
!      into account masking
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer, Save :: num_cycles = 1 ! Number of growth cycles for mask growth
     Integer :: retstat ! Return status variable
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable
     Integer :: y ! Loop variable
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(4) ! User messages
     Logical*1, Allocatable :: TMP_MASK(:, :) ! Dynamic temporary mask
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKGROW ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_MASKGROW ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Try to get number of growth cycles from database
        Call IO_INQ_IKEYVALUE ('MASK_GROWTH_CYCLES', num_cycles, retstat, &
          status)
 
!     Number of cycles
        MESSAGE(1) = 'Enter number of cycles to grow the mask'
        If (gui) Then
           Call GS_INPI (.True., 1, 40, .True., 'NUMBER OF GROWTH CYCLES', 1, &
             MESSAGE, 1, 'Enter integer within given range', num_cycles, &
             status)
        Else
           Call IO_INPI (.True., 1, 40, .True., 'NUMBER OF GROWTH CYCLES', 1, &
             MESSAGE, 1, 'Enter integer within given range', num_cycles, &
             status)
        End If
 
!     Save number of growth cycles from database
        Call IO_SET_IKEYVALUE ('MASK_GROWTH_CYCLES', num_cycles, retstat, &
          status)
 
!     Get dynamic work array space for temporary mask
        Allocate (TMP_MASK(xmaxdat, ymaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MASKGROW ' // Version)
           Return
        End If

!     Grow mask by required number of cycles
        Call MA_MASKGROW (xmaxdat, ymaxdat, xnumdat, ynumdat, xstrelm, &
          ystrelm, xendelm, yendelm, num_cycles, MASK, TMP_MASK, status)
 
!     Free dynamic array space
        Deallocate (TMP_MASK)
 
     End If
 
     End Subroutine F2D_MASKGROW
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

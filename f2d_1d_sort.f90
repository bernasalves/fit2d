!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *******************
!  *                 *
!  * f2d_1d_sort.f90 *
!  *                 *
!  *******************
 
!+ F2D_1D_SORT - Fit 2-D 1-D SORTing
     Subroutine F2D_1D_SORT (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
!  Description:
!    1-D sorting of "DATA(xmaxdat, ymaxdat)" in the region "(xstrelm, ystrelm)"
!    to "(xendelm, yendelm)" in X or Y direction.
!  Method:
!    Uses "MA_VECTOR_SORT".
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Oct-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Data array to be sorted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer num_sort ! Number of elements to sort
     Integer stat ! Status return variable for "Allocate"
     Integer x ! Loop variable for X-direction
     Logical x_direction ! .True., if sorting is in the X-direction, otherwise
!      the Y-direction
     Integer y ! Loop variable for Y-direction
!  Local Arrays:
     Real, Allocatable :: WORK(:) ! Dynamic work array for sorting
     Real, Allocatable :: WORK2(:) ! Dynamic work array for storing columns
!  Local Data:
     Data x_direction / .True. /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1D_SORT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_1D_SORT ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        If (gui) Then
 
           Call GS_INPL (.True., 0, 1, .True., &
             '1-D SORTING IN IN X-DIRECTION', 1, &
             '"YES" if sorting in along the X-direction, otherwise' // &
             'the Y-direction', 1, &
             'Enter "YES" or "NO"', x_direction, status)

        Else
 
           Call IO_INPL (.True., 0, 1, .True., &
             '1-D SORTING IN IN X-DIRECTION', 1, &
             '"YES" if sorting in along the X-direction, otherwise' // &
             'the Y-direction', 1, &
             'Enter "YES" or "NO"', x_direction, status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (x_direction) Then
           num_sort = xendelm - xstrelm + 1
        Else
           num_sort = yendelm - ystrelm + 1
        End If

        Allocate (WORK(num_sort), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_1D_SORT ' // Version)
           Return
        End If

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        If (x_direction) Then

!        1-D sorting along X-direction
           Do y = ystrelm, yendelm

              Call MA_VECTOR_SORT (xmaxdat, num_sort, DATA(xstrelm, y), WORK, &
                status)

           End Do

        Else

!        1-D sorting along Y-direction
           Allocate (WORK2(num_sort), Stat = stat)
           If (stat .Ne. 0) Then
              status = St_mod_fit2d + St_bad_malloc
              Call ST_SAVE ('Subroutine F2D_1D_SORT ' // Version)
                Return
             End If

           Do x = xstrelm, xendelm

!           Copy column to work array
              Do y = ystrelm, yendelm
                 WORK2(y - ystrelm + 1) = DATA(x, y)
              End Do

              Call MA_VECTOR_SORT (num_sort, num_sort, WORK2, WORK, status)

!           Copy work array to column 
              Do y = ystrelm, yendelm
                 DATA(x, y) = WORK2(y - ystrelm + 1)
              End Do

           End Do

!        Free work array
           Deallocate (WORK2)

        End If

!     Free work array
        Deallocate (WORK)

     End If
 
     End Subroutine F2D_1D_SORT
!********1*********2*********3*********4*********5*********6*********7*********8

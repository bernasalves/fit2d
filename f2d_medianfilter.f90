!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_medianfilter.f90 *
!  *                      *
!  ************************
 
!+ F2D_MEDIANFILTER - Fit 2-D MEDIAN FILTERing
     Subroutine F2D_MEDIANFILTER (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MDATA, status)
!  Description:
!    Median filter of "DATA(xmaxdat, ymaxdat)" in the region 
!    "(xstrelm, ystrelm)" to "(xendelm, yendelm)" with a choice of filter size.
!  Method:
!    Uses "MA_MEDIANFILTER".
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Mar-2006: V0.4 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    04-Sep-1996: V0.3 GUI option, removed old method code (Hammersley)
!    09-Apr-1994: V0.2 Much faster filter algorithm (Hammersley)
!    27-May-1988: V0.1 Original (Hammersley)
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
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be filtered
!  Export:
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Filtered data array
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: max_column ! Dimension of "COLUMN" array
     Integer :: max_sort ! Dimension of "SORT" and "KEYS" arrays
     Integer stat ! Status return variable for "Allocate"
     Integer, Save :: x_size = 3 ! Size of median filter in X-direction
     Integer, Save :: y_size = 3 ! Size of median filter Y-direction
!    Logical new ! .True., if new algorithm is to be used
!  Local Arrays:
     Real, Allocatable :: COLUMN(:) ! Pointer for dynamic array "COLUMN"
     Real, Allocatable :: KEYS(:) ! Pointer for dynamic array "KEYS"
     Real, Allocatable :: SORT(:) ! Pointer for dynamic array "SORT"
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MEDIANFILTER ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MEDIANFILTER ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
        If (gui) Then
 
           Call GS_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'MEDIAN FILTER X-SIZE', 1, &
             'Size of filter window in X-direction', 1, &
             'Enter integer within given range', x_size, status)
           Call GS_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'MEDIAN FILTER Y-SIZE', 1, &
             'Size of filter window in Y-direction', 1, &
             'Enter integer within given range', y_size, status)
 
        Else
 
           Call IO_INPI (.True., 1, xendelm - xstrelm + 1, .True., &
             'MEDIAN FILTER X-SIZE', 1, &
             'Size of filter window in X-direction', 1, &
             'Enter integer within given range', x_size, status)
           Call IO_INPI (.True., 1, yendelm - ystrelm + 1, .True., &
             'MEDIAN FILTER Y-SIZE', 1, &
             'Size of filter window in Y-direction', 1, &
             'Enter integer within given range', y_size, status)
 
        End If
 
!     Allocate dynamic work arrays
        max_sort = x_size * y_size
        max_column = y_size
        Allocate (COLUMN(max_column), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MEDIANFILTER ' // Version)
           Return
        End If
        Allocate (SORT(max_sort), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MEDIANFILTER ' // Version)
           Return
        End If
        Allocate (KEYS(max_sort), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_MEDIANFILTER ' // Version)
           Return
        End If
 
!     Filter input array ROI (output in memory)
        Call MA_MEDFILTER (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, x_size, y_size, max_sort, max_column, xmaxdat, ymaxdat, &
          COLUMN, SORT, KEYS, MDATA, status)
 
!     Free dynamic work arrays
        Deallocate (COLUMN)
        Deallocate (SORT)
        Deallocate (KEYS)
 
     End If
 
     End Subroutine F2D_MEDIANFILTER
!********1*********2*********3*********4*********5*********6*********7*********8

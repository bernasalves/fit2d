!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_arrays.f90 *
!  *                *
!  ******************
 
!+ F2D_ARRAYS - FIT 2-D ARRAYS: allocate memory for arrays
     Subroutine F2D_ARRAYS (xmaxdat, ymaxdat, &
       memory_exist, variances_exist, shared_memory, memory_id, &
       mask_exist, &
       pDATA, pMASK, pXAXIS, pYAXIS, pVARIANCES, pMDATA, pMXAXIS, pMYAXIS, &
       pMVARIANCES, results, status)
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
!    09-Dec-2014: V0.5 Make result vectors a data structure (Hammersley)
!    21-Sep-2011: V0.4 Add "main" and "memory" data structures (Hammersley)
!    09-Mar-2004: V0.3 Always allocate a mask array (Hammersley)
!    18-Nov-2003: V0.2 Add mask support (Hammersley)
!    12-Nov-2003: V0.1 Original (Hammersley)
!  Modules:
     Use IO_LIB
     Use FIT2D_LIB
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
!     Integer, Intent(IN) :: max_vec_values ! First dimension of "VECTORS"
!      array (maximum number of values which can be stored in a vector)
!     Integer, Intent(IN) :: max_vectors ! Second dimension of "VECTORS"
!      array (maximum number of vectors which can be stored)
     Logical, Intent(IN) :: memory_exist ! .True. if memory array exists
     Logical, Intent(IN) :: variances_exist ! .True., if a data variance
!      array is created
     Logical, Intent(IN) :: shared_memory ! .True., if shared memory is to used
     Integer, Intent(IN) :: memory_id ! Identifier of shared memory (if used)
!  Import/Export:
!  Export:
!     Type(IMAGE_DATA), Intent(OUT) :: main ! Main data-set
!     Type(IMAGE_DATA), Intent(OUT) :: memory ! Memory data-set
     Logical, Intent(OUT) :: mask_exist ! .True. if mask array exists
     Integer, Intent(OUT) :: pDATA ! Pointer to dynamic array "DATA"
     Integer, Intent(OUT) :: pMASK ! Pointer to dynamic array "MASK"
     Integer, Intent(OUT) :: pXAXIS ! Pointer to dynamic array "XAXIS"
     Integer, Intent(OUT) :: pYAXIS ! Pointer to dynamic array "YAXIS"
     Integer, Intent(OUT) :: pVARIANCES ! Pointer to dynamic array
!      "VARIANCES"
     Integer, Intent(OUT) :: pMDATA ! Pointer to dynamic array "MDATA"
     Integer, Intent(OUT) :: pMXAXIS ! Pointer to dynamic array "MXAXIS"
     Integer, Intent(OUT) :: pMYAXIS ! Pointer to dynamic array "MYAXIS"
     Integer, Intent(OUT) :: pMVARIANCES ! Pointer to dynamic array
!      "MVARIANCES"
     Type(RESULT_VECTORS), Intent(OUT) :: results ! Result vector
!      used to store vectors of 1-D values
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: i ! Loop variable
     Integer :: iostat ! Fortran Allocate return status variable
     Character(Len = 80) :: message ! User messages
     Integer :: retstat ! Return status variable
     Integer :: stat ! Status return variable for "Allocate"
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If
 

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Arguments would appear to be reasonable, go ahead.


!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ARRAYS: Start'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!  Always used a mask
     mask_exist = .True.
 
     If (shared_memory) Then
 
        Write (message, '(''NOTE: Memory ID = '', i10)') memory_id
        Call IO_WRITE (message, status)
 
!     Allocate memory in shared memory segment
        Call F2D_SHAREDARRAYSC (xmaxdat, ymaxdat, memory_exist, &
          variances_exist, shared_memory, memory_id, mask_exist, pDATA, pMASK, &
          pXAXIS, pYAXIS, pVARIANCES, pMDATA, pMXAXIS, pMYAXIS, pMVARIANCES, &
          retstat)
 
     Else

!     Code for data-structures**************************************************
!        main%dim%x = xmaxdat
!        main%dim%y = ymaxdat
!        Allocate (main%DATA(xmaxdat, ymaxdat), Stat = iostat)
!        If (iostat .Ne. 0) Then
!           status = St_mod_fit2d + St_bad_malloc
!           Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!           Return
!        End If
!        Allocate (main%XAXIS(xmaxdat), Stat = iostat)
!        If (iostat .Ne. 0) Then
!           status = St_mod_fit2d + St_bad_malloc
!           Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!           Return
!        End If
!        Allocate (main%YAXIS(ymaxdat), Stat = iostat)
!        If (iostat .Ne. 0) Then
!           status = St_mod_fit2d + St_bad_malloc
!           Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!           Return
!        End If
!        Allocate (main%MASK(xmaxdat, ymaxdat), Stat = iostat)
!        If (iostat .Ne. 0) Then
!           status = St_mod_fit2d + St_bad_malloc
!           Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!           Return
!        End If
!        main%size%x = 0
!        main%size%y = 0
!        main%roi%x_start = 0
!        main%roi%y_start = 0
!        main%roi%x_end = 0
!        main%roi%y_end = 0
!        main%title = 'Main Data Array'
!        main%x_label = 'Pixels'
!        main%y_label = 'Pixels'
!        main%z_label = 'Intensity'

!        If (memory_exist) Then
!           memory%dim%x = xmaxdat
!           memory%dim%y = ymaxdat
!           Allocate (memory%DATA(xmaxdat, ymaxdat), Stat = iostat)
!           If (iostat .Ne. 0) Then
!              status = St_mod_fit2d + St_bad_malloc
!              Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!              Return
!           End If
!           Allocate (memory%XAXIS(xmaxdat), Stat = iostat)
!           If (iostat .Ne. 0) Then
!              status = St_mod_fit2d + St_bad_malloc
!              Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!              Return
!           End If
!           Allocate (memory%YAXIS(ymaxdat), Stat = iostat)
!           If (iostat .Ne. 0) Then
!              status = St_mod_fit2d + St_bad_malloc
!              Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!              Return
!           End If
!           Allocate (memory%MASK(xmaxdat, ymaxdat), Stat = iostat)
!           If (iostat .Ne. 0) Then
!              status = St_mod_fit2d + St_bad_malloc
!              Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!              Return
!           End If
!           memory%size%x = 0
!           memory%size%y = 0
!           memory%roi%x_start = 0
!           memory%roi%y_start = 0
!           memory%roi%x_end = 0
!           memory%roi%y_end = 0
!           main%title = 'Second Data Array'
!           main%x_label = 'Pixels'
!           main%y_label = 'Pixels'
!           main%z_label = 'Intensity'
!        End If
!
!        If (variances_exist) Then
!
!           main%variances_exist = .True.
!           Allocate (main%VARIANCES(xmaxdat, ymaxdat), Stat = iostat)
!           If (iostat .Ne. 0) Then
!              status = St_mod_fit2d + St_bad_malloc
!              Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!              Return
!           End If
! 
!           If (memory_exist) Then
!
!              memory%variances_exist = .True.
!              Allocate (memory%VARIANCES(xmaxdat, ymaxdat), Stat = iostat)
!              If (iostat .Ne. 0) Then
!                 status = St_mod_fit2d + St_bad_malloc
!                 Call ST_SAVE ('Subroutine F2D_ARRAYS' // Version)
!                 Return
!              End If
! 
!           End If
! 
!        End If
 

!     End of code for data-structures*******************************************

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ARRAYS: Before IO_MALLOC'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Use malloc to allocate ordinary memory
        Call IO_MALLOC (xmaxdat * ymaxdat * 4, pDATA, status)
        Call IO_MALLOC (xmaxdat * 4, pXAXIS, status)
        Call IO_MALLOC (ymaxdat * 4, pYAXIS, status)
        Call IO_MALLOC (xmaxdat * ymaxdat, pMASK, status)
 
        If (memory_exist) Then
           Call IO_MALLOC (xmaxdat * ymaxdat * 4, pMDATA, status)
           Call IO_MALLOC (xmaxdat * 4, pMXAXIS, status)
           Call IO_MALLOC (ymaxdat * 4, pMYAXIS, status)
        End If
 
        If (variances_exist) Then
           Call IO_MALLOC (xmaxdat * ymaxdat * 4, pVARIANCES, status)
 
           If (memory_exist) Then
              Call IO_MALLOC (xmaxdat * ymaxdat * 4, pMVARIANCES, status)
           End If
 
        End If
 
     End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ARRAYS: Before MA_L1VALUE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Initialise mask to all good elements
     Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xmaxdat, ymaxdat, .False., &
       %val(pMASK), status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ARRAYS: Before Vectors allocated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!  Create "VECTORS" array
!    Call IO_MALLOC (max_vec_values * max_vectors * 4, pVECTORS, status)

     results%max_values = 3000
     results%max_vectors = 100
     results%num_vectors = 0

     Allocate (results%VECTORS(results%max_values, results%max_vectors), &
       Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_ma + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If

     Allocate (results%STARTS(results%max_vectors), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_ma + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If

     Allocate (results%ENDS(results%max_vectors), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_ma + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If

     Allocate (results%TITLES(results%max_vectors), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_ma + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_ARRAYS ' // Version)
        Return
     End If

!  Initialise vectors array
     results%VECTORS = 0.0
     results%num_vectors = 0
 
!  Set out of range limits to defined vector ranges
     Do i = 1, results%max_vectors
        results%STARTS(i) = results%max_values + 1
        results%ENDS(i) = 0
     End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_ARRAYS: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End Subroutine F2D_ARRAYS
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_cyclicshift.f90 *
!  *                     *
!  ***********************
 
!+ F2D_CYCLICSHIFT - Fit 2-D CYCLIC SHIFT arrays
     Subroutine F2D_CYCLICSHIFT (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, retstat, memory_exist, MXAXIS, &
       MYAXIS, MDATA, MASK, MVARIANCES, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, &
       mxlabel, mylabel, mzlabel, status)
!  Description:
!    Cyclically shifts rows of "DATA(xmaxdat, ymaxdat)" in the region
!    from "(xstrelm, ystrelm)" to "(xendelm, yendelm)", output in
!    "MDATA". i.e. "DATA(x, y)" becomes "MDATA(y, x)". If variances
!    exist they too are tranposed.
!  Method:
!    Fortran array operations
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Dec-2014: V0.1 Original, based on "F2D_TRANSPOSE" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be transposed
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance array to be
!      transposed
     Integer, Intent(IN) :: xnumdat ! Total number of defined data elements
!      in the X-direction
     Integer, Intent(IN) :: ynumdat ! Total number of defined data elements
!      in the Y-direction
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Character(Len = *), Intent(IN) :: title ! Title of data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Logical, Intent(IN) :: variances_exist ! .True., if variances arrays exist
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status, operation performed correctly
!      1 = Bad status, operation not performed due to wrong ROI and array shape
     Logical, Intent(OUT) :: memory_exist ! .True., if the memory has been
!      defined
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Averaged X-axis values
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Averaged Y-axis values
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Result of transposing
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be affected, .True. = masked/bad data point
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Transposed variance
!      array
     Integer, Intent(OUT) :: mxnumdat ! Total number of defined data
!      elements in the X-direction for memory array
     Integer, Intent(OUT) :: mynumdat ! Total number of defined data
!      elements in the Y-direction for memory array
     Integer, Intent(OUT) :: mxstrelm ! X-Start of transposed output region
     Integer, Intent(OUT) :: mystrelm ! Y-Start of transposed output region
     Integer, Intent(OUT) :: mxendelm ! X-End of transposed output region
     Integer, Intent(OUT) :: myendelm ! Y-End of transposed output region
     Character(Len = *), Intent(OUT) :: mtitle ! Title of data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for memory data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for memory data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for memory data
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: stat ! Status return variable for "Allocate"
     Integer, Save :: y_shift = 1 ! Shift applied to rows
     Integer :: y_positive ! Equivalent positive shift
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(3) ! User messages
     Logical*1, Allocatable :: TMP_MASK(:, :) ! Temporary mask for storing 
!      cyclic shift
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CYCLICSHIFT: Start'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CYCLICSHIFT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_CYCLICSHIFT ' // Version)

     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Entered F2D_CYCLICSHIFT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Shift in Y-direction
        Call GS_INPI (.True., -(yendelm - ystrelm), yendelm - ystrelm, .True., &
          'NUMBER OF ROWS TO SHIFT', 1, &
          'Number of rows to shift vertically (cyclic)', 1, &
          'Enter a valid integer within given range', y_shift, status)

!     Check for cancel
        If (status .Eq. St_escapevalue) Then
           Call ST_DEF_SYSTEM (status)
           Return
        End If

!     Calculate equivalent positive shift, if negative
        If (y_shift .Lt. 0) Then
           y_positive = y_shift + (yendelm - ystrelm + 1)
        Else
           y_positive = y_shift
        End If

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CYCLICSHIFT: Before MA_RMOVE'')')
!        Write (*, '(''y_positive = '', i8)') y_positive
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Shift data
        Call MA_RMOVE (xmaxdat, ymaxdat, DATA, &
          xstrelm, ystrelm, xendelm, yendelm - y_positive, xmaxdat, ymaxdat, &
          xstrelm, ystrelm + y_positive, MDATA, status)

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CYCLICSHIFT: After first MA_RMOVE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        Call MA_RMOVE (xmaxdat, ymaxdat, DATA, xstrelm, &
          yendelm - y_positive + 1, xendelm, yendelm, xmaxdat, ymaxdat, &
          xstrelm, ystrelm, MDATA, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''F2D_CYCLICSHIFT: After MA_RMOVE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Allocate temporary mask
        Allocate (TMP_MASK(xmaxdat, yendelm), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_ma + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CYCLICSHIFT ' // Version)
           Return
        End If

!     Shift mask
        Call MA_L1MOVE (xmaxdat, ymaxdat, MASK, &
          xstrelm, ystrelm, xendelm, yendelm - y_positive, xmaxdat, ymaxdat, &
          xstrelm, ystrelm + y_positive, TMP_MASK, status)
        Call MA_L1MOVE (xmaxdat, ymaxdat, MASK, xstrelm, &
          yendelm - y_positive + 1, xendelm, yendelm, xmaxdat, ymaxdat, &
          xstrelm, ystrelm, TMP_MASK, status)
        Call MA_L1COPY (xmaxdat, yendelm, TMP_MASK, &
          xstrelm, ystrelm, xendelm, yendelm, xmaxdat, ymaxdat, MASK, status)

        Deallocate (TMP_MASK)

        If (variances_exist) Then

!        Shift variances
           Call MA_RMOVE (xmaxdat, ymaxdat, VARIANCES, &
             xstrelm, ystrelm, xendelm, yendelm - y_positive, &
             xmaxdat, ymaxdat, xstrelm, ystrelm + y_positive, MVARIANCES, &
             status)
           Call MA_RMOVE (xmaxdat, ymaxdat, VARIANCES, &
             xstrelm, yendelm - y_positive + 1, xendelm, yendelm, &
             xmaxdat, ymaxdat, xstrelm, ystrelm, MVARIANCES, status)
 
        End If
 
!     Set character strings
        mtitle = title
        mxlabel = ylabel
        mylabel = xlabel
        mzlabel = zlabel
 
!     Swap axis values
        Call MA_RCOPY (xmaxdat, 1, XAXIS, 1, 1, xendelm, 1, xmaxdat, 1, &
          MXAXIS, status)
        Call MA_RCOPY (ymaxdat, 1, YAXIS, 1, 1, yendelm, 1, ymaxdat, 1, &
          MYAXIS, status)
 
!     Set memory defined data
        mxstrelm = xstrelm
        mystrelm = ystrelm
        mxendelm = xendelm
        myendelm = yendelm
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!     Set memory to be defined if everything is O.K.
        If (status .Eq. St_goodvalue) Then
           memory_exist = .True.
           retstat = 0
        End If
 
     End If
 
     End Subroutine F2D_CYCLICSHIFT
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

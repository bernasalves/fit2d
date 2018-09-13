!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_transpose.f90 *
!  *                   *
!  *********************
 
!+ F2D_TRANSPOSE - Fit 2-D  TRANSPOSE data arrays
     Subroutine F2D_TRANSPOSE (gui, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       VARIANCES, xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, title, &
       xlabel, ylabel, zlabel, variances_exist, retstat, memory_exist, MXAXIS, &
       MYAXIS, MDATA, MASK, MVARIANCES, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, &
       mtitle, mxlabel, mylabel, mzlabel, status)
!  Description:
!    Transposes elements of "DATA(xmaxdat, ymaxdat)" in the region
!    from "(xstrelm, ystrelm)" to "(xendelm, yendelm)", output in
!    "MDATA". i.e. "DATA(x, y)" becomes "MDATA(y, x). If variances
!    exist they too are tranposed.
!  Method:
!    Uses "MA_TRANSPOSE" to perform the transposing.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Jan-2015: V0.6 Add "MASK" (hammersley)
!    25-Nov-1998: V0.5 Use "GS_FWARNING" and remove call to "GS_INQ_PROMPT" 
!      (Hammersley)
!    04-Sep-1997: V0.4 Add return status variable to indicate whether or not the
!      operation was successful. Use graphics window for warning messages if the
!      GUI is being used (Hammersley)
!    01-Aug-1997: V0.3 Swap axis labels (Hammersley)
!    08-Nov-1995: V0.2 Correct bug which stops this working properly (the 
!      memory axis arrays were not being defined) (Hammersley)
!    24-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
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
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! Data mask
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
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
!  Local Variables:
     Integer :: stat ! Status return variable for "Allocate"
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(3) ! User messages
     Logical*1, Allocatable :: TMP_MASK(:, :) ! Temporary mask
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_TRANSPOSE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_TRANSPOSE ' // Version)
 
     Else If (yendelm .Gt. xmaxdat) Then
 
        If (gui) Then
 
           MESSAGE(1) = 'The operation is not possible because the'
           MESSAGE(2) = 'second dimension of the work arrays is not'
           MESSAGE(3) = 'large enough to contain the transposed data'
           Call GS_FWARNING (3, 3, MESSAGE, status)
 
        Else
           Call IO_WRITE ('WARNING: The operation is not ' // &
             'possible because the second dimension of', status)
           Call IO_WRITE ('         the work arrays is not ' // &
             'large enough to contain the transposed', status)
           Call IO_WRITE ('         X-limit of the ROI', status)
        End If
 
        retstat = 1
 
     Else If (xendelm .Gt. ymaxdat) Then
 
        If (gui) Then
 
           MESSAGE(1) = 'The operation is not possible because the'
           MESSAGE(2) = 'first dimension of the work arrays is not'
           MESSAGE(3) = 'large enough to contain the transposed data'
           Call GS_FWARNING (3, 3, MESSAGE, status)
 
        Else
           Call IO_WRITE ('WARNING: The operation is not ' // &
             'possible because the first dimension of', status)
           Call IO_WRITE ('         the work arrays is not ' // &
             'large enough to contain the transposed', status)
           Call IO_WRITE ('         Y-limit of the ROI', status)
        End If
 
        retstat = 1
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Transpose data
        Call MA_RTRANSPOSE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
          yendelm, xmaxdat, ymaxdat, MDATA, mxstrelm, mystrelm, mxendelm, &
          myendelm, status)

!     Allocate memory for temporary mask
        Allocate (TMP_MASK(xmaxdat, ymaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_TRANSPOSE ' // Version)
           Return
        End If

!     Copy mask to "TMP_MASK"
        Call MA_L1COPY (xmaxdat, ymaxdat, MASK, xstrelm, ystrelm, xendelm, &
          yendelm, xmaxdat, ymaxdat, TMP_MASK, status)

!     Transpose mask
        Call MA_L1TRANSPOSE (xmaxdat, ymaxdat, TMP_MASK, &
          xstrelm, ystrelm, xendelm, yendelm, xmaxdat, ymaxdat, &
          MASK, mxstrelm, mystrelm, mxendelm, myendelm, status)

!     Free memory
        Deallocate (TMP_MASK)

        If (variances_exist) Then
 
!        Transpose data variances by set amount (no normalisation)
           Call MA_RTRANSPOSE (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
             xendelm, yendelm, xmaxdat, ymaxdat, MVARIANCES, mxstrelm, &
             mystrelm, mxendelm, myendelm, status)
 
        End If
 
!     Set character strings
        mtitle = title
        mxlabel = ylabel
        mylabel = xlabel
        mzlabel = zlabel
 
!     Swap axis values
        Call MA_RCOPY (xmaxdat, 1, XAXIS, 1, 1, xendelm, 1, ymaxdat, 1, &
          MYAXIS, status)
        Call MA_RCOPY (ymaxdat, 1, YAXIS, 1, 1, yendelm, 1, xmaxdat, 1, &
          MXAXIS, status)
 
!     Set memory defined data
        mxnumdat = mxendelm
        mynumdat = myendelm
 
!     Set memory to be defined if everything is O.K.
        If (status .Eq. St_goodvalue) Then
           memory_exist = .True.
           retstat = 0
        End If
 
     End If
 
     End Subroutine F2D_TRANSPOSE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

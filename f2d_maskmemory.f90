!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_maskmemory.f90 *
!  *                    *
!  **********************
 
!+ F2D_MASKMEMORY - FIT 2-D transfer MASK to MEMORY
     Subroutine F2D_MASKMEMORY (title, xlabel, ylabel, zlabel, xmaxdat, &
       ymaxdat, xnumdat, ynumdat, XAXIS, YAXIS, MASK, mtitle, mxlabel, &
       mylabel, mzlabel, mxstrelm, mystrelm, mxendelm, myendelm, mxnumdat, &
       mynumdat, MXAXIS, MYAXIS, MEMORY, status)
!  Description:
!    Sets whole of memory region out to "xnumdat, ynumdat" elements with 1.0, if
!    the element is masked, and 0.0 otherwise.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Mar-1995: V0.1 Original, (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis data values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
!  Import/Export:
!  Export:
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MXAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MYAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MEMORY(xmaxdat, ymaxdat) ! Memory data
!      
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKMEMORY ' // Version)
        Return
     End If
 
!  Check that the input arguments are reasonable
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat.Le.0) Then
        status = St_bad_dim2
     Else If (xnumdat .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ynumdat .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
 
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_MASKMEMORY ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Transfer mask values to memory
        Do y = 1, ynumdat
 
           Do x = 1, xnumdat
 
              If (MASK(x, y)) Then
                 MEMORY(x, y) = 1.0
              Else
                 MEMORY(x, y) = 0.0
              End If
 
           End Do
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Set character labels, axis values, etc.
        mtitle = 'Data Mask'
        mxlabel = xlabel
        mylabel = ylabel
        mzlabel = '(1.0 equals masked element)'
        mxstrelm = 1
        mystrelm = 1
        mxendelm = xnumdat
        myendelm = ynumdat
        mxnumdat = xnumdat
        mynumdat = ynumdat
        Call MA_RCOPY (xmaxdat, 1, XAXIS, 1, 1, xnumdat, 1, xmaxdat, 1, &
          MXAXIS, status)
        Call MA_RCOPY (ymaxdat, 1, YAXIS, 1, 1, ynumdat, 1, ymaxdat, 1, &
          MYAXIS, status)
 
     End If
 
     End Subroutine F2D_MASKMEMORY
!********1*********2*********3*********4*********5*********6*********7*********8

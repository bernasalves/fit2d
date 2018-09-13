!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_exchange.f90 *
!  *                  *
!  ********************
 
!+ F2D_EXCHANGE - FIT 2-D EXCHANGE current data with memory
     Subroutine F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
       xlabel, ylabel, zlabel, variances_exist, data_defined, x_pixel_size, &
       y_pixel_size, xstrelm, ystrelm, xendelm, yendelm, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
       mzlabel, memory_defined, mx_pixel_size, my_pixel_size, status)
!  Description:
!    Exchange arrays (by pointers) and auxillary information for current data 
!    array and "memory"
!
!    Note the pointers to the main program arrays are obtained through named 
!    common
!  Keywords:
!    Exchange.Arrays
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Jan-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointer to program arrays
!  Import/Export:
     Integer, Intent(INOUT) :: xmaxdat ! X-direction dimension for data arrays
     Integer, Intent(INOUT) :: ymaxdat ! Y-direction dimension for data arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements in Y-direction
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Logical, Intent(INOUT) :: variances_exist ! .True., if the variances exist
     Logical, Intent(INOUT) :: data_defined ! .True. if the current data is
!      defined
     Real, Intent(INOUT) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(INOUT) :: mynumdat ! Defines Y-extent of data region
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: mxstrelm ! Start X-element of memory data region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Start Y-element of memory data region
     Character(Len = *), Intent(INOUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(INOUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(INOUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(INOUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(INOUT) :: memory_defined ! .True. if the memory contains 
!      data
     Real, Intent(INOUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(INOUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXCHANGE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_EXCHANGE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE
!     Exchange current data with data in memory
!     Call MA_RSWAP (f2d_xmaxdat, f2d_ymaxdat,
!     :       f2d_xmaxdat, f2d_ymaxdat, 1, 1,
!     :       Max(f2d_xnumdat,mxnumdat), Max(f2d_ynumdat, mynumdat),
!     :       %val(pDATA), %val(pMDATA), status)
!     Call MA_RSWAP (f2d_xmaxdat, 1, f2d_xmaxdat, 1, 1, 1,
!     :       Max(f2d_xnumdat, mxnumdat), 1, %val(pXAXIS),
!     :       %val(pMXAXIS), status)
!     Call MA_RSWAP (f2d_ymaxdat, 1, f2d_ymaxdat, 1, 1, 1,
!     :       Max(f2d_ynumdat, mynumdat), 1, %val(pYAXIS),
!     :       %val(pMYAXIS), status)
!     If (variance_array) Then
!     Call MA_RSWAP (f2d_xmaxdat, f2d_ymaxdat,
!     :          f2d_xmaxdat, f2d_ymaxdat, 1, 1,
!     :          Max(f2d_xnumdat,mxnumdat), Max(f2d_ynumdat, mynumdat),
!     :          %val(pVARIANCES), %val(pMVARIANCES), status)
!     End If
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE
 
!**NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE
!     Swap pointers (much more efficient, but only works if pointers are 
!     used and can be swapped, so old code should be preserved as comments
        Call IO_ISWAP (pDATA, pMDATA, status)
        Call IO_ISWAP (pXAXIS, pMXAXIS, status)
        Call IO_ISWAP (pYAXIS, pMYAXIS, status)
        Call IO_ISWAP (pVARIANCES, pMVARIANCES, status)
!**NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE***NEW-CODE
 
!     Swap region limits
        Call IO_ISWAP (xnumdat, mxnumdat, status)
        Call IO_ISWAP (ynumdat, mynumdat, status)
        Call IO_ISWAP (xstrelm, mxstrelm, status)
        Call IO_ISWAP (ystrelm, mystrelm, status)
        Call IO_ISWAP (xendelm, mxendelm, status)
        Call IO_ISWAP (yendelm, myendelm, status)
        Call IO_CSWAP (title, mtitle, status)
        Call IO_CSWAP (xlabel, mxlabel, status)
        Call IO_CSWAP (ylabel, mylabel, status)
        Call IO_CSWAP (zlabel, mzlabel, status)
        Call IO_RSWAP (x_pixel_size, mx_pixel_size, status)
        Call IO_RSWAP (y_pixel_size, my_pixel_size, status)
        Call IO_LSWAP (data_defined, memory_defined, status)
 
     End If
 
     End Subroutine F2D_EXCHANGE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

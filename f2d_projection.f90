!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_projection.f90 *
!  *                    *
!  **********************
 
!+ F2D_PROJECTION - FIT2D: PROJECTION of region onto a 1-D line
     Subroutine F2D_PROJECTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, x_pixel_size, y_pixel_size, mxnumdat, &
       mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, mystrelm, &
       mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, memory_defined, &
       mx_pixel_size, my_pixel_size, status)
!  Description:
!    Graphical input of two points defining a line, and two more points defining
!    the width of the region to projection onto the line. The 1-D projection is 
!    calculated and displayed as an X/Y graph.
!  Keywords:
!    Slice.Calculation/Display, Cut~2-D~image.Calculation/Display
!    Projection.Calculate/Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Apr-1998: V0.7 Changes to call to "F2D_INP_PROJECTION" (Hammersley)
!    26-Aug-1996: V0.6 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.5 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    16-Jan-1996: V0.4 De-bugging new bug ! (Hammersley)
!    10-Dec-1995: V0.3 Interactive control of X/Y graph (Hammersley)
!    26-Oct-1995: V0.2 Use "spy-glass" during coordinate input (Hammersley)
!    27-Sep-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! Defines starting X-point of ROI
     Integer, Intent(IN) :: ystrelm ! Defines starting Y-point of ROI
     Integer, Intent(IN) :: xendelm ! Defines end X-point of ROI
     Integer, Intent(IN) :: yendelm ! Defines end Y-point of ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Real, Intent(IN) :: x_pixel_size ! Size of one unit (raw pixel) in
!      X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of one unit (raw pixel) in
!      Y-direction (metres)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data
!      X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data
!      Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Slice data
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Variances of slice
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory is defined
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer :: retstat ! Return status variable from "MA_PROJECTION":
!      0 = good status
!      1 = Array not large enough for whole slice
     Integer stat ! Status return variable for "Allocate"
!  Local Arrays:
     Real, Allocatable :: WORK(:) ! Pointer to dynamically allocated work array
     Real :: X_REGION(5) ! X-coordinates of projection region
     Real :: Y_REGION(5) ! X-coordinates of projection region
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_PROJECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PROJECTION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_PROJECTION ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Define projection region
        Call F2D_INP_PROJECTION (.False., .False., dummy, dummy, xmaxdat, &
          ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, &
          dummy, title, xlabel, ylabel, zlabel, x_pixel_size, y_pixel_size, &
          X_REGION, Y_REGION, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Allocate memory for work array
        Allocate (WORK(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTION ' // Version)
           Return
        End If
 
!     Output patience message
        Call GS_FPROMPT ( 1, 1, &
          'CALCULATING PROJECTION (this may take a while)', status)
        Call GS_UPDATE (status)
 
!     Calculate slice through data
        Call MA_PROJECTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, VARIANCES, x_pixel_size, y_pixel_size, X_REGION, &
          Y_REGION, 0, variances_exist, (x_pixel_size + y_pixel_size) / 2.0, &
          xmaxdat, retstat, mxnumdat, MX_AXIS, MDATA, MVARIANCES, WORK, status)
 
!     Free memory for work array
        Deallocate (WORK)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_PROJECTION: After MA_PROJECTION'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     New title
        mtitle = '1-D Projection: ' // Trim(title)
        mxlabel = 'Pixels'
        mylabel = 'Not defined'
        mzlabel = zlabel
 
        Call IO_WRITE ('INFO: The "Projection" is stored in the memory', &
          status)
 
!     Display slice as an X/Y graph including errors if present
        If (variances_exist) Then
 
!        Set the graph to display Y-symmetric error bars only
           Call GS_SET_ERRORBARS (.False., .True., status)
 
        Else
 
!        Set no error bar output
           Call GS_SET_ERRORBARS (.False., .False., status)
 
        End If
 
!     Display data
        Call GS_INT_XYEGRAPH (xmaxdat, 1, 1, 1, mxnumdat, MX_AXIS, MDATA, &
          dummy, dummy, MVARIANCES, MVARIANCES, mtitle, mxlabel, mzlabel, &
          status)
 
!     Set memory variables
        memory_defined = .True.
        mynumdat = 1
        mxstrelm = 1
        mystrelm = 1
        mxendelm = mxnumdat
        myendelm = mynumdat
        mx_pixel_size = x_pixel_size
        my_pixel_size = y_pixel_size
        MY_AXIS(1) = 1.0

     End If
 
     End Subroutine F2D_PROJECTION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8

!  *****************
!  *               *
!  * f2d_slice.f90 *
!  *               *
!  *****************
 
!+ F2D_SLICE - FIT2D: input, calculate and display SLICE
     Subroutine F2D_SLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
!  Description:
!    Graphical input of two points defining a slice through a 2-D region. The
!    slice is calculated and displayed as an X/Y graph
!  Keywords:
!    Slice.Calculation/Display, Cut~2-D~image.Calculation/Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Aug-1996: V0.7 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.6 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    10-Dec-1995: V0.5 Interactive control of X/Y graph (Hammersley)
!    24-Oct-1995: V0.4 Output spy-glass (Hammersley)
!    26-Jun-1995: V0.3 Create slice in memory (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    08-Dec-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: xnumdat ! Number of data elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of data elements in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! The estimated
!      variance values
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
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat, ymaxdat) ! Slice data
     Real, Intent(OUT) :: MVARIANCES(xmaxdat, ymaxdat) ! Variances of slice data
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data region
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Logical, Intent(OUT) :: memory_defined ! .True. if the memory contains data
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
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: retstat ! Return status variable from "MA_SLICE":
!      0 = good status
!      1 = Array not large enough for whole slice
     Real :: xstrco ! X-coordinate of starting point of slice
     Real :: ystrco ! Y-coordinate of starting point of slice
     Real :: xendco ! X-coordinate of end point of slice
     Real :: yendco ! Y-coordinate of end point of slice
!  Local Arrays:
     Real :: X_COORDINATES(2) ! X-coordinates of input limits
     Real :: Y_COORDINATES(2) ! Y-coordinates of input limits
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SLICE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_SLICE ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input 2 coordinates for limits of new ROI
        num_coordinates = 2
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'ENTER ENDS OF SLICE (TWO COORDINATES)', 1, &
          'Click on 2 X/Y coordinates, to define slice', .True., 2, &
          num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Convert data coordinates to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(1), xstrco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(1), ystrco, &
          status)
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(2), xendco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(2), yendco, &
          status)
 
!     Calculate slice through data
        Call MA_SLICE (xmaxdat, ymaxdat, DATA, VARIANCES, xstrco, ystrco, &
          xendco, yendco, 0, variances_exist, xmaxdat, retstat, mxnumdat, &
          MX_AXIS, MDATA, MVARIANCES, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''F2D_SLICE: After MA_SLICE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     New title
        mtitle = '1-D Slice: ' // Trim(title)
        mxlabel = 'Pixels'
        mylabel = 'Not defined'
        mzlabel = zlabel
 
        Call IO_WRITE ('INFO: The "Slice" is stored in the memory', status)
 
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
 
     End Subroutine F2D_SLICE
!********1*********2*********3*********4*********5*********6*********7*********8

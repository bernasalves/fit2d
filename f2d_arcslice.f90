!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_arcslice.f90 *
!  *                  *
!  ********************
 
!+ F2D_ARCSLICE - FIT2D: input, calculate and display ARC SLICE
     Subroutine F2D_ARCSLICE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, xnumdat, ynumdat, X_AXIS, Y_AXIS, DATA, VARIANCES, title, &
       xlabel, ylabel, zlabel, variances_exist, x_pixel_size, y_pixel_size, &
       mxnumdat, mynumdat, MX_AXIS, MY_AXIS, MDATA, MVARIANCES, mxstrelm, &
       mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, mzlabel, &
       memory_defined, mx_pixel_size, my_pixel_size, status)
!  Description:
!    Graphical input of three points defining an arc slice through a 2-D region.
!    The slice is calculated and displayed as an X/Y graph.
!  Keywords:
!    Slice.Calculation/Display, Cut~2-D~image.Calculation/Display
!  Method:
!    "MA_CAL_CENTRE" is used to calculate the centre of the circle and 
!    "MA_ARCSLICE" is used to calculate the values of the slice.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Aug-1996: V0.8 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.7 Changes to "GS_INPS_COORDINATES"!    (Hammersley)
!    10-Dec-1995: V0.6 Interactive control of X/Y graph (Hammersley)
!    26-Oct-1995: V0.5 Output "spy-glass" during coordinate input (Hammersley)
!    12-Sep-1995: V0.4 Obtain GUI region (Hammersley)
!    26-Jun-1995: V0.3 Store arc slice in the memory (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    09-Dec-1993: V0.1 Original, based on "F2D_SLICE" (Hammersley)
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
     Real, Intent(IN) :: VARIANCES(xmaxdat, ymaxdat) ! Variance values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: variances_exist ! .True., if the variances exist
     Real, Intent(IN) :: x_pixel_size ! Size of one unit (raw) in X-direction
!      (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of one unit (raw) in Y-direction
!      (metres)
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
     Real, Intent(OUT) :: MX_AXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(OUT) :: MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(OUT) :: MDATA(xmaxdat*ymaxdat) ! Slice data
     Real, Intent(OUT) :: MVARIANCES(xmaxdat*ymaxdat) ! Variances of slice data
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
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: retstat ! Return status variable from "MA_ARCSLICE":
!      0 = good status
!      1 = Array not large enough for whole slice
     Real :: radius ! Radius of arc
     Real :: xcentre ! X-coordinate of centre of arc
     Real :: xendco ! X-coordinate of end point of slice
     Real :: xmidco ! X-coordinate of a point on the arc slice
     Real :: xstrco ! X-coordinate of starting point of slice
     Real :: ycentre ! Y-coordinate of centre of arc
     Real :: yendco ! Y-coordinate of end point of slice
     Real :: ymidco ! Y-coordinate of a point on the arc slice
     Real :: ystrco ! Y-coordinate of starting point of slice
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User help messages
     Real :: X_COORDINATES(3) ! X-coordinates of points on arc
     Real :: Y_COORDINATES(3) ! Y-coordinates of points on arc
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ARCSLICE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ARCSLICE ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Input 3 coordinates for limits and curvature of ROI
        num_coordinates = 3
        MESSAGE(1) = 'In order to define an arc, three coordinates must'
        MESSAGE(2) = 'be input in the following order: starting point,'
        MESSAGE(3) = 'a point on the arc, and the end point. Note that'
        MESSAGE(4) = 'changing the order of the first and third point'
        MESSAGE(5) = 'will result in the 1-D slice being the opposite'
        MESSAGE(6) = 'way round.'
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'ENTER ARC THREE COORDINATES (START, MIDDLE, END)', 6, MESSAGE, &
          .False., 3, num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
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
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(2), xmidco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(2), ymidco, &
          status)
        Call MA_DC2PIXC (xmaxdat, xnumdat, X_AXIS, X_COORDINATES(3), xendco, &
          status)
        Call MA_DC2PIXC (ymaxdat, ynumdat, Y_AXIS, Y_COORDINATES(3), yendco, &
          status)
 
!     Calculate centre of arc
        Call MA_CAL_CIRCENTRE (xstrco, ystrco, xmidco, ymidco, xendco, yendco, &
          retstat, xcentre, ycentre, radius, status)
 
!     Calculate arc slice through data
        Call MA_ARCSLICE (xmaxdat, ymaxdat, DATA, VARIANCES, xstrco, ystrco, &
          xmidco, ymidco, xendco, yendco, xcentre, ycentre, 0, &
          variances_exist, xmaxdat, retstat, mxnumdat, MX_AXIS, MDATA, &
          MVARIANCES, status)
 
        mtitle = 'Arc Slice: ' // Trim(title)
        mxlabel = 'Pixels'
        mylabel = 'Not defined'
        mzlabel = zlabel
 
        Call IO_WRITE ('INFO: The "Arc-Slice" is stored in the memory', &
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
 
     End Subroutine F2D_ARCSLICE
!********1*********2*********3*********4*********5*********6*********7********8


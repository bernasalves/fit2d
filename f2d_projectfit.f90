!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_projectfit.f90 *
!  *                    *
!  **********************
 
!+ F2D_PROJECTFIT - FIT2D: PROJECTION of region onto a 1-D line and FIT about 
!  symmetry point to give beam centre
     Subroutine F2D_PROJECTFIT (mask_data, xmaxdat, ymaxdat, DATA, MASK, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, experiment, status)
!  Description:
!    Graphical input of two points defining a line, and two more
!    points defining the width of the region to projection onto the
!    line. The 1-D projection is calculated and displayed as an X/Y
!    graph. This is fitted to give a symmetry centre
!  Keywords:
!    Slice.Calculation/Display, Cut~2-D~image.Calculation/Display
!    Projection.Calculate/Display
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.5 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    15-Mar-2006: V0.4 Calculate beam centre in pixel coordinates (Hammersley)
!    13-Mar-2006: V0.3 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    28-May-1998: V0.2 Re-draw 2-D image after 1-D projection (Hammersley)
!    25-May-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc'
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! Defines starting X-point of ROI
     Integer, Intent(IN) :: ystrelm ! Defines starting Y-point of ROI
     Integer, Intent(IN) :: xendelm ! Defines end X-point of ROI
     Integer, Intent(IN) :: yendelm ! Defines end Y-point of ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
!  Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer element ! Loop variable for elements
     Integer :: end_projection ! End element of projection to use
     Integer :: num_projection ! Number of elements in the projection
     Integer :: retstat ! Return status variable from 'MA_PROJECTION':
!      0 = good status
!      1 = Array not large enough for whole slice
     Integer stat ! Status return variable for "Allocate"
     Integer :: str_projection ! First element of projection to use
     Real :: distance ! Distance in pixels of projection
     Real :: symmetry ! Symmetry point
     Real :: x_end ! X-coordinate of centre of end of projected region
     Real :: x_start ! X-coordinate of centre of start of projected region
     Real :: x_symmetry ! X-coordinate of 2-D symmetry point
     Real :: xmaxdddr ! The maximum X-coordinate for the displayed
!      data display region
     Real :: xmindddr ! The minimum X-coordinate for the displayed
!      data display region
     Real :: y_end ! Y-coordinate of centre of end of projected region
     Real :: y_start ! Y-coordinate of centre of start of projected region
     Real :: y_symmetry ! Y-coordinate of 2-D symmetry point
     Real :: ymaxdddr ! The maximum Y-coordinate for the displayed
!      data display region
     Real :: ymindddr ! The minimum Y-coordinate for the displayed
!      data display region
!  Local Arrays:
     Real, Allocatable :: AXIS(:) ! Dynamically allocated axis array
     Logical*1, Allocatable :: MASK1D(:) ! Dynamically allocated 1-D mask array
     Real, Allocatable :: PROJECTION(:) ! Dynamically allocated projection array
     Real, Allocatable :: SMOOTHED(:) ! Dynamically allocated smoothed 
!      projection array
     Real, Allocatable :: WORK(:) ! Dynamically allocated work array
     Real :: X_REGION(5) ! X-coordinates of projection region
     Real :: Y_REGION(5) ! X-coordinates of projection region
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_PROJECTFIT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!     Define projection region
        Call F2D_INP_PROJECTION (.False., .False., dummy, dummy, xmaxdat, &
          ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, &
          dummy, title, xlabel, ylabel, zlabel, &
          experiment%x_pixel_size, experiment%y_pixel_size, &
          X_REGION, Y_REGION, status)
 
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Allocate memory for projection, axis, mask, and work array
        Allocate (PROJECTION(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
           Return
        End If
        Allocate (SMOOTHED(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
           Return
        End If
        Allocate (AXIS(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
           Return
        End If
        Allocate (MASK1D(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
           Return
        End If
        Allocate (WORK(xmaxdat), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_PROJECTFIT ' // Version)
           Return
        End If

!     Output patience message
        Call GS_FPROMPT ( 1, 1, &
          'CALCULATING PROJECTION (this may take a while)', status)
        Call GS_UPDATE (status)
 
!     Calculate slice through data
        Call MA_PROJECTION (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, dummy, &
          experiment%x_pixel_size, experiment%y_pixel_size, X_REGION, &
          Y_REGION, 0, .False., &
          (experiment%x_pixel_size + experiment%y_pixel_size) / 2.0, xmaxdat, &
          retstat, num_projection, AXIS, PROJECTION, dummy, WORK, status)
 
!     Free memory for work array
        Deallocate (WORK)
 
!     Smooth data
        Call MA_TOPHATCON (xmaxdat, 1, PROJECTION, 1, 1, &
          num_projection, 1, 5, 1, .False., xmaxdat, 1, SMOOTHED, status)
 
!     Initialise mask to all good elements
        Call MA_L1VALUE (xmaxdat, 1, 1, 1, xmaxdat, 1, .False., MASK1D, status)
 
!     Display projection array
        Call GS_MPLOT (.False., xmaxdat, 1, SMOOTHED, MASK1D, &
          AXIS, dummy, 1, 1, num_projection, 1, 'Projection', 'Pixels', &
          ylabel, zlabel, status)
 
!     Interactive masking
        str_projection = 1
        end_projection = num_projection
        Call F2D_1DMASK (.True., xmaxdat, 1, num_projection, 1, &
          SMOOTHED, AXIS, dummy, 'MASK OUT POINTS TO IGNORE', &
          'Pixels', ylabel, zlabel, experiment, &
          str_projection, 1, end_projection, 1, MASK1D, status)
 
!     Output user message
        Call GS_FPROMPT (1, 1, 'OPTIMISING SYMMETRY POINT', status)
        Call GS_UPDATE (status)
 
!     Optimise symmetry point
        Call F2D_LSQ1DSYMMETRY (xmaxdat, str_projection, end_projection, &
          SMOOTHED, MASK1D, symmetry, status)
 
!     Draw symmetry line
        Call GS_LINESTYLE (4, 1.0, Gs_red, status)
        Call LG_CLIP (.True., status)
        Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
        Call GS_LINE (symmetry, xmindddr, symmetry, xmaxdddr, status)
 
!     Reflect axis values
        Do element = str_projection, end_projection
           AXIS(element) = symmetry + (symmetry - (Real(element) - 0.5))
        End Do
 
!     Draw reflected coordinates
        Call GS_SET_CURVEMARKERS (3, 1, Gs_blue, 1.0, status)
        Call GS_MARKERS (xmaxdat, str_projection, end_projection, AXIS, &
          SMOOTHED, 3, status)
        Call LG_CLIP (.False., status)
 
!     Wait until user clicks to continue
        Call GS_CONTINUE (1, 1, 'CLICK TO CONTINUE', status)
 
!     Re-display main data array
        Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
          Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
          zlabel, status)
 
!     Calculate equivalent coordinate in the 2-D image
        x_start = (X_REGION(1) + X_REGION(4)) / 2.0
        y_start = (Y_REGION(1) + Y_REGION(4)) / 2.0
        x_end = (X_REGION(2) + X_REGION(3)) / 2.0
        y_end = (Y_REGION(2) + Y_REGION(3)) / 2.0
        distance = Sqrt (((x_end - x_start) * experiment%x_pixel_size)**2 + &
          ((y_end - y_start) * experiment%y_pixel_size)**2) / &
          (0.5 * (experiment%x_pixel_size + experiment%y_pixel_size))
        distance = Int(distance) + 1.0
 
        x_symmetry = x_start + (x_end - x_start) * symmetry / distance
        y_symmetry = y_start + (y_end - y_start) * symmetry / distance
 
!     Convert to data coordinates
        Call MA_PC2DC (xmaxdat, xendelm, X_AXIS, x_symmetry, &
          experiment%x_beam, status)
        Call MA_PC2DC (ymaxdat, yendelm, Y_AXIS, y_symmetry, &
          experiment%y_beam, status)
 
!     Allow user to change beam centre by keyboard input
        Call F2D_INP_BEAMCENTRE (.True., experiment%x_beam, experiment%y_beam, &
          status)
        experiment%beam_centre_set = .True.

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''1-D symmetry point = '', g14.7)') symmetry
!     Write (*, '(''Symmetry point = '', 2g14.7)')
!     :       x_symmetry, y_symmetry
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Free projection and axis arrays
        Deallocate (PROJECTION)
        Deallocate (SMOOTHED)
        Deallocate (MASK1D)
        Deallocate (AXIS)
 
     End If
 
     End Subroutine F2D_PROJECTFIT
!********1*********2*********3*********4*********5*********6*********7*********8

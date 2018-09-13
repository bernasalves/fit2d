!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  *  f2d_surface.f90 *
!  *                  *
!  ********************
 
!+ F2D_SURFACE - FIT 2-D SURFACE interpolation
     Subroutine F2D_SURFACE (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       memory_defined, SURFACE, status)
!  Description:
!    1. Outputs ROI as pixel image
!    2. The user inputs a number of points to define "knot"
!    points of interpolating surface.
!    3. Calculates interpolated surface in "SURFACE"
!  Keywords:
!    Surface~Interpolation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    05-Apr-2006: V0.6 Remove calls to gui region (Hammersley)
!    16-Dec-1996: V0.5 Avoid open strings crossing lines (Hammersley)
!    30-Nov-1996: V0.4 Changes to "MA_SURFACE" due to use
!      of "SURFIT" to fit a bi-cubic spline to the surface (Hammersley)
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    12-Nov-1993: V0.2 Change "MA_DC2PIXEL" to "MA_DC2PIXC" (Hammersley)
!    22-Dec-1992: V0.1 Original, based on "FIT2SUR" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
!  Export:
     Logical, Intent(OUT) :: memory_defined ! Set to .True., if the surface
!      is fitted and calculated, otherwise left unchanged
     Real, Intent(OUT) :: SURFACE(xmaxdat, ymaxdat) ! The interpolated
!      surfaced at all points in the ROI
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.6' ! Version number
     Integer, Parameter :: Max_coordinates = 500 ! Dimension of coordinate array
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: dummy ! Variable for argument list
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: retstat ! Return status:
!      0 = Good status
!      1 = Bad status, problem fitting coordinates, surface not calculated
     Integer :: x ! Array element position for X-direction
     Integer :: xend ! End X-element for intensity averaging
     Integer :: xstr ! Start X-element for intensity averaging
     Integer :: y ! Array element position for Y-direction
     Integer :: yend ! End Y-element for intensity averaging
     Integer :: ystr ! Start Y-element for intensity averaging
     Real :: chi_squared ! Chi squared of surface to given coordinates
     Real, Save :: rms_misfit = 100.0 ! Required maximum RMS discrepancy
!      between the entered coordinate surface value and the fitted surface
     Real :: sum ! Sum of intensity in region
     Real :: x_pixel ! X-coordinate, convert to pixel coordinates
     Real :: y_pixel ! Y-coordinate, convert to pixel coordinates
!  Local Arrays:
     Real :: WEIGHTS(Max_coordinates) ! Weight of each data point
     Real :: X_COORDINATES(Max_coordinates) ! X-coordinates of "knot" points
     Real :: Y_COORDINATES(Max_coordinates) ! Y-coordinates of "knot" points
     Real :: Z_COORDINATES(Max_coordinates) ! Z-coordinates of "knot" points
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SURFACE ' // Version)
        Return
     End If
 
!  Check that the input variables are reasonable
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_SURFACE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Issue user warning
        Call IO_WRITE (' ', status)
        Call IO_WRITE ('NOTE: The surface fitting method ' // &
          'has been changed since Version 8.*', status)
        Call IO_WRITE ('      (Previously polynomials were ' // &
          'used, and now bi-cubic splines are', status)
        Call IO_WRITE ('      used. Please examine results carefully.)', &
          status)
        Call IO_WRITE (' ', status)
 
!     Create image
        Call GS_2DIMAGE (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
          ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
!     Output user message
        Call IO_WRITE ('INFO: Control passed to graphics window', status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input coordinates for surface interpolation
        num_coordinates = 0
        Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'CLICK ON COORDINATES, DEFINING THE SURFACE', 1, &
          'Click on X/Y coordinates, defining the surface', .False., &
          max_coordinates, num_coordinates, X_COORDINATES, Y_COORDINATES, &
          status)
 
!     Output prompt message
        Call GS_FPROMPT (1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', status)
        Call GS_UPDATE (status)
 
        Do coordinate = 1, num_coordinates
 
!        Convert data coordinates to pixel coordinates
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, &
             X_COORDINATES(coordinate), x_pixel, status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, &
             Y_COORDINATES(coordinate), y_pixel, status)
 
           x = Int(x_pixel) + 1
           y = Int(y_pixel) + 1
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_pixel = '', g,'' y_pixel = '', g)')
!        :                x_pixel, y_pixel
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Intensity = '', g)') intensity
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Calculate average intensity based on 3*3 square around the pixel
           xstr = Max(1, x - 1)
           xend = Min(xendelm, x + 1)
           ystr = Max(1, y - 1)
           yend = Min(yendelm, y + 1)
           sum = 0.0
 
           Do y = ystr, yend
 
              Do x = xstr, xend
                 sum = sum + DATA(x, y)
              End Do
 
           End Do
 
           Z_COORDINATES(coordinate) = sum / Real((xend - xstr + 1) * &
             (yend - ystr + 1))
 
        End Do
 
!     Input required closeness of fit
        Call IO_INPR (.True., 0.0, 1.7e38, .True., 'RMS CLOSENESS OF FIT', 1, &
          'Enter required maximum RMS discrepancy between data and fit', &
          1, 'Enter real number', rms_misfit, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Form interpolation surface
        Call MA_SINTERPOLATE (0, .False., rms_misfit, Max_coordinates, &
          num_coordinates, X_COORDINATES, Y_COORDINATES, Z_COORDINATES, &
          xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, X_AXIS, &
          Y_AXIS, WEIGHTS, retstat, chi_squared, SURFACE, status)
 
        If (retstat .Eq. 0) Then
           memory_defined = .True.
 
           Write (message, &
             '(''INFO: Achieved RMS closeness of fit = '', g14.7)') &
             Sqrt(chi_squared / Real(num_coordinates))
           Call IO_WRITE (message, status)
 
        End If
 
     End If
 
     End Subroutine F2D_SURFACE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

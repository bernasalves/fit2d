!********1*********2*********3*********4*********5*********6*********7**

!  ********************
!  *                  *
!  * f2d_inpradia.f90 *
!  *                  *
!  ********************

!+ F2D_INPRADIA -  INPut RADIA of powder rings to fit
     Subroutine F2D_INPRADIA (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       full_info, x_pixel_size, y_pixel_size, x_beam, y_beam, radius1, &
       radius2, angle1, half_search_distance, Max_radia, num_radia, RADIA, &
       status)
!  Description:
!    Graphical input of radia of powder rings to be used for fitting
!    beam centre and tilt.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    15-Mar-2006: V0.8 Change to definition of beam centre (Hammersley)
!    16-Dec-1996: V0.7 Avoid open strings crossing lines (Hammersley)
!    26-Aug-1996: V0.6 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.5 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    26-Oct-1995: V0.4 Output "spy-glass" during coordinate input (Hammersley)
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    29-Jan-1995: V0.2 Output graphically the initial ring used (Hammersley)
!    25-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Logical, Intent(IN) :: full_info ! .True., if full information is
!      required by the user
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the X-direction
!      (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the Y-direction
!      (metres)
     Real, Intent(IN) :: x_beam ! X-coordinate of beam centre (pixels)
     Real, Intent(IN) :: y_beam ! Y-coordinate of beam centre (pixels)
     Real, Intent(IN) :: radius1 ! Radius of previously input powder ring
!      (metres)
     Real, Intent(IN) :: radius2 ! Second radius of ellipse (metres)
     Real, Intent(IN) :: angle1 ! Orientation angle of first axis of best
!      fit ellipse
     Real, Intent(IN) :: half_search_distance ! Distance in metres for pixel
!      search around each powder ring for calculating the profile
     Integer, Intent(IN) :: Max_radia ! Dimension size of 'RADIA' array
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: num_radia ! Number of radia entered for further
!      powder rings to be used in the tilt / beam centre fitting
     Real, Intent(OUT) :: RADIA(Max_radia) ! The radius of powder rings to
!      be used for fitting the tilt and beam centre (metres)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_coordinates = 14 ! Dimension of coordinate arrays
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: num_coordinates ! Number of entered graphical coordinates
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User help text
     Real :: X_COORDINATES(Max_coordinates) ! X-coordinates of input points on
!      rings
     Real :: Y_COORDINATES(Max_coordinates) ! Y-coordinates of input points on
!      rings
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INPRADIA ' // Version)
        Return
     End If
 
!  Check input arguments
     If (Max_radia .Le. 0) Then
        status = St_bad_dim1
     Else If (radius1 .Le. 0.0) Then
        status = St_bad_real1
     End If

!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_INPRADIA ' // Version)
     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Draw ellipse on top of graphics
        Call F2D_ELLIPSE (x_pixel_size, y_pixel_size, x_beam, y_beam, radius1, &
          radius2, angle1, Gs_red, status)

!     Draw inner ellipse on top of graphics
        Call F2D_ELLIPSE (x_pixel_size, y_pixel_size, x_beam, y_beam, radius1 &
          - half_search_distance, radius2 - half_search_distance, angle1, &
          Gs_white, status)

!     Draw outer ellipse on top of graphics
        Call F2D_ELLIPSE (x_pixel_size, y_pixel_size, x_beam, y_beam, radius1 &
          + half_search_distance, radius2 + half_search_distance, angle1, &
          Gs_white, status)

!     Input coordinates of other radia
        num_coordinates = 0
        MESSAGE(1) = 'Click on any other rings to be used in fitting'
        MESSAGE(2) = 'the tilt / beam centre.  It is possible to'
        MESSAGE(3) = 'continue without further rings, but generally'
        MESSAGE(4) = 'it is better to use several rings.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'CLICK ON OTHER RINGS TO USE IN FITTING TILT', 4, &
          MESSAGE, .False., Max_coordinates, num_coordinates, X_COORDINATES, &
          Y_COORDINATES, status)

!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           Return
        End If

!     Convert to metre units
        Do coordinate = 1, num_coordinates
           X_COORDINATES(coordinate) = X_COORDINATES(coordinate) * &
             x_pixel_size
           Y_COORDINATES(coordinate) = Y_COORDINATES(coordinate) * &
             y_pixel_size
        End Do

!     Store previously input radius as first radius in ring file
        RADIA(1) = radius1

!     Calculate radia
        Do coordinate = 1, num_coordinates
           RADIA(coordinate + 1) = &
             Sqrt( (X_COORDINATES(coordinate) - x_beam * x_pixel_size)**2 &
             + (Y_COORDINATES(coordinate) - y_beam * y_pixel_size)**2)
        End Do
 
!     Set total number of known radia
        num_radia = num_coordinates + 1

!     Output radia to user if in full information mode
        If (full_info) Then
           Call IO_WRITE (' ', status)

           Do coordinate = 1, num_radia
              Write (MESSAGE(1), '(''INFO: Radius of powder ring '', i2, ' // &
                ''' (mm) = '', f10.3)') coordinate, RADIA(coordinate) * 1000.0
              Call IO_WRITE (MESSAGE(1), status)
           End Do
 
           Do coordinate = 1, num_radia
              Write (MESSAGE(1), '(''INFO: Radius of powder ring '', i2, ' // &
                ''' (X-pixels) = '', f10.3)') coordinate, RADIA(coordinate) / &
                x_pixel_size
              Call IO_WRITE (MESSAGE(1), status)
           End Do

        End If

     End If

     End Subroutine F2D_INPRADIA
!********1*********2*********3*********4*********5*********6*********7**

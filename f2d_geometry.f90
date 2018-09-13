!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ********************
!  *                  *
!  * f2d_geometry.f90 *
!  *                  *
!  ********************
 
!+ F2D_GEOMETRY: define experimental GEOMETRY
     Subroutine F2D_GEOMETRY (gui, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, &
       experiment, status)
!  Description:
!    User input of experimental geometry
!  Keywords:
!    Geometry.Experimental.Define, Define.Geometry.Experimental,
!    Input.Geometry.Experimental
!  Method:
!    Stores geometry parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    30-Aug-2006: V0.10 Add rotation about beam (Hammersley)
!    14-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    11-Feb-1996: V0.8 Choice of GUI (Hammersley)
!    03-Mar-1995: V0.7 Dummy variable added to "F2D_BEAMCENTRE" argument 
!      list (Hammersley)
!    24-Jan-1995: V0.6 Input beam centre using "F2D_BEAMCENTRE" (Hammersley)
!    21-Jan-1995: V0.5 Enter X and Y pixel sizes separately (Hammersley)
!    16-Aug-1994: V0.4 Option to find centre from three or more
!    coordinates on circle (least squares solution) (Hammersley)
!    14-Mar-1994: V0.3 Remember last input method (Hammersley)
!    23-Apr-1993: V0.2 Detector rotation included (Hammersley)
!    09-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'f2d_fitcircle.inc' ! Circle least squares fitting common
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
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
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable for 'F2D_BEAMCENTRE'
     Integer :: method_used ! The input method that was used to
!      specify the beam centre:
!      1 = Keyboard,            only 'x_beam, y_beam' are defined
!      2 = Single cursor point,   "      "       "     "     "
!      3 = Symmetric points,      "      "       "     "     "
!      4 = Points on circle,  'radius1' and 'radial_error' are defined
!      5 = Points on ellipse, 'radius1', radius2','angle' and
!          'radial_error' are defined
     Integer :: retstat ! Return status variable
     Logical, Save :: first = .True. ! .True., if first call to subroutine
     Real :: angle1 ! Orientation angle of first axis of best fit ellipse
     Real :: radial_error ! Estimated average error (radially) in
!    coordinate positions
     Real :: radius1 ! First axis radius of best fit ellipse
     Real :: radius2 ! Second axis radius of best fit ellipse
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GEOMETRY ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_GEOMETRY ' // Version)
     Else
 
!     Enter beam centre on detector
        Call F2D_BEAMCENTRE (gui, .False., xmaxdat, ymaxdat, DATA, dummy, &
          X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
          ylabel, zlabel, .True., method_used, &
          experiment, radius1, radius2, angle1, radial_error, status)
 
        If (gui) Then
 
           Call F2D_GUI_EXPERIMENT (.False., experiment, status)
 
        Else
 
!        Input wavelength
           Call F2D_INP_WAVELENGTH (gui, experiment%wavelength, status)
           experiment%wavelength_set = .True.

!        Input sample to detector distance
           Call F2D_INP_SAMPLEDISTANCE (gui, experiment%detector_distance, &
             status)
           experiment%detector_distance_set = .True.

!        Input X/Y sizes of one pixel
           Call F2D_INP_PIXELSIZES (gui, experiment%x_pixel_size, &
             experiment%y_pixel_size, status)
           experiment%pixel_sizes_set = .True.

!        Rotation angle of tilt plane and tilt angle of detector
           Call F2D_INP_DETECTORTILT (gui, experiment%tilt_plane_rotation, &
             experiment%tilt_angle, status)
           experiment%tilt_set = .True.

!        Rotation angle of detector relative to ideal
           experiment%detector_rotation = &
             experiment%detector_rotation * 180.0 / Pi
           Call IO_INPR (.True., -360.0, 360.0, .True., &
             'ROTATION OFFSET ANGLE OF DETECTOR (DEGREES)', 1, &
             'Rotation angle FROM ideal X-axis (synchrotron ' // &
             'plane) TO actual detector X-axis', 1, &
             'Must be within given range', experiment%detector_rotation, status)
           experiment%detector_rotation = &
             experiment%detector_rotation * Pi / 180.0
 
        End If
 
     End If
 
     End Subroutine F2D_GEOMETRY
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_inp_ellipse.f90 *
!  *                     *
!  ***********************
 
!+ F2D_INP_ELLIPSE -  TILT/ beam CENTRE determination
     Subroutine F2D_INP_ELLIPSE (full_info, xmaxdat, ymaxdat, xstrelm, &
       ystrelm, xendelm, yendelm, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, &
       ylabel, zlabel, experiment, retstat, &
       radius1, radius2, angle1, half_search_distance, status)
!  Description:
!    Initialise input and fitting an ellipse to define beam centre
!    and ellipse parameters
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Mar-2006: V0.14 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.13 Avoid open strings crossing lines (Hammersley)
!    22-Oct-1996: V0.12 Cope with beam centre not being found (Hammersley)
!    27-Sep-1996: V0.11 Make sure that the beam centre doesn't change
!      if "NO CHANGE" or "KEYBOARD" input is used (Hammersley)
!    26-Aug-1996: V0.10 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.9 Changes to "GS_INPS_FCOORDINATES" (Hammersley)
!    13-Feb-1996: V0.8 Take account of new beam centre input method: 2-D 
!      Gaussian fitting (Hammersley)
!    04-Feb-1996: V0.7 Changes to "F2D_BEAMCENTRE" for GUI (Hammersley)
!    26-Oct-1995: V0.6 Output "spy-glass" during coordinate input (Hammersley)
!    20-Jun-1995: V0.5 Convert to GS graphics library (Hammersley)
!    03-Mar-1995: V0.4 Plot masked out regions (Hammersley)
!    22-Feb-1995: V0.3 Don't output limit ellipses graphically (Hammersley)
!    29-Jan-1995: V0.2 Display ellipse (Hammersley)
!    25-Jan-1995: V0.1 Original, extracted from "F2D_TILTCENTRE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'f2d_fitcircle.inc' ! Circle least squares
!    fitting common
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: full_info ! .True., if full output information is to
!      be produced
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
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Integer, Intent(OUT) :: retstat ! Routine status return variable:
!      0 = Good status, return arguments defined
!      -1 = User escape or other 'status' problem
     Real, Intent(OUT) :: radius1 ! First radius of ellipse (metres)
     Real, Intent(OUT) :: radius2 ! Second radius of ellipse (metres)
     Real, Intent(OUT) :: angle1 ! Orientation angle of first axis of best
!      fit ellipse
     Real, Intent(OUT) :: half_search_distance ! Distance in metres for pixel
!      search around each powder ring for calculating the profile
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.14' ! Version number
!  Local Variables:
     Integer :: method_used ! The input method that was used to specify the 
!      beam centre:
!        1 = Keyboard,            only "x_beam, y_beam" are defined
!        2 = Single cursor point,   "      "       "     "     "
!        3 = Symmetric points,      "      "       "     "     "
!        4 = Points on circle,  "radius1" and "radial_error" are defined
!        5 = Points on ellipse, "radius1", radius2", "angle" and
!            "radial_error" are defined
!        6 = Fitted 2-D Gaussian
     Integer :: num_coordinates ! Number of user input coordinates
     Real :: radial_error1 ! Estimated average error (radially) in coordinate 
!      positions (metres)
     Real :: x_dc ! X-direction data coordinate (metres)
     Real :: y_dc ! Y-direction data coordinate (metres)
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(11) ! User help text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_ELLIPSE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_INP_ELLIPSE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     At present need to draw image
        Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
          Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
          zlabel, status)
 
!     By default set good return status
        retstat = 0
 
!     Enter initial estimate of beam centre on detector
        Call F2D_BEAMCENTRE (.True., .True., xmaxdat, ymaxdat, DATA, MASK, &
          X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
          ylabel, zlabel, full_info, method_used, experiment, &
          radius1, radius2, angle1, radial_error1, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           retstat = -1
           Return
        End If
 
!     Check for bad centre
        If (method_used .Eq. -1) Then
           retstat = -1
           Return
        End If
 
!     Special code is necessary depending on the type of method
!     used to input the beam centre
        If (method_used .Le. 1) Then
 
!        The image has not been produced: draw image
           Call GS_2DMIMAGE (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
             Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
             ylabel, zlabel, status)
 
        End If
 
        If (method_used .Le. 3 .Or. method_used .Eq. 6) Then
 
!        Need to enter coordinate on powder ring
           num_coordinates = 1
           MESSAGE(1) = 'Click on initial powder ring to be used for'
           MESSAGE(1) = 'determining the tilt/ beam centre.'
           Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, &
             title, xlabel, ylabel, zlabel, &
             'CLICK ON POWDER RING TO FIT', 1, &
             MESSAGE, .False., 1, num_coordinates, x_dc, y_dc, status)
 
!        Check for user escape
           If (status .Ne. St_goodvalue) Then
              Return
           End If
 
           radius1 = Sqrt( &
             ((x_dc - experiment%x_beam) * experiment%x_pixel_size)**2 + &
             ((y_dc - experiment%y_beam) * experiment%y_pixel_size)**2)
           radius2 = radius1
           angle1 = 0.0
 
        Else If (method_used .Eq. 4) Then
 
!        Need to set second radius and angle
           radius2 = radius1
           angle1 = 0.0
        End If
 
!     Draw ellipse on top of graphics
        Call F2D_ELLIPSE (experiment%x_pixel_size, experiment%y_pixel_size, &
          experiment%x_beam, experiment%y_beam, &
          radius1, radius2, angle1, Gs_red, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input limits of radial search of powder rings
        num_coordinates = 1
        MESSAGE(1) = 'Click on a coordinate to define one side of'
        MESSAGE(2) = 'theprofile search limit. The search limit will'
        MESSAGE(3) = 'be symmetric either side of ring positions.'
        MESSAGE(4) = 'Ideally the whole of the choosen powder rings'
        MESSAGE(5) = 'should lie within the search annuli. Similarly'
        MESSAGE(6) = 'other rings should not enter the search region.'
        MESSAGE(7) = 'However a double ring which lies completely'
        MESSAGE(8) = 'within the search region may be fitted'
        MESSAGE(9) = 'successfully provided that the intensities of'
        MESSAGE(10) = 'the two components are constant around the'
        MESSAGE(11) = 'azimuth or vary together.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'CLICK ON RING PROFILE SEARCH LIMIT', 11, MESSAGE, &
          .False., 1, num_coordinates, x_dc, y_dc, status)

!     Check for user escape
        If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        half_search_distance = Abs( Sqrt( &
          ((x_dc - experiment%x_beam) * experiment%x_pixel_size)**2 + &
          ((y_dc - experiment%y_beam) * experiment%y_pixel_size)**2) - radius1)
 
        If (full_info) Then

           Call IO_WRITE (' ', status)
           Write (MESSAGE(1), '(''INFO: The search distance ' // &
             'either side of the powder rings (mm) = '', f7.3)') &
             half_search_distance * 1000.0
           Call IO_WRITE (MESSAGE(1), status)
           Write (MESSAGE(1), '(''INFO: The search distance ' // &
             'either side of the powder rings (X-pixels) = '', f7.2)') &
             half_search_distance / experiment%x_pixel_size
           Call IO_WRITE (MESSAGE(1), status)
        End If
 
     End If
 
     End Subroutine F2D_INP_ELLIPSE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 
 
 
 
 

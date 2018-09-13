!*********1*********2*********3*********4*********5*********6*********7**
 
!  *******************
!  *                 *
!  * f2d_pixelxy.f90 *
!  *                 *
!  *******************
 
!+ F2D_PIXELXY - FIT 2-D PIXEL (X/Y) coordinate and values
     Subroutine F2D_PIXELXY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, EXPERIMENT, status)
!  Description:
!    The user inputs a number of coordinates, the coordinates
!    are returned, as is the value at the point, the average
!    value about the point.
!  Keywords:
!    Pixel.Information, Coordinate.Values
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    13-Mar-2006: V0.11 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.10 Avoid open strings crossing lines (Hammersley)
!    27-Aug-1996: V0.9 Don't change message style (Hammersley)
!    26-Aug-1996: V0.8 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.7 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    30-Jan-1996: V0.6 Remove redundant call to "GS_INQ_DDR" (Hammersley)
!    26-Oct-1995: V0.5 Use "spy-glass" output during coordinate input 
!      (Hammersley)
!    20-Jun-1995: V0.4 Convert to GS graphics library (Hammersley)
!    24-Jan-1995: V0.3 Input pixel sizes from arguments (Hammersley)
!    09-Dec-1993: V0.2 Reduce size of message area (Hammersley)
!    16-Nov-1993: V0.1 Original, based on "F2D_PEEP" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics System constants
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
     TYPE(EXPERIMENTAL_DETAILS), Intent(IN) :: EXPERIMENT ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer :: num_coordinates ! Number of entered coordinates
     Integer :: num_message ! Number of lines to output in message
     Integer :: x ! Array element position for X-direction
     Integer :: xend ! End X-element for intensity averaging
     Integer :: xstr ! Start X-element for intensity averaging
     Integer :: y ! Array element position for Y-direction
     Integer :: yend ! End Y-element for intensity averaging
     Integer :: ystr ! Start Y-element for intensity averaging
     Logical :: continue ! .True., whilst more coordinates to input
     Real :: angle ! Angle of Bragg reflection
     Real :: d_spacing ! Bragg d spacing assuming n=1
     Real :: intensity ! Intensity at input point
     Real :: radial_distance ! Distance radially from beam centre
     Real :: sum ! Sum of intensity in region
     Real :: x_dc ! X-data coordinate
     Real :: x_pixel ! X-pixel coordinate
     Real :: y_dc ! Y-data coordinate
     Real :: y_pixel ! Y-pixel coordinate
     Real :: xmax_prompt ! The maximum X-coordinate for the GUI region
     Real :: xmin_prompt ! The minimum X-coordinate for the GUI region
     Real :: xmaxmes ! Maximum X-coordinate of message region
     Real :: xminmes ! Minimum X-coordinate of message region
     Real :: ymax_prompt ! The maximum Y-coordinate for the GUI region
     Real :: ymin_prompt ! The minimum Y-coordinate for the GUI region
     Real :: ymaxmes ! Maximum Y-coordinate of message region
     Real :: yminmes ! Minimum Y-coordinate of message region
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(8) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PIXELXY ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm.Gt.xmaxdat .Or. xendelm.Lt.xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm.Gt.ymaxdat .Or. yendelm.Lt.ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PIXELXY ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Arguments would appear to be reasonable, go ahead.
 
!     Save current message region
        Call GS_INQ_MESSAGE (xminmes, yminmes, xmaxmes, ymaxmes, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Find out current GUI prompt region
        Call GS_INQ_GUIREGION (xmin_prompt, ymin_prompt, xmax_prompt, &
          ymax_prompt, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input coordinates until finish or no more space
        continue = .True.
        Do While (continue)
 
!        Input coordinates
           num_coordinates = 0
           MESSAGE(1) = 'Click on coordinate to obtain information'
           MESSAGE(2) = 'on position and intensity. If the ' // 'experimental'
           MESSAGE(3) = 'geometry has been defined, d-spacing ' // &
             'information'
           MESSAGE(4) = 'will also be output.'
           Call GS_INPS_COORDINATES (.False., .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, &
             title, xlabel, ylabel, zlabel, xmin_prompt, ymin_prompt, &
             xmax_prompt, ymax_prompt, 'CLICK ON IMAGE PIXEL', 4, MESSAGE, &
             .False., 1, num_coordinates, x_dc, y_dc, status)
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
              continue = .False.
              num_coordinates = 0
           Else If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Locator position returned
           If (num_coordinates .Eq. 1) Then
 
              Write (MESSAGE(1), '(''Data coordinate ' // &
                '= ('', g12.5,'','', g12.5,'')'')') x_dc, y_dc
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x_dc, x_pixel, &
                status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, y_dc, y_pixel, &
                status)
 
              x = Int(x_pixel) + 1
              y = Int(y_pixel) + 1
 
              Write (MESSAGE(2), '(''Pixel = ('', i5,'','', i5,'')'')') x, y
 
              intensity = DATA(x, y)
 
              Write (MESSAGE(3), '(''Pixel intensity = '', g12.5)') intensity
 
!           Calculate average intensity based on 3*3 square around the
!           pixel
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
 
              Write (MESSAGE(4), '(''Average 3*3 intensity = '', g12.5)') sum &
                / Real((xend - xstr + 1) * (yend - ystr + 1))
 
!           Calculate average intensity based on 5*5 square around
!           the pixel
              xstr = Max(1, x - 2)
              xend = Min(xendelm, x + 2)
              ystr = Max(1, y - 2)
              yend = Min(yendelm, y + 2)
              sum = 0.0
 
              Do y = ystr, yend
 
                 Do x = xstr, xend
                    sum = sum + DATA(x, y)
                 End Do
 
              End Do
 
              Write (MESSAGE(5), '(''Average 5*5 intensity = '', g12.5)') sum &
                / Real((xend - xstr + 1) * (yend - ystr + 1))
              num_message = 5
 
              If (EXPERIMENT%wavelength_set .And. EXPERIMENT%beam_centre_set &
                .And. EXPERIMENT%detector_distance_set) Then
 
!              Calculate distance from beam centre
                 radial_distance = Sqrt( ((x_dc - EXPERIMENT%x_beam) * &
                   EXPERIMENT%x_pixel_size)**2 + &
                   ((y_dc - EXPERIMENT%y_beam) * EXPERIMENT%y_pixel_size)**2)
                 angle = 0.5 * Atan(radial_distance / &
                   EXPERIMENT%detector_distance)
                 d_spacing = EXPERIMENT%wavelength / (2.0 * Sin(angle))
 
                 Write (MESSAGE(6), '(''d spacing (order = 1) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 1.0e10
                 Write (MESSAGE(7), '(''d spacing (order = 2) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 2.0 * 1.0e10
                 Write (MESSAGE(8), '(''d spacing (order = 3) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 3.0 * 1.0e10
                 num_message = 8
 
              End If
 
!           Output text to user
              Call GS_SET_MESSAGE (xminmes, yminmes, xmaxmes, &
                Max(yminmes + 0.1, Min(ymax_prompt, ymaxmes)), status)
              Call GS_MESSAGE (8, num_message, MESSAGE, status)
 
           Else
              continue = .False.
           End If
 
!        Check status
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Reset message region
        Call GS_SET_MESSAGE (xminmes, yminmes, xmaxmes, ymaxmes, status)
 
     End If
 
     End Subroutine F2D_PIXELXY
!********1*********2*********3*********4*********5*********6*********7**

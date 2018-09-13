!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  *  f2d_peep.f90 *
!  *               *
!  *****************
 
!+ F2D_PEEP - FIT 2-D PEEP at pixels
     Subroutine F2D_PEEP (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, VARIANCES, title, xlabel, ylabel, &
       zlabel, variances_exist, experiment, status)
!  Description:
!    1. Outputs ROI as pixel image
!    2. The user inputs a number of coordinates, the coordinates
!    are returned, as is the value at the point, the average
!    value about the point.
!  Keywords:
!    Surface~Interpolation
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    14-Mar-2006: V0.9 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    16-Dec-1996: V0.8 Avoid open strings crossing lines (Hammersley)
!    19-Mar-1996: V0.7 Allow output and control of 1-D X/Y graphs (Hammersley)
!    30-Jan-1996: V0.6 Remove redundant call to "GS_INQ_DDR" (Hammersley)
!    03-Jan-1996: V0.5 Changes necessary for IBM AIX "xlf" compiler (Hammersley)
!    20-Jun-1995: V0.4 Convert to GS graphics library (Hammersley)
!    24-Jan-1995: V0.3 Input pixel sizes from arguments (Hammersley)
!    09-Mar-1993: V0.2 Use "GR_INP_COORDINATE" (Hammersley)
!    21-Jan-1993: V0.1 Original, based on "F2D_SURFACE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
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
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number 
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: num_coordinates ! Number of entered coordinates
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
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEEP ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xendelm .Gt. xmaxdat .Or. xendelm .Lt. xstrelm) Then
        status = St_bad_adr1
     Else If (yendelm .Gt. ymaxdat .Or. yendelm .Lt. ystrelm) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PEEP ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Create image
        Call GS_PLOT (xmaxdat, ymaxdat, DATA, X_AXIS, Y_AXIS, xstrelm, &
          ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input coordinates until finish or no more space
        continue = .True.
        Do While (continue)
 
!        Input coordinates
           num_coordinates = 0
           Call GS_INP_COORDINATES ( 0.0, 0.0, 0.7071, 0.1, &
             'CLICK ON IMAGE PIXEL', 1, 'Click on pixel of interest', .False., &
             1, num_coordinates, x_dc, y_dc, status)
 
!        Locator position returned
           If (num_coordinates .Eq. 1) Then
 
              Write (MESSAGE, '(''Data coordinate ' // &
                '= ('', g12.5,'','', g12.5,'')'')') x_dc, y_dc
              Call IO_WRITE (message, status)
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x_dc, x_pixel, &
                status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, y_dc, y_pixel, &
                status)
 
              x = Int(x_pixel) + 1
              y = Int(y_pixel) + 1
 
              Write (message, '(''Pixel = ('', i5,'','', i5,'')'')') x, y
              Call IO_WRITE (message, status)
 
              intensity = DATA(x, y)
 
              Write (message, '(''Pixel intensity = '', g12.5)') intensity
              Call IO_WRITE (message, status)
 
!           Calculate average intensity based on 3*3 square around the pixel
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
 
              Write (message, '(''Average 3*3 intensity = '', g12.5)') sum / &
                Real((xend - xstr + 1) * (yend - ystr + 1))
              Call IO_WRITE (message, status)
 
!           Calculate average intensity based on 5*5 square around the pixel
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
 
              Write (message, '(''Average 5*5 intensity = '', g12.5)') sum / &
                Real((xend - xstr + 1) * (yend - ystr + 1))
              Call IO_WRITE (message, status)
 
              If (experiment%wavelength_set) Then
 
!              Calculate distance from beam centre
                 radial_distance = Sqrt( ((x_dc - experiment%x_beam) * &
                   experiment%x_pixel_size)**2 + &
                   ((y_dc - experiment%y_beam) * experiment%y_pixel_size)**2)
                 angle = 0.5 * Atan(radial_distance / &
                   experiment%detector_distance)
                 d_spacing = experiment%wavelength / (2.0 * Sin(angle))
 
                 Write (message, '(''d spacing (order = 1) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 1.0e10
                 Call IO_WRITE (message, status)
                 Write (message, '(''d spacing (order = 2) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 2.0 * 1.0e10
                 Call IO_WRITE (message, status)
                 Write (message, '(''d spacing (order = 3) = '', ' // &
                   '1pg10.5, '' Angstroms'')') d_spacing * 3.0 * 1.0e10
                 Call IO_WRITE (message, status)
 
              End If
 
           Else
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_PEEP
!********1*********2*********3*********4*********5*********6*********7*********8

!*********1*********2*********3*********4*********5*********6*********7********8
 
!  *******************
!  *                 *
!  * f2d_numbers.f90 *
!  *                 *
!  *******************
 
!+ F2D_NUMBERS - FIT 2-D NUMBERS output for pixel values
     Subroutine F2D_NUMBERS (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, status)
!  Description:
!    The user inputs a number of coordinates, the pixel values centred
!    on the clicked position are output.
!  Keywords:
!    Pixel.Information, Coordinate.Values
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.5 Avoid open strings crossing lines (Hammersley)
!    21-Oct-1996: V0.4 Make definition of "MESSAGE" consistent (Hammersley)
!    26-Aug-1996: V0.3 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.2 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    04-Feb-1996: V0.1 Original, based on "F2D_PIXELXY" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 7) :: cvalue ! Value converted to character form
     Character(Len = 10) :: evalue ! Value converted to exponential character 
!      form
     Character(Len = 80) :: line ! Line of output
     Character(Len = 80) :: eline ! Line of output for exponentials
     Integer :: dummy ! Dummy variable
     Integer :: num_coordinates ! Number of entered coordinates
     Integer :: x ! Array element position for X-direction
     Integer :: xend ! End X-element for intensity averaging
     Integer :: xstr ! Start X-element for intensity averaging
     Integer :: y ! Array element position for Y-direction
     Integer :: yend ! End Y-element for intensity averaging
     Integer :: ystr ! Start Y-element for intensity averaging
     Logical :: continue ! .True., whilst more coordinates to input
     Logical :: exponential ! .True., if exponential output is required
     Real :: maximum ! Maximum data value
     Real :: minimum ! Minimum data value
     Real :: range ! Range of data values
     Real :: x_dc ! X-data coordinate
     Real :: x_pixel ! X-pixel coordinate
     Real :: y_dc ! Y-data coordinate
     Real :: y_pixel ! Y-pixel coordinate
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User help text
!  External Functions:
     Character(Len = 7), External :: Io_itoc ! Conver integer to a character
!      string
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_NUMBERS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_NUMBERS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Input coordinates until finish or no more space
        continue = .True.
        Do While (continue)
 
!        Input coordinates
           num_coordinates = 0
           MESSAGE(1) = 'Click on centre of region, for which intensity'
           MESSAGE(2) = 'values are to be output. The intensity information'
           MESSAGE(3) = 'of the 11 * 11 region centred on the clicked'
           MESSAGE(4) = 'pixel will be output in the terminal window.'
           MESSAGE(5) = 'When finished click in the text prompt box.'
           Call GS_INPS_FCOORDINATES (.False., .True., xmaxdat, ymaxdat, &
             xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, X_AXIS, Y_AXIS, &
             title, xlabel, ylabel, zlabel, 'CLICK ON IMAGE PIXEL', 5, &
             MESSAGE, .False., 1, num_coordinates, x_dc, y_dc, status)
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
              Return
           Else If (status .Ne. St_goodvalue) Then
              Return
           End If
 
!        Locator position returned
           If (num_coordinates .Eq. 1) Then
 
              Write (MESSAGE(1), '(''INFO: Data coordinate ' // &
                '= ('', g12.5,'','', g12.5,'')'')') x_dc, y_dc
              Call IO_WRITE (MESSAGE(1), status)
 
!           Convert data coordinates to pixel coordinates
              Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, x_dc, x_pixel, status)
              Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, y_dc, y_pixel, status)
 
              x = Int(x_pixel) + 1
              y = Int(y_pixel) + 1
 
              Write (MESSAGE(1), '(''      Pixel = ('', i5,'','', i5,'')'')') &
                x, y
              Call IO_WRITE (MESSAGE(1), status)
 
!           Output 11 x 11 square of values around pixel
              xstr = x - 5
              xend = x + 5
              ystr = y - 5
              yend = y + 5
 
!           Find minimum and maximum values
              minimum = 1.7e38
              maximum = -1.7e38
              Do y = Max(1, ystr), Min(yendelm, yend)
 
                 Do x = Max(1, xstr), Min(xendelm, xend)
 
                    minimum = Min(minimum, DATA(x, y))
                    maximum = Max(maximum, DATA(x, y))
 
                 End Do
 
              End Do
 
!           Calculate range
              range = maximum - minimum
 
!           Calculate if exponent format is required
              exponential = maximum .Gt. 999999.0 .Or. minimum .Lt. -99999.0 &
                .Or. range .Lt. 1.0
 
!           Output numbers
              If (exponential) Then
 
                 Do y = yend, ystr, -1
 
                    If (y .Ge. ystrelm .And. y .Le. yendelm) Then
 
                       line = ' '
                       eline = ' '
                       Do x = xstr, xend
 
                          If (x .Ge. xstrelm .And. x .Le. xendelm) Then
                             Write (evalue, '(1pe10.3)') DATA(x, y)
                             cvalue = ' ' // evalue(1: 6)
                             evalue = '   ' // evalue(7: 10)
                          Else
 
!                          Add a blank
                             cvalue = '      .'
                             evalue = '      .'
                          End If
 
!                       Add text to line
                          If (Len_trim(line) .Gt. 0) Then
                             line = Trim(line) // cvalue
                          Else
                             line = cvalue
                          End If
 
!                       Add exponential text to line
                          If (Len_trim(eline) .Gt. 0) Then
                             eline = Trim(eline) // evalue
                          Else
                             eline = evalue
                          End If
 
                       End Do
 
!                    Output lines
                       Call IO_WRITE (line, status)
                       Call IO_WRITE (eline, status)
 
                    Else
 
!                    Write line of blanks
                       Call IO_WRITE ('      .      .      ' // &
                         '.      .      .      .      .      ' // &
                         '.      .      .      .', status)
 
                    End If
 
                 End Do
 
              Else
 
!              Integer output
                 Do y = yend, ystr, -1
 
                    If (y .Ne. yend) Then
                       Call IO_WRITE (' ', status)
                    End If
                    If (y .Ge. ystrelm .And. y .Le. yendelm) Then
 
                       line = ' '
                       Do x = xstr, xend
 
                          If (x .Ge. xstrelm .And. x .Le. xendelm) Then
                             Write (cvalue, '(i7)') Nint(DATA(x,y))
 
                          Else
 
!                          Add a blank
                             cvalue = '      .'
                          End If
 
!                       Add text to line
                          If (Len_trim(line) .Gt. 0) Then
                             line = Trim(line) // cvalue
                          Else
                             line = cvalue
                          End If
 
                       End Do
 
!                    Output line
                       Call IO_WRITE (line, status)
 
                    Else
 
!                    Write line of blanks
                       Call IO_WRITE ('      .      .      ' // &
                         '.      .      .      .      .      ' // &
                         '.      .      .      .', status)
 
                    End If
 
                 End Do
 
              End If
 
           Else
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_NUMBERS
!*********1*********2*********3*********4*********5*********6*********7********8
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_move.f90 *
!  *              *
!  ****************
 
!+ F2D_MOVE - Fit 2-D  MOVE data
     Subroutine F2D_MOVE (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, VARIANCES, &
       xstrelm, ystrelm, xendelm, yendelm, variances_exist, x_pixel_size, &
       y_pixel_size, OUTXAXIS, OUTYAXIS, OUTPUT, OUTVARIANCES, xstrout, &
       ystrout, xendout, yendout, status)
!  Description:
!    Moves "DATA(xmaxdat,ymaxdat)" in the region "(xstrelm, ystrelm)" to 
!    "(xendelm, yendelm)"
!  Method:
!    Rotation and translate is performed by "MA_RROTATE" which uses 
!    sub-pixelling to give more accurate distribution of intensity from an 
!    input pixel to an output pixel.
!  Deficiencies:
!    If variances exist thet are rotated and rebinned in exactly the same 
!    manner as the data.
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-1998: V0.11 Use "MA_2DTRANSFORM" instead of old "MA_RTRANSFORM" 
!      (Hammersley)
!    16-Dec-1996: V0.10 Avoid open strings crossing lines (Hammersley)
!    17-Apr-1996: V0.9 Default output region to whole array (Hammersley)
!    23-Feb-1996: V0.8 Add time for operation output (Hammersley)
!    21-Feb-1996: V0.7 Option to use exact triangular area re-binning 
!      (Hammersley)
!    07-Sep-1995: V0.6 Change default values and save values between calls 
!      (Hammersley)
!    12-Nov-1993: V0.5 Change "MA_DC2PIXEL" to "MA_DC2PIXC" (Hammersley)
!    23-Aug-1993: V0.4 Output estimated distance error, when two equivalent 
!      coordinate option is being used (Hammersley)
!    30-Apr-1993: V0.3 Allow input of two input coordinates and two output 
!      coordinates to determine translation and rotation (Hammersley)
!    08-Mar-1993: V0.2 Add arbitrary rotation, followed by translation 
!      (Hammersley)
!    01-Mar-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat,ymaxdat) ! Data array to be moved
     Real, Intent(IN) :: VARIANCES(xmaxdat,ymaxdat) ! Variance array to be
!      moved
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Logical, Intent(IN) :: variances_exist ! .True., if variances arrays exist
     Real, Intent(IN) :: x_pixel_size ! Size of a pixel in the current data
!      in the X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a pixel in the current data
!      in the Y-direction (metres)
!  Export:
     Real, Intent(OUT) :: OUTXAXIS(xmaxdat) ! Averaged X-axis values
     Real, Intent(OUT) :: OUTYAXIS(ymaxdat) ! Averaged Y-axis values
     Real, Intent(OUT) :: OUTPUT(xmaxdat, ymaxdat) ! Result of movening
     Real, Intent(OUT) :: OUTVARIANCES(xmaxdat, ymaxdat) ! Moved variance array
     Integer, Intent(OUT) :: xstrout ! X-Start of moved output region
     Integer, Intent(OUT) :: ystrout ! Y-Start of moved output region
     Integer, Intent(OUT) :: xendout ! X-End of moved output region
     Integer, Intent(OUT) :: yendout ! Y-End of moved output region
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
!  Local Variables:
     Integer, Save :: sub_pixels = 0 ! Number of sub-pixels in each direction
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Logical, Save :: direct_values = .True. ! .True., if direct values are
!      used to define translation and rotation, otherwise two input
!      coordinates and two output coordinates are defined
     Real, Save :: angle = 0.0 ! Angle of rotation
     Real :: distance_error ! Half the difference in distance between
!      each pair of coordinates. This is the estimate of the error
!      in distance between the input and output transformation
     Real :: time_cpu ! CPU time
     Real :: timstr, timeend ! Time in seconds
     Real :: x_centre ! X-data coordinate of centre of rotation
     Real :: x_fix ! X-pixel coordinate of centre of rotation
     Real, Save :: x_in1 = 100.0 ! X-coordinate of input points
     Real, Save :: x_in2 = 200.0 ! X-coordinate of input points
     Real, Save :: x_out1 = 200.0 ! X-coordinate of output points
     Real, Save :: x_out2 = 200.0 ! X-coordinate of output points
     Real :: x_move_pix ! X-translation converted to pixel units
     Real, Save :: x_translate = 0.0 ! X-component of translation vector
     Real :: y_centre ! Y-data coordinate of centre of rotation
     Real :: y_fix ! Y-pixel coordinate of centre of rotation
     Real, Save :: y_in1 = 100.0 ! Y-coordinate of input point
     Real, Save :: y_in2 = 100.0 ! Y-coordinate of input point
     Real, Save :: y_out1 = 100.0 ! Y-coordinate of output point
     Real, Save :: y_out2 = 200.0 ! Y-coordinate of output point
     Real :: y_move_pix ! Y-translation converted to pixel units
     Real, Save :: y_translate = 0.0 ! Y-component of translation vector
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(10) ! User messages
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered Subroutine F2D_MOVE '')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MOVE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MOVE ' // Version)
 
     Else
 
!     Arguments would appear to be reasonable, go ahead.
 
!     Enter type of definition of movement
        MESSAGE(1) = 'Either the centre and angle of rotation, ' // &
          'and the translation vector'
        MESSAGE(2) = 'may be specified, OR the coordinates of ' // &
          'two input points and two'
        MESSAGE(3) = 'output points may be entered. If the ' // &
          'indirect method is chosen'
        MESSAGE(4) = 'the distance between the points should be ' // &
          'equal for this to make'
        MESSAGE(5) = 'sense. However, the distances are different ' // &
          'this cannot be done'
        MESSAGE(6) = 'exactly. In this case the rotation angle ' // &
          'will bring the two'
        MESSAGE(7) = 'lines together, with the two lines sharing ' // &
          'the same mid-point.'
        MESSAGE(8) = ' '
        MESSAGE(9) = '"YES" to specify rotation and translation, ' // &
          '"NO" to specify two input'
        MESSAGE(10) = 'coordinates and two output coordinates'
        Call IO_INPL (.True., 0, 1, .True., &
          'DIRECT DEFINITION TRANSLATION/ROTATION', 10, MESSAGE, 1, &
          'Enter "YES" or "NO"', direct_values, status)
 
        If (direct_values) Then
 
!        Give information
           Call IO_WRITE ('INFO: First the data is rotated ' // &
             'about a fixed coordinate, THEN a', status)
           Call IO_WRITE ('INFO: translation is applied. ' // &
             '(Output in the memory)', status)
 
!        Enter angle of rotation
           angle = angle * 180.0 / Pi
           Call IO_INPR (.True., -360.0, 360.0, .True., &
             'ROTATION ANGLE (DEGREES)', 1, 'Angle of rotation in ' // &
             'degrees (positive angle is anti-clockwise rotation)', 1, &
             'Enter angle within given range', angle, status)
           angle = angle * Pi / 180.0
 
           If (angle .Ne. 0.0) Then
 
!           Enter centre of rotation
              x_centre = (XAXIS(xendelm) + XAXIS(xstrelm)) / 2.0
              y_centre = (YAXIS(yendelm) + YAXIS(ystrelm)) / 2.0
              Call IO_INPR (.False., 0.0, 0.0, .True., 'X CENTRE ROTATION', 1, &
                'X-coordinate of centre of rotation', 1, 'Enter real number', &
                x_centre, status)
              Call IO_INPR (.False., 0.0, 0.0, .True., 'Y CENTRE ROTATION', 1, &
                'Y-coordinate of centre of rotation', 1, 'Enter real number', &
                y_centre, status)
 
           End If
 
!        Enter translation vector
           Call IO_INPR (.False., 0.0, 0.0, .True., 'X TRANSLATION', 1, &
             'X-component of translation vector (data coordinates)', 1, &
             'Enter real number', x_translate, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., 'Y TRANSLATION', 1, &
             'Y-component of translation vector (data coordinates)', 1, &
             'Enter real number', y_translate, status)
 
        Else
 
!        Give information
           Call IO_WRITE ('INFO: Enter two input coordinates ' // &
             'followed by their corresponding', status)
           Call IO_WRITE ('INFO: output coordinates ' // &
             'from which a rotation and/or ', status)
           Call IO_WRITE ('INFO: translation will ' // &
             'be automatically calculated.', status)
           Call IO_WRITE ('INFO: If the distances between ' // &
             'the input and output coordinates', status)
           Call IO_WRITE ('INFO: are different ' // &
             'the operation cannot be exact,', status)
           Call IO_WRITE ('INFO: but the vectors will be ' // &
             'aligned with their mid-points equal.', status)
 
!        Enter coordinates
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'FIRST INPUT X-COORDINATE', 1, &
             'Enter X-component of first input coordinate', 1, &
             'Enter real number', x_in1, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'FIRST INPUT Y-COORDINATE', 1, &
             'Enter Y-component of first input coordinate', 1, &
             'Enter real number', y_in1, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'SECOND INPUT X-COORDINATE', 1, &
             'Enter X-component of second input coordinate', 1, &
             'Enter real number', x_in2, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'SECOND INPUT Y-COORDINATE', 1, &
             'Enter Y-component of second input coordinate', 1, &
             'Enter real number', y_in2, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'FIRST OUTPUT X-COORDINATE', 1, &
             'Enter X-component of first output coordinate', 1, &
             'Enter real number', x_out1, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'FIRST OUTPUT Y-COORDINATE', 1, &
             'Enter Y-component of first output coordinate', 1, &
             'Enter real number', y_out1, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'SECOND OUTPUT X-COORDINATE', 1, &
             'Enter X-component of second output coordinate', 1, &
             'Enter real number', x_out2, status)
           Call IO_INPR (.False., 0.0, 0.0, .True., &
             'SECOND OUTPUT Y-COORDINATE', 1, &
             'Enter Y-component of second output coordinate', 1, &
             'Enter real number', y_out2, status)
 
!        Calculate rotation and translate to optimally move the input
!        coordinates to the output coordinates
           Call MA_CAL_2DTRANSFORMATION (x_in1, y_in1, x_in2, y_in2, x_out1, &
             y_out1, x_out2, y_out2, x_centre, y_centre, angle, x_translate, &
             y_translate, distance_error, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''angle, x_trans, y_trans = '', 3f12.4)')
!        :          angle, x_translate, y_translate
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Write (MESSAGE(1), '(''INFO: Estimated distance error ' // &
             'at transformation coordinates = '', f8.4)') distance_error
           Call IO_WRITE (MESSAGE(1), status)
 
        End If
 
!     Convert to pixel coordinates
        Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x_centre, x_fix, status)
        Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, y_centre, y_fix, status)
 
!     Convert to pixel units
        x_move_pix = x_translate / (XAXIS(xstrelm + 1) - XAXIS(xstrelm))
        y_move_pix = y_translate / (YAXIS(ystrelm + 1) - YAXIS(ystrelm))
 
!     Number of sub-pixels
        MESSAGE(1) = 'Two different re-binning methods are ' // &
          'available. The fastest just puts'
        MESSAGE(2) = 'the whole contents of each input pixel ' // &
          'into one output pixel,'
        MESSAGE(3) = 'depending on where the centre of the ' // &
          'input pixel is transformed to in'
        MESSAGE(4) = 'the output array. For this option enter: 1.'
        MESSAGE(5) = '   Alternatively exact area overlap ' // &
          'calculations are applied and'
        MESSAGE(6) = 'intensity is distributed amongst the ' // &
          'overlapped output pixels in'
        MESSAGE(7) = 'proportion to covered area.'
        MESSAGE(8) = '   Previously, there was the option to ' // &
          'sub-divide input pixels into a '
        MESSAGE(9) = 'defined number of sub-pixels. Since the ' // &
          'exact area re-binning takes'
        MESSAGE(10) = 'the same time as 2*2 sub-pixel ' // &
          're-binning, exact area is now used instead.'
        Call IO_INPI (.True., 0, 100, .True., &
          'SUB-PIXELS (0 = TRIANGLE RE-BINNING)', 10, MESSAGE, 1, &
          'Enter integer within give range', sub_pixels, status)
 
!     Output region
        xstrout = 1
        ystrout = 1
        xendout = xmaxdat
        yendout = ymaxdat
        Call IO_INPI (.True., 1, xmaxdat, .True., 'X-MINIMUM PIXEL', 1, &
          'Pixel in X-direction for start of output region', 1, &
          'Enter integer within give range', xstrout, status)
        Call IO_INPI (.True., 1, ymaxdat, .True., 'Y-MINIMUM PIXEL', 1, &
          'Pixel in Y-direction for start of output region', 1, &
          'Enter integer within give range', ystrout, status)
        Call IO_INPI (.True., xstrout, xmaxdat, .True., 'X-MAXIMUM PIXEL', 1, &
          'Pixel in X-direction for end of output region', 1, &
          'Enter integer within give range', xendout, status)
        Call IO_INPI (.True., ystrout, ymaxdat, .True., 'Y-MAXIMUM PIXEL', 1, &
          'Pixel in Y-direction for end of output region', 1, &
          'Enter integer within give range', yendout, status)
 
!     Set axis values
        Do x = 1, xendout
           OUTXAXIS(x) = XAXIS(xstrelm) + &
             Real(x - xstrelm) * (XAXIS(xstrelm + 1) - XAXIS(xstrelm))
        End Do
 
        Do y = 1, yendout
           OUTYAXIS(y) = YAXIS(ystrelm) + &
             Real(y - ystrelm) * (YAXIS(ystrelm + 1) - YAXIS(ystrelm))
        End Do
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu, status)
 
!     Rotate about fixed point, then translate image
        If (sub_pixels .Ne. 1) Then
           Call MA_2DTRANSFORM (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
             xendelm, yendelm, x_fix, y_fix, angle, x_move_pix, y_move_pix, &
             x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrout, ystrout, &
             xendout, yendout, OUTPUT, status)
        Else
           Call MA_RROTATE (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, xendelm, &
             yendelm, x_fix, y_fix, angle, x_move_pix, y_move_pix, sub_pixels, &
             sub_pixels, xmaxdat, ymaxdat, xstrout, ystrout, xendout, yendout, &
             OUTPUT, status)
        End If
 
!     Store start time
        Call IO_TIMES (timeend, time_cpu, status)
 
        Write (message, '(''INFO: Time for transformation ' // &
          '= '', f12.2, '' seconds'')') timeend - timstr
        Call IO_WRITE (message, status)
 
 
        If (variances_exist) Then
 
!        Rotate variances about fixed point, then translate image
           If (sub_pixels .Ne. 1) Then
              Call MA_RTRANSFORM (xmaxdat, ymaxdat, VARIANCES, xstrelm, &
                ystrelm, xendelm, yendelm, x_fix, y_fix, angle, x_move_pix, &
                y_move_pix, x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, &
                xstrout, ystrout, xendout, yendout, OUTVARIANCES, status)
           Else
              Call MA_RROTATE (xmaxdat, ymaxdat, VARIANCES, xstrelm, ystrelm, &
                xendelm, yendelm, x_fix, y_fix, angle, x_move_pix, y_move_pix, &
                sub_pixels, sub_pixels, xmaxdat, ymaxdat, xstrout, ystrout, &
                xendout, yendout, OUTVARIANCES, status)
           End If
 
        End If
 
     End If
 
     End Subroutine F2D_MOVE
!********1*********2*********3*********4*********5*********6*********7*********8
 

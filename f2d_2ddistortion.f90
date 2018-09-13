!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_2ddistortion.f90 *
!  *                      *
!  ************************
 
!+ F2D_2DDISTORTION: 2-D DISTORTION calculation
     Subroutine F2D_2DDISTORTION (xmax_peaks, ymax_peaks, xnumpeaks, &
       ynumpeaks, X_PEAKS, Y_PEAKS, grid_spacing, x_pixel_size, y_pixel_size, &
       X_DISTORTION, Y_DISTORTION, status)
!  Description:
!    Calculate distortion values with user choice of pixel sizes etc. The 
!    distortion is defined as the true (ideal) position minus the measured 
!    position. Therefore, the ideal position = the measured position + the 
!    distortion value.
!  Keywords:
!    Distortion.Values
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Dec-1996: V0.10 Avoid open strings crossing lines (Hammersley)
!    30-Nov-1996: V0.9 Account for hexagonal grids (Hammersley)
!    12-Aug-1995: V0.8 Input "grid_spacing" from the argument list
!      instead of inputting it from the user (Hammersley)
!    18-Jan-1995: V0.7 Immediate return if user escape is entered (Hammersley)
!    19-Dec-1994: V0.6 Convert internal definition of pixel sizes to metre units
!      (Hammersley)
!    24-Mar-1994: V0.5 Don't accept ideal hole, If it has not been found
!      (Hammersley)
!    25-Jan-1994: V0.4 Calculate RMS pixel sizes (Hammersley)
!    12-Nov-1993: V0.3 Convert sum variables to double precision to
!      avoid square root of negative number problems (Hammersley)
!    10-Nov-1993: V0.2 Add calculate of error estimates (Hammersley)
!    01-Nov-1993: V0.1 Original, based on "F2D_DISTORTION" (Hammersley)
!  Modules:
     Use IO_LIB
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmax_peaks ! First dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: ymax_peaks ! Second dimension size of "X_PEAKS" and 
!      "Y_PEAKS"
     Integer, Intent(IN) :: xnumpeaks ! Number of peaks in X-direction of grid
     Integer, Intent(IN) :: ynumpeaks ! Number of peaks in Y-direction of grid
     Real, Intent(IN) :: X_PEAKS(xmax_peaks, ymax_peaks) ! X-coordinates of
!      peak centres
     Real, Intent(IN) :: Y_PEAKS(xmax_peaks, ymax_peaks) ! Y-coordinates of
!      peak centres
     Real, Intent(IN) :: grid_spacing ! Distance in microns from centre to
!      centre of grid holes
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size ! Size of pixel in X-direction (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Size of pixel in Y-direction (metres)
     Real, Intent(INOUT) :: X_DISTORTION(xmax_peaks, ymax_peaks)
!      X-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
     Real, Intent(INOUT) :: Y_DISTORTION(xmax_peaks, ymax_peaks)
!      Y-distortion at each peak (The distortion is measured as true position 
!      minus measured position)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number 
!  Local Variables:
     Double Precision :: angle_columns ! Rotation angles of the grid columns 
!      relative to detector
     Double Precision :: angle_grid ! Rotation angles of the grid (rows and 
!      columns) relative to detector
     Double Precision :: angle_rows ! Rotation angles of the grid rows relative
!      to detector
     Double Precision :: distance ! Calculated distances
     Double Precision :: est_error ! Estimated error in average values
     Double Precision :: square_column ! Sum of squares for columns
     Double Precision :: square_row ! Sum of squares for rows
     Double Precision :: square_spots ! Sum of squares of distances between 
!      spots
     Double Precision :: total_column ! Total distance or rotations for rows
     Double Precision :: total_row ! Total distance or rotations for columns
     Integer :: end ! Element number of end peak in a line
     Integer :: num_columns ! Number of columns used to create an average
     Integer :: num_rows ! Number of rows used to create an average
     Integer :: num_spots ! Number of spots used to create an RMS distance
     Integer :: start ! Element number of first peak in a line
     Integer :: x_ideal ! X-number of ideally positioned grid hole
     Integer :: xpeak ! Loop variable for peaks in X-direction of grid
     Integer :: y_ideal ! Y-number of ideally positioned grid hole
     Integer :: ypeak ! Loop variable for peaks in Y-direction of grid
     Logical :: hexagonal_grid ! .True., if the grid seems to be hexagonal
!      and is treated as such
     Logical :: input_ok ! .True., if the input ideal hole has been found
     Real :: cos_rotation ! Cosine of rotation angle
     Real :: pixel_size ! Average size of pixel in metres
     Real :: sin_rotation ! Sine of rotation angle
     Real :: x_end ! X-coordinate of end hole
     Real :: x_rotated ! Ideal X-pixel coordinate of rotated grid hole
     Real :: x_start ! X-coordinate of start hole
     Real :: xmm_ideal ! X-coordinate in mm of ideal hole postion
     Real :: xpc_ideal ! X-pixel coordinate of ideal hole postion
     Real :: y_end ! Y-coordinate of end hole
     Real :: y_rotated ! Ideal Y-pixel coordinate of rotated grid hole
     Real :: y_start ! Y-coordinate of start hole
     Real :: ymm_ideal ! Y-coordinate in mm of ideal hole postion
     Real :: ypc_ideal ! Y-pixel coordinate of ideal hole postion
!  Local Arrays:
     Character(Len = 80) :: ERROR(1) ! User error message text
     Character(Len = 80) :: MESSAGE(1) ! User message text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_2DDISTORTION ' // Version)
     Else
 
!     Allow user set distortion variables
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate average X-pixel size based on ends of the rows
        num_rows = 0
        total_row = 0.0d0
        square_row = 0.0d0
        Do ypeak = 1, ynumpeaks
 
           If (X_PEAKS(1, ypeak) .Gt. -1.7e38) Then
              start = 1
           Else If (X_PEAKS(2, ypeak) .Gt. -1.7e38) Then
              start = 2
           Else
              start = 0
           End If
 
           If (X_PEAKS(xnumpeaks, ypeak) .Gt. -1.7e38) Then
              end = xnumpeaks
           Else If (X_PEAKS(xnumpeaks-1, ypeak) .Gt. -1.7e38) Then
              end = xnumpeaks - 1
           Else
              end = 0
           End If
 
           If (start .Ne. 0 .And. end .Ne. 0 .And. start .Lt. end) Then
              x_start = X_PEAKS(start, ypeak)
              y_start = Y_PEAKS(start, ypeak)
              x_end = X_PEAKS(end, ypeak)
              y_end = Y_PEAKS(end, ypeak)
              distance = Sqrt ((x_end - x_start)**2 + (y_end - y_start)**2) / &
                Real(end - start)
              total_row = total_row + distance
              square_row = square_row + distance**2
              num_rows = num_rows + 1
           End If
 
        End Do
 
!     Calculate average pixel size and error in average
        distance = (total_row / Dble(num_rows))
        x_pixel_size = (grid_spacing / distance) * 1.0e-6
        If (num_rows .Gt. 1) Then
           est_error = Sqrt( (square_row - Dble(num_rows) * distance**2) / &
             Dble(num_rows - 1))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''DEBUG: Number of pixels between holes = '',
!        :          f12.4, '' +- '', f10.4)') distance, est_error
!        Write (*, '(''DEBUG: square_row, num_row = '',
!        :          1pe12.5, i6)') square_row, num_rows
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           est_error = x_pixel_size * est_error / distance
        Else
           est_error = x_pixel_size
        End If
 
!     Output information
        Write (MESSAGE(1), '(''INFO: Average pixel size in X-direction = '', ' &
          // 'f12.4, '' +- '', f10.4, '' microns'')') x_pixel_size * 1.0e6, &
          est_error * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate average Y-pixel size based on ends of columns
        num_columns = 0
        total_column = 0.0d0
        square_column = 0.0d0
        Do xpeak = 1, xnumpeaks
 
           If (X_PEAKS(xpeak, 1) .Gt. -1.7e38) Then
              start = 1
           Else If (X_PEAKS(xpeak, 2) .Gt. -1.7e38) Then
              start = 2
           Else
              start = 0
           End If
 
           If (X_PEAKS(xpeak, ynumpeaks) .Gt. -1.7e38) Then
              end = ynumpeaks
           Else If (X_PEAKS(xpeak, ynumpeaks - 1) .Gt. -1.7e38) Then
              end = ynumpeaks - 1
           Else
              end = 0
           End If
 
           If (start .Ne. 0 .And. end .Ne. 0 .And. start .Lt. end) Then
              x_start = X_PEAKS(xpeak, start)
              y_start = Y_PEAKS(xpeak, start)
              x_end = X_PEAKS(xpeak, end)
              y_end = Y_PEAKS(xpeak, end)
              distance = Sqrt ((x_end - x_start)**2 + (y_end - y_start)**2) / &
                Real(end - start)
              total_column = total_column + distance
              square_column = square_column + distance**2
              num_columns = num_columns + 1
           End If
 
        End Do
 
!     Calculate average pixel size and error in average
        distance = (total_column / Dble(num_columns))
        y_pixel_size = (grid_spacing / distance) * 1.0e-6
        If (num_columns .Gt. 1) Then
           est_error = Sqrt( (square_column - Dble(num_columns) * distance**2) &
             / Dble(num_columns - 1))
           est_error = y_pixel_size * est_error / distance
        Else
           est_error = y_pixel_size
        End If
 
!     Output information
        Write (MESSAGE(1), '(''INFO: Average pixel size in ' // &
          'Y-direction = '', f12.4, '' +- '', f10.4, '' microns'')') &
          y_pixel_size * 1.0e6, est_error * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
 
        distance = ((total_row + total_column) / Dble(num_rows + num_columns))
        pixel_size = (grid_spacing / distance) * 1.0e-6
        If (num_rows + num_columns .Gt. 1) Then
           est_error = Sqrt( (square_row + square_column - Dble(num_rows + &
             num_columns) * distance**2) / Dble(num_rows + num_columns - 1))
           est_error = pixel_size * est_error / distance
        Else
           est_error = pixel_size
        End If
 
!     Output information
        Write (MESSAGE(1), '(''INFO: Overall average pixel size = '', f12.4, ' &
          // ''' +- '', f10.4, '' microns'')') pixel_size * 1.0e6, &
          est_error * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate RMS X-pixel size from all adjacent spots (horizontal)
        num_spots = 0
        square_spots = 0.0d0
        Do ypeak = 1, ynumpeaks
 
           Do xpeak = 1, xnumpeaks - 1
 
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38 .And. &
                X_PEAKS(xpeak + 1, ypeak) .Gt. -1.7e38) Then
 
!              Adjacent spots exist
                 x_start = X_PEAKS(xpeak, ypeak)
                 y_start = Y_PEAKS(xpeak, ypeak)
                 x_end = X_PEAKS(xpeak + 1, ypeak)
                 y_end = Y_PEAKS(xpeak + 1, ypeak)
                 square_spots = square_spots + (x_end - x_start)**2 + &
                   (y_end - y_start)**2
                 num_spots = num_spots + 1
 
              End If
 
           End Do
 
        End Do
 
!     Calculate average pixel size and error in average
        distance = Sqrt(square_spots / Dble(num_spots))
        x_pixel_size = (grid_spacing / distance) * 1.0e-6
 
!     Output information
        Write (MESSAGE(1), &
          '(''INFO: RMS pixel size in X-direction = '', f12.4)') &
          x_pixel_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate RMS Y-pixel size from all adjacent spots (vertical)
        num_spots = 0
        square_spots = 0.0d0
        Do ypeak = 1, ynumpeaks - 1
 
           Do xpeak = 1, xnumpeaks
 
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38 .And. X_PEAKS(xpeak, &
                ypeak+1) .Gt. -1.7e38) Then
 
!              Adjacent spots exist
                 x_start = X_PEAKS(xpeak, ypeak)
                 y_start = Y_PEAKS(xpeak, ypeak)
                 x_end = X_PEAKS(xpeak, ypeak + 1)
                 y_end = Y_PEAKS(xpeak, ypeak + 1)
                 square_spots = square_spots + (x_end - x_start)**2 + &
                   (y_end - y_start)**2
                 num_spots = num_spots + 1
 
              End If
 
           End Do
 
        End Do
 
!     Calculate average pixel size and error in average
        distance = Sqrt(square_spots / Dble(num_spots))
        y_pixel_size = (grid_spacing / distance) * 1.0e-6
 
!     Output information
        Write (MESSAGE(1), &
          '(''INFO: RMS pixel size in Y-direction = '', f12.4)') &
          y_pixel_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate average rotation angle from the rows
        total_row = 0.0d0
        num_rows = 0
        square_row = 0.0d0
        Do ypeak = 1, ynumpeaks
 
           If (X_PEAKS(1, ypeak) .Gt. -1.7e38) Then
              start = 1
           Else If (X_PEAKS(2, ypeak) .Gt. -1.7e38) Then
              start = 2
           Else
              start = 0
           End If
 
           If (X_PEAKS(xnumpeaks, ypeak) .Gt. -1.7e38) Then
              end = xnumpeaks
           Else If (X_PEAKS(xnumpeaks - 1, ypeak) .Gt. -1.7e38) Then
              end = xnumpeaks - 1
           Else
              end = 0
           End If
 
           If (start .Ne. 0 .And. end .Ne. 0 .And. start .Lt. end) Then
              x_start = X_PEAKS(start, ypeak)
              y_start = Y_PEAKS(start, ypeak)
              x_end = X_PEAKS(end, ypeak)
              y_end = Y_PEAKS(end, ypeak)
              num_rows = num_rows + 1
 
!           Calculate rotation angle, taking into account non-square pixel size
              angle_rows = Atan2((y_end - y_start) * y_pixel_size, &
                (x_end - x_start) * x_pixel_size)
              total_row = total_row + angle_rows
              square_row = square_row + angle_rows**2
           End If
 
        End Do
 
!     Calculate average angle and error in average
        angle_rows = total_row / Dble(num_rows)
        If (num_rows .Gt. 1) Then
           est_error = Sqrt( (square_row - Dble(num_rows) * angle_rows**2) / &
             Dble(num_rows - 1))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''DEBUG: angle_rows = '',
!        :          f12.4, '' +- '', f10.4)') angle_rows, est_error
!        Write (*, '(''DEBUG: square_row, num_row = '',
!        :          1pe12.5, i6)') square_row, num_rows
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Else
           est_error = angle_rows
        End If
 
!     Output information
        Write (MESSAGE(1), &
          '(''INFO: Average rotation based on rows = '', f12.6, ' // &
          ''' +- '', f10.4, '' degrees'')') angle_rows * 180.0 / Pi, &
          est_error * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate average rotation angle from the columns
        num_columns = 0
        total_column = 0.0d0
        square_column = 0.0d0
        Do xpeak = 1, xnumpeaks
 
           If (X_PEAKS(xpeak, 1) .Gt. -1.7e38) Then
              start = 1
           Else If (X_PEAKS(xpeak, 2) .Gt. -1.7e38) Then
              start = 2
           Else
              start = 0
           End If
 
           If (X_PEAKS(xpeak, ynumpeaks) .Gt. -1.7e38) Then
              end = ynumpeaks
           Else If (X_PEAKS(xpeak, ynumpeaks - 1) .Gt. -1.7e38) Then
              end = ynumpeaks - 1
           Else
              end = 0
           End If
 
           If (start .Ne. 0 .And. end .Ne. 0 .And. start .Lt. end) Then
              x_start = X_PEAKS(xpeak, start)
              y_start = Y_PEAKS(xpeak, start)
              x_end = X_PEAKS(xpeak, end)
              y_end = Y_PEAKS(xpeak, end)
              num_columns = num_columns + 1
 
!           Calculate rotation angle, taking into account non-square pixel size
              angle_columns = Atan2((x_start - x_end) * x_pixel_size, &
                (y_end - y_start) * y_pixel_size)
              total_column = total_column + angle_columns
              square_column = square_column + angle_columns**2
           End If
 
        End Do
 
!     Calculate average angle and error in average
        angle_columns = total_column / Dble(num_columns)
        If (num_columns .Gt. 1) Then
           est_error = Sqrt( (square_column - Dble(num_columns) * &
             angle_columns**2) / Dble(num_columns - 1))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''DEBUG: angle_columns = '',
!        :          f12.4, '' +- '', f10.4)') angle_columns, est_error
!        Write (*, '(''DEBUG: square_column, num_columns = '',
!        :          1pe12.5, i6)') square_column, num_columns
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Else
           est_error = angle_columns
        End If
 
!     Output information
        Write (MESSAGE(1), '(''INFO: Average rotation based on columns = '', ' &
          // 'f12.6, '' +- '', f10.4, '' degrees'')') angle_columns * 180.0 / &
          Pi, est_error * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Check for hexagonal grids
        If (Abs(angle_columns - angle_rows) .Gt. Pi / 3.0 - Pi / 9.0 .And. &
          Abs(angle_columns - angle_rows) .Le. Pi / 3.0 + Pi / 9.0) Then
 
!        Output informaton message
           Call IO_WRITE ('INFO: The grid appears to be ' // &
             'hexagonal and distortion values will be', status)
           Call IO_WRITE ('      calculated on this ' // &
             'assumption. (Something is very wrong if the', status)
           Call IO_WRITE ('      grid is actually cartesian.)', status)
 
           hexagonal_grid = .True.
 
        Else
           hexagonal_grid = .False.
        End If
 
        angle_grid = (total_row + total_column) / Dble(num_rows + num_columns)
        If (num_rows + num_columns .Gt. 1) Then
           est_error = Sqrt( (square_row + square_column - Dble(num_rows + &
             num_columns) * angle_grid**2) / Dble(num_rows + num_columns - 1))
        Else
           est_error = angle_grid
        End If
        angle_grid = angle_grid * 180.0 / Pi
 
!     Output information
        Call IO_WRITE ('INFO: Average rotation (both rows ' // &
          'and columns) = ',  status)
        Write (MESSAGE(1), &
          '(''INFO: '', f12.6, '' +- '', f10.4, '' degrees'')') angle_grid, &
          est_error * 180.0 / Pi
        Call IO_WRITE (MESSAGE(1), status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Enter X-size of ideal pixel
        MESSAGE(1) = 'Enter required pixel X-size for calculating distortion'
        ERROR(1) = 'Value must be within given range'
        x_pixel_size = x_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.0001, 1.0e6, .True., &
          'IDEAL X-PIXEL SIZE (MICRONS)', 1, MESSAGE, 1, ERROR, x_pixel_size, &
          status)
        x_pixel_size = x_pixel_size * 1.0e-6
 
!     Enter Y-size of ideal pixel
        MESSAGE(1) = 'Enter required pixel Y-size for calculating distortion'
        y_pixel_size = y_pixel_size * 1.0e6
        Call IO_INPR (.True., 0.0001, 1.0e6, .True., &
          'IDEAL Y-PIXEL SIZE (MICRONS)', 1, MESSAGE, 1, ERROR, y_pixel_size, &
          status)
        y_pixel_size = y_pixel_size * 1.0e-6
 
!     Enter rotation angle of grid on detector
        MESSAGE(1) = 'Enter rotation angle of grid on detector'
        Call IO_INPD(.True., -360.0d0, 360.0d0, .True., &
          'GRID ROTATION ANGLE (DEGREES)', 1, MESSAGE, 1, ERROR, angle_grid, &
          status)
 
!     Don't accept ideal peak position if it is missing
        input_ok = .False.
 
        Do While (.Not. input_ok)
 
!        Enter X-number of peak which is considered to have zero distortion
           MESSAGE(1) = 'Enter number (X-direction) of hole which is ' // &
             'considered to have zero distortion'
           x_ideal = (xnumpeaks + 1) / 2
           Call IO_INPI (.True., 1, xnumpeaks, .True., &
             'X-NUMBER OF IDEAL HOLE', 1, MESSAGE, 1, ERROR, x_ideal, status)
 
!        Enter Y-number of peak which is considered to have zero distortion
           MESSAGE(1) = 'Enter number (Y-direction) of hole which is ' // &
             'considered to have zero distortion'
           y_ideal = (ynumpeaks + 1) / 2
           Call IO_INPI (.True., 1, ynumpeaks, .True., &
             'Y-NUMBER OF IDEAL HOLE', 1, MESSAGE, 1, ERROR, y_ideal, status)
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              Return
 
!           Check if defined ideal peak has been found
           Else If (X_PEAKS(x_ideal, y_ideal) .Le. -1.7e38) Then
              Call IO_WRITE ('WARNING: Specified grid ' // &
                'position is missing from data image, please', status)
              Call IO_WRITE ('         specify another peakposition', &
                status)
           Else
              input_ok = .True.
           End If
 
        End Do
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate distortions at peak positions based on entered values
 
!     Find pixel coordinate of ideal hole
        xpc_ideal = X_PEAKS(x_ideal, y_ideal)
        ypc_ideal = Y_PEAKS(x_ideal, y_ideal)
 
!     Calculate sine and cosine of rotation from ideal to measured
        cos_rotation = Cos(angle_grid * Pi / 180.0)
        sin_rotation = Sin(angle_grid * Pi / 180.0)
 
!     Calculate distortions for each peak
        Do ypeak = 1, ynumpeaks
 
           Do xpeak  = 1, xnumpeaks
 
              If (X_PEAKS(xpeak, ypeak) .Gt. -1.7e38) Then
 
!              Account for rotation of grid
                 xmm_ideal = Real(xpeak - x_ideal)
                 ymm_ideal = Real(ypeak - y_ideal)
                 x_rotated = (cos_rotation * xmm_ideal - sin_rotation * &
                   ymm_ideal) * grid_spacing / (x_pixel_size * 1.0e6) + &
                   xpc_ideal
                 y_rotated = (sin_rotation * xmm_ideal + cos_rotation * &
                   ymm_ideal) * grid_spacing / (y_pixel_size * 1.0e6) + &
                   ypc_ideal
                 X_DISTORTION(xpeak, ypeak) = x_rotated - X_PEAKS(xpeak, ypeak)
                 Y_DISTORTION(xpeak, ypeak) = y_rotated - Y_PEAKS(xpeak, ypeak)

              Else
 
!              Set distortion to zero for display purposes
                 X_DISTORTION(xpeak, ypeak) = 0.0
                 Y_DISTORTION(xpeak, ypeak) = 0.0

              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_2DDISTORTION
!********1*********2*********3*********4*********5*********6*********7*********8

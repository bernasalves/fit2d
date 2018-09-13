!********1*********2*********3*********4*********5*********6*********7**
 
!  **********************
!  *                    *
!  * f2d_in_spatial.f90 *
!  *                    *
!  **********************
 
!+ F2D_IN_SPATIAL: INput SPATIAL distortion interpolation function
     Subroutine F2D_IN_SPATIAL (gui, xmaxknots, ymaxknots, file_name, retstat, &
       x_min, y_min, x_max, y_max, cor_grid_spacing, x_pixel_size, &
       y_pixel_size, x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, &
       y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
!  Description:
!    Inputs coefficients of spline interpolation functions for X/Y
!    spatial distortions from an ASCII file
!  Keywords:
!    Input.Spatial~Distortion, Spatial~Distortion.Input
!  Method:
!    Simple ASCII file
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    12-Oct-1999: V0.10 Use FIO instead of IO routines (Hammersley)
!    15-Jul-1998: V0.9 Convert to new I/O system to avoid problem on Linux 
!      system of needing write permission to open a file for reading 
!      (Hammersley)
!    05-Dec-1997: V0.8 "Close" file, in not properly opened (Hammersley)
!    29-Mar-1996: V0.7 Make sure that the input file is closed properly even 
!      when an error condition occurs (Hammersley)
!    26-Feb-1996: V0.6 Option of GUI (Hammersley)
!    18-Jan-1995: V0.5 Rename "x_cor_size" to "x_pixel_size" (Hammersley)
!    07-Feb-1994: V0.4 Open ASCII file for "READ" (not "WRITE" as was previously
!      the case) (Hammersley)
!    13-Oct-1993: V0.3 Input corrected pixel sizes (Hammersley)
!    01-Oct-1993: V0.2 Convert spline defining arrays to single precision 
!      (Hammersley)
!    13-Aug-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is being 
!      used
     Integer, Intent(IN) :: xmaxknots ! Maximum number of knots for X-direction
     Integer, Intent(IN) :: ymaxknots ! Maximum number of knots for Y-direction
!  Import/Export:
     Character(Len = *), Intent(INOUT) :: file_name ! Name of input file when 
!      the GUI is being used
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good, values returned
!      1 = Arrays too small for file coefficients
!      2 = Wrong type of file (no spline coefficients)
!      3 = File not opened successfully
     Real, Intent(OUT) :: x_min ! Minimum X-value for which spline function
!      should be valid
     Real, Intent(OUT) :: y_min ! Minimum Y-value for which spline function
!      should be valid
     Real, Intent(OUT) :: x_max ! Maximum X-value for which spline function
!      should be valid
     Real, Intent(OUT) :: y_max ! Maximum Y-value for which spline function
!      should be valid
     Real, Intent(OUT) :: cor_grid_spacing ! Grid spacing for correction
!      function
     Real, Intent(OUT) :: x_pixel_size ! Size of corrected pixel in metres
!      in X-direction
     Real, Intent(OUT) :: y_pixel_size ! Size of corrected pixel in metres
!      in Y-direction
     Integer, Intent(OUT) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(OUT) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(OUT) :: X_LAMBDA(xmaxknots) ! X-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(OUT) :: X_MU(ymaxknots) ! Y-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(OUT) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(OUT) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(OUT) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(OUT) :: Y_LAMBDA(xmaxknots) ! X-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(OUT) :: Y_MU(ymaxknots) ! Y-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(OUT) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for Y-distortion function
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.10' ! Version number
!  Local Variables:
     Character(Len = 80) :: buffer ! Input buffer for file input
     Character(Len = 80), Save :: spline_name = 'spatial.dat' ! Name of input 
!      file
     Character(Len = 80) :: file_type ! Type of file, first line must
!      contain correct text to be considered of the correct file type
     Integer :: current ! Points to current input real to use
     Integer :: file_stat ! "IO_OPEN ASCIIFILE" return status variable
     Integer :: in_id ! Identifier for input file
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_IN_SPATIAL ' // Version)
        Return
     End If
 
!  Check input arguments
     If (xmaxknots .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxknots .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_IN_SPATIAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Input arguments appear to be correct
 
!     Initialise variables
        retstat = 0
 
        If (gui) Then
           Call FIO_OPENFILE (.False., 'READ_ASCII', file_name, file_stat, &
             in_id, status)
        Else
 
!        Open ASCII file for input
           Call IO_WRITE ('Name of file for spatial ' // &
             'distortion interpolation function', status)
           Call FIO_OPENFILE (.True., 'READ_ASCII', file_name, file_stat, &
             in_id, status)
        End If
 
        If (file_stat .Eq. 0) Then
 
!        Read spline from file
           Call FIO_ASCIIREADC (in_id, Len(file_type), retstat, file_type)
 
!        Check that the first line contains correct text
           If (file_type(1: 25) .Ne. 'SPATIAL DISTORTION SPLINE') Then
 
              If (file_type(1: 30) .Eq. 'DISTORTION FUNCTION FROM IDEAL') Then
                 Call IO_WRITE ('WARNING: File is wrong ' // &
                   'sort of distortion spline function', status)
                 Call IO_WRITE ('         (ideal grid to ' // &
                   'distorted grid spline)', status)
              Else
                 Call IO_WRITE ('WARNING: File is not ' // &
                   'a distortion spline function file', status)
              End If
 
!           Close file
              Call FIO_FILECLOSEC (in_id, retstat)
              retstat = 2
              Return
           End If
 
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Read (buffer, Err = 777, Fmt = '(4e14.7)') x_min, y_min, x_max, y_max
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_min, y_min, x_max, y_max = '', 4e14.7)')
!        :          x_min, y_min, x_max, y_max
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Read (buffer, Err = 777, Fmt = '(3e14.7)') cor_grid_spacing, &
             x_pixel_size, y_pixel_size
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''spacing, x_p_size, y_p_size = '', 3e14.7)')
!        :          cor_grid_spacing, x_pixel_size, y_pixel_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Read (buffer, Err = 777, Fmt = '(2i6)') x_xnumknots, x_ynumknots
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_xnumknots, x_ynumknots = '', 2i6)')
!        :          x_xnumknots, x_ynumknots
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Convert pixel sizes from microns to metres
           x_pixel_size = x_pixel_size * 1.0e-6
           y_pixel_size = y_pixel_size * 1.0e-6
 
!        Check arrays are big enough for coefficients
           If (x_xnumknots .Gt. xmaxknots .Or. x_ynumknots .Gt. ymaxknots) &
             Then
 
!           Close file
              Call FIO_FILECLOSEC (in_id, retstat)
              retstat = 1
              Return
           End If
 
!        Input X distortion X-axis knot positions
           current = 71
           Do x = 1, x_xnumknots
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                X_LAMBDA(x)
              current = current + 14
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x, X_LAMBDA(x) = '', i6, e14.7)')
!           :             x, X_LAMBDA(x)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
!        Input X distortion Y-axis knot positions
           current = 71
           Do y = 1, x_ynumknots
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                X_MU(y)
              current = current + 14
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''y, X_MU(y) = '', i6, e14.7)')
!           :             y, X_MU(y)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
!        Input X distortion coefficients
           current = 71
           Do x = 1, (x_xnumknots - 4) * (x_ynumknots - 4)
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                X_COEFFS(x)
              current = current + 14
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x, X_COEFFS(x) = '', i6, e14.7)')
!           :             x, X_COEFFS(x)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
           Read (buffer, Err = 777, Fmt = '(2i6)') y_xnumknots, y_ynumknots
 
!        Check arrays are big enough for coefficients
           If (y_xnumknots .Gt. xmaxknots .Or. y_ynumknots .Gt. ymaxknots) Then
 
!           Close file
              Call FIO_FILECLOSEC (in_id, retstat)
              retstat = 1
              Return

           End If
 
!        Input Y distortion X-axis knot positions
           current = 71
           Do x = 1, y_xnumknots
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                Y_LAMBDA(x)
              current = current + 14
 
           End Do
 
!        Input Y distortion Y-axis knot positions
           current = 71
           Do y = 1, y_ynumknots
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                Y_MU(y)
              current = current + 14
 
           End Do
 
!        Input X distortion coefficients
           current = 71
           Do x = 1, (y_xnumknots - 4) * (y_ynumknots - 4)
 
              If (current .Eq. 71) Then
                 current = 1
                 Call FIO_ASCIIREADC (in_id, Len(buffer), retstat, buffer)
              End If
 
              Read (buffer(current: current + 13), Err = 777, Fmt = '(e14.7)') &
                Y_COEFFS(x)
              current = current + 14
 
           End Do
 
!        Close file
           Call FIO_FILECLOSEC (in_id, retstat)
 
        Else
 
!        File not opened correctly
 
!        Close file
           Call FIO_FILECLOSEC (in_id, retstat)
 
           retstat = 3
        End If
 
     End If
 
     Return
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!  Error during read statement
     777    status = St_mod_fit2d + St_bad_read
     Call ST_SAVE ('Subroutine F2D_IN_SPATIAL ' // Version)
 
!  Close file
     Call FIO_FILECLOSEC (in_id, retstat)
     Return
 
     End Subroutine F2D_IN_SPATIAL
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 

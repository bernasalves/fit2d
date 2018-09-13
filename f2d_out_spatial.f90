!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_out_spatial.f90 *
!  *                     *
!  ***********************
 
!+ F2D_OUT_SPATIAL: OUTput SPATIAL distortion interpolation function
     Subroutine F2D_OUT_SPATIAL (distorted_spline, x_min, y_min, x_max, y_max, &
       cor_grid_spacing, x_cor_size, y_cor_size, xmaxknots, ymaxknots, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, status)
!  Description:
!  Keywords:
!    Output.Spatial~Distortion, Spatial~Distortion.Output
!  Method:
!    Simple ASCII file
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    15-Oct-1997: V0.8 Change default name of spline output file to 
!      "spatial.spline" (Hammersley)
!    16-Dec-1996: V0.7 Avoid open strings crossing lines (Hammersley)
!    29-Mar-1996: V0.6 Check "write" status return values to stop crashes when
!      the disk is full (Hammersley)
!    19-Dec-1994: V0.5 Change definition of pixel sizes to metre units 
!      (Hammersley)
!    06-Sep-1994: V0.4 Record in output file if the distortion function is from
!      the ideal grid to the distorted grid (Hammersley)
!    13-Oct-1993: V0.3 Output corrected pixel sizes (Hammersley)
!    01-Oct-1993: V0.2 Convert spline defining arrays to single precision 
!      (Hammersley)
!    13-Aug-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: distorted_spline ! .True., if the distortion
!      measurements are calculated in the distorted space (default), i.e. the
!      distortion values are those to go from the distorted pixel values to 
!      ideal pixel grid , measured in the distorted pixel grid. Otherwise the
!      values are those to go from an ideal pixel grid to the distorted 
!      position, measured in the ideal pixel grid
     Real, Intent(IN) :: x_min ! Minimum X-value for which spline function
!      should be valid
     Real, Intent(IN) :: y_min ! Minimum Y-value for which spline function
!      should be valid
     Real, Intent(IN) :: x_max ! Maximum X-value for which spline function
!      should be valid
     Real, Intent(IN) :: y_max ! Maximum Y-value for which spline function
!      should be valid
     Real, Intent(IN) :: cor_grid_spacing ! Grid spacing for correction
!      function
     Real, Intent(IN) :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Intent(IN) :: y_cor_size ! Size of corrected pixel in metres in
!      Y-direction
     Integer, Intent(IN) :: xmaxknots ! Maximum number of knots for
!      X-direction
     Integer, Intent(IN) :: ymaxknots ! Maximum number of knots for
!      Y-direction
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(xmaxknots) ! X-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_MU(ymaxknots) ! Y-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(xmaxknots) ! X-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_MU(ymaxknots) ! Y-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((xmaxknots - 4) * (ymaxknots - 4))
!      Coefficients of spline function for Y-distortion function
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Io_out = 17 ! Unit number for output
!  Local Variables:
     Character(Len = 80), Save :: file_name = 'spatial.spline'
!      Name of output file
     Character(Len = 80) :: message ! User messages
     Integer :: iostat ! Write status return variable
     Integer :: retstat ! "IO_OPEN ASCIIFILE" return status variable
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_OUT_SPATIAL ' // Version)
        Return
     End If
 
!  Check input arguments
     If (xmaxknots .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxknots .Le. 0) Then
        status = St_bad_dim2
     Else If (x_xnumknots .Lt. 1 .Or. y_xnumknots .Lt. 1 .Or. x_xnumknots .Gt. &
       xmaxknots .Or. y_xnumknots .Gt. xmaxknots) Then
        status = St_bad_adr1
     Else If (x_ynumknots .Lt. 1 .Or. y_ynumknots .Lt. 1 .Or. x_ynumknots .Gt. &
       ymaxknots .Or. y_ynumknots .Gt. ymaxknots) Then
        status = St_bad_adr2
     End If
 
!  Re-check input status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_OUT_SPATIAL ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input arguments appear to be correct
 
!     Open ASCII file for output
        Call IO_WRITE ('Name of file for spatial distortion ' // &
          'interpolation function', status)
        Call IO_OPEN_ASCIIFILE (.True., 'WRITE', Io_out, file_name, retstat, &
          status)
 
        If (retstat .Eq. 0) Then
 
!        Add extra line if the distortion function is defined from an ideal grid
!        to a distorted grid
           If (.Not. distorted_spline) Then
              Write (Io_out, '(''DISTORTION FUNCTION FROM IDEAL ' // &
                'TO DISTORTED GRID, MEASURED ON IDEAL'')', Iostat = iostat)
           End If
 
!        Write spline to file
           Write (Io_out, '(''SPATIAL DISTORTION SPLINE ' // &
             'INTERPOLATION COEFFICIENTS'')', Iostat = iostat)
 
           If (iostat .Ne. 0) Then
              Call IO_WRITE ('WARNING: Problem outputting ' // &
                'spatial distortion file (quota full ?)', status)
              Write (message, '(''         System dependent ' // &
                'error code = '', i6)') iostat
              Call IO_WRITE (message, status)
           End If
 
           Write (Io_out, '()', Iostat = iostat)
           Write (Io_out, '(''  VALID REGION'')', Iostat = iostat)
           Write (Io_out, '(4e14.7)', Iostat = iostat) x_min, y_min, x_max, &
             y_max
           Write (Io_out, '()', Iostat = iostat)
           Write (Io_out, '(''  GRID SPACING, X-PIXEL SIZE, Y-PIXEL SIZE'')', &
             Iostat = iostat)
           Write (Io_out, '(3e14.7)', Iostat = iostat) cor_grid_spacing, &
             x_cor_size * 1.0e6, y_cor_size * 1.0e6
           Write (Io_out, '()', Iostat = iostat)
           Write (Io_out, '(''  X-DISTORTION'')', Iostat = iostat)
           Write (Io_out, '(2i6)', Iostat = iostat) x_xnumknots, x_ynumknots
           Write (Io_out, '(5e14.7)', Iostat = iostat) (X_LAMBDA(x), x = 1, &
             x_xnumknots)
           Write (Io_out, '(5e14.7)', Iostat = iostat) (X_MU(y), y = 1, &
             x_ynumknots)
           Write (Io_out, '(5e14.7)', Iostat = iostat) (X_COEFFS(x), x = 1, &
             (x_xnumknots - 4) * (x_ynumknots - 4))
           Write (Io_out, '()', Iostat = iostat)
           Write (Io_out, '(''  Y-DISTORTION'')', Iostat = iostat)
           Write (Io_out, '(2i6)', Iostat = iostat) y_xnumknots, y_ynumknots
           Write (Io_out, '(5e14.7)', Iostat = iostat) (Y_LAMBDA(x), x = 1, &
             y_xnumknots)
           Write (Io_out, '(5e14.7)', Iostat = iostat) (Y_MU(y), y = 1, &
             y_ynumknots)
           Write (Io_out, '(5e14.7)', Iostat = iostat) (Y_COEFFS(x), x = 1, &
             (y_xnumknots - 4) * (y_ynumknots - 4))
 
!        Close file
           Close (Io_out)
 
        End If
 
     End If
 
     End Subroutine F2D_OUT_SPATIAL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 

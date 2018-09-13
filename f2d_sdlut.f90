!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *****************
!  *               *
!  * f2d_sdlut.f90 *
!  *               *
!  *****************
 
!+ F2D_SDLUT - FIT2D: create Spatial Distortion Look-Up Table
     Subroutine F2D_SDLUT ( x_minimum, y_minimum, x_maximum, y_maximum, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, &
       xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, &
       sd_lut_defined, status)
!  Description:
!    Creates spatial distortion look-up table, for fast calculation
!    of triangular re-binned distortion correction.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Sep-1997: V0.4 Change progress message (Hammersley)
!    16-Dec-1996: V0.3 Avoid open strings crossing lines (Hammersley)
!    05-Oct-1996: V0.2 Account for rounded sums of fractions of the input pixel
!      being too large (Hammersley)
!    03-Oct-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum ! Minimum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_minimum ! Minimum Y-value applicable to spline
!      interpolation
     Real, Intent(IN) :: x_maximum ! Maximum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_maximum ! Maximum Y-value applicable to spline
!      interpolation
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots) ! X-Positions of spline "knots" 
!      for X-distortion function
     Real, Intent(IN) :: X_MU(x_ynumknots) ! Y-Positions of spline "knots" 
!      for X-distortion function
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots) ! X-Positions of spline "knots" 
!      for Y-distortion function
     Real, Intent(IN) :: Y_MU(y_ynumknots) ! Y-Positions of spline "knots" for 
!      Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
!      Coefficients of spline function for Y-distortion function
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xnum_lut ! Number of pixels to define in LUT in
!      the X-direction
     Integer, Intent(IN) :: ynum_lut ! Number of pixels to define in LUT in
!      the Y-direction
!  Import/Export:
!  Export:
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 8 pixels which are the 
!      targetpixel and the 8 nearest pixels with the exception of the upper
!      right pixel. Each fraction is stored as a byte, so change to unsigned, 
!      and divide by 256, to obtain the required fraction. Since the total must 
!      equal 1.0, the fraction for the upper right pixel can be deduced. The 
!      order of the fractions is lower left, lower middle, lower right, left of 
!      target, target, right of target, upper left, and upper middle.
     Logical, Intent(OUT) :: sd_lut_defined ! .True., if the spatial distortion
!      look-up table is probably defined
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
     Integer, Parameter :: y_rows = 2000 ! Number of rows of distortion to
!      calculate at once
!  Local Variables:
     Integer :: maxwork ! First dimension size of temporary arrays
     Integer stat ! Status return variable for "Allocate"
     Integer :: x_low ! Lower X-pixel limit of spline region
     Integer :: x_up ! Upper X-pixel limit of spline  region
     Integer :: y_low ! Lower Y-pixel limit of spline region
     Integer :: y_up ! Upper Y-pixel limit of spline region
!  Local Arrays:
     Real, Allocatable :: X_COORDINATES(:) ! Dynamic work array used to store 
!      X-coordinates of edges of pixels
     Real, Allocatable :: Y_COORDINATES(:) ! Dynamic work array used to store 
!      Y-coordinates of edges of pixels
     Real, Allocatable :: X_DISTORTION(:, :) ! X-distortion values 
     Real, Allocatable :: Y_DISTORTION(:, :) ! Y-distortion values 
!  Internal Functions:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(''Entry to Subroutine F2D_SDLUT'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_SDLUT ' // version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably
!  defined
     If (xmaxdat .Lt. 1 .Or. xmax_lut .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1 .Or. ymax_lut .Lt. 1) Then
        status = St_bad_dim2
     Else If (xnum_lut .Lt. 1 .Or. xnum_lut .Gt. xmax_lut) Then
        status = St_bad_adr1
     Else If (ynum_lut .Lt. 1 .Or. ynum_lut .Gt. ymax_lut) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_SDLUT ' // version)
 
     Else

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
        sd_lut_defined = .False.

!     Set Intersection of ROI and valid spline region
        x_low = 1
        y_low = 1
        x_up = Min(xmaxdat, Int(x_maximum - 0.00001) + 1)
        y_up = Min(ymaxdat, Int(y_maximum - 0.00001) + 1)
 
!     Obtain dynamic array space for internal arrays
        maxwork = x_up + 1
        Allocate (X_DISTORTION(maxwork, y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (Y_DISTORTION(maxwork, y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (X_COORDINATES(maxwork), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(y_rows), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If

 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate spatial distortion
        Call F2D_SDLUTSUB ( x_minimum, y_minimum, x_maximum, y_maximum, &
          x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
          y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, &
          YAXIS, maxwork, y_rows, xmax_lut, ymax_lut, xnum_lut, ynum_lut, &
          X_COORDINATES, Y_COORDINATES, X_DISTORTION,  Y_DISTORTION, &
          X_SD, Y_SD, INT_REBINNED, sd_lut_defined, status)
 
!     Free dynamic array space
        Deallocate (X_DISTORTION)
        Deallocate (Y_DISTORTION)
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)

     End If
 
     End Subroutine F2D_SDLUT
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!+ F2D_SDLUTSUB - FIT2D: create Spatial Distortion Look-Up Table
     Subroutine F2D_SDLUTSUB ( x_minimum, y_minimum, x_maximum, y_maximum, &
       x_xnumknots, x_ynumknots, X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, &
       y_ynumknots, Y_LAMBDA, Y_MU, Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, &
       maxwork, y_rows, xmax_lut, ymax_lut, xnum_lut, ynum_lut, X_COORDINATES, &
       Y_COORDINATES, X_DISTORTION, Y_DISTORTION, X_SD, Y_SD, INT_REBINNED, &
       sd_lut_defined, status)
!  Description:
!    Creates spatial distortion look-up table, for fast calculation
!    of triangular re-binned distortion correction.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Sep-1997: V0.4 Take into account unsigned byte nature of fraction 
!      array when correcting total fractions to equal 255, and account for all 
!      possible rounding effects (Hammersley)
!    04-Mar-1997: V0.3 Shift fractions down and left if none of the output 
!      fractions of the first row or column contain output intensity 
!      (Hammersley)
!    09-Oct-1996: V0.2 Give warning message if distortions of over +-127 pixels 
!      are encountered, but carry on regardless (Hammersley)
!    03-Oct-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Real, Intent(IN) :: x_minimum ! Minimum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_minimum ! Minimum Y-value applicable to spline
!      interpolation
     Real, Intent(IN) :: x_maximum ! Maximum X-value applicable to spline
!      interpolation
     Real, Intent(IN) :: y_maximum ! Maximum Y-value applicable to spline
!      interpolation
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots) ! X-Positions of spline "knots" 
!      for X-distortion function
     Real, Intent(IN) :: X_MU(x_ynumknots) ! Y-Positions of spline "knots" for 
!      X-distortion function
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots) ! X-Positions of spline "knots" 
!      for Y-distortion function
     Real, Intent(IN) :: Y_MU(y_ynumknots) ! Y-Positions of spline "knots" for 
!      Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
!      Coefficients of spline function for Y-distortion function
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Integer, Intent(IN) :: maxwork ! Dimension of work arrays, must be at
!      least "xnum_lut + 1"
     Integer, Intent(IN) :: y_rows ! Second dimension of work arrays; number
!      of rows of the distortion to be calculated in one block
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xnum_lut ! Number of elements to define in LUT
!      in X-direction
     Integer, Intent(IN) :: ynum_lut ! Number of elements to define in LUT
!      in X-direction
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxwork) ! Used to store
!      X-coordinates for edges of pixels
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows) ! Used to store
!      Y-coordinates for edges of pixels
     Real, Intent(INOUT) :: X_DISTORTION(maxwork * y_rows) ! Work array for
!      X-distortion values
     Real, Intent(INOUT) :: Y_DISTORTION(maxwork * y_rows) ! Work array for
!      Y-distortion values
!  Export:
     Byte, Intent(OUT) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(OUT) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(OUT) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 8 pixels which are the 
!      target pixel and the 8 nearest pixels with the exception of the upper
!      right pixel. Each fraction is stored as a byte, so change to unsigned, 
!      and divide by 256, to obtain the required fraction. Since the total must 
!      equal 1.0, the fraction for the upper right pixel can be deduced. The 
!      order of the fractions is lower left, lower middle, lower right, left of 
!      target, target, right of target, upper left, and upper middle.
     Logical, Intent(OUT) :: sd_lut_defined ! .True., if the spatial distortion
!      look-up table is properly defined
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: cal_rows ! The number of rows to distortion values to be 
!      calculated in one call to "MA_CAL_2DNCUBSPLINE"
     Integer :: index ! Loop variable for output pixel fractions
     Integer :: l_index ! Index into lower element of distortion arrays
     Integer :: largest_pixel ! The pixel out of the 9 target pixels
!      with the largest output
     Integer :: lower ! The row within the distortion arrays which
!      contains the lower edge corners
     Integer :: num_add ! Number of fraction units to add to make 255
     Integer :: num_subtract ! Number of fraction units to subtract
     Integer :: pixel ! Loop variable for pixels
     Integer :: rebin_value ! Fraction of input pixel rebinned into
!      an output pixel, multipled by 255 and rounded
     Integer :: second_pixel ! The pixel out of the 9 target pixels
!      with the second largest output
     Integer stat ! Status return variable for "Allocate"
     Integer :: sum ! Sum of re-binned rounded scaled fractions
     Integer :: third_pixel ! The pixel out of the 9 target pixels
!      with the third largest output
     Integer :: u_index ! Index into upper element of distortion arrays
     Integer :: x ! Loop variable for X-direction
     Integer :: x_num_warnings ! Number of warning messages in X-direction
     Integer :: x_sd_pixels ! The rounded spatial distortion in the
!      X-direction in pixels
     Integer :: xmax_work ! Dimension size of "X_WORK"
     Integer :: xnumelm ! Number of input elements in X-direction
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_calculated ! Last calculated Y-pixel number for block of
!      calculated distortion values
     Integer :: y_lbase ! Base index into distortion arrays for lower Y-rows
     Integer :: y_num_warnings ! Number of warning messages in Y-direction
     Integer :: y_sd_pixels ! The rounded spatial distortion in the
!      Y-direction in pixels
     Integer :: y_start ! Starting Y-pixel number for block of
!      calculated distortion values
     Integer :: y_ubase ! Base index into distortion arrays for upper Y-rows
     Integer :: ybis ! Loop variable for Y-direction
     Integer :: ymax_work ! Dimension size of "Y_WORK"
     Logical :: x_problem ! .True., if distortion values greater than
!      +-127 pixels are found in the X-direction
     Logical :: y_problem ! .True., if distortion values greater than
!      +-127 pixels are found in the Y-direction
     Real :: area1 ! Area of "lower" triangle of quadrilateral
     Real :: area2 ! Area of "upper" triangle of quadrilateral
     Real :: height ! Height of triangle from "(xi3, yi3)" and line from
!      "(xi1, yi1)" to "(xi2, yi2)"
     Real :: largest ! The largest re-bin fraction
     Real :: second ! The second largest re-bin fraction
     Real :: third ! The third largest re-bin fraction
     Real :: time_cpu_end ! CPU time at end of process
     Real :: time_cpu_str ! CPU time at start of process
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
     Real :: x_min ! Minimum X-position of a quadrilateral
     Real :: x_half ! The average spacing from centre of a pixel to its
!      edges in the X-direction
     Real :: x1, x2, x3, x4 ! X-coordinates of corners of quadrilateral
     Real :: xi1, yi1, xi2, yi2, xi3, yi3 ! Permuted triangle vertices
     Real :: xc1, yc1, xc2, yc2, xc3, yc3, xc4, yc4 ! Recentred corner
!      coordinates
     Real :: y1, y2, y3, y4 ! Y-coordinates of corners of quadrilateral
!      edges in the Y-direction
     Real :: y_min ! Minimum Y-coordinate of a quadrilateral
     Real :: y_half ! The average spacing from centre of a pixel to its
     Real :: y_dash ! Y-coordinate of intersection of vertical from
!      "(xi3, yi3)" and line between "(xi1, yi1)" and "(xi2, yi2)"
!  Local Arrays:
     Real :: FRACTIONS(9) ! The fraction of an input pixel re-binned
!      into each output pixel around the target output pixel
     Real, Allocatable :: X_IWORK(:) ! Dynamic work array
     Real, Allocatable :: X_WORK(:) ! Dynamic work array
     Real, Allocatable :: Y_IWORK(:) ! Dynamic work array
     Real, Allocatable :: Y_WORK(:) ! Dynamic work array
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_SDLUTSUB'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_SDLUTSUB ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1 .Or. xmax_lut .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1 .Or. ymax_lut .Lt. 1) Then
        status = St_bad_dim2
     Else If (xnum_lut .Lt. 1 .Or. xnum_lut .Gt. xmax_lut) Then
        status = St_bad_adr1
     Else If (ynum_lut .Lt. 1 .Or. ynum_lut .Gt. ymax_lut) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_SDLUTSUB ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Initialise variables
        x_half = (XAXIS(xnum_lut) - XAXIS(1)) / (2.0 * Real(xnum_lut - 1))
        y_half = (YAXIS(ynum_lut) - YAXIS(1)) / (2.0 * Real(ynum_lut - 1))
        x_problem = .False.
        y_problem = .False.
 
!     Obtain dynamic array space for internal arrays
        xnumelm = xnum_lut + 1
 
!     Calculate size of work arrays for "MA_CAL_2DNCUBSPLINE"
        xmax_work = 4 * (xnum_lut + 1)
        ymax_work = 4 * y_rows
        Allocate (X_WORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (Y_WORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (X_IWORK(xmax_work / 4), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If
        Allocate (Y_IWORK(ymax_work / 4), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_SDLUT ' // Version)
           Return
        End If

!     Set X-coordinates of input pixel edges (which never vary)
        Do x = 0, xnumelm - 2
           X_COORDINATES(x + 1) = XAXIS(x + 1) - x_half
        End Do
        X_COORDINATES(xnumelm) = XAXIS(xnum_lut) + x_half
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(10f8.1)')
!     :       (X_COORDINATES(x), x = 1, 32)
!     Write (*, '(''...'')')
!     Write (*, '(10f8.1)')
!     :       (X_COORDINATES(x), x = xnumelm-32, xnumelm)
!     Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Loop through input data, one row at a time, and calculate
!     output pixel positions, and add appropriate elements of
!     output array
        Call IO_WRITE ('INFO: Starting to create spatial ' // &
          'distortion look-up table', status)
        x_num_warnings = 0
        y_num_warnings = 0
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu_str, status)
 
        y_start =  -(y_rows - 1)
        y_calculated = 1
        Do y = 1, ynum_lut
 
!        Calculate new block of distortion values if necessary
           If (y .Ge. y_calculated) Then
 
!           Number of rows to calculate distortion
              cal_rows = Min(y_rows, ynum_lut + 2 - y_calculated)
 
!           (NOTE: THE FIRST ROW OF EACH BLOCK IS THE LAST ROW OF THE
!           PREVIOUS BLOCK, SO THIS COULD BE MORE EFFICIENT)
 
!           Set Y-coordinates of input pixel edges
              Do ybis = 0, cal_rows - 2
                 Y_COORDINATES(ybis + 1) = YAXIS(ybis + y_calculated) - y_half
              End Do
              Y_COORDINATES(cal_rows) = YAXIS(cal_rows + y_calculated - 2) + &
                y_half
 
!           Calculate distortions at the corners of all the pixels
              Call MA_CAL_2DNCUBSPLINE (1, x_xnumknots, x_xnumknots, X_LAMBDA, &
                x_ynumknots, x_ynumknots, X_MU, &
                (x_xnumknots - 4) * (x_ynumknots - 4), X_COEFFS, maxwork, &
                X_COORDINATES, y_rows, Y_COORDINATES, 1, 1, xnumelm, cal_rows, &
                xmax_work, ymax_work, X_DISTORTION, X_WORK, &
                Y_WORK, X_IWORK, Y_IWORK, status)
 
              Call MA_CAL_2DNCUBSPLINE (1, y_xnumknots, y_xnumknots, Y_LAMBDA, &
                y_ynumknots, y_ynumknots, Y_MU, &
                (y_xnumknots - 4) * (y_ynumknots - 4), Y_COEFFS, maxwork, &
                X_COORDINATES, y_rows, Y_COORDINATES, 1, 1, xnumelm, cal_rows, &
                xmax_work, ymax_work, Y_DISTORTION, X_WORK, &
                Y_WORK, X_IWORK, Y_IWORK, status)
 
!           Update last calculated row
              y_start = y_start + y_rows - 1
              y_calculated = y_calculated + y_rows - 1
 
           End If
 
!        The arrays "X_DISTORTION" and "Y_DISTORTION" now contain
!        the distortion for the corners of the pixels from row
!        "y_start" to "y_calculated". The first row is the lower
!        edge of "y_start" and the second row is the upper edge
!        of "y_start" and the lower edge of "y_start + 1". The first
!        element in the x-direction is the left corner of the first
!        pixel, and the next element is the right edge.
 
!        Calculate row within distortion arrays which corresponds
!        to the lower edge of the pixels
           lower = y - y_start
           y_ubase = lower * maxwork
           y_lbase = y_ubase - maxwork
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''maxwork, maxwork*y_rows = '', 2i6)')
!        :          maxwork, maxwork*y_rows
!        Write (*, '(''y, y_start = '', 2i6)') y, y_start
!        Write (*, '(''x, y_lbase, y_ubase = '', 3i6)')
!        :          x, y_lbase, y_ubase
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Calculate initial "right" edge coordinates: really the left edge
           x2 = X_DISTORTION(1 + y_lbase)
           y2 = Y_DISTORTION(1 + y_lbase)
           x4 = X_DISTORTION(1 + y_ubase)
           y4 = Y_DISTORTION(1 + y_ubase)
 
!        Loop over row of input pixels, calculating coverage of
!        output pixels
           l_index = y_lbase + 1
           u_index = y_ubase + 1
           Do x = 1, xnumelm - 1
 
!           Coordinates of four corners distortion values
              l_index = l_index + 1
              u_index = u_index + 1
              x1 = x2
              y1 = y2
              x2 = X_DISTORTION(l_index)
              y2 = Y_DISTORTION(l_index)
              x3 = x4
              y3 = y4
              x4 = X_DISTORTION(u_index)
              y4 = Y_DISTORTION(u_index)
 
!           Coordinates of four corners of pixel
              xc1 = x1
              xc2 = x2 + 1.0
              xc3 = x3
              xc4 = x4 + 1.0
              yc1 = y1
              yc2 = y2
              yc3 = y3 + 1.0
              yc4 = y4 + 1.0
 
!           Calculate areas of the output triangles
 
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD
!           Call MA_CAL_TRIAREA (xc1, yc1, xc2, yc2, xc3, yc3,
!           :                area1, status)
!           Call MA_CAL_TRIAREA (xc4, yc4, xc2, yc2, xc3, yc3,
!           :                area2, status)
!**OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD-CODE***OLD
 
!**INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSER
!           Check for trivial cases
              If (Abs((xc2 - xc1) / xc1) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc3 - xc1) * (yc2 - yc1))
              Else If (Abs((xc3 - xc1) / xc1) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc2 - xc1) * (yc3 - yc1))
              Else If (Abs((xc3 - xc2) / xc2) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc1 - xc3) * (yc3 - yc2))
              Else If (Abs((yc2 - yc1) / yc1) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc2 - xc1) * (yc3 - yc1))
              Else If (Abs((yc3 - yc1) / yc1) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc3 - xc1) * (yc2 - yc1))
              Else If (Abs((yc3 - yc2) / yc2) .Lt. 1.0e-5) Then
                 area1 = Abs(0.5 * (xc3 - xc2) * (yc3 - yc1))
              Else
 
!              Swap if necessary, so that '(xi3, yi3)' is the middle
!              X-coordinate
                 If (xc3 .Gt. xc1) Then
 
                    If (xc3 .Lt. xc2) Then
                       xi1 = xc1
                       yi1 = yc1
                       xi2 = xc2
                       yi2 = yc2
                       xi3 = xc3
                       yi3 = yc3
                    Else
 
                       If (xc1 .Lt. xc2) Then
                          xi1 = xc1
                          yi1 = yc1
                          xi2 = xc3
                          yi2 = yc3
                          xi3 = xc2
                          yi3 = yc2
                       Else
                          xi1 = xc3
                          yi1 = yc3
                          xi2 = xc2
                          yi2 = yc2
                          xi3 = xc1
                          yi3 = yc1
                       End If
 
                    End If
 
                 Else
 
                    If (xc3 .Gt. xc2) Then
                       xi1 = xc1
                       yi1 = yc1
                       xi2 = xc2
                       yi2 = yc2
                       xi3 = xc3
                       yi3 = yc3
                    Else
 
                       If (xc2 .Lt. xc1) Then
                          xi1 = xc1
                          yi1 = yc1
                          xi2 = xc3
                          yi2 = yc3
                          xi3 = xc2
                          yi3 = yc2
                       Else
                          xi1 = xc3
                          yi1 = yc3
                          xi2 = xc2
                          yi2 = yc2
                          xi3 = xc1
                          yi3 = yc1
                       End If
 
                    End If
 
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!              Calculate point on line 1-2 directly below '(xi3, yi3)'
                 y_dash = yi1 + (xi3 - xi1) * (yi2 - yi1) / (xi2 - xi1)
                 height = (yi3 - y_dash)
 
!              Calculate area of interior triangle
                 area1 = Abs(0.5 * (xi2 - xi1) * height)
 
              End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
 
!           Check for trivial cases
              If (Abs((xc2 - xc4) / xc4) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc3 - xc4) * (yc2 - yc4))
              Else If (Abs((xc3 - xc4) / xc4) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc2 - xc4) * (yc3 - yc4))
              Else If (Abs((xc3 - xc2) / xc2) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc4 - xc3) * (yc3 - yc2))
              Else If (Abs((yc2 - yc4) / yc4) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc2 - xc4) * (yc3 - yc4))
              Else If (Abs((yc3 - yc4) / yc4) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc3 - xc4) * (yc2 - yc4))
              Else If (Abs((yc3 - yc2) / yc2) .Lt. 1.0e-5) Then
                 area2 = Abs(0.5 * (xc3 - xc2) * (yc3 - yc4))
              Else
 
!              Swap if necessary, so that '(xi3, yi3)' is the middle
!              X-coordinate
                 If (xc3 .Gt. xc4) Then
 
                    If (xc3 .Lt. xc2) Then
                       xi1 = xc4
                       yi1 = yc4
                       xi2 = xc2
                       yi2 = yc2
                       xi3 = xc3
                       yi3 = yc3
                    Else
 
                       If (xc4 .Lt. xc2) Then
                          xi1 = xc4
                          yi1 = yc4
                          xi2 = xc3
                          yi2 = yc3
                          xi3 = xc2
                          yi3 = yc2
                       Else
                          xi1 = xc3
                          yi1 = yc3
                          xi2 = xc2
                          yi2 = yc2
                          xi3 = xc4
                          yi3 = yc4
                       End If
 
                    End If
 
                 Else
 
                    If (xc3 .Gt. xc2) Then
                       xi1 = xc4
                       yi1 = yc4
                       xi2 = xc2
                       yi2 = yc2
                       xi3 = xc3
                       yi3 = yc3
                    Else
 
                       If (xc2 .Lt. xc4) Then
                          xi1 = xc4
                          yi1 = yc4
                          xi2 = xc3
                          yi2 = yc3
                          xi3 = xc2
                          yi3 = yc2
                       Else
                          xi1 = xc3
                          yi1 = yc3
                          xi2 = xc2
                          yi2 = yc2
                          xi3 = xc4
                          yi3 = yc4
                       End If
 
                    End If
 
                 End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!              Calculate point on line 1-2 directly below '(xi3, yi3)'
                 y_dash = yi1 + (xi3 - xi1) * (yi2 - yi1) / (xi2 - xi1)
                 height = (yi3 - y_dash)
 
!              Calculate area of interior triangle
                 area2 = Abs(0.5 * (xi2 - xi1) * height)
 
              End If
 
!**INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSERTED-CODE***INSER
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''raw xc1, xc2, xc3, xc4 = '', 4f12.3)')
!           :       xc1, xc2, xc3, xc4
!           Write (*, '(''raw yc1, yc2, yc3, yc4 = '', 4f12.3)')
!           :       yc1, yc2, yc3, yc4
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!           Calculate minimum corner position
              x_min = Min(xc1, xc2, xc3, xc4)
              y_min = Min(yc1, yc2, yc3, yc4)
 
!           Integer pixel shift
              If (x_min .Ge. 0) Then
                 x_sd_pixels = Int(x_min)
              Else
                 x_sd_pixels = Int(x_min) - 1
              End If
              If (y_min .Ge. 0) Then
                 y_sd_pixels = Int(y_min)
              Else
                 y_sd_pixels = Int(y_min) - 1
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''x_sd_pixels, y_sd_pixels = '', 2i6)')
!           :             x_sd_pixels, y_sd_pixels
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              If (Abs(x_sd_pixels) .Le. 127) Then
 
!              Store pixel shifts in look-up table
                 x_problem = .True.
                 X_SD(x, y) = x_sd_pixels
 
              Else
 
!              True distortion is too great to store
                 If (x_sd_pixels .Gt. 127) Then
                    X_SD(x, y) = 127
                 Else
                    X_SD(x, y) = -127
                 End If
 
              End If
 
              If (Abs(y_sd_pixels) .Le. 127) Then
 
!              Store pixel shifts in look-up table
                 Y_SD(x, y) = y_sd_pixels
              Else
 
!              True distortion is too great to store
                 y_problem = .True.
                 If (y_sd_pixels .Gt. 127) Then
                    Y_SD(x, y) = 127
                 Else
                    Y_SD(x, y) = -127
                 End If
 
 
              End If
 
!           Check that distortion is not a point discontinuity
              If (area1 + area2 .Gt. 1.0e-6) Then
 
!              Correct output positions to 3x3 'target'
                 xc1 = xc1 - Real(x_sd_pixels)
                 xc2 = xc2 - Real(x_sd_pixels)
                 xc3 = xc3 - Real(x_sd_pixels)
                 xc4 = xc4 - Real(x_sd_pixels)
                 yc1 = yc1 - Real(y_sd_pixels)
                 yc2 = yc2 - Real(y_sd_pixels)
                 yc3 = yc3 - Real(y_sd_pixels)
                 yc4 = yc4 - Real(y_sd_pixels)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''cor xc1, xc2, xc3, xc4 = '', 4f12.3)')
!              :       xc1, xc2, xc3, xc4
!              Write (*, '(''cor yc1, yc2, yc3, yc4 = '', 4f12.3)')
!              :       yc1, yc2, yc3, yc4
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Set output pixels to zero
                 Do pixel = 1, 9
                    FRACTIONS(pixel) = 0.0
                 End Do
 
!              Distribute intensity according to fractional
!              covered area
                 Call MA_TRIREBIN (xc1, yc1, xc2, yc2, xc3, yc3, 1.0 / (area1 &
                   + area2), area1, 3, 3, 1, 1, 3, 3, x_num_warnings, &
                   y_num_warnings, FRACTIONS, status)
                 Call MA_TRIREBIN (xc4, yc4, xc2, yc2, xc3, yc3, 1.0 / (area1 &
                   + area2), area2, 3, 3, 1, 1, 3, 3, x_num_warnings, &
                   y_num_warnings, FRACTIONS, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(3f6.3)') (FRACTIONS(pixel), pixel=7,9)
!              Write (*, '(3f6.3)') (FRACTIONS(pixel), pixel=4,6)
!              Write (*, '(3f6.3)') (FRACTIONS(pixel), pixel=1,3)
!              Read (*, *)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!              Store fractions multipled by 255.0
                 sum = 0
                 Do pixel = 1, 9
 
                    rebin_value = Nint(FRACTIONS(pixel) * 255.0)
                    sum = sum + rebin_value
                    If (rebin_value .Le. 127) Then
                       INT_REBINNED(pixel, x, y) = rebin_value
                    Else
                       INT_REBINNED(pixel, x, y) = rebin_value - 256
                    End If
 
                 End Do
 
!              Don't allow rounding error to lead to non-conservation
!              of intensity
                 If (sum .Ne. 255) Then
 
                    If (sum .Eq. 254 .Or. sum .Eq. 256) Then
 
!                    Add or subtract one unit to/from largest affected pixel
                       largest = FRACTIONS(1)
                       largest_pixel = 1
                       Do pixel = 2, 9
 
                          If (FRACTIONS(pixel) .Gt. largest) Then
                             largest_pixel = pixel
                             largest = FRACTIONS(pixel)
                          End If
 
                       End Do
 
                       If (sum .Eq. 254) Then
 
                          Call F2D_SDADD1( INT_REBINNED(largest_pixel, x, y))
 
                       Else
 
                          Call F2D_SDSUB1( INT_REBINNED(largest_pixel, x, y))
                       End If
 
                    Else If (sum .Eq. 253 .Or. sum .Eq. 257) Then
 
!                    Add or subtract one unit to/from largest and
!                    second largest affected pixel
                       If (FRACTIONS(1) .Ge. FRACTIONS(2)) Then
                          largest = FRACTIONS(1)
                          largest_pixel = 1
                          second = FRACTIONS(2)
                          second_pixel = 2
                       Else
                          largest = FRACTIONS(2)
                          largest_pixel = 2
                          second = FRACTIONS(1)
                          second_pixel = 1
                       End If
                       Do pixel = 3, 9
 
                          If (FRACTIONS(pixel) .Gt. largest) Then
                             second = largest
                             second_pixel = largest_pixel
                             largest_pixel = pixel
                             largest = FRACTIONS(pixel)
                          Else If (FRACTIONS(pixel) .Gt. second) Then
                             second = FRACTIONS(pixel)
                             second_pixel = pixel
                          End If
 
                       End Do
                       If (sum .Eq. 253) Then
                          Call F2D_SDADD1( INT_REBINNED(largest_pixel, x, y))
                          Call F2D_SDADD1( INT_REBINNED(second_pixel, x, y))
                       Else
                          Call F2D_SDSUB1( INT_REBINNED(largest_pixel, x, y))
                          Call F2D_SDSUB1( INT_REBINNED(second_pixel, x, y))
                       End If
 
                    Else If (sum .Eq. 252 .Or. sum .Eq. 258) Then
 
!                    Add one unit to largest, second, and third
!                    largest affected pixels
                       If (FRACTIONS(1) .Ge. FRACTIONS(2)) Then
 
                          If (FRACTIONS(2) .Ge. FRACTIONS(3)) Then
                             largest = FRACTIONS(1)
                             largest_pixel = 1
                             second = FRACTIONS(2)
                             second_pixel = 2
                             third = FRACTIONS(3)
                             third_pixel = 3
                          Else If (FRACTIONS(1) .Ge. FRACTIONS(3)) Then
                             largest = FRACTIONS(1)
                             largest_pixel = 1
                             second = FRACTIONS(3)
                             second_pixel = 3
                             third = FRACTIONS(2)
                             third_pixel = 2
                          Else
                             largest = FRACTIONS(3)
                             largest_pixel = 3
                             second = FRACTIONS(1)
                             second_pixel = 1
                             third = FRACTIONS(2)
                             third_pixel = 2
                          End If
 
                       Else If (FRACTIONS(2) .Ge. FRACTIONS(3)) Then
 
                          If (FRACTIONS(1) .Ge. FRACTIONS(3)) Then
                             largest = FRACTIONS(2)
                             largest_pixel = 2
                             second = FRACTIONS(1)
                             second_pixel = 1
                             third = FRACTIONS(3)
                             third_pixel = 3
                          Else
                             largest = FRACTIONS(2)
                             largest_pixel = 2
                             second = FRACTIONS(3)
                             second_pixel = 3
                             third = FRACTIONS(1)
                             third_pixel = 1
                          End If
 
                       Else
                          largest = FRACTIONS(3)
                          largest_pixel = 3
                          second = FRACTIONS(2)
                          second_pixel = 2
                          third = FRACTIONS(1)
                          third_pixel = 1
                       End If
                       Do pixel = 4, 9
 
                          If (FRACTIONS(pixel) .Gt. largest) Then
                             third = second
                             third_pixel = second_pixel
                             second = largest
                             second_pixel = largest_pixel
                             largest_pixel = pixel
                             largest = FRACTIONS(pixel)
                          Else If (FRACTIONS(pixel) .Gt. second) Then
                             third = second
                             third_pixel = second_pixel
                             second = FRACTIONS(pixel)
                             second_pixel = pixel
                          Else If (FRACTIONS(pixel) .Gt. third) Then
                             third = second
                             third_pixel = second_pixel
                          End If
 
                       End Do
 
                       If (sum .Eq. 252) Then
 
                          Call F2D_SDADD1( INT_REBINNED(largest_pixel, x, y))
                          Call F2D_SDADD1( INT_REBINNED(second_pixel, x, y))
                          Call F2D_SDADD1( INT_REBINNED(third_pixel, x, y))
 
                       Else
 
                          Call F2D_SDSUB1( INT_REBINNED(largest_pixel, x, y))
                          Call F2D_SDSUB1( INT_REBINNED(second_pixel, x, y))
                          Call F2D_SDSUB1( INT_REBINNED(third_pixel, x, y))
 
                       End If
 
                    Else If (sum .Eq. 251) Then
 
                       Call F2D_SDADD1( INT_REBINNED(1, x, y))
                       Call F2D_SDADD1( INT_REBINNED(2, x, y))
                       Call F2D_SDADD1( INT_REBINNED(4, x, y))
                       Call F2D_SDADD1( INT_REBINNED(5, x, y))
 
                    Else If (sum .Eq. 250) Then
 
                       Call F2D_SDADD1( INT_REBINNED(1, x, y))
                       Call F2D_SDADD1( INT_REBINNED(2, x, y))
                       Call F2D_SDADD1( INT_REBINNED(4, x, y))
                       Call F2D_SDADD1( INT_REBINNED(5, x, y))
                       Call F2D_SDADD1( INT_REBINNED(6, x, y))
 
                    Else If (sum .Le. 249) Then
 
                       num_add = 255 - sum
                       Do index = 1, num_add
                          Call F2D_SDADD1( INT_REBINNED(index, x, y))
                       End Do
 
                    Else If (sum .Ge. 259) Then
 
                       num_subtract = sum - 255
                       index = 1
                       Do While (num_subtract .Gt. 0)
 
                          If (INT_REBINNED(index, x, y) .Ne. 0) Then
 
                             num_subtract = num_subtract - 1
                             Call F2D_SDSUB1( INT_REBINNED(index, x, y))
 
                          End If
 
                          index = index + 1
 
                       End Do
 
                    End If
 
                 End If
 
              Else
                 Do pixel = 1, 4
                    INT_REBINNED(pixel, x, y) = 0
                 End Do
                 INT_REBINNED(5, x, y) = -1
                 Do pixel = 6, 9
                    INT_REBINNED(pixel, x, y) = 0
                 End Do
 
              End If
 
!           If first column of output fractions are all zero
!           left shift fractions and add 1 to the X-distortion shift
              If (INT_REBINNED(1, x, y) .Eq. 0 .And. INT_REBINNED(4, x, y) &
                .Eq. 0 .And. INT_REBINNED(7, x, y) .Eq. 0) Then
 
                 If (X_SD(x, y) .Lt. 127) Then
                    X_SD(x, y) = X_SD(x, y) + 1
                 End If
                 INT_REBINNED(1, x, y) = INT_REBINNED(2, x, y)
                 INT_REBINNED(2, x, y) = INT_REBINNED(3, x, y)
                 INT_REBINNED(3, x, y) = 0
                 INT_REBINNED(4, x, y) = INT_REBINNED(5, x, y)
                 INT_REBINNED(5, x, y) = INT_REBINNED(6, x, y)
                 INT_REBINNED(6, x, y) = 0
                 INT_REBINNED(7, x, y) = INT_REBINNED(8, x, y)
                 INT_REBINNED(8, x, y) = INT_REBINNED(9, x, y)
                 INT_REBINNED(9, x, y) = 0
              End If
 
!           If first row of output fractions are all zero down shift fractions 
!           and add 1 to the Y-distortion  shift
              If (INT_REBINNED(1, x, y) .Eq. 0 .And. INT_REBINNED(2, x, y) &
                .Eq. 0 .And. INT_REBINNED(3, x, y) .Eq. 0) Then
 
                 If (Y_SD(x, y) .Lt. 127) Then
                    Y_SD(x, y) = Y_SD(x, y) + 1
                 End If
                 INT_REBINNED(1, x, y) = INT_REBINNED(4, x, y)
                 INT_REBINNED(4, x, y) = INT_REBINNED(7, x, y)
                 INT_REBINNED(7, x, y) = 0
                 INT_REBINNED(2, x, y) = INT_REBINNED(5, x, y)
                 INT_REBINNED(5, x, y) = INT_REBINNED(8, x, y)
                 INT_REBINNED(8, x, y) = 0
                 INT_REBINNED(3, x, y) = INT_REBINNED(6, x, y)
                 INT_REBINNED(6, x, y) = INT_REBINNED(9, x, y)
                 INT_REBINNED(9, x, y) = 0
              End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Check that the sum equals 255
!           sum = 0
!           Do index = 1, 9
!           If (INT_REBINNED(index, x, y) .Ge. 0) Then
!           sum = sum + INT_REBINNED(index, x, y)
!           Else
!           sum = sum + INT_REBINNED(index, x, y) + 256
!           End If
 
!           End Do
 
!           If (sum .Ne. 255) Then
 
!           Write (message, '(''ERROR: The sum of the ' //
!           :                'fractions is not 255 at pixel '', 2i6)')
!           :                x, y
!           Call IO_WRITE (message, status)
!           Write (message, '(''       Sum of output ' //
!           :                'fractions = '', i5)') sum
!           Call IO_WRITE (message, status)
 
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
           End Do
 
!        Check for problems
           If (status .Ne. St_goodvalue) Then
              Return
 
!           Output progress
           Else If (Mod(y, 200) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows calculated = '', ' // &
                'i6, '' ('',i3, ''%)'')') y, Int(100.0 * Real(y) / &
                Real(ynum_lut))
              Call IO_WRITE (message, status)
 
           End If
 
        End Do
 
!     Time for operation
        Call IO_TIMES (timend, time_cpu_end, status)
        Write (message, '(''INFO: Time for creation of LUT = '',' // &
          'f12.2, '' seconds'')') timend - timstr
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: CPU Time for creation of ' // &
          'LUT = '',  f12.2, '' seconds'')') time_cpu_end - time_cpu_str
        Call IO_WRITE (message, status)
 
!     Output warning message if the distortion was too large to
!     be stored in a single byte
        If (x_problem) Then
 
           Call IO_WRITE ('WARNING: At one or more '  // &
             'positions the X-distortion was too large to be', status)
           Call IO_WRITE ('         stored in the look-up '  // &
             'table (+-127 pixels). At these points', status)
           Call IO_WRITE ('         the distortion is set '  // &
             'to the maximum storable value. If this', status)
           Call IO_WRITE ('         only affects the '  // &
             'extremes of the detector it is probably not', status)
           Call IO_WRITE ('         important.', status)
 
        End If
 
        If (y_problem) Then
 
           Call IO_WRITE ('WARNING: At one or more '  // &
             'positions the Y-distortion was too large to be', status)
           Call IO_WRITE ('         stored in the look-up '  // &
             'table (+-127 pixels). At these points', status)
           Call IO_WRITE ('         the distortion is set '  // &
             'to the maximum storable value. If this', status)
           Call IO_WRITE ('         only affects the '  // &
             'extremes of the detector it is probably not', status)
           Call IO_WRITE ('         important.', status)
 
        End If
 
!     Output message if pixels have been ignored owing to re-binning over more 
!     than three output pixels
        If (x_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' triangles ' // &
             'have been ignored, because they require'')') x_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than three output pixels (X-direction).', status)
        End If
 
        If (y_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' triangles ' // &
             'have been ignored, because they require'')') y_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than three output pixels (Y-direction).', status)
        End If
 
!     Free dynamic array space
        Deallocate (X_WORK)
        Deallocate (X_IWORK)
        Deallocate (Y_WORK)
        Deallocate (Y_IWORK)
 
        If (status .Eq. St_goodvalue) Then
           sd_lut_defined = .True.
        End If
 
     End If
 
     End Subroutine F2D_SDLUTSUB
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_sdadd1.f90 *
!  *                *
!  ******************
 
!+ F2D_SDADD1 - FIT2D: Add 1 to an (unsigned) byte value
     Subroutine F2D_SDADD1 (byte_value)
!  Description:
!    Adds one to a byte value taking into account the signed
!    nature of the storage
!  Method:
!  Deficiencies:
!    Note: No range checking, must be used in correct context
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Sep-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Byte, Intent(INOUT) :: byte_value ! Value to be incremented
!  Export:
!  Status:
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: int_value ! Integer value
!  Local Arrays:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
     If (byte_value .Ge. 0) Then
        int_value = byte_value + 1
     Else
        int_value = 256 + byte_value + 1
     End If
 
     If (int_value .Le. 127) Then
        byte_value = int_value
     Else
        byte_value = int_value - 256
     End If
 
     End Subroutine F2D_SDADD1
!********1*********2*********3*********4*********5*********6*********7*********8
!********1*********2*********3*********4*********5*********6*********7*********8
 
!+ F2D_SDSUB1 - FIT2D: SUBtract 1 from an (unsigned) byte value
     Subroutine F2D_SDSUB1 (byte_value)
!  Description:
!    Subtracts one from a byte value taking into account the signed
!    nature of the storage
!  Method:
!  Deficiencies:
!    Note: No range checking, must be used in correct context
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    10-Sep-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
!    Include 'st_symbols.inc'
!  Import:
!  Import/Export:
     Byte, Intent(INOUT) :: byte_value ! Value to be incremented
!  Export:
!  Status:
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: int_value ! Integer value
!  Local Arrays:
!  External Functions:
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7---------8
     If (byte_value .Ge. 0) Then
        int_value = byte_value - 1
     Else
        int_value = 256 + byte_value - 1
     End If
 
     If (int_value .Le. 127) Then
        byte_value = int_value
     Else
        byte_value = int_value - 256
     End If
 
     End Subroutine F2D_SDSUB1
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

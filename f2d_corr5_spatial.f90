!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_corr5_spatial.f90 *
!  *                       *
!  *************************
 
!+ F2D_CORR5_SPATIAL - FIT 2-D CORRect SPATIAL distortion
     Subroutine F2D_CORR5_SPATIAL (xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, x_low, y_low, x_up, y_up, x_xnumknots, x_ynumknots, &
       X_LAMBDA, X_MU, X_COEFFS, y_xnumknots, y_ynumknots, Y_LAMBDA, Y_MU, &
       Y_COEFFS, xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, maxxwork, y_rows, &
       X_COORDINATES, Y_COORDINATES, X_DISTORTION, Y_DISTORTION, MDATA, &
       status)
!  Description:
!    Corrects spatial distortions by calculating corrected position
!    of input pixels from two bi-cubic spline functions. The centre
!    of the edges of each pixel are transformed to the corrected grid
!    and the intensity of the input pixel is distributed all the
!    covered output pixels in proportion to the covered area.
!  Method:
!    "MA_CAL_2DNCUBSPLINE" is used to calculate the transformed positions of a 
!    block of a varable number of lines of pixels at the time.
!    The corrected position for corners of each pixel are calculated,
!    and the average of the appropriate corners is used to calculate
!    the X and Y-positions of the centres of the edges. The intensity
!    of the input pixel is distributed over covered output pixels in
!    proportion to cover area.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    31-Mar-2006: V0.5 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    25-Feb-2004: V0.4 Correct output region when input region axis values do 
!      not start at zero (Hammersley)
!    12-Mar-1998: V0.3 Remove inner loops for distribution of intensity 
!      (Hammersley)
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    29-Aug-1996: V0.1 Original, based on "F2D_CORR1_SPATIAL" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region of interest
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region of interest
     Integer, Intent(IN) :: xendelm ! End X-element of region of interest
     Integer, Intent(IN) :: yendelm ! End Y-element of region of interest
     Real, Intent(IN) :: overload_value ! Value above which pixels are
!      considered to be over-loaded. This value is added to all overlapped 
!      pixels
     Integer, Intent(IN) :: x_low ! Lower X-pixel limit of spline region
     Integer, Intent(IN) :: y_low ! Lower Y-pixel limit of spline region
     Integer, Intent(IN) :: x_up ! Upper X-pixel limit of spline  region
     Integer, Intent(IN) :: y_up ! Upper Y-pixel limit of spline region
     Integer, Intent(IN) :: x_xnumknots ! Number of spline "knots" used for
!      X-direction for X-distortion function
     Integer, Intent(IN) :: x_ynumknots ! Number of spline "knots" used for
!      Y-direction of X-distortion function
     Real, Intent(IN) :: X_LAMBDA(x_xnumknots) ! X-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_MU(x_ynumknots) ! Y-Positions of spline
!      "knots" for X-distortion function
     Real, Intent(IN) :: X_COEFFS((x_xnumknots - 4) * (x_ynumknots - 4))
!      Coefficients of spline function for X-distortion function
     Integer, Intent(IN) :: y_xnumknots ! Number of spline "knots" used for
!      X-direction for Y-distortion function
     Integer, Intent(IN) :: y_ynumknots ! Number of spline "knots" used for
!      Y-direction of Y-distortion function
     Real, Intent(IN) :: Y_LAMBDA(y_xnumknots) ! X-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_MU(y_ynumknots) ! Y-Positions of spline
!      "knots" for Y-distortion function
     Real, Intent(IN) :: Y_COEFFS((y_xnumknots - 4) * (y_ynumknots - 4))
!      Coefficients of spline function for Y-distortion function
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(IN) :: maxxwork ! Dimension of work arrays, must be at
!      least "xendelm - xstrelm + 2"
     Integer, Intent(IN) :: y_rows ! Second dimension of work arrays; number
!      of rows of the distortion to be calculated in one block
!  Import/Export:
     Real, Intent(INOUT) :: X_COORDINATES(maxxwork) ! Used to store
!      X-coordinates for edges of pixels
     Real, Intent(INOUT) :: Y_COORDINATES(y_rows) ! Used to store
!      Y-coordinates for edges of pixels
     Real, Intent(INOUT) :: X_DISTORTION(maxxwork, y_rows) ! Work array for
!      X-distortion values
     Real, Intent(INOUT) :: Y_DISTORTION(maxxwork, y_rows) ! Work array for
!      Y-distortion values
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Array containing data
!      to be fitted
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: cal_rows ! The number of rows to distortion values to
!      be calculated in one call
     Integer :: lower ! The row within the distortion arrays which
!      contains the lower edge corners
     Integer stat ! Status return variable for "Allocate"
     Integer :: x ! Loop variable for X-direction
     Integer :: x_num_warnings ! Number of warning messages in X-direction
     Integer :: x_out ! Loop variable for output pixels
     Integer :: xend ! End output X-pixel covered by an input pixel
     Integer :: xendout ! End of output region
     Integer :: xfract ! Index for X-fractions
     Integer :: xmax_work ! Dimension of dynamic work array "X_WORK"
     Integer :: xnumelm ! Number of input elements in X-direction
     Integer :: xnumout ! Number of X-output elements
     Integer :: xstart ! Starting output X-pixel covered by an input pixel
     Integer :: xstrout ! Start of output region
     Integer :: y ! Loop variable for Y-direction
     Integer :: y1 ! Loop variable for Y-direction
     Integer :: y_calculated ! Last calculated Y-pixel number for block of
!      calculated distortion values
     Integer :: y_num_warnings ! Number of warning messages in Y-direction
     Integer :: y_out ! Loop variable for output pixels
     Integer :: y_start ! Starting Y-pixel number for block of calculated 
!      distortion values
     Integer :: yend ! End output Y-pixel covered by an input pixel
     Integer :: yendout ! End of output region
     Integer :: yfract ! Index for Y-fractions
     Integer :: ymax_work ! Dimension of dynamic work array "Y_WORK"
     Integer :: ynumelm ! Number of input elements in Y-direction
     Integer :: ynumout ! Number of output elements
     Integer :: ystart ! Starting output Y-pixel covered by an input pixel
     Integer :: ystrout ! End of output region
     Real :: intensity ! Value of an input pixel
     Real :: time_cpu_spline ! Total CPU time used for the splines
     Real :: time_cpu_end ! CPU time at end of process
     Real :: time_cpu_str ! CPU time at start of process
!    Real time_scpu_end ! Spline CPU time at end of process
!    Real time_scpu_str ! Spline CPU time at start of process
     Real :: time_spline ! Total elapse time used for the splines
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
!    Real timends ! Spline Time at end of operation
!    Real timstrs ! Spline Time at start of operation
     Real :: x_distance ! X-direction size of input pixel, or covered
!      size of output pixel
     Real :: x_half ! The average spacing from centre of a pixel to its
!      edges in the X-direction
     Real :: x_left ! The position of the left edge (centre) of an input
!      pixel in the correct grid
     Real :: x_right ! The position of the right edge (centre) of an input
!      pixel in the correct grid
     Real :: y_distance ! Y-direction size of input pixel, or covered
!      size of output pixel
     Real :: y_half ! The average spacing from centre of a pixel to its
!      edges in the Y-direction
     Real :: y_lower ! The position of the lower edge (centre) of an input
!      pixel in the correct grid
     Real :: y_upper ! The position of the upper edge (centre) of an input
!      pixel in the correct grid
!  Local Arrays:
     Integer, Allocatable :: X_IWORK(:) ! Dynamic work array
     Real, Allocatable :: X_WORK(:) ! Dynamic work array
     Real :: XFRACTIONS(4) ! Fractions of output pixel covered by
!      input pixel in X-direction
     Integer, Allocatable :: Y_IWORK(:) ! Dynamic work array
     Real, Allocatable :: Y_WORK(:) ! Dynamic work array
     Real :: YFRACTIONS(4) ! Fractions of output pixel covered by
!      input pixel in Y-direction
!  Local Data:
!  Saved Variables:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CORR5_SPATIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CORR5_SPATIAL ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1) Then
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. &
       xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. &
       yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CORR5_SPATIAL ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Initialise variables
        x_half = (XAXIS(xendelm) - XAXIS(xstrelm)) / &
          (2.0 * Real(xendelm - xstrelm))
        y_half = (YAXIS(yendelm) - YAXIS(ystrelm)) / &
          (2.0 * Real(yendelm - ystrelm))
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_half, y_half = '', 2f12.6)') x_half, y_half
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Obtain dynamic array space for internal arrays
        xnumelm = x_up - x_low + 2
        ynumelm = y_up - y_low + 2
 
!     Calculate size of work arrays for "MA_CAL_2DNCUBSPLINE"
        xmax_work = 4 * (x_up + 1)
        ymax_work = 4 * y_rows
        Allocate (X_WORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR5_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_WORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR5_SPATIAL ' // Version)
           Return
        End If
        Allocate (X_IWORK(xmax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR5_SPATIAL ' // Version)
           Return
        End If
        Allocate (Y_IWORK(ymax_work), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_CORR5_SPATIAL ' // Version)
           Return
        End If

!     Set X-coordinates of input pixel edges (which never vary)
        Do x = 0, xnumelm - 2
           X_COORDINATES(x + 1) = XAXIS(x + x_low) - x_half
        End Do
        X_COORDINATES(xnumelm) = XAXIS(x_up) + x_half
 
!     Loop through input data, one row at a time, and calculate
!     output pixel positions, and add appropriate elements of output array
        Call IO_WRITE ('INFO: Correcting data for spatial distortion', status)
        x_num_warnings = 0
        y_num_warnings = 0
 
        time_cpu_spline = 0.0
        time_spline = 0.0
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu_str, status)
 
        y_start = y_low - y_rows + 1
        y_calculated = y_low
        Do y = y_low, y_up
 
!        Calculate new block of distortion values if necessary
           If (y .Ge. y_calculated) Then
 
!           Number of rows to calculate distortion
              cal_rows = Min(y_rows, y_up + 2 - y_calculated)
 
!           (NOTE: THE FIRST ROW OF EACH BLOCK IS THE LAST ROW OF THE
!           PREVIOUS BLOCK, SO THIS COULD BE MORE EFFICIENT)
 
!           Set Y-coordinates of input pixel edges
              Do y1 = 0, cal_rows - 2
                 Y_COORDINATES(y1 + 1) = YAXIS(y1 + y_calculated) - y_half
              End Do
              Y_COORDINATES(cal_rows) = YAXIS(cal_rows + y_calculated - 2) + &
                y_half
 
!           Store spline start time
!           Call IO_TIMES (timstrs, time_scpu_str, status)
 
!           Calculate distortions at the corners of all the pixels
              Call MA_CAL_2DNCUBSPLINE (1, x_xnumknots, x_xnumknots, X_LAMBDA, &
                x_ynumknots, x_ynumknots, X_MU, (x_xnumknots - 4) * &
                (x_ynumknots - 4), X_COEFFS, maxxwork, X_COORDINATES, y_rows, &
                Y_COORDINATES, 1, 1, xnumelm, cal_rows, xmax_work, ymax_work, &
                X_DISTORTION, X_WORK, Y_WORK, X_IWORK, Y_IWORK, status)
 
              Call MA_CAL_2DNCUBSPLINE (1, y_xnumknots, y_xnumknots, Y_LAMBDA, &
                y_ynumknots, y_ynumknots, Y_MU, (y_xnumknots - 4) * &
                (y_ynumknots - 4), Y_COEFFS, maxxwork, X_COORDINATES, y_rows, &
                Y_COORDINATES, 1, 1, xnumelm, cal_rows, xmax_work, ymax_work, &
                Y_DISTORTION, X_WORK, Y_WORK, X_IWORK, Y_IWORK, status)
 
              If (status .Ne. St_goodvalue) Then
                 Return
              End If
 
!           Store spline end time
!           Call IO_TIMES (timends, time_scpu_end, status)
 
!           time_cpu_spline = time_cpu_spline +
!           :             time_scpu_end - time_scpu_str
!           time_spline = time_spline + timends - timstrs
 
!           Update last calculated row
              y_start = y_start + y_rows - 1
              y_calculated = y_calculated + y_rows - 1
 
           End If
 
!        The arrays "X_DISTORTION" and "Y_DISTORTION" now contain
!        the distortion for the corners of the pixels from row
!        "y_start" to "y_calculated". The first row is the lower
!        edge of "y_start" and the second row is the upper edge
!        of "y_start" and the lower edge of "y_start+1". The first
!        element in the x-direction is the left corner of the first
!        pixel, and the next element is the right edge.
 
!        Calculate row within distortion arrays which corresponds
!        to the lower edge of the pixels
           lower = y - y_start + 1
 
!        Calculate initial "right" edge which will be used for left
!        edge
           x_right = (X_DISTORTION(1, lower) + X_DISTORTION(1, lower + 1)) / &
             2.0 + XAXIS(x_low) - XAXIS(1)
!        x_right = (X_DISTORTION(1, lower) +
!        :          X_DISTORTION(1, lower + 1)) / 2.0 +
!        :          XAXIS(x_low) - x_half
 
!        Loop over row of input pixels, calculating coverage of
!        output pixels
           Do x = 1, xnumelm - 1
              intensity = DATA(x + x_low - 1, y)
 
!           Calculate positions of corrected pixel edges
              x_left = x_right
              x_right = (X_DISTORTION(x + 1, lower) + X_DISTORTION(x + 1, &
                lower + 1)) / 2.0 + XAXIS(x + x_low - 1) - XAXIS(1) + 2.0 * &
                x_half
              y_lower = (Y_DISTORTION(x, lower) + Y_DISTORTION(x + 1, lower)) &
                / 2.0 + YAXIS(y) - YAXIS(1)
              y_upper = (Y_DISTORTION(x, lower + 1) + Y_DISTORTION(x + 1, &
                lower + 1)) / 2.0 + YAXIS(y) - YAXIS(1) + 2.0 * y_half
!           x_right = (X_DISTORTION(x + 1, lower) +
!           :             X_DISTORTION(x + 1, lower + 1)) / 2.0 +
!           :             XAXIS(x + x_low - 1) + x_half
!           y_lower = (Y_DISTORTION(x, lower) +
!           :             Y_DISTORTION(x + 1, lower)) / 2.0 +
!           :             YAXIS(y) - y_half
!           y_upper = (Y_DISTORTION(x, lower + 1) +
!           :             Y_DISTORTION(x + 1, lower + 1)) / 2.0 +
!           :             YAXIS(y) + y_half
 
!           Calculate pixel coordinate distances covered by the
!           input pixel
              x_distance = x_right - x_left
              y_distance = y_upper - y_lower
              xstart = Int(x_left) + 1
              ystart = Int(y_lower) + 1
              xend = Int(x_right) + 1
              yend = Int(y_upper) + 1
 
!           Calculate fractions of distance for X-direction
              If (xstart .Eq. xend) Then
                 XFRACTIONS(1) = 1.0
              Else If (xend - xstart .Eq. 1) Then
                 XFRACTIONS(1) = (Real(xstart) - x_left) / x_distance
                 XFRACTIONS(2) = 1.0 - XFRACTIONS(1)
              Else If (xend - xstart .Eq. 2) Then
                 XFRACTIONS(1) = (Real(xstart) - x_left) / x_distance
                 XFRACTIONS(2) = 1.0 / x_distance
                 XFRACTIONS(3) = 1.0 - XFRACTIONS(1) - XFRACTIONS(2)
              Else If (xend - xstart .Eq. 3) Then
                 XFRACTIONS(1) = (Real(xstart) - x_left) / x_distance
                 XFRACTIONS(2) = 1.0 / x_distance
                 XFRACTIONS(3) = 1.0 / x_distance
                 XFRACTIONS(4) = 1.0 - XFRACTIONS(1) - XFRACTIONS(2) - &
                   XFRACTIONS(3)
              Else
 
!              One input pixel is trying to be binned into more
!              than four output pixels
                 x_num_warnings = x_num_warnings + 1
 
                 xend = xstart
                 XFRACTIONS(1) = 0.0
              End If
 
!           Calculate fractions of distance for Y-direction
              If (ystart .Eq. yend) Then
                 YFRACTIONS(1) = 1.0
              Else If (yend - ystart .Eq. 1) Then
                 YFRACTIONS(1) = (Real(ystart) - y_lower) / y_distance
                 YFRACTIONS(2) = 1.0 - YFRACTIONS(1)
              Else If (yend - ystart .Eq. 2) Then
                 YFRACTIONS(1) = (Real(ystart) - y_lower) / y_distance
                 YFRACTIONS(2) = 1.0 / y_distance
                 YFRACTIONS(3) = 1.0 - YFRACTIONS(1) - YFRACTIONS(2)
              Else If (yend - ystart .Eq. 3) Then
                 YFRACTIONS(1) = (Real(ystart) - y_lower) / y_distance
                 YFRACTIONS(2) = 1.0 / y_distance
                 YFRACTIONS(3) = 1.0 / y_distance
                 YFRACTIONS(4) = 1.0 - YFRACTIONS(1) - YFRACTIONS(2) - &
                   YFRACTIONS(3)
 
              Else
 
!              One input pixel is trying to be binned into more
!              than four output pixels
                 y_num_warnings = y_num_warnings + 1
 
                 yend = ystart
                 YFRACTIONS(1) = 0.0
 
              End If
 
!           Calculate start and end pixels and number
              xstrout = Max(1, xstart)
              ystrout = Max(1, ystart)
              xendout = Min(xmaxdat, xend)
              yendout = Min(ymaxdat, yend)
              xnumout = xendout - xstrout + 1
              ynumout = yendout - ystrout + 1
              xfract = xstrout - xstart + 1
              yfract = ystrout - ystart + 1
 
!           Transfer over-loaded pixel values without proportional
!           distribution of intensity
              If (intensity .Ge. overload_value) Then
 
                 Do y_out = ystrout, yendout
 
                    Do x_out = xstrout, xendout
 
!                    Add full overloaded intensity to output
                       MDATA(x_out, y_out) = MDATA(x_out, y_out) + intensity
 
                    End Do
 
                 End Do
 
              Else
 
!              Loop over output pixels affected by input pixel
                 If (ynumout .Eq. 2) Then
 
                    If (xnumout .Eq. 2) Then
 
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
                       MDATA(xendout, ystrout) = MDATA(xendout, ystrout) + &
                         intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract)
                       MDATA(xstrout, yendout) = MDATA(xstrout, yendout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract + &
                         1)
                       MDATA(xendout, yendout) = MDATA(xendout, yendout) + &
                         intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract + 1)
 
                    Else If (xnumout .Eq. 1) Then
 
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
                       MDATA(xstrout, yendout) = MDATA(xstrout, yendout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract + &
                         1)
 
                    Else If (xnumout .Eq. 3) Then
 
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
                       MDATA(xstrout + 1, ystrout) = MDATA(xstrout + 1, &
                         ystrout) + intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract)
                       MDATA(xendout, ystrout) = MDATA(xendout, ystrout) + &
                         intensity * XFRACTIONS(xfract + 2) * &
                         YFRACTIONS(yfract)
                       MDATA(xstrout, yendout) = MDATA(xstrout, yendout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract + &
                         1)
                       MDATA(xstrout + 1, yendout) = MDATA(xstrout + 1, &
                         yendout) + intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract + 1)
                       MDATA(xendout, yendout) = MDATA(xendout, yendout) + &
                         intensity * XFRACTIONS(xfract + 2) * &
                         YFRACTIONS(yfract + 1)
 
                    Else
 
                       Do x_out = xstrout, xendout
 
!                       Calculate output pixel
                          MDATA(x_out, ystrout) = MDATA(x_out, ystrout) + &
                            intensity * XFRACTIONS(x_out - xstart + 1) * &
                            YFRACTIONS(yfract)
                          MDATA(x_out, yendout) = MDATA(x_out, yendout) + &
                            intensity * XFRACTIONS(x_out - xstart + 1) * &
                            YFRACTIONS(yfract + 1)
 
                       End Do
 
                    End If
 
                 Else If (ynumout .Eq. 1) Then
 
                    If (xnumout .Eq. 2) Then
 
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
                       MDATA(xendout, ystrout) = MDATA(xendout, ystrout) + &
                         intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract)
 
                    Else If (xnumout .Eq. 1) Then
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
 
                    Else If (xnumout .Eq. 3) Then
 
                       MDATA(xstrout, ystrout) = MDATA(xstrout, ystrout) + &
                         intensity * XFRACTIONS(xfract) * YFRACTIONS(yfract)
                       MDATA(xstrout + 1, ystrout) = MDATA(xstrout + 1, &
                         ystrout) + intensity * XFRACTIONS(xfract + 1) * &
                         YFRACTIONS(yfract)
                       MDATA(xendout, ystrout) = MDATA(xendout, ystrout) + &
                         intensity * XFRACTIONS(xfract + 2) * &
                         YFRACTIONS(yfract)
 
                    Else
 
                       Do x_out = xstrout, xendout
 
!                       Calculate output pixel
                          MDATA(x_out, ystrout) = MDATA(x_out, ystrout) + &
                            intensity * XFRACTIONS(x_out - xstart + 1) * &
                            YFRACTIONS(yfract)
 
                       End Do
 
                    End If
 
                 Else
 
                    Do y_out = ystrout, yendout
 
                       If (xnumout .Eq. 2) Then
 
                          MDATA(xstrout, y_out) = MDATA(xstrout, y_out) + &
                            intensity * XFRACTIONS(xfract) * &
                            YFRACTIONS(y_out - ystart + 1)
                          MDATA(xendout, y_out) = MDATA(xendout, y_out) + &
                            intensity * XFRACTIONS(xfract + 1) * &
                            YFRACTIONS(y_out - ystart + 1)
 
                       Else If (xnumout .Eq. 1) Then
                          MDATA(xstrout, y_out) = MDATA(xstrout, y_out) + &
                            intensity * XFRACTIONS(xfract) * &
                            YFRACTIONS(y_out - ystart + 1)
 
                       Else If (xnumout .Eq. 3) Then
 
                          MDATA(xstrout, y_out) = MDATA(xstrout, y_out) + &
                            intensity * XFRACTIONS(xfract) * &
                            YFRACTIONS(y_out - ystart + 1)
                          MDATA(xstrout + 1, y_out) = MDATA(xstrout + 1, &
                            y_out) + intensity * XFRACTIONS(xfract + 1) * &
                            YFRACTIONS(y_out - ystart + 1)
                          MDATA(xendout, y_out) = MDATA(xendout, y_out) + &
                            intensity * XFRACTIONS(xfract + 2) * &
                            YFRACTIONS(y_out - ystart + 1)
 
                       Else
 
                          Do x_out = xstrout, xendout
 
!                          Calculate output pixel
                             MDATA(x_out, y_out) = MDATA(x_out, y_out) + &
                               intensity * XFRACTIONS(x_out - xstart + 1) * &
                               YFRACTIONS(y_out - ystart + 1)
 
                          End Do
 
                       End If
 
                    End Do
 
                 End If
 
              End If
 
           End Do
 
!        Output progress
           If (Mod(y - y_low + 1, 500) .Eq. 0) Then
 
              Write (message, '(''INFO: Number of rows re-binned = '', ' // &
                'i6, '' ('',i3, ''%)'')') y - y_low + 1, &
                Int(100.0 * Real(y - y_low + 1) / Real(y_up - y_low + 1))
              Call IO_WRITE (message, status)
 
           End If
 
        End Do
 
!     Time for operation
        Call IO_TIMES (timend, time_cpu_end, status)
        Write (message, '(''INFO: Time for correction = '', ' // &
          'f12.2, '' seconds'')') timend - timstr
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: CPU Time for correction = '', ' // &
          'f12.2, '' seconds'')') time_cpu_end - time_cpu_str
        Call IO_WRITE (message, status)
!     Write (message, '(''INFO: Time for splines = '', ' //
!     :       'f12.2, '' seconds'')') time_spline
!     Call IO_WRITE (message, status)
!     Write (message, '(''INFO: CPU Time for splines = '', ' //
!     :       'f12.2, '' seconds'')') time_cpu_spline
!     Call IO_WRITE (message, status)
 
!     Output message if pixels have been ignored owing to re-binning
!     over more than three output pixels
        If (x_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' input pixels ' // &
             'have been ignored, because they require'')') x_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than four output pixels (X-direction).', status)
        End If
 
        If (y_num_warnings .Gt. 0) Then
           Write (message, '(''WARNING: '', i10, '' input pixels ' // &
             'have been ignored, because they require'')') y_num_warnings
           Call IO_WRITE (message, status)
           Call IO_WRITE ('         re-binning into more ' // &
             'than four output pixels (Y-direction).', status)
        End If
 
!     Free dynamic array space
        Deallocate (X_WORK)
        Deallocate (X_IWORK)
        Deallocate (Y_WORK)
        Deallocate (Y_IWORK)
 
     End If
 
     End Subroutine F2D_CORR5_SPATIAL
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 

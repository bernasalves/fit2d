!********1*********2*********3*********4*********5*********6*********7**
 
!  *********************
!  *                   *
!  * f2d_corr_fast.f90 *
!  *                   *
!  *********************
 
!+ F2D_CORR_FAST - FIT 2-D CORRect spatial distortion FAST
     Subroutine F2D_CORR_FAST (gui, x_cor_size, y_cor_size, xmax_lut, &
       ymax_lut, xnum_lut, ynum_lut, X_SD, Y_SD, INT_REBINNED, xmaxdat, &
       ymaxdat, XAXIS, YAXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, &
       overload_value, MXAXIS, MYAXIS, MDATA, mxstrelm, mystrelm, mxendelm, &
       myendelm, status)
!  Description:
!    Applies spatial distortion correction using precalculated
!    look-up tables
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    23-Feb-1999: V0.4 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.3 Change to use IO internal database routines (Hammersley)
!    16-Dec-1996: V0.2 Avoid open strings crossing lines (Hammersley)
!    03-Oct-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Logical, Intent(IN) :: gui ! .True., if the graphics window is to be
!      used for user prompts, messages and input
     Real, Intent(IN) :: x_cor_size ! Size of corrected pixel in metres in
!      X-direction
     Real, Intent(IN) :: y_cor_size ! Size of corrected pixel in metres in
!      Y-direction
     Integer, Intent(IN) :: xmax_lut ! First dimension of look-up tables
     Integer, Intent(IN) :: ymax_lut ! Second dimension of look-up tables
     Integer, Intent(IN) :: xnum_lut ! Number of pixels to define in LUT in
!      the X-direction
     Integer, Intent(IN) :: ynum_lut ! Number of pixels to define in LUT in
!      the Y-direction
     Byte, Intent(IN) :: X_SD(xmax_lut, ymax_lut) ! Rounded X-distortion in
!      pixels
     Byte, Intent(IN) :: Y_SD(xmax_lut, ymax_lut) ! Rounded Y-distortion in
!      pixels
     Byte, Intent(IN) :: INT_REBINNED(9, xmax_lut, ymax_lut)
!      "Fraction" of each input pixel rebinned into the 9 pixels which are the 
!      target pixel and the 8 nearest pixels with the exception of the upper
!      right pixel. Each fraction is stored as a byte, so change to unsigned, 
!      and divide by 256, to obtain the required fraction. Since the total must 
!      equal 1.0, the fraction for the upper right pixel can be deduced. The 
!      order of the fractions is lower left, lower middle, lower right, left of 
!      target, target, right of target, upper left, and upper middle.
     Integer, Intent(IN) :: xmaxdat ! First dimension of data arrays
     Integer, Intent(IN) :: ymaxdat ! Second dimension of data arrays
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array containing data X-coordinates
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array containing data Y-coordinates
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Array containing data to
!      be fitted
     Integer, Intent(IN) :: xendelm ! End X-element of region to be fitted
     Integer, Intent(IN) :: xstrelm ! Starting X-element of region to be fitted
     Integer, Intent(IN) :: yendelm ! End Y-element of region to be fitted
     Integer, Intent(IN) :: ystrelm ! Starting Y-element of region to be fitted
!  Import/Export:
     Real, Intent(INOUT) :: overload_value ! Value above which pixels are
!      considered to be over-loaded
     Real, Intent(INOUT) :: MXAXIS(xmaxdat) ! Memory X-coordinates
     Real, Intent(INOUT) :: MYAXIS(ymaxdat) ! Memory Y-coordinates
     Real, Intent(INOUT) :: MDATA(xmaxdat, ymaxdat) ! Corrected data
     Integer, Intent(INOUT) :: mxstrelm ! Start X-element of memory data region
     Integer, Intent(INOUT) :: mystrelm ! Start Y-element of memory data region
     Integer, Intent(INOUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(INOUT) :: myendelm ! End Y-element of memory data region
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: db_stat ! Data Store status return variable
!    Integer pixel ! Loop variable for pixels
!    Integer rebin_int ! Rounded fraction as a byte value
!    Integer sum ! Sum of re-binning fractions (*255.0)
     Integer :: x ! Loop variable
!    Integer xi ! Inner loop variable
!    Integer x_base ! Base X-pixel for intensity re-binning
     Integer :: x_low ! Starting X-pixel for re-binning
!    Integer x_out ! Output X-pixel
     Integer :: x_up ! End X-pixel for re-binning
     Integer :: y ! Loop variable
!    Integer yi ! Inner loop variable
!    Integer y_base ! Base Y-pixel for intensity re-binning
     Integer :: y_low ! Starting Y-pixel for re-binning
!    Integer y_out ! Output Y-pixel
     Integer :: y_up ! End Y-pixel for re-binning
!    Logical continue ! .True., whilst input is not good
!    Real intensity ! Intensity of a pixel
!    Real rebin_fraction ! Fraction of input intensity to be output
!    into the output pixel
     Real :: step ! The axis value step between adjacent pixel positions
     Real :: time_cpu_end ! CPU time at end of process
     Real :: time_cpu_str ! CPU time at start of process
     Real :: timend ! Time at end of operation
     Real :: timstr ! Time at start of operation
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(23) ! User messages
!  Internal Functions:
!  External Functions:
     Character(Len = 20), External :: Io_rtoc ! Convert real value to character
!      form
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*,'(/1x,''Entry to Subroutine F2D_CORR_FAST'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine  F2D_CORR_FAST ' // Version)
        Return
     End If
 
!  Check that the subroutine argument variables are reasonably defined
     If (xmaxdat .Lt. 1 .Or. xmax_lut .Lt. 1) Then
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''xmaxdat, xmax_lut = '', 2i6)') xmaxdat, xmax_lut
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        status = St_bad_dim1
     Else If (ymaxdat .Lt. 1 .Or. ymax_lut .Lt. 1) Then
        status = St_bad_dim2
     Else If (xstrelm .Le. 0 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Le. 0 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
 
!     Add module number to status value
        status = St_mod_fit2d + status
 
!     Save details of subroutine call
        Call ST_SAVE ('Subroutine F2D_CORR_FAST ' // Version)
 
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Output message telling user of the corrected pixel sizes
        Write (MESSAGE(1), '(''INFO: The corrected pixel ' // &
          'dimension in X is '', f12.4, '' microns'')') x_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Write (MESSAGE(1), '(''INFO: The corrected pixel ' // &
          'dimension in Y is '', f12.4, '' microns'')') y_cor_size * 1.0e6
        Call IO_WRITE (MESSAGE(1), status)
        Call IO_WRITE (' ', status)
 
!     Set Intersection of ROI and valid spline region
        x_low = xstrelm
        y_low = ystrelm
        x_up = Min(xnum_lut, xendelm)
        y_up = Min(ynum_lut, yendelm)
 
!     Obtain overloaded intensity value from internal data-base
        Call IO_INQ_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
          status)
 
        If (.Not. gui) Then
 
!        Overloaded pixel value
           MESSAGE(1) = 'In order to avoid over-loaded pixels ' // &
             'being re-binned and their intensity'
           MESSAGE(2) = 'spread out to an undetermined value, ' // &
             'you can enter a "over-loaded" pixel'
           MESSAGE(3) = 'value. All input pixels which have ' // &
             'this value or more, will cause one or'
           MESSAGE(4) = 'more output pixels to be incremented ' // &
             'by the value regardless of the normal'
           MESSAGE(5) = 'proportional are re-binning algorithm. ' // &
             'Thus over-loaded pixels in the'
           MESSAGE(6) = 'output image can be easily identified ' // &
             'and ignored.'
           MESSAGE(7) = '(This can be turned-off by entering a ' // &
             'very large value.)'
           Call IO_INPR (.True., 0.0, 1.7e38, .True., &
             'OVER-LOADED PIXEL VALUE', 7, MESSAGE, 1, &
             'Enter a real value within given range', overload_value, status)
 
           If (status .Eq. St_escapevalue) Then
              Return
           End If
 
!        Save overloaded intensity value in internal data-base
           Call IO_SET_RKEYVALUE ('#OVERLOAD_VALUE', overload_value, db_stat, &
             status)
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Set output ROI
        mxstrelm = 1
        mystrelm = 1
        mxendelm = xmaxdat
        myendelm = ymaxdat
 
!     Initialise all pixels in output region of memory
        Call MA_RVALUE (xmaxdat, ymaxdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, 0.0, MDATA, status)
 
!     Store start time
        Call IO_TIMES (timstr, time_cpu_str, status)
 
!     Perform spatial correction (in "C" because unsigned bytes)
        Call F2D_CORR_FASTC (xmax_lut, ymax_lut, X_SD, Y_SD, INT_REBINNED, &
          overload_value, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, DATA, mxstrelm, mystrelm, mxendelm, myendelm, MDATA)
 
!     Time for operation
        Call IO_TIMES (timend, time_cpu_end, status)
 
!     Do y = ystrelm, yendelm
 
!     Do x = xstrelm, xendelm
 
!     intensity = DATA(x, y)
!     x_base = X_SD(x, y) + x
!     y_base = Y_SD(x, y) + y
 
!     pixel = 0
!     Do yi = 0, 2
 
!     y_out = y_base + yi
!     Do xi = 0, 2
 
!     x_out = x_base + xi
 
!     pixel = pixel + 1
 
!     rebin_int = INT_REBINNED(pixel, x, y)
!     If (rebin_int .Lt. 0) Then
!     rebin_int = rebin_int + 256
!     End If
 
!     Only bother with pixels with intensity
!     If (rebin_int .Gt. 0) Then
 
!     If (x_out .Ge. mxstrelm .And.
!     :                      y_out .Ge. mystrelm .And.
!     :                      x_out .Le. mxendelm .And.
!     :                      y_out .Le. myendelm) Then
 
!     Calculate fraction of input pixel in this
!     output pixel
!     rebin_fraction = Real(rebin_int) / 255.0
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
!     rebin_fraction = Real(rebin_int)
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
 
!     If (intensity .Lt. overload_value) Then
 
!     MDATA(x_out, y_out) =
!     :                            MDATA(x_out, y_out) +
!     :                            rebin_fraction * intensity
!     Else
 
!     Saturated pixel, add to all affected
!     pixels
!     MDATA(x_out, y_out) =
!     :                            MDATA(x_out, y_out) + intensity
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
!     :                            MDATA(x_out, y_out) + intensity*255.0
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
 
!     End If
 
!     End If
 
!     End If
 
!     End Do
 
!     End Do
 
!     End Do
 
!     End Do
 
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
!     It seems to make little difference whether the divide by 255.0 is
!     for every re-binning (~4 per pixel) or once per pixel at the end,
!     which means going through the array again
 
!     Now divide by 255.0
        Call MA_RCMULT (xmaxdat, ymaxdat, mxstrelm, mystrelm, mxendelm, &
          myendelm, 1.0 / 255.0, MDATA, status)
!**ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-CODE***ALTERNATIVE-
 
        Write (message, '(''INFO: Time for correction = '', ' // &
          'f12.2, '' seconds'')') timend - timstr
        Call IO_WRITE (message, status)
        Write (message, '(''INFO: CPU Time for correction = '', ' // &
          'f12.2, '' seconds'')') time_cpu_end - time_cpu_str
        Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Create X-axis values
        Do x = 1, xendelm
           MXAXIS(x) = XAXIS(x)
        End Do
 
!     If the output axis extends beyond the input axis the extra
!     elements must be interpolated
        If (mxendelm .Gt. xendelm) Then
 
           step = XAXIS(xendelm) - XAXIS(xendelm - 1)
           Do x = xendelm + 1, mxendelm
              MXAXIS(x) = XAXIS(xendelm) + step * (x - xendelm)
           End Do
 
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Create X-axis values
        Do y = 1, yendelm
           MYAXIS(y) = YAXIS(y)
        End Do
 
!     If the output axis extends beyond the input axis the extra
!     elements must be interpolated
        If (myendelm .Gt. yendelm) Then
 
           step = YAXIS(yendelm) - YAXIS(yendelm - 1)
           Do y = yendelm + 1, myendelm
              MYAXIS(y) = YAXIS(yendelm) + step * (y - yendelm)
           End Do
 
        End If
 
!     Output user information
        If (.Not. gui) Then
           Call IO_WRITE ('NOTE: Corrected data is in the ' // &
             '"memory" array. Use "EXCHANGE" in the', status)
           Call IO_WRITE ('      main menu to transfer to ' // &
             'the working current data array.', status)
        End If
 
     End If
 
     End Subroutine F2D_CORR_FAST
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 
 
 

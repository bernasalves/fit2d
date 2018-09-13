!********1*********2*********3*********4*********5*********6*********7**
 
!  *******************
!  *                 *
!  * f2d_entropy.f90 *
!  *                 *
!  *******************
 
!+ F2D_ENTROPY - Fit 2-D calculate image ENTROPY (bits per pixel) pixel value 
!  frequency statistics
     Subroutine F2D_ENTROPY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, WORK, status)
!  Description:
!    Produces histogram of pixel values and calculates probability occurence of
!    each value and hence the entropy in bits per pixel.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    19-Jan-1997: V0.3 Output ideal bits length for optimum code, and calculate
!      standard deviation of distribution, calculate Huffman code (Hammersley)
!    11-Jan-1997: V0.2 Predictor part removed, calculate statistics on frequency
!      histogram (Hammersley)
!    23-Feb-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of region of interest
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of region of interest
     Integer, Intent(IN) :: xendelm ! Defines X-end of region of interest
     Integer, Intent(IN) :: yendelm ! Defines Y-end of region of interest
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array
!  Export:
     Integer, Intent(OUT) :: WORK(-65535: 65535) ! Used to create histogram
!      of the nearest integer pixel to pixel differences
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Character(Len = 20) :: code_lowest ! Partial Huffman code for lowest
!      frequency symbol
     Character(Len = 80) :: message ! User text
     Double Precision :: bit_rate ! Bit rate of Huffman code
     Double Precision :: entropy ! Entropy of image ROI
     Double Precision :: log2constant ! Scaling to log base 2 values
     Double Precision :: mean ! Weighted mean value of frequency histogram
     Double Precision :: probability ! Probability of a particular value
     Double Precision :: sigma ! Standard deviation of frequency histogram
     Double Precision :: sqr_mean ! Square of weighted mean components histogram
     Double Precision :: sum_entropy ! Average entropy of image ROI
     Integer :: data_value ! Rounded value of a data point
     Integer :: imean ! Rounded version of the mean
     Integer :: iteration ! Loop variable for iterations in calculating
!      Huffman code
     Integer :: limit ! Symmetric limit about zero for checking range 
!      frequencies
     Integer :: lower ! Lower limit for checking pixel value frequencies
     Integer :: lowest ! Lowest frequency
     Integer :: next_lowest ! Second lowest frequency
     Integer :: num_overflow ! Number of pixel differences which are too big to
!      be stored in the probability histogram
     Integer :: num_pixels ! Total number of pixels in ROI
     Integer :: sum ! Sum of pixels within a defined value range
     Integer :: sym_lowest ! Symbol with the lowest frequency
     Integer :: sym_next_lowest ! Symbol with the second lowest frequency
     Integer :: upper ! Upper limit for checking pixel value frequencies
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
!    Real ideal_bit_length ! Optimum number of bits used to encode
!    signal
     Real :: percentage ! Percentage of pixels within a defined value range
!  Local Arrays:
     Character(Len = 16) :: CODES(-128: 127) ! Huffman codes for each symbol
     Integer :: HUFFMAN(-128: 127) ! Combined numbers of pixel
     Integer :: INDICES(2, 255) ! The pairs of symbols combined at each stage
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_ENTROPY ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_ENTROPY ' // Version)
 
     Else
 
!     Initialise variables
        num_overflow = 0
        num_pixels = (xendelm - xstrelm + 1) * (yendelm - ystrelm + 1)
 
!     Initialise differences array
        Do x = -65535, 65535
           WORK(x) = 0
        End Do
 
!     Loop through data
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
!           Calculate difference to previous pixel
              data_value = Nint(DATA(x, y))
 
!           Increment histogram
              If (data_value .Lt. -65535 .Or. data_value .Gt. 65535) Then
                 num_overflow = num_overflow + 1
              Else
                 WORK(data_value) = WORK(data_value) + 1
              End If
 
           End Do
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Do x = -64, 64
           Write (*, '(''Value '', i4, '' frequency = '', i6)') x, WORK(x)
        End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Output message if some of the pixel differences are too large
!     to enter into the histogram
        Call IO_WRITE ('INFO: Number of pixel differences ' // &
          'which are too large to enter', status)
        Write (message, '(''      histogram = '', i4, ' // &
          ''' probability = '', 1pe12.5)') num_overflow, Real(num_overflow) / &
          Real(num_pixels)
        Call IO_WRITE (message, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Calculate entropy of image
        log2constant = Log(2.0d0)
        sum_entropy = 0.0d0
        Do x = -65535, 65535
 
           If (WORK(x) .Gt. 0) Then
              probability = Dble(WORK(x)) / Dble(num_pixels)
              entropy = - Log(probability) * probability / log2constant
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           If (x .Gt. -30 .And. x .Lt. 30) Then
!           Write (*, '(
!           :                ''x, number, probability, entropy = '', ' //
!           :                '2i6, f12.8, f12.5)') x, WORK(x), probability,
!           :                entropy
!           End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
              sum_entropy = sum_entropy + entropy
 
           End If
 
        End Do
 
        Write (message, '(''INFO: Entropy of image (bits ' // &
          'per pixel) = '', f12.5)') sum_entropy
        Call IO_WRITE (message, status)
        Write (message, '(''      Theoretical maximum ' // &
          'compression ratio = '', f12.5)') 16.0 / sum_entropy
        Call IO_WRITE (message, status)
        Call IO_WRITE ('NOTE: This assumes 16 bit integer ' // 'data values', &
          status)
 
!     Calculate weighted mean and standard deviation of histogram
        mean = 0.0d0
        sqr_mean = 0.0d0
        Do x = -65535, 65535
           mean = mean + Dble(x) * Dble(WORK(x)) / Dble(num_pixels)
           sqr_mean = sqr_mean + Dble(x**2) * Dble(WORK(x))
        End Do
 
        sigma = Sqrt((sqr_mean - Dble(num_pixels) * mean * mean) / &
          Dble(num_pixels-1))
!     sigma = Sqrt(sqr_mean / Dble(num_pixels))
 
        Write (message, '(''INFO: Weighted mean value of ' // &
          'frequency histogram = '', f14.7)') mean
        Call IO_WRITE (message, status)
        Write (message, '(''      Standard deviation of ' // &
          'frequency histogram = '', f14.7)') sigma
        Call IO_WRITE (message, status)
!     Write (message, '(''      RMS of ' //
!     :       'frequency histogram = '', f14.7)') sigma
!     Call IO_WRITE (message, status)
 
        imean = Nint(mean)
 
!     Find percentage of pixel values within defined ranges
!     symmetric about the mean
        Do limit = 0, 40
 
           lower = imean - limit
           upper = imean + limit
 
!        Sum pixels
           sum = 0
           Do x = lower, upper
              sum = sum + WORK(x)
           End Do
 
           probability = sum / Real(num_pixels)
           percentage = 100.0 * probability
 
!        Write (message, '(''INFO: '', f7.3, ''% of pixels ' //
!        :          'have values been '', i4, '': '', i4)')
!        :          percentage, lower, upper
!        Call IO_WRITE (message, status)
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Calculate theoretical bit lengths for symbols
!     Do limit = lower, upper
 
!     probability = WORK(limit) / Real(num_pixels)
 
!     ideal_bit_length = -Log(probability) / log2constant
 
!     Write (message, '(''INFO: Symbol '', i4, '' ideal ' //
!     :          'number of bits = '', f7.4)') limit, ideal_bit_length
!     Call IO_WRITE (message, status)
 
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
!     Huffman code
 
!     Transfer values to Huffman table
        sum = 0
        Do x = -127, 127
           HUFFMAN(x) = WORK(x)
           sum = sum + WORK(x)
        End Do
 
!     Define "ELSE" code in Huffman table
        HUFFMAN(-128) = num_pixels - sum
 
        Write (*, '(''Number of escaped pixels = '', i10)') num_pixels - sum
 
!     Find and combine lowest frequency symbol pairs
        Do iteration = 1, 255
 
!        Find lowest frequency symbols
           lowest = num_pixels + 1
           sym_lowest = -129
           next_lowest = num_pixels + 1
           sym_next_lowest = -129
           Do x = -128, 127
 
              If (HUFFMAN(x) .Lt. lowest) Then
                 next_lowest = lowest
                 sym_next_lowest = sym_lowest
                 lowest = HUFFMAN(x)
                 sym_lowest = x
              Else If (HUFFMAN(x) .Lt. next_lowest) Then
                 next_lowest = HUFFMAN(x)
                 sym_next_lowest = x
              End If
 
           End Do
 
!        Save values
           INDICES(1, iteration) = sym_lowest
           INDICES(2, iteration) = sym_next_lowest
 
!        Combine frequencies
           HUFFMAN(sym_lowest) = num_pixels + 1
           HUFFMAN(sym_next_lowest) = lowest + next_lowest
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output combining table
!     Do iteration = 1, 254
!     Write (*, '('' lowest, next lowest = '', 2i6)')
!     :          INDICES(1, iteration), INDICES(2, iteration)
!     End Do
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Clear codewords
        Do x = -128, 127
           CODES(x) = ' '
        End Do
 
!     Construct code words
        CODES(sym_next_lowest) = '0'
        CODES(sym_lowest) = '1'
        Do iteration = 254, 1, -1
 
           sym_lowest = INDICES(1, iteration)
           sym_next_lowest = INDICES(2, iteration)
           code_lowest = CODES(sym_next_lowest)
           CODES(sym_next_lowest) = Trim(code_lowest) // '0'
           CODES(sym_lowest) = Trim(code_lowest) // '1'
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Output Huffman codes
        Do x = -127, 127
 
           Write (message, '(''Value '', i4, '' code length ' // &
             '= '', i3, '' Code = '', a)') x, Len_trim(CODES(x)), CODES(x)
           Call IO_WRITE (message, status)
 
        End Do
        Write (message, '(''INFO: Escape symbol: Code = '', a)') CODES(-128)
        Call IO_WRITE (message, status)
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Calculate bit rate of Huffman code
        bit_rate = 0.0d0
        Do x = -127, 127
           bit_rate = bit_rate + Len_trim(CODES(x)) * Dble(WORK(x)) / &
             Dble(num_pixels)
        End Do
        bit_rate = bit_rate + Dble(16 + Len_trim(CODES(-128))) * &
          Dble(num_pixels - sum) / Dble(num_pixels)
 
        Write (message, '(''INFO: Huffman code bit rate (bits ' // &
          'per pixel) = '', f12.5)') bit_rate
        Call IO_WRITE (message, status)
        Write (message, '(''      Huffman code ' // &
          'compression ratio = '', f12.5)') 16.0 / bit_rate
        Call IO_WRITE (message, status)
        Write (message, '(''      Huffman code ' // &
          'efficiency = '', f6.2, ''%'')') 100.0 * sum_entropy / bit_rate
        Call IO_WRITE (message, status)
        Call IO_WRITE ('NOTE: This assumes 16 bit integer ' // 'data values', &
          status)
 
     End If
 
     End Subroutine F2D_ENTROPY
!********1*********2*********3*********4*********5*********6*********7**
 
 
 
 

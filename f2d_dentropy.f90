!********1*********2*********3*********4*********5*********6*********7**
 
!  ********************
!  *                  *
!  * f2d_dentropy.f90 *
!  *                  *
!  ********************
 
!+ F2D_DENTROPY - Fit 2-D calculate image Difference ENTROPY (bits per pixel)
     Subroutine F2D_DENTROPY (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, DATA, WORK, status)
!  Description:
!    Produces histogram of differences from one pixel to the next in the image. 
!    At the start of each line the difference is with the first pixel of the 
!    previous line. From this histogram the probability of each value is 
!    calculated and the entropy in bits per pixel is calculated.
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    11-Jan-1997: V0.2 Renamed from "F2D_ENTROPY" (Hammersley)
!    23-Feb-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array "DATA"
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array "DATA"
     Integer, Intent(IN) :: xstrelm ! Defines X-start of active data region
     Integer, Intent(IN) :: ystrelm ! Defines Y-start of active data region
     Integer, Intent(IN) :: xendelm ! Defines X-end of active data region
     Integer, Intent(IN) :: yendelm ! Defines Y-end of active data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! Data array to be differenced
!  Export:
     Integer, Intent(OUT) :: WORK(-65535: 65535) ! Used to create histogram
!      of the nearest integer pixel to pixel differences
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User text
     Double Precision :: entropy ! Entropy of image ROI
     Double Precision :: log2constant ! Scaling to log base 2 values
     Double Precision :: probability ! Probability of a particular value
     Double Precision :: sum_entropy ! Average entropy of image ROI
     Integer :: difference ! The difference between two pixel values
     Integer :: num_overflow ! Number of pixel differences which are too big to
!      be stored in the probability histogram
     Integer :: num_pixels ! Total number of pixels in ROI
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: reference ! The reference value to base the difference upon
!  Local Arrays:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DENTROPY ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_DENTROPY ' // Version)
 
     Else
 
!     Initialise variables
        num_overflow = 0
        num_pixels = (xendelm - xstrelm + 1) * (yendelm - ystrelm + 1)
 
!     Initialise differences array
        Do x = -65535, 65535
           WORK(x) = 0
        End Do
 
!     Loop through data
        reference = DATA(xstrelm, ystrelm)
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
!           Calculate difference to previous pixel
              difference = Nint(DATA(x, y) - reference)
              reference = DATA(x, y)
 
!           Increment histogram
              If (difference .Lt. -65535 .Or. difference .Gt. 65535) Then
                 num_overflow = num_overflow + 1
              Else
                 WORK(difference) = WORK(difference) + 1
              End If
 
           End Do
           reference = DATA(xstrelm, y)
 
        End Do
 
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
 
     End If
 
     End Subroutine F2D_DENTROPY
!********1*********2*********3*********4*********5*********6*********7**

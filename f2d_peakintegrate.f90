!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_peakintegrate.f90 *
!  *                       *
!  *************************
 
!+ F2D_PEAKINTEGRATE - FIT 2-D PEAK INTEGRATE-ion
     Subroutine F2D_PEAKINTEGRATE (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, gain, y_ref, mode, xmax_work, ymax_work, &
       IS_PEAK, PEAK_PIXELS, SUMS, SQUARES, peak, status)
!  Description:
!    Quick and dirty peak integration and estimation of sigma(I)
!  Keywords:
!    Peak integration
!  Method:
!    Adds up all identified peak pixels from "IS_PEAK", subtracting the mean 
!    background value, plus a ring of all adjacent pixels. The sigma is 
!    estimated from the counts and standard deviation of the background values.
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Nov-2006: V0.5 Use "PEAK_SEARCH_CONTROL" structure (Hammersley)
!    15-Nov-2006: V0.4 Use "PEAK_STRUCTURE" to hold results (Hammersley)
!    26-Jun-1998: V0.3 Calculate background subtracted centroid position of 
!      peak (Hammersley)
!    11-Feb-1996: V0.2 Calculate local means and sigmas (Hammersley)
!    09-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'io.inc' ! Data Structure definitions
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Real, Intent(IN) :: gain ! Detector gain
     Integer, Intent(IN) :: y_ref ! Reference Y-pixel number for strip
v     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode ! Mode and control of peak 
!      search
     Integer, Intent(IN) :: xmax_work ! Dimension of "IS_PEAK" array
     Integer, Intent(IN) :: ymax_work ! Dimension of "IS_PEAK" array
     Byte, Intent(IN) :: IS_PEAK(xmax_work, ymax_work) ! Work array for pixel
!      status 
!        0: if considered part of the general background
!        1: if candidate for part of a peak
!        2: if rejected
     Integer, Intent(IN) :: PEAK_PIXELS(xmax_work) ! Number of peak pixels
!      in a box column
     Double Precision, Intent(IN) :: SUMS(xmax_work) ! Contains sums of
!      counts
     Double Precision, Intent(IN) :: SQUARES(xmax_work) ! Contains sums of
!      squares of counts
!  Import/Export:
     Type(PEAK_STRUCTURE), Intent(OUT) :: peak ! Peak parameters and status
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Double Precision, Save :: mean = 100000.0 ! Mean background around peak
     Double Precision, Save :: sigma = 1000000.0 ! Standard deviation of
!      background around peak
     Double Precision :: sum_squared ! Sum of squared data values
     Double Precision :: term ! Mid-way term for calculating "sigma"
     Integer :: distance ! Maximum number of pixels from the "centre" to search 
!       for the peak intensity
     Integer :: num_pixels ! Number of pixels in a peak
     Integer :: x ! Loop variable for X-direction
     Integer :: x_back ! X-element in background/peak array
!    Integer x_data ! X-element in data array
!    Integer x_low ! Lowest X-pixel in peak
!    Integer x_up ! Highest X-pixel in peak
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_back ! Y-element in background/peak array
!    Integer y_data ! Y-element in data array
!    Integer y_low ! Lowest Y-pixel in peak
!    Logical continue ! .True., whilst a search continues
!    Logical y_continue ! .True., whilst a Y-direction search
!    continues
     Real :: data_value ! Background subtracted data value
     Real :: normalise ! Normalisation total for centroiding
     Real :: x_pc ! X-pixel coordinate
     Real :: x_weight ! Weighting in X-direction for centroid
     Real :: y_pc ! Y-pixel coordinate
     Real :: y_weight ! Weighting in Y-direction for centroid
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_PEAKINTEGRATE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. xmax_work .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. ymax_work .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PEAKINTEGRATE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''Entered F2D_PEAKINTEGRATE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        distance = mode%min_spacing / 2

        x_back = peak%x - xstrelm + 1
        y_back = peak%y - y_ref + 1
 
!     Calculate mean background value and standard deviation for peak area 
        num_pixels = 0
        mean = 0.0
        sum_squared = 0.0
        Do x = Max(1, x_back - mode%box_size / 2), &
          Min(xmax_work, x_back + mode%box_size / 2)
           num_pixels = num_pixels + PEAK_PIXELS(x)
           mean = mean + SUMS(x)
           sum_squared = sum_squared + SQUARES(x)
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE: sum_squared calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        If (num_pixels .Ge. mode%min_back_pixels) Then
           mean = mean / Dble(num_pixels)
           term = Max(0.0d0, sum_squared - Dble(num_pixels) * mean * mean)
           sigma = Sqrt(term / Dble(num_pixels - 1))
 
!        Calculate error in the estimated mean
           sigma = sigma / Sqrt(Real(num_pixels))
 
!        Account for smoothing
           sigma = sigma * Dble(mode%blur_size**2)
 
        Else
           mean = 0.0
           sigma = 0.0
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE: mean / sigma calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate peak integrated intensity
        num_pixels = 0
        peak%intensity = 0.0
        Do y = Max(ystrelm, peak%y - distance), Min(yendelm, peak%y + distance)
 
           y_back = y - y_ref + 1
           Do x = Max(xstrelm, peak%x - distance), Min(xendelm, peak%x + &
             distance)
 
              x_back = x - xstrelm + 1

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
              If (x_back .Lt. 1 .Or. x_back .Gt. xmax_work .Or. &
                y_back .Lt. 1 .Or. y_back .Gt. ymax_work ) Then
                 Write (*, '(''x_back, y_back = '', 2i6)') x_back, y_back
                 Write (*, '(''x, y, y_ref = '', 3i6)') x, y, y_ref
                 Write (*, '(''xmax_work, ymax_work = '', 2i6)') &
                   xmax_work, ymax_work
              End If
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

              If (IS_PEAK(x_back, y_back) .Eq. 1) Then
                 num_pixels = num_pixels + 1
                 peak%intensity = peak%intensity + DATA(x, y)
              End If
 
           End Do
 
        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE: total peak intensity calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate sigma(I)
        peak%sigma = Sqrt(Max(0.0, (peak%intensity * gain) + &
          (sigma * Real(num_pixels))**2))
 
!     Subtract mean background
        peak%intensity = peak%intensity - mean * Real(num_pixels)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE: Integrated intensity calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate background subtracted centroid
        x_weight = 0.0
        y_weight = 0.0
        normalise = 0.0
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''xstrelm, ystrelm, xendelm, yendelm = '', 4i6)')
!     :       xstrelm, ystrelm, xendelm, yendelm
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
        Do y = Max(ystrelm, peak%y - distance), Min(yendelm, peak%y + &
          distance)
 
           y_pc = Real(y) - 0.5
 
           Do x = Max(xstrelm, peak%x - distance), &
             Min(xendelm, peak%x + distance)
              x_pc = Real(x) - 0.5
 
              data_value = DATA(x, y) - mean
              x_weight = x_weight + data_value * x_pc
              y_weight = y_weight + data_value * y_pc
              normalise = normalise + data_value
 
           End Do
 
        End Do
 
        If (normalise .Gt. 0.0) Then
           peak%x_centre = x_weight / normalise
           peak%y_centre = y_weight / normalise
        Else
           peak%status = peak%status + 10
        End If
 
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''mean, normalise, x_weight, y_weight = '',
!     :       4(1pg14.5))') mean, normalise, x_weight, y_weight
!     Write (*, '(''x_centre, y_centre = '', 2f12.3)')
!     :       x_centre, y_centre
 
!     Write (*, '(''mean, sigma, gain = '', 3(1pg14.5))')
!     :       mean, sigma, gain
!     Write (*, '(''num_pixels, intensity, psigma = '', i5,
!     :       2(1pg14.5))') num_pixels, intensity, peak_sigma
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_PEAKINTEGRATE
!********1*********2*********3*********4*********5*********6*********7*********8

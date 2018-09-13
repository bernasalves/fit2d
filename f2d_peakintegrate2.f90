!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_peakintegrate2.f90 *
!  *                        *
!  **************************
 
!+ F2D_PEAKINTEGRATE2 - FIT 2-D PEAK INTEGRATE-ion
     Subroutine F2D_PEAKINTEGRATE2 (xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, MASK, gain, mode, SIGNIFICANT, peak, status)
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
!    24-Oct-2007: V0.8 Remove "PEAK_PIXELS" from argument list (Hammersley)
!    12-Feb-2007: V0.7 Change argument list (Hammersley)
!    27-Nov-2006: V0.6 Non-strip version (Hammersley)
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
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True. if element is
!      masked-off
     Real, Intent(IN) :: gain ! Detector gain
     Type(PEAK_SEARCH_CONTROL), Intent(IN) :: mode ! Mode and control of peak 
!      search
     Logical*1, Intent(IN) :: SIGNIFICANT(xendelm, yendelm) ! Pixel significance
!  Import/Export:
     Type(PEAK_STRUCTURE), Intent(OUT) :: peak ! Peak parameters and status
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
!  Local Variables:
     Double Precision :: mean ! Mean background around peak
     Double Precision :: sigma ! Standard deviation of background around peak
     Double Precision :: sum ! Sum of background values around peak
     Double Precision :: sum_squared ! Sum of squared data values
     Double Precision :: term ! Mid-way term for calculating "sigma"
     Integer :: num_back_pixels ! Number of pixels in background region
     Integer :: num_peak_pixels ! Number of pixels in peak
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
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
        Call ST_SAVE ('Subroutine F2D_PEAKINTEGRATE2 ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0 .Or. xendelm .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0 .Or. yendelm .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_PEAKINTEGRATE2 ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''Entered F2D_PEAKINTEGRATE2'')')
        Write (*, '(''peak%x, peak%y = '', 2i6)') peak%x, peak%y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate mean background value and standard deviation for peak area 
        num_peak_pixels = 0
        peak%intensity = 0.0
        num_back_pixels = 0
        sum = 0.0
        sum_squared = 0.0

        Do y = Max(1, peak%y - mode%min_spacing / 2), &
          Min(yendelm, peak%y + mode%min_spacing / 2)

           Do x = Max(1, peak%x - mode%min_spacing / 2), &
             Min(xendelm, peak%x + mode%min_spacing / 2)

              If (SIGNIFICANT(x, y)) Then
                 num_peak_pixels = num_peak_pixels + 1
                 peak%intensity = peak%intensity + DATA(x, y)
              Else

                 If (.Not. MASK(x, y)) Then
                    num_back_pixels = num_back_pixels + 1
                    sum = sum + DATA(x, y)
                    sum_squared = sum_squared + DATA(x, y)**2
                 End If

              End If

           End Do

        End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''num_peak_pixels = '', i6)') num_peak_pixels
        Write (*, '(''num_back_pixels = '', i6)') num_back_pixels
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

        If (num_back_pixels .Ge. mode%min_back_pixels) Then
           mean = sum / Dble(num_back_pixels)
           term = Max(0.0d0, sum_squared - Dble(num_back_pixels) * mean * mean)
           sigma = Sqrt(term / Dble(num_back_pixels - 1))
 
!        Calculate error in the estimated mean
           sigma = sigma / Sqrt(Real(num_back_pixels))
 
!        Account for smoothing
           sigma = sigma * Dble(mode%blur_size**2)
 
        Else
           mean = 0.0
           sigma = 0.0
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE2: mean / sigma calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!     Calculate sigma(I)
        peak%sigma = Sqrt(Max(0.0, (peak%intensity * gain) + &
          (sigma * Real(num_peak_pixels))**2))
 
!     Subtract mean background
        peak%intensity = peak%intensity - mean * Real(num_peak_pixels)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE2: Integrated intensity calculated'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Calculate background subtracted centroid
        x_weight = 0.0
        y_weight = 0.0
        normalise = 0.0
        Do y = Max(1, peak%y - mode%min_spacing / 2), &
          Min(yendelm, peak%y + mode%min_spacing / 2)

           y_pc = Real(y) - 0.5

           Do x = Max(1, peak%x - mode%min_spacing / 2), &
             Min(xendelm, peak%x + mode%min_spacing / 2)

              x_pc = Real(x) - 0.5

              If (SIGNIFICANT(x, y)) Then
                 data_value = DATA(x, y) - mean
                 x_weight = x_weight + data_value * x_pc
                 y_weight = y_weight + data_value * y_pc
                 normalise = normalise + data_value
              End If

           End Do
  
        End Do
 
        If (normalise .Gt. 0.0) Then
           peak%x_centre = x_weight / normalise
           peak%y_centre = y_weight / normalise
        Else
           peak%status = peak%status + 10
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''mean, normalise, x_weight, y_weight = '',
!     :       4(1pg14.5))') mean, normalise, x_weight, y_weight
        Write (*, '(''x_centre, y_centre = '', 2f12.3)') &
          peak%x_centre, peak%y_centre
        Write (*, '(''num_peak_pixels, intensity, psigma = '', i5, &
          2(1pg14.5))') num_peak_pixels, peak%intensity, peak%sigma
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
        Write (*, '(''F2D_PEAKINTEGRATE2: End'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

     End If
 
     End Subroutine F2D_PEAKINTEGRATE2
!********1*********2*********3*********4*********5*********6*********7*********8

!********1*********2*********3*********4*********5*********6*********7**
 
!  ************************
!  *                      *
!  * f2d_beamgaussian.f90 *
!  *                      *
!  ************************
 
!+ F2D_BEAMGAUSSIAN -  BEAM centre fit GAUSSIAN
     Subroutine F2D_BEAMGAUSSIAN (xmaxdat, ymaxdat, DATA, xstrelm, ystrelm, &
       xendelm, yendelm, x_pixel, y_pixel, mode, retstat, x_beam, y_beam, &
       status)
!  Description:
!    Determine approximate parameters of a well defined Gaussian
!    and refine to give centre of Gaussian
!  Keywords:
!    Beam~Centre.Fit.Gaussian, Fit.Gaussian.Beam~Centre
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    11-Mar-1998: V0.7 Set return return status 'retstat' to zero if the 
!      fitting returns a negative value, which is a halt condition (Hammersley)
!    10-Jan-1997: V0.6 Convert input position to pixel numbers (Hammersley)
!    19-Dec-1996: V0.5 Investigating poor centring of Gaussian (Hammersley)
!    16-Dec-1996: V0.4 Avoid open strings crossing lines (Hammersley)
!    22-Oct-1996: V0.3 Output results (Hammersley)
!    26-Apr-1996: V0.2 Explicit conversion of reals to integers for
!      stricter Ultrix compiler (Hammersley)
!    06-Feb-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: x_pixel ! X-pixel of approximate centre
     Integer, Intent(IN) :: y_pixel ! Y-pixel of approximate centre
     Integer, Intent(IN) :: mode ! Unused at present, should be set to 0
!  Import/Export:
!  Export:
     Integer, Intent(OUT) :: retstat ! Return status:
!      0 = Good status, fit O.K.
!      Other values bad
     Real, Intent(OUT) :: x_beam ! X-coordinate of beam centre (pixel
!      coordinate)
     Real, Intent(OUT) :: y_beam ! Y-coordinate of beam centre (pixel
!      coordinate)
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
!  Local Variables:
     Character(Len = 80) :: message ! User messages
     Integer :: x ! Loop variable for X-direction
     Integer :: x_centre ! Initial X-centre in pixels
     Integer :: x_end ! X-end of search region in pixels
     Integer :: x_start ! X-start of search region in pixels
     Integer :: xendbck ! X-pixel for end of background search region
     Integer :: xstrbck ! X-pixel for start of background search region
     Integer :: y ! Loop variable for Y-direction
     Integer :: y_centre ! Initial Y-centre in pixels
     Integer :: y_end ! Y-end of search region in pixels
     Integer :: y_start ! Y-start of search region in pixels
     Integer :: yendbck ! Y-pixel for end of background search region
     Integer :: ystrbck ! Y-pixel for start of background search region
     Logical :: continue ! .True., if the search continues
     Real :: angle ! Angle of first axis
     Real :: chi_squared ! Fitted chi squared value
     Real :: edge_maximum ! Maximum value on an edge
     Real :: maximum ! Maximum data value in search region
     Real :: minimum ! Minimum data value in search region
     Real :: sigma1 ! Standard deviation of first axis
     Real :: sigma2 ! Standard deviation of second axis
!  Local Arrays:
!  Local Data:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_BEAMGAUSSIAN'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_BEAMGAUSSIAN ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. xendelm .Gt. &
       xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. yendelm .Gt. &
       ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_BEAMGAUSSIAN ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7--
 
!     Calculate initial search region
        x_centre = x_pixel
        y_centre = y_pixel
 
!     Expand search region until the edge intensity is at most half
!     the peak intensity
        x_start = Max(xstrelm, x_centre - 1)
        y_start = Max(ystrelm, y_centre - 1)
        x_end = Min(xendelm, x_centre + 1)
        y_end = Min(yendelm, y_centre + 1)
 
        continue = .True.
        Do While (continue)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''x_start, y_start, x_end, y_end = '',
!        :          4i6)') x_start, y_start, x_end, y_end
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Find maximum pixel value
           maximum = -1.7e38
           minimum = 1.7e38
           Do y = y_start, y_end
 
              Do x = x_start, x_end
 
                 If (DATA(x, y) .Gt. maximum) Then
                    maximum = DATA(x, y)
                    x_centre = x
                    y_centre = y
                 End If
 
                 minimum = Min(minimum, DATA(x, y))
 
              End Do
 
           End Do
 
!        Find maximum value along one edge
           edge_maximum = -1.7e38
           Do y = y_start, y_end
              edge_maximum = Max(edge_maximum, DATA(x_start, y))
              edge_maximum = Max(edge_maximum, DATA(x_end, y))
           End Do
 
           Do x = x_start, x_end
              edge_maximum = Max(edge_maximum, DATA(x, y_start))
              edge_maximum = Max(edge_maximum, DATA(x, y_end))
           End Do
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''maximum, edge_maximum = '', 2g14.7)')
!        :          maximum, edge_maximum
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check that maximum is greater than 1.5 times edge maximum
           If (maximum .Gt. 1.5 * edge_maximum) Then
              continue = .False.
              retstat = 0
 
           Else If (x_start .Gt. xstrelm .Or. y_start .Gt. ystrelm .Or. x_end &
             .Lt. xendelm .Or. y_end .Lt. yendelm) Then
 
!           Increase size of search area
              x_start = Max(xstrelm, x_start - 1)
              y_start = Max(ystrelm, y_start - 1)
              x_end = Min(xendelm, x_end + 1)
              y_end = Min(yendelm, y_end + 1)
 
           Else
 
              continue = .False.
              retstat = 1
           End If
 
        End Do
 
!     Try to find reasonable estimate for background
        If (x_start .Ge. 11) Then
           xstrbck = x_start - 10
           xendbck = x_start - 1
        Else If (xendelm .Ge. x_end + 10) Then
           xstrbck = x_end + 1
           xendbck = x_end + 10
        Else
           xstrbck = xstrelm
           xendbck = x_start
        End If
        If (y_start .Ge. 11) Then
           ystrbck = y_start - 10
           yendbck = y_start - 1
        Else If (yendelm .Ge. y_end + 10) Then
           ystrbck = y_end + 1
           yendbck = y_end + 10
        Else
           ystrbck = ystrelm
           yendbck = y_start
        End If
        minimum = 1.78e38
        Do x = xstrbck, xendbck
 
           Do y = ystrbck, yendbck
 
              If (DATA(x, y) .Lt. minimum) Then
                 minimum = DATA(x, y)
              End If
 
           End Do
 
        End Do
 
!     Check return status
        If (retstat .Ne. 0) Then
           Call IO_WRITE ('ERROR: Problem with finding ' // &
             'initial parameters for the Gaussian fit', status)
           Return
        End If
 
!     Calculate crude initial estimates of Gaussian standard deviation
        maximum = maximum - minimum
        sigma1 = 0.9 * 2.0 / 2.35482 * Min(Abs(x_centre - x_start), &
          Abs(x_centre - x_end), Abs(y_centre - y_start), Abs(y_centre - &
          y_start))
        sigma2 = sigma1 * 0.95
        angle = 0.0
        x_beam = Real(x_centre) - 0.5
        y_beam = Real(y_centre) - 0.5
 
!     Calculate limits of region to be refined
        x_start = Max(xstrelm, Int(x_centre - 6.0 * sigma1))
        y_start = Max(ystrelm, Int(y_centre - 6.0 * sigma1))
        x_end = Min(xendelm, Int(x_centre + 6.0 * sigma1) + 1)
        y_end = Min(yendelm, Int(y_centre + 6.0 * sigma1) + 1)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_start, y_start, x_end, y_end = '', 4i6)') &
!       x_start, y_start, x_end, y_end
!     Write (*, '(''x_beam, y_beam = '', 2g14.7)') x_beam, y_beam
!     Write (*, '(''intensity = '', g14.7)') maximum
!     Write (*, '(''sigma1, sigma2 = '', 2g14.7)') sigma1, sigma2
!     Write (*, '(''angle = '', g14.7)') angle * 180.0 / Pi
!     Write (*, '(''background = '', g14.7)') minimum
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Fit 2-D Gaussian to data
        Call F2D_LSQGAUSSIAN (xmaxdat, ymaxdat, DATA, x_start, y_start, x_end, &
          y_end, x_beam, y_beam, maximum, sigma1, sigma2, angle, minimum, &
          retstat, chi_squared, status)
 
!     Negative return status values are still good
        If (retstat .Lt. 0) Then
           retstat = 0
        End If
 
!     Output results
        Write (message, '(''INFO: X-centre = '', g14.7, '' (pixels)'')') &
          x_beam
        Call IO_WRITE (message, status)
        Write (message, '(''      Y-centre = '', g14.7, '' (pixels)'')') &
          y_beam
        Call IO_WRITE (message, status)
        Write (message, '(''      Gaussian peak intensity = '', g14.7)') &
          maximum
        Call IO_WRITE (message, status)
        Write (message, '(''      Angle of first ' // &
          'axis = '', g14.7, '' (degrees))'')') angle * 180.0 / Pi
        Call IO_WRITE (message, status)
        Write (message, '(''      Sigma of first ' // &
          'axis = '', g14.7, '' (pixels))'')') sigma1
        Call IO_WRITE (message, status)
        Write (message, '(''      Sigma of second ' // &
          'axis = '', g14.7, '' (pixels))'')') sigma2
        Call IO_WRITE (message, status)
        Write (message, '(''      Background average intensity = '', g14.7)') &
          minimum
        Call IO_WRITE (message, status)
 
     End If
 
     End Subroutine F2D_BEAMGAUSSIAN
!********1*********2*********3*********4*********5*********6*********7**
 
 
 

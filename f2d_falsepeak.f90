!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_falsepeak.f90 *
!  *                   *
!  *********************
 
!+ F2D_FALSEPEAK: add FALSE PEAK
     Subroutine F2D_FALSEPEAK (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, title, xlabel, ylabel, zlabel, DATA, status)
!  Description:
!    Add false peak to image, based on neighbouring peak.
!  Keywords:
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    09-Apr-1999: V0.1 Original, based on "F2D_CALIBRATE2DGRID" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! The data values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: dummy ! Dummy variable
     Integer :: half_box ! Half the size of the output region box
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: x ! Loop varaible for first direction
     Integer :: x_centre ! X-pixel for centre of output
     Integer :: x_low ! Position of lower X-pixel for interpolation
     Integer :: xend_out ! X-end pixel of output region
     Integer :: xstr_out ! X-start pixel of output region
     Integer :: y ! Loop variable for second direction
     Integer :: y_centre ! Y-pixel for centre of output
     Integer :: y_low ! Position of lower Y-pixel for interpolation
     Integer :: yend_out ! Y-end pixel of output region
     Integer :: ystr_out ! Y-start pixel of output region
     Real :: back_intensity ! Intensity of background pixel
     Real :: high_fraction ! Fraction of higher bin for interpolation
     Real :: low_fraction ! Fraction of lower bin for interpolation
     Real :: x_back ! X-coordinate of background pixel
     Real :: x_end ! X-coordinate of peak away from false peak
     Real :: x_in ! X-coordinate for intensity to copy
     Real :: x_start ! X-coordinate of peak neighbouring false peak
     Real :: x_vector ! X-component of vector from false peak to real peak
     Real :: y_back ! Y-coordinate of background pixel
     Real :: y_end ! Y-coordinate of peak away from false peak
     Real :: y_in ! Y-coordinate for intensity to copy
     Real :: y_start ! Y-coordinate of peak neighbouring false peak
     Real :: y_vector ! Y-component of vector from false peak to real peak
     Real :: y1 ! Interpolated lower intensity
     Real :: y2 ! Interpolated upper intensity
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(3) ! User messages
!    Internal Functions:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_FALSEPEAK ' // Version)
     Else
 
!     Display data
        Call GS_2DIMAGE (xmaxdat, ymaxdat, DATA, XAXIS, YAXIS, xstrelm, &
          ystrelm, xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input centre of peak
        num_coordinates = 1
        MESSAGE(1) = 'Click on peak next to false peak to create'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, XAXIS, YAXIS, &
          title, xlabel, ylabel, zlabel, &
          'Click on centre of peak next to false peak', 1, MESSAGE, .False., &
          1, num_coordinates, x_start, y_start, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
           Return
        End If
 
!     Display coordinate
        Call GS_MARK (x_start, y_start, 1.0, status)
        Call GS_UPDATE (status)
 
!     Input Centre of peak next peak
        num_coordinates = 1
        MESSAGE(1) = 'Click on peak away from false peak position'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, XAXIS, YAXIS, &
          title, xlabel, ylabel, zlabel, &
          'Click on peak away from false peak position', 1, MESSAGE, .False., &
          1, num_coordinates, x_end, y_end, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
           Return
        End If
 
!     Display coordinate
        Call GS_MARK (x_end, y_end, 1.0, status)
        Call GS_UPDATE (status)
 
!     Input background position
        num_coordinates = 1
        MESSAGE(1) = 'Click on background'
        Call GS_INPS_FCOORDINATES ( .False., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, dummy, XAXIS, YAXIS, &
          title, xlabel, ylabel, zlabel, 'Click on background', 1, MESSAGE, &
          .False., 1, num_coordinates, x_back, y_back, status)
 
!     Check for user escape
        If (status .Ne. St_goodvalue) Then
 
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
           End If
           Return
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Background intensity
        back_intensity = DATA(Int(x_back) + 1, Int(y_back) + 1)
 
!     Translation vector from false peak to real
        x_vector = x_end - x_start
        y_vector = y_end - y_start
        half_box = Nint(Max(Abs(x_vector / 2.0), Abs(y_vector / 2.0)))
        x_centre = Int(x_start - x_vector) + 1
        y_centre = Int(y_start - y_vector) + 1
 
!     Output region
        xstr_out = Max(xstrelm, x_centre - half_box)
        ystr_out = Max(ystrelm, y_centre - half_box)
        xend_out = Min(xendelm, x_centre + half_box)
        yend_out = Min(yendelm, y_centre + half_box)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''x_vector, y_vector = '', 2f12.5)')
!     :       x_vector, y_vector
!     Write (*, '(''xstr_out, ystr_out, xend_out, yend_out = '',
!     :       4i6)') xstr_out, ystr_out, xend_out, yend_out
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Loop through output region
        Do y = ystr_out, yend_out
           y_in = Real(y) - 0.5 + y_vector
 
           Do x = xstr_out, xend_out
              x_in = Real(x) - 0.5 + x_vector
 
              If (x_in .Gt. Real(xstrelm - 0.5) .And. x_in .Lt. Real(xendelm - &
                0.5) .And. y_in .Gt. Real(ystrelm - 0.5) .And. y_in .Lt. &
                Real(yendelm - 0.5)) Then
 
                 x_low = Int(x_in + 0.5001)
                 y_low = Int(y_in + 0.5001)
 
!              Linear interpolation in X-direction
                 high_fraction = x_in + 0.5 - Real(x_low)
                 low_fraction = 1.0 - high_fraction
 
                 y1 = low_fraction * DATA(x_low, y_low) + high_fraction * &
                   DATA(x_low + 1, y_low)
                 y2 = low_fraction * DATA(x_low, y_low + 1) + high_fraction * &
                   DATA(x_low + 1, y_low + 1)
 
!              Linear interpolation in Y-direction
                 high_fraction = y_in + 0.5 - Real(y_low)
                 low_fraction = 1.0 - high_fraction
                 DATA(x, y) = DATA(x, y) + low_fraction * y1 + high_fraction * &
                   y2 - back_intensity
 
              End If
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_FALSEPEAK
!********1*********2*********3*********4*********5*********6*********7*********8
 

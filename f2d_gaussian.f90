!********1*********2*********3*********4*********5*********6*********7*********8

!  ********************
!  *                  *
!  * f2d_gaussian.f90 *
!  *                  *
!  ********************
 
!+ F2D_GAUSSIAN: add GAUSSIAN to data
     Subroutine F2D_GAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, status)
!  Description:
!    User specifies a Gaussian to be added to the ROI
!  Keywords:
!    Gaussian.Add, Add.Gaussian, Input.Gaussian
!  Method:
!    Uses "MA_2DGAUSSIAN" to calculate 2-D Gaussian
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    09-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: xmaxdat ! First dimension of array
     Integer, Intent(IN) :: ymaxdat ! Second dimension of array
     Integer, Intent(IN) :: xstrelm ! Defines start of ROI in X-direction
     Integer, Intent(IN) :: ystrelm ! Defines start of ROI in Y-direction
     Integer, Intent(IN) :: xendelm ! Defines end of ROI in X-direction
     Integer, Intent(IN) :: yendelm ! Defines end of ROI in Y-direction
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Array to contain X-coordinate grid
!      data
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Array to contain Y-coordinate grid
!      data
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! Array to contain data
!      values
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Real, Save :: intensity = 1000.0
     Real, Save :: orientation = 0.0
     Real, Save :: sigma1 = 50.0
     Real, Save :: sigma2 = 25.0
     Real, Save :: xcentre
     Real, Save :: ycentre
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GAUSSIAN ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_GAUSSIAN ' // Version)
     Else
 
!     Input peak centre
        xcentre = (XAXIS(xstrelm) + XAXIS(xendelm)) / 2.0
        ycentre = (YAXIS(ystrelm) + YAXIS(yendelm)) / 2.0
        Call IO_INPR (.False., 0.0, 0.0, .True., 'PEAK CENTRE X-COORDINATE ', &
          1, 'Enter X-coordinate of centre of peak', 1, &
          'Must be valid real number', xcentre, status)
        Call IO_INPR (.False., 0.0, 0.0, .True., 'PEAK CENTRE Y-COORDINATE ', &
          1, 'Enter Y-coordinate of centre of peak', 1, &
          'Must be valid real number', ycentre, status)
 
!     Input peak intensity
        Call IO_INPR (.False., 0.0, 0.0, .True., 'PEAK MAXIMUM INTENSITY', 1, &
          'Enter maximum intensity of peak', 1, 'Must be valid real number', &
          intensity, status)
 
!     Input peak intensity
        orientation = orientation * 180.0 / 3.1415926
        Call IO_INPR (.True., -360.0, 360.0, .True., &
          'ORIENTATION OF FIRST AXIS', 1, &
          'Orientation in degrees, anti-clockwise from the X-axis', 1, &
          'Must be valid real number', orientation, status)
        orientation = orientation * 3.1415926 / 180.0
 
!     Standard deviation axis one
        Call IO_INPR (.True., 0.0, 1.7e38, .True., &
          'STANDARD DEVIATION WIDTH FOR FIRST AXIS', 1, &
          'Peak width in first axis (standard deviation size)', 1, &
          'Must be valid real number', sigma1, status)
 
!     Standard deviation axis two
        Call IO_INPR (.True., 0.0, 1.7e38, .True., &
          'STANDARD DEVIATION WIDTH FOR SECOND AXIS', 1, &
          'Peak width in second axis (standard deviation size)', 1, &
          'Must be valid real number', sigma2, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Add Gaussian to data
        Call MA_2DGAUSSIAN (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
          yendelm, XAXIS, YAXIS, intensity, xcentre, ycentre, sigma1, sigma2, &
          orientation, 5.0, 1, DATA, status)
 
     End If
 
     End Subroutine F2D_GAUSSIAN
!********1*********2*********3*********4*********5*********6*********7*********8

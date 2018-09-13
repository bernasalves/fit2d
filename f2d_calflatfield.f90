!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_calflatfield.f90 *
!  *                      *
!  ************************
 
!+ F2D_CALFLATFIELD: Calibrate FLAT FIELD correction
     Subroutine F2D_CALFLATFIELD (max_pixels, PROFILE, x_centre, y_centre, &
       x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
!  Description:
!    Correct flat field measurement for source anisotropy and 1/r**2 fall-off
!    for a flat detector.
!  Keywords:
!    Correct.Flat~Field
!  Method:
!    Weighted average
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    03-Feb-1994: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
     Integer, Intent(IN) :: max_pixels ! Dimension size of profile array
     Real, Intent(IN) :: PROFILE(max_pixels) ! Reciprocal of expected profile 
!      at detector
     Real, Intent(IN) :: x_centre ! X-coordinate of centre of flat field
!      measurement
     Real, Intent(IN) :: y_centre ! Y-coordinate of centre of flat field
!      measurement
     Real, Intent(IN) :: x_pixel_size ! Size of pixel in metres in X-direction
     Real, Intent(IN) :: y_pixel_size ! Size of pixel in metres in Y-direction
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
     Real, Intent(INOUT) :: DATA(xmaxdat, ymaxdat) ! On input contains flat
!      field measurement. On output contains corrected flat field
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: pixel ! Appropriate pixel in 1-D profile
     Integer :: x ! Loop variable for X-direction
     Integer :: y ! Loop variable for Y-direction
     Real :: radial_distance ! Distance from centre of flat field
!  Local Arrays:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CALFLATFIELD ' // Version)
     Else
 
!     Loop over ROI
        Do y = ystrelm, yendelm
 
           Do x = xstrelm, xendelm
 
!           Calculate distance from centre in terms of X-pixels
              radial_distance = Sqrt( ((Real(x) - x_centre - 0.5) * &
                x_pixel_size)**2 + ((Real(y) - y_centre - 0.5) * &
                y_pixel_size)**2)
 
!           Calculate appropriate pixel from 1-D profile
              pixel = Nint(radial_distance / x_pixel_size)
 
!           Check that pixel number is within range
              pixel = Min(max_pixels, Max(1, pixel))
 
!           Correct intensity for source distribution
              DATA(x, y) = DATA(x, y) * PROFILE(pixel)
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_CALFLATFIELD
!********1*********2*********3*********4*********5*********6*********7*********8

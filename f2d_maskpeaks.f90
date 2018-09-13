!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_maskpeaks.f90 *
!  *                   *
!  *********************
 
!+ F2D_MASKPEAKS - FIT 2-D MASK PEAKS
     Subroutine F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
       xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, mask_radius, &
       title, xlabel, ylabel, zlabel, MASK, status)
!  Description:
!    The user is requested to click on coordinate positions to mask off. The 
!    pixels around each coordinate slected are masked-off in a radius of 
!    "mask_radius" from the centre pixel.
!  Keywords:
!    Mask.Input, Mask.Definition, Define.Mask, Input.Mask
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    12-Mar-2004: V0.7 Change user prompt (Hammersley)
!    26-Aug-1996: V0.6 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.5 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    26-Oct-1995: V0.4 Output "spy-glass" during coordinate input (Hammersley)
!    12-Sep-1995: V0.3 Use GUI region information (Hammersley)
!    20-Jun-1995: V0.2 Convert to GS graphics library (Hammersley)
!    04-Mar-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to display
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: mask_radius ! Radius of region to be masked out
!      from the centre of each pixel in pixel units
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be fitted, .True. = masked/bad data point, 
!      not to be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.7' ! Version number
     Integer, Parameter :: Max_Coordinates = 200 ! Maximum number of
!      coordinates which can be defined
!  Local Variables:
     Integer :: coordinate ! Loop variable for coordinates
     Integer :: num_coordinates ! Number of coordinates entered by the user
     Integer :: search_zone ! Number of pixels to search around centre
!      for pixels to be masked off
     Integer :: x ! Loop variable for the X-direction
     Integer :: x_pixel ! X-pixel number for coordinate
     Integer :: y ! Loop variable for the Y-direction
     Integer :: y_pixel ! Y-pixel number for coordinate
     Real :: distance ! Distance from centre of peak to remove to a pixel value
     Real :: x_pc ! X-pixel coordinates
     Real :: y_pc ! Y-pixel coordinates
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User help text
     Real :: X_COORDINATES(Max_Coordinates) ! X-coordinates of "peaks" to remove
     Real :: Y_COORDINATES(Max_Coordinates) ! Y-coordinates of "peaks" to remove
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKPEAKS ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKPEAKS ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Initialise variables
        search_zone = Nint(mask_radius)
 
!     Input points to mask-off
        num_coordinates = 0
        MESSAGE(1) = 'Click on coordinates to define centre of'
        MESSAGE(2) = 'circular regions to mask-out. Click in prompt'
        MESSAGE(3) = 'test region to end defining coordinates.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, 'CLICK ON PEAKS TO MASK', 3, MESSAGE, &
          .False., Max_Coordinates, num_coordinates, X_COORDINATES, &
          Y_COORDINATES, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        Do coordinate = 1, num_coordinates
 
!        Calculate pixel position
           Call MA_DC2PIXC (xmaxdat, xendelm, X_AXIS, &
             X_COORDINATES(coordinate), x_pc, status)
           Call MA_DC2PIXC (ymaxdat, yendelm, Y_AXIS, &
             Y_COORDINATES(coordinate), y_pc, status)
 
!        Pixel which was clicked upon
           x_pixel = Int(x_pc) + 1
           y_pixel = Int(y_pc) + 1
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Go through square contains pixels to be masked-off
           Do y = Max(1, y_pixel - search_zone), &
             Min(ynumdat, y_pixel + search_zone)
 
              Do x = Max(1, x_pixel - search_zone), &
                Min(xnumdat, x_pixel + search_zone)
 
!              Calculate radial distance from pixel to centre
                 distance = Sqrt( (Real(x - x_pixel))**2 + &
                   (Real(y - y_pixel))**2)
 
                 If (distance .Le. mask_radius) Then
                    MASK(x, y) = .True.
                 End If
 
              End Do
 
           End Do
 
        End Do
 
     End If
 
     End Subroutine F2D_MASKPEAKS
!********1*********2*********3*********4*********5*********6*********7*********8

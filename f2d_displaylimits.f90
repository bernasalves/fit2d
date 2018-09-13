!********1*********2*********3*********4*********5*********6*********7**
 
!  *************************
!  *                       *
!  * f2d_displaylimits.f90 *
!  *                       *
!  *************************
 
!+ F2D_DISPLAYLIMITS: DISPLAYed pixel LIMITS
     Subroutine F2D_DISPLAYLIMITS (status)
!  Description:
!    Maximum number of displayed pixels in images
!  Keywords:
!    Image.Maximum~Pixels, Maximum~Pixels.Images
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    09-Aug-1995: V0.4 (50 years after Nagasaki) Define maximum number of pixels
!      in one dimension to be output in a PostScript file image (Hammersley)
!    20-Jun-1995: V0.3 Convert to GS graphics library (Hammersley)
!    15-Oct-1993: V0.2 Request number of pixels for 3-D surface plot 
!      (Hammersley)
!    11-Oct-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc'
!  Import:
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.4' ! Version number
!  Local Variables:
     Integer :: max_image_pixels ! Maximum number of pixels to be displayed in 
!      any one direction for image display
     Integer :: max_3d_pixels ! Maximum number of pixels to be displayed in any
!      one direction for 3-D surface plot display
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User messages
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7--
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_DISPLAYLIMITS ' // Version)
     Else
 
!     Inquire current number of pixels
        Call GS_INQ_PIXELLIMITS (max_image_pixels, max_3d_pixels, status)
 
!     Ask for required value
        MESSAGE(1) = 'Enter the Maximum number of pixels to ' // &
          'display in an image (or contour'
        MESSAGE(2) = 'plot) in one direction. If there are ' // &
          'more than this number of pixels'
        MESSAGE(3) = 'to display in either the X or the ' // &
          'Y-direction then the image will be'
        MESSAGE(4) = 'displayed with automatic rebinning of ' // &
          'the necessary number of pixels'
        MESSAGE(5) = '(in each direction). The rebinning ' // &
          'includes dividing by the number of'
        MESSAGE(6) = 'pixels, so the displayed range should be ' // &
          'about the same.'
        Call IO_INPI (.True., 1, 10000, .True., &
          'IMAGE: MAXIMUM PIXEL DIMENSION', 6, MESSAGE, 1, &
          'Enter integer in given range', max_image_pixels, status)
 
!     Ask for required value
        MESSAGE(1) = 'Enter the Maximum number of pixels to ' // &
          'display in a 3-D surface'
        MESSAGE(2) = 'plot in one direction. If there are ' // &
          'more than this number of pixels'
        MESSAGE(3) = 'to display in either the X or the ' // &
          'Y-direction then the image will be'
        MESSAGE(4) = 'displayed with automatic re-binning of ' // &
          'the necessary number of pixels'
        MESSAGE(5) = '(in each direction). The re-binning ' // &
          'includes dividing by the number of'
        MESSAGE(6) = 'pixels, so the displayed range should be ' // &
          'about the same.'
        Call IO_INPI (.True., 1, 10000, .True., &
          '3-D PLOT: MAXIMUM PIXEL DIMENSION', 6, MESSAGE, 1, &
          'Enter integer in given range', max_3d_pixels, status)
 
!     Set input value
        Call GS_SET_PIXELLIMITS (max_image_pixels, max_3d_pixels, status)
 
     End If
 
     End Subroutine F2D_DISPLAYLIMITS
!********1*********2*********3*********4*********5*********6*********7**

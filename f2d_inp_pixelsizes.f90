!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **************************
!  *                        *
!  * f2d_inp_pixelsizes.f90 *
!  *                        *
!  **************************
 
!+ F2D_INP_PIXELSIZES -  INPut PIXEL SIZES (X/Y dimensions of a single pixel)
     Subroutine F2D_INP_PIXELSIZES (gui, x_pixel_size, y_pixel_size, status)
!  Description:
!    User input of X/Y pixel sizes. The pixel dimensions are returned in metre 
!    units, whereas the user is prompted to enter the distance in microns.
!  Keywords:
!    Pixel.Sizes, Sizes.Pixels
!  Method:
!    Uses "IO_INPR"
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Feb-1999: V0.5 All data-base saving and recovering routines renamed 
!      (Hammersley)
!    15-Dec-1998: V0.4 Change to use IO internal database routines (Hammersley)
!    27-Oct-1998: V0.3 Save values in internal data-base (Hammersley)
!    11-Feb-1996: V0.2 Option of GUI (Hammersley)
!    24-Jan-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is being 
!      used
!  Import/Export:
     Real, Intent(INOUT) :: x_pixel_size ! X-direction pixel size (metres)
     Real, Intent(INOUT) :: y_pixel_size ! Y-direction pixel size (metres)
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
!  Local Variables:
     Integer :: retstat ! Return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
!      3 = Problem converting character string to a real value
!  Local Arrays:
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_PIXELSIZES ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input X size of one pixel
        x_pixel_size = x_pixel_size * 1.0e6
 
        If (gui) Then
 
           Call GS_INPR (.True., 0.001, 10000.0, .True., &
             'X-DIRECTION PIXEL SIZE (MICRONS)', 1, &
             'Enter X-dimension size of one raw pixel in microns ', 1, &
             'Must be valid real number', x_pixel_size, status)
 
        Else
 
           Call IO_INPR (.True., 0.001, 10000.0, .True., &
             'X-DIRECTION PIXEL SIZE (MICRONS)', 1, &
             'Enter X-dimension size of one raw pixel in microns ', 1, &
             'Must be valid real number', x_pixel_size, status)
 
        End If
 
        x_pixel_size = x_pixel_size * 1.0e-6
 
!     Input Y size of one pixel
        y_pixel_size = y_pixel_size * 1.0e6
 
        If (gui) Then
 
           Call GS_INPR (.True., 0.001, 10000.0, .True., &
             'Y-DIRECTION PIXEL SIZE (MICRONS)', 1, &
             'Enter Y-dimension size of one raw pixel in microns ', 1, &
             'Must be valid real number', y_pixel_size, status)
 
        Else
 
           Call IO_INPR (.True., 0.001, 10000.0, .True., &
             'Y-DIRECTION PIXEL SIZE (MICRONS)', 1, &
             'Enter Y-dimension size of one raw pixel in microns ', 1, &
             'Must be valid real number', y_pixel_size, status)
 
        End If
 
        y_pixel_size = y_pixel_size * 1.0e-6
 
!     Save input pixel sizes
        Call IO_SET_RKEYVALUE ('X_PIXEL_SIZE', x_pixel_size, retstat, status)
        Call IO_SET_RKEYVALUE ('Y_PIXEL_SIZE', y_pixel_size, retstat, status)
 
     End If
 
     End Subroutine F2D_INP_PIXELSIZES
!********1*********2*********3*********4*********5*********6*********7*********8
 

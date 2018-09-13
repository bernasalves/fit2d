!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_symfun.f90 *
!  *                *
!  ******************
 
!+ F2D_SYMFUN - FIT 2-D SYMmetric FUNction calculation
     Subroutine F2D_SYMFUN (rad_pixel_size, maxrad, numrad, PROFILE, &
       x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, ystrelm, &
       xendelm, yendelm, DATA, status)
!  Description:
!    Calculate circularly symmetric function from 1-D profile and centre of 
!    symmetry, input interactively.
!  Keywords:
!    Symmetric~Function.Calculation, Calculate.Symmetric~Function,
!    Function.Radially~Symmetric.Calculation
!  Method:
!    Uses MA_SYMFUN to calculate radial profile of input parameters
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    20-Mar-1995: V0.2 Take into account pixel size (Hammersley)
!    16-Feb-1993: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Real, Intent(IN) :: rad_pixel_size ! Size of a radial profile pixel(metres)
     Integer, Intent(IN) :: maxrad ! Dimension size of "RADAXIS" and "PROFILE"
     Integer, Intent(IN) :: numrad ! Number of defined elements in "PROFILE"
     Real, Intent(IN) :: PROFILE(maxrad) ! The averaged radial profile
!      calculated from a user input centre
     Real, Intent(IN) :: x_pixel_size ! Size of a corrected pixel in the
!      current data in the X-direction (metres)
     Real, Intent(IN) :: y_pixel_size ! Size of a corrected pixel in the
!      current data in the Y-direction (metres)
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: DATA(xmaxdat, ymaxdat) ! The data values, with the
!      symmetric function added
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.2' ! Version number
!  Local Variables:
     Real :: xcensym ! The X-coordinate for the centre of radial symmetry
     Real :: ycensym ! The Y-coordinate for the centre of radial symmetry
!  Local Arrays:
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_SYMFUN ' // Version)
        Return
     End If
 
!  Input coordinate of centre for radial symmetry
     Call IO_INPR (.False., 0.0, 0.0, .False., &
       'X-COORDINATE OF CIRCULAR SYMMETRY', 1, 'X-centre for radial profile', &
       1, 'Must be valid real', xcensym, status)
     Call IO_INPR (.False., 0.0, 0.0, .False., &
       'Y-COORDINATE OF CIRCULAR SYMMETRY', 1, 'Y-centre for radial profile', &
       1, 'Must be valid real', ycensym, status)
 
!  Calculate radial profile
     Call MA_SYMFUN (rad_pixel_size, maxrad, numrad, PROFILE, xcensym, &
       ycensym, 1, x_pixel_size, y_pixel_size, xmaxdat, ymaxdat, xstrelm, &
       ystrelm, xendelm, yendelm, DATA, status)
 
     End Subroutine F2D_SYMFUN
!********1*********2*********3*********4*********5*********6*********7*********8

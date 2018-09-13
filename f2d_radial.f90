!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ******************
!  *                *
!  * f2d_radial.f90 *
!  *                *
!  ******************
 
!+ F2D_RADIAL - FIT 2-D RADIAL profile
     Subroutine F2D_RADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
       yendelm, XAXIS, YAXIS, DATA, MASK, variances_exist, RADAXIS, RPROFILE, &
       PROERRS, status)
!  Description:
!    Calculate radial profile from input centre of symmetry.
!  Keywords:
!    Radial~Profile.Calculation, Calculate.Radial~Profile,
!    Profile.Radial.Calculation
!  Method:
!    Uses MA_RADIAL to calculate radial profile of input
!    parameters
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    28-Mar-2006: V0.3 Use Fortran "Allocate" for dynamic arrays (Hammersley)
!    22-Nov-1993: V0.2 Radial axis calculated by "MA_RADIAL" (Hammersley)
!    09-Feb-1993: V0.1 Original, based on "FIT2RAD" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! Data mask, .True.,
!      indicates masked data point (i.e. point not considered in fitting)
     Logical, Intent(IN) :: variances_exist ! .True., if the variances array
!      exists
!  Import/Export:
!  Export:
     Real, Intent(OUT) :: RADAXIS(xmaxdat) ! The radial positions at which
!      the radial profile has been calculated
     Real, Intent(OUT) :: RPROFILE(xmaxdat) ! The averaged radial profile
!      calculated from a user input centre
     Real, Intent(OUT) :: PROERRS(xmaxdat) ! The estimated standard
!      variances in the radial profile
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer stat ! Status return variable for "Allocate"
     Real :: spacing ! World-coordinate spacing between pixels for calculation 
!      of the radial profile
     Real :: xcensym ! The X-coordinate for the centre of radial symmetry for 
!      calculation of the radial profile
     Real :: ycensym ! The Y-coordinate for the centre of radial symmetry for 
!      calculation of the radial profile
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(2) ! Text to be output giving full 
!      explanation of required parameter
     Real, Allocatable :: NUMPIXELS(:) ! Dynamic work array
!  External Functions:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_RADIAL'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_RADIAL ' // Version)
        Return
     End If
 
!  Input world coordinate system spacing between pixels for the
!  calculation of the radial profile
     spacing = 1.0
     MESSAGE(1) = 'The world-coordinate spacing between pixel'
     MESSAGE(2) = 'positions for the calculation of the radial profile'
     Call IO_INPR (.True., 0.0, 1.7e38, .True., 'RADIAL PIXEL SPACING', 2, &
       MESSAGE, 1, 'Must be in set range', spacing, status)
 
!  Input coordinate of centre for radial profile
     Call IO_INPR (.False., 0.0, 0.0, .False., &
       'X-COORDINATE OF RADIAL SYMMETRY', 1, 'X-centre for radial profile', 1, &
       'Must be valid real', xcensym, status)
     Call IO_INPR (.False., 0.0, 0.0, .False., &
       'Y-COORDINATE OF RADIAL SYMMETRY', 1, 'Y-centre for radial profile', 1, &
       'Must be valid real', ycensym, status)
 
!  Get dynamic work array space
     Allocate (NUMPIXELS(xmaxdat), Stat = stat)
     If (stat .Ne. 0) Then
        status = St_mod_fit2d + St_bad_malloc
        Call ST_SAVE ('Subroutine F2D_RADIAL ' // Version)
        Return
     End If
 
!  Calculate radial profile
     Call MA_RADIAL (xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, yendelm, &
       XAXIS, YAXIS, DATA, MASK, variances_exist, xcensym, ycensym, spacing, &
       1, xmaxdat, RADAXIS, RPROFILE, PROERRS, NUMPIXELS, status)
 
!  Free dynamic array space
     Deallocate (NUMPIXELS)
 
     End Subroutine F2D_RADIAL
!********1*********2*********3*********4*********5*********6*********7*********8

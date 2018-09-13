!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ************************
!  *                      *
!  * f2d_1dmaskregion.f90 *
!  *                      *
!  ************************
 
!+ F2D_1DMASKREGION - FIT 2-D 1-D data MASK REGION
     Subroutine F2D_1DMASKREGION (xmaxdat, ymaxdat, xnumdat, ynumdat, XAXIS, &
       YAXIS, DATA, xstrelm, ystrelm, xendelm, yendelm, mask_data, title, &
       xlabel, ylabel, zlabel, MASK, status)
!  Description:
!    The user is requested to define a rectangular region which either
!    defines masked-off pixels in the image, i.e. the areas which will
!    be ignored by the fitting program, or defines region to
!    un-mask depending on the value of "mask_data".
!  Keywords:
!    Mask.Input, Mask.Definition, Define.Mask, Input.Mask
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    04-Feb-1997: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of elements in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of elements in Y-direction
     Real, Intent(IN) :: XAXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: YAXIS(ymaxdat) ! Axis data for the Y-axis
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to display
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! Y-pixel number for the end of the ROI
     Logical, Intent(IN) :: mask_data ! .True., if defined polygon regions
!      are to be masked off. Otherwise the regions are unmasked
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
!  Import/Export:
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be fitted, .True. = masked/bad data point, not to
!      be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.1' ! Version number
!  Local Variables:
     Integer :: num_coordinates ! Number of returned coordinates
     Integer :: x ! Loop variable for X-direction
     Integer :: xend ! X-end element of masking or un-masking region
     Integer :: xstr ! X-start element of masking or un-masking region
     Integer :: y ! Loop variable for Y-direction
     Integer :: yend ! Y-end element of masking or un-masking region
     Integer :: ystr ! Y-start element of masking or un-masking region
     Logical :: x_linear ! .True., if the X-axis scale is to be linear
     Logical :: y_linear ! .True., if the Y-axis scale is to be linear
     Real :: value ! Data point value
     Real :: x1 ! First X-pixel coordinate of limit
     Real :: x2 ! Second X-pixel coordinate of limit
     Real :: xp1 ! First X-pixel coordinate of limit
     Real :: xp2 ! Second X-pixel coordinate of limit
     Real :: y1 ! First Y-pixel coordinate of limit
     Real :: y2 ! Second Y-pixel coordinate of limit
     Real :: yp1 ! First Y-pixel coordinate of limit
     Real :: yp2 ! Second Y-pixel coordinate of limit
!  Local Arrays:
     Real :: X_COORDINATES(2) ! X-coordinates of rectangle limits
     Real :: Y_COORDINATES(2) ! Y-coordinates of rectangle limits
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_1DMASKREGION ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_1DMASKREGION ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input 2 coordinates for limits of display region
        num_coordinates = 2
        Call GS_INP_FCOORDINATES ( &
          'CLICK ON OPPOSITE CORNERS OF THE RECTANGLE', 1, &
          'Click on 2 X/Y coordinates, to define rectangle', .False., 2, &
          num_coordinates, X_COORDINATES, Y_COORDINATES, status)
 
!     Inquire X/Y graph log/linear requirements
        Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
 
!     Set minimum and maximum limits
        x1 = Min(X_COORDINATES(1), X_COORDINATES(2))
        x2 = Max(X_COORDINATES(1), X_COORDINATES(2))
        y1 = Min(Y_COORDINATES(1), Y_COORDINATES(2))
        y2 = Max(Y_COORDINATES(1), Y_COORDINATES(2))
 
        If (.Not. x_linear) Then
           x1 = 10**x1
           x2 = 10**x2
        End If
 
        If (.Not. y_linear) Then
           y1 = 10**y1
           y2 = 10**y2
        End If
 
        If (xendelm .Ne. xstrelm) Then
 
!        Convert data coordinates to pixel coordinates
           Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x1, xp1, status)
           Call MA_DC2PIXC (xmaxdat, xendelm, XAXIS, x2, xp2, status)
 
!        Convert to integers
           xstr = Int(xp1) + 1
           xend = Int(xp2) + 1
           xstr = Max(1, xstr)
           xend = Min(xnumdat, xend)
 
           Do x = xstr, xend
 
              value = DATA(x, ystrelm)
 
              If (value .Ge. y1 .And. value .Le. y2) Then
 
!              Data point is inside the region
                 If (mask_data) Then
                    MASK(x, ystrelm) = .True.
                 Else
                    MASK(x, ystrelm) = .False.
                 End If
 
              End If
 
           End Do
 
        Else
 
!        Convert data coordinates to pixel coordinates
           Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, x1, yp1, status)
           Call MA_DC2PIXC (ymaxdat, yendelm, YAXIS, x2, yp2, status)
 
!        Convert to integers
           ystr = Int(yp1) + 1
           yend = Int(yp2) + 1
           ystr = Max(1, ystr)
           yend = Min(ynumdat, yend)
 
           Do y = ystr, yend
 
              value = DATA(xstrelm, y)
 
              If (value .Ge. y1 .And. value .Le. y2) Then
 
!              Data point is inside the region
                 If (mask_data) Then
                    MASK(xstrelm, y) = .True.
                 Else
                    MASK(xstrelm, y) = .False.
                 End If
 
              End If
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_1DMASKREGION
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ***********************
!  *                     *
!  * f2d_maskpolygon.f90 *
!  *                     *
!  ***********************
 
!+ F2D_MASKPOLYGON - FIT 2-D data MASK
     Subroutine F2D_MASKPOLYGON (xmaxdat, ymaxdat, XAXIS, YAXIS, DATA, &
       xstrelm, ystrelm, xendelm, yendelm, mask_data, title, xlabel, ylabel, &
       zlabel, MASK, status)
!  Description:
!    The user is requested to define a polygon region which either defines 
!    masked-off pixels in the image, i.e. the areas which will be ignored by the
!    fitting program, or defines region to un-mask depending on the value of 
!    "mask_data".
!  Keywords:
!    Mask.Input, Mask.Definition, Define.Mask, Input.Mask
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Aug-1996: V0.9 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    23-Aug-1996: V0.8 Changes to "GS_INPS_COORDINATES" (Hammersley)
!    29-Mar-1996: V0.7 Output user message whilst calculating masked region 
!      (Hammersley)
!    26-Oct-1995: V0.6 Output "spy-glass" during coordinate input (Hammersley)
!    12-Sep-1995: V0.5 Use GUI region information (Hammersley)
!    20-Jun-1995: V0.4 Convert to GS graphics library (Hammersley)
!    04-Mar-1995: V0.3 Allow both masking and un-masking operations (Hammersley)
!    28-Feb-1995: V0.2 Make "MASK" elements a single byte (Hammersley)
!    05-Feb-1993: V0.1 Original, based on "FIT2MASK" (Hammersley)
!  Type Definitions:
     Implicit None
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
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
!      defining regions not to be fitted, .True. = masked/bad data
!      point, not to be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.9' ! Version number
     Integer, Parameter :: Maxcoords = 200 ! Maximum number of coordinates
!      which can be defined for one polygon
     Integer, Parameter :: Maxcross = 200 ! Maximum number of intersections
!      of lines of equal y, with the boundary of the polygon, at values of y
!      which are local minima and maxima defined for one polygon
!  Local Variables:
     Integer :: numcoords ! Number of coordinates entered by the user to
!      define a dead zone polygon
     Integer :: x ! Loop variable for the X-direction
     Integer :: xmaxpix ! The maximum X pixel number for the search region
     Integer :: xminpix ! The minimum X pixel number for the search region
     Integer :: y ! Loop variable for the Y-direction
     Integer :: ymaxpix ! The maximum Y pixel number for the search region
     Integer :: yminpix ! The minimum Y pixel number for the search region
     Real :: inpoly ! Greater than zero indicates point is within the polygon,
!      zero if on the boundary, otherwise negative
     Real :: x_max_input ! The maximum X-coordinate for the GUI region
     Real :: x_min_input ! The minimum X-coordinate for the GUI region
     Real :: xmaximum ! The maximum X pixel number for the search region
     Real :: xminimum ! The minimum X pixel number for the search region
     Real :: xposition ! X-coordinate position
     Real :: y_max_input ! The maximum Y-coordinate for the GUI region
     Real :: y_min_input ! The minimum Y-coordinate for the GUI region
     Real :: ymaximum ! The maximum Y pixel number for the search region
     Real :: yminimum ! The minimum Y pixel number for the search region
     Real :: yposition ! Y-coordinate position
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(5) ! User help text
     Integer*2 :: INDP(2,Maxcross) ! Work array for GA05A
     Real :: DUMMY1(1) ! Dummy array for GA05A
     Real :: DUMMY2(1) ! Dummy array for GA05A
     Real :: DUMMY3(1) ! Dummy array for GA05A
     Real :: WORKP(Maxcross*4) ! Work array for GA05A
     Real :: XCOORDS(Maxcoords) ! X-coordinates to define dead zone polygon
     Real :: YCOORDS(Maxcoords) ! Y-coordinates to define dead zone polygon
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASKPOLYGON ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_MASKPOLYGON ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Input points to define 2-D polygon
        numcoords = 0
        MESSAGE(1) = 'Click on coordinates to define vertices of'
        MESSAGE(2) = 'polygon region. (Clicking just outside the'
        MESSAGE(3) = 'image region is equivalent to clicking on'
        MESSAGE(4) = 'the edge.) Click in the prompt text box to'
        MESSAGE(5) = 'finish entering vertices.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, XAXIS, YAXIS, title, xlabel, &
          ylabel, zlabel, 'ENTER COORDINATES OF MASKED REGION', 5, MESSAGE, &
          .True., Maxcoords - 1, numcoords, XCOORDS, YCOORDS, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           status = St_goodvalue
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        If (numcoords .Gt. 1) Then
 
           Call GS_FPROMPT (1, 1, 'PLEASE WAIT: CALCULATING POLYGON MASK', &
             status)
 
!        Update workstation
           Call GS_UPDATE (status)
 
!        Find points inside polygon region
 
!        Make regions closed, by specifying end point as the start point
           XCOORDS(numcoords + 1) = XCOORDS(1)
           YCOORDS(numcoords + 1) = YCOORDS(1)
 
!        Specify polygon region in form for checking on whether a
!        point is inside or outside the dead zone
           Call MA_GA05A (numcoords+1, 1, WORKP, Maxcross*4, INDP, Maxcross, &
             XCOORDS, YCOORDS, DUMMY1, DUMMY2, DUMMY3)
 
!        Find maximum and minimum X/Y values to define search region
           Call MA_RMINMAX (Maxcoords, 1, XCOORDS, 1, 1, numcoords, 1, &
             xminimum, xmaximum, status)
           Call MA_RMINMAX (Maxcoords, 1, YCOORDS, 1, 1, numcoords, 1, &
             yminimum, ymaximum, status)
 
!        Convert world coordinate limits to pixel numbers
           Call GS_CAL_WCTOPIX (xmaxdat, xendelm, XAXIS, xminimum, xminpix, &
             status)
           Call GS_CAL_WCTOPIX (xmaxdat, xendelm, XAXIS, xmaximum, xmaxpix, &
             status)
           Call GS_CAL_WCTOPIX (ymaxdat, yendelm, YAXIS, yminimum, yminpix, &
             status)
           Call GS_CAL_WCTOPIX (ymaxdat, yendelm, YAXIS, ymaximum, ymaxpix, &
             status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*,'(1x,''xminpix,xmaxpax = '',2i8)')
!        :           xminpix,xmaxpix
!        Write (*,'(1x,''yminpix,ymaxpax = '',2i8)')
!        :           yminpix,ymaxpix
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Within defined search region, mask off all points within
!        defined polygon
           Do y = yminpix, ymaxpix
              yposition = YAXIS(y)
 
              Do x = xminpix, xmaxpix
 
!              Define X/Y position
                 xposition = XAXIS(x)
 
!              Test if point is within the polygon
                 Call MA_GA05B (xposition, yposition, inpoly, numcoords + 1, &
                   1, WORKP, Maxcross*4, INDP, Maxcross, XCOORDS, YCOORDS, &
                   DUMMY1, DUMMY2, DUMMY3)
 
                 If (inpoly.Ge.0) Then
 
!                 The point is inside the polygon region
                    If (mask_data) Then
                       MASK(x, y) = .True.
                    Else
                       MASK(x, y) = .False.
                    End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*,'(1x,''Pixel '',2i4,'' is masked off''
!                 :                   )') x,y
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 End If
 
              End Do
 
           End Do
 
        End If
 
     End If
 
     End Subroutine F2D_MASKPOLYGON
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

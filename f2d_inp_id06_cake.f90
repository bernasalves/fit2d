!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *************************
!  *                       *
!  * f2d_inp_id06_cake.f90 *
!  *                       *
!  *************************
 
!+ F2D_INP_ID06_CAKE: INPut CAKE region
     Subroutine F2D_INP_ID06_CAKE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
       DATA, MASK, X_AXIS, &
       Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
       zlabel, azimuth_start, azimuth_end, experiment, &
       start_azimuth, end_azimuth, inner_limit, outer_limit, status)
!  Description:
!    User input of cake definition
!  Keywords:
!    Cake.Define, Define.Cake
!  Method:
!    Stores geometry parameters in internal database
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    16-Oct-2014: V0.3 Use beam centre for inner limit, if no click (Hammersley)
!    29-Apr-2014: V0.2 Correct "outer_limit" and use azimuth limits (Hammersley)
!    01-Apr-2014: V0.1 Original, based on "F2D_INP_CAKE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc'
     Include 'gs_constants.inc' ! GS constants
!  Import:
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
     Integer, Intent(IN) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! The data mask
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Real, Intent(IN) :: azimuth_start ! Azimuth at start of full input data 
!      region (radians)
     Real, Intent(IN) :: azimuth_end ! Azimuth at start of full input data 
!      region (radians)
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
     Real, Intent(OUT) :: start_azimuth ! Angle of azimuth of start of
!      region in radians
     Real, Intent(OUT) :: end_azimuth ! Angle of azimuth of end of region in
!      radians
     Real, Intent(OUT) :: inner_limit ! Inner radius in metres
     Real, Intent(OUT) :: outer_limit ! Outer radius in metres
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.3' ! Version number
!  Local Variables:
     Integer :: method_used ! The input method that was used to
!      specify the beam centre:
!        1 = Keyboard,            only "x_beam, y_beam" are defined
!        2 = Single cursor point,   "      "       "     "     "
!        3 = Symmetric points,      "      "       "     "     "
!        4 = Points on circle,  "radius1" and "radial_error" are defined
!        5 = Points on ellipse, "radius1", "radius2","angle" and
!            "radial_error" are defined
     Integer :: num_coordinates ! Number of coordinates input
     Integer :: retstat ! Return status variable
     Logical :: continue ! .True., until input is O.K.
     Logical :: start_zero ! .True., if start line was not clicked
!      for, but was chosen to be zero
     Real :: angle ! Angle of "radius1" of ellipse to X-axis
!      (anti-clockwise radians)
     Real :: radial_error ! Error in pixels in fitting circle
     Real :: radius1 ! First radius of fitted circle
     Real :: radius2 ! Second radius of fitted circle
     Real :: x1_coordinate ! Input X-coordinate
     Real :: x2_coordinate ! Input X-coordinate
     Real :: x3_coordinate ! Input X-coordinate
     Real :: xmaxdddr ! X-maximum of displayed data display region
     Real :: xmindddr ! X-minimum of displayed data display region
     Real :: y1_coordinate ! Input Y-coordinate
     Real :: y2_coordinate ! Input Y-coordinate
     Real :: y3_coordinate ! Input Y-coordinate
     Real :: ymaxdddr ! Y-maximum of displayed data display region
     Real :: ymindddr ! Y-minimum of displayed data display region
!  Local Arrays:
     Character(Len = 80) :: MESSAGE(6) ! User help text
!  External Functions:
!  Local Data:
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_INP_ID06_CAKE ' // Version)
        Return
     End If
 
!  Check that the region to be examined is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     Else If (xstrelm .Lt. 1 .Or. xstrelm .Gt. xendelm .Or. &
       xendelm .Gt. xmaxdat) Then
        status = St_bad_adr1
     Else If (ystrelm .Lt. 1 .Or. ystrelm .Gt. yendelm .Or. &
       yendelm .Gt. ymaxdat) Then
        status = St_bad_adr2
     End If
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_INP_ID06_CAKE ' // Version)
     Else
 
!     Inquire current displayed data display region
        Call GS_INQ_DDDR (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
 
!     Set clipping window to displayed graph position
        Call LG_CLIPWINDOW (xmindddr, ymindddr, xmaxdddr, ymaxdddr, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''X/Y beam (metres) = '', 2g14.5)')
!     :       x_beam, y_beam
!     Write (*, '(''X/Y beam (pixels) = '', 2g14.5)')
!     :       x_beam_pc, y_beam_pc
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Input coordinate to defined starting azimuth
        num_coordinates = 0
        MESSAGE(1) = 'Click on a coordinate to define the starting'
        MESSAGE(2) = 'azimuth of the region to be transformed to'
        MESSAGE(3) = '2-theta or other scans. If you click in the'
        MESSAGE(4) = 'message box (yellow, therefore active), then'
        MESSAGE(5) = 'the X-axis (azimuth 0.0) will be used as the'
        MESSAGE(6) = 'start of the region.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, 'STARTING AZIMUTH (CLICK HERE FOR 0.0)', 6, &
          MESSAGE, .False., 1, num_coordinates, x1_coordinate, y1_coordinate, &
          status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Draw inverse video line
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
 
        If (num_coordinates .Eq. 1) Then
           
           Call LG_CLIP (.True., status)
           Call GS_LINE (0.0, y1_coordinate, &
             Real(xnumdat), y1_coordinate, status)
           Call LG_CLIP (.False., status)
 
!        Calculate starting azimuth
           start_azimuth = azimuth_start + (azimuth_end - azimuth_start) * &
             (y1_coordinate - Real(ystrelm)) / Real(yendelm - ystrelm)
           start_zero = .False.
 
        Else
           start_azimuth = 0.0
           start_zero = .True.
 
        End If
 
!     Input coordinate to defined end azimuth
        num_coordinates = 0
        MESSAGE(1) = 'Click on a coordinate to define the end'
        MESSAGE(2) = 'azimuth of the region to be transformed to'
        MESSAGE(3) = '2-theta or other scans. If you click in the'
        MESSAGE(4) = 'message box (yellow, therefore active), then'
        MESSAGE(5) = 'the complete data will be used.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, xstrelm, &
          ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, title, &
          xlabel, ylabel, zlabel, &
          'END AZIMUTH (CLICK HERE FOR START + 360.0)', 5, MESSAGE, .False., &
          1, num_coordinates, x2_coordinate, y2_coordinate, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        If (num_coordinates .Eq. 1) Then
 
!        Draw inverse video line from beam centre to point
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
           Call LG_CLIP (.True., status)
           Call GS_LINE (0.0, y2_coordinate, &
             Real(xnumdat), y2_coordinate, status)
           Call LG_CLIP (.False., status)
 
!        Calculate end azimuth
           end_azimuth = azimuth_start + (azimuth_end - azimuth_start) * &
             (y2_coordinate - Real(ystrelm)) / Real(yendelm - ystrelm)
        Else
           end_azimuth = 2.0 * Pi
        End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
     Write (*, '(''end azimuth (degrees) = '', g14.5)') end_azimuth * 180.0 / Pi
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Input coordinate to defined inner limit
        num_coordinates = 0
        MESSAGE(1) = 'Click on a coordinate to define the ' // &
          'inner limit'
        MESSAGE(2) = 'of the "CAKE" region to be transformed. ' // &
          'By clicking'
        MESSAGE(3) = 'in the prompt box (yellow therefore ' // &
          'active) the beam'
        MESSAGE(4) = 'centre will be used and the output ' // &
          'scans will be'
        MESSAGE(5) = 'defined starting from 0.0 angle or ' // &
          'radius in the'
        MESSAGE(6) = '2-theta / radial direction.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, &
          title, xlabel, ylabel, zlabel, &
          'INNER LIMIT (CLICK HERE FOR BEAM CENTRE)', 6, MESSAGE, .False., &
          1, num_coordinates, x3_coordinate, y3_coordinate, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
        If (num_coordinates .Eq. 1) Then
 
!        Calculate inner radius
           inner_limit = x3_coordinate * experiment%x_pixel_size
 
!        Draw line
           Call LG_CLIP (.True., status)
           Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
           Call GS_LINE (x3_coordinate, 0.0, x3_coordinate, Real(ynumdat), &
             status)
           Call LG_CLIP (.False., status)
 
        Else
           inner_limit = 0.0
!           inner_limit = experiment%x_beam * experiment%x_pixel_size
        End If
 
!     Input coordinate to defined outer limit
        num_coordinates = 1
        MESSAGE(1) = 'Click on coordinate to define outer limit'
        MESSAGE(2) = 'of "cake" region to be transformed.'
        Call GS_INPS_FCOORDINATES (.True., .True., xmaxdat, ymaxdat, &
          xstrelm, ystrelm, xendelm, yendelm, DATA, MASK, X_AXIS, Y_AXIS, &
          title, xlabel, ylabel, zlabel, 'CLICK TO DEFINE OUTER LIMIT', 2, &
          MESSAGE, .False., 1, num_coordinates, x3_coordinate, &
          y3_coordinate, status)
 
!     Check for user escape
        If (status .Eq. St_escapevalue) Then
           Return
        Else If (status .Ne. St_goodvalue) Then
           Return
        End If
 
!     Calculate outer radius
        Call GS_LINESTYLE (Lg_solid, 1.0, Gs_inverse, status)
        outer_limit = x3_coordinate * experiment%x_pixel_size
 
!     Draw outer line
        Call LG_CLIP (.True., status)
        Call GS_LINE (x3_coordinate, 0.0, x3_coordinate, Real(ynumdat), &
          status)
        Call LG_CLIP (.False., status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''outer limit (pixels) = '', g14.5)') outer_limit
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!     Check outer radius is greater than inner radius
        If (outer_limit .Le. inner_limit) Then
           Call Io_rswap (inner_limit, outer_limit, status)
        End If
 
           
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Save cake parameters in internal data-base
        Call IO_SET_RKEYVALUE ('CAKE_START_AZIMUTH', start_azimuth, retstat, &
          status)
        Call IO_SET_RKEYVALUE ('CAKE_END_AZIMUTH', end_azimuth, retstat, &
          status)
        Call IO_SET_RKEYVALUE ('CAKE_INNER_LIMIT', inner_limit, retstat, &
          status)
        Call IO_SET_RKEYVALUE ('CAKE_OUTER_LIMIT', outer_limit, retstat, &
          status)
 
     End If
 
     End Subroutine F2D_INP_ID06_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8

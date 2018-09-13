!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_cake.f90 *
!  *              *
!  ****************
 
!+ F2D_CAKE - FIT 2-D interactive "CAKE" selection
     Subroutine F2D_CAKE (gui, xmaxdat, ymaxdat, data_defined, title, xlabel, &
       ylabel, zlabel, variances_exist, xnumdat, ynumdat, xstrelm, ystrelm, &
       xendelm, yendelm, experiment, lorentz_geometry, data_type, &
       memory_defined, mask_data, &
       mask_memory, mtitle, mxlabel, mylabel, mzlabel, mxnumdat, mynumdat, &
       mxstrelm, mystrelm, mxendelm, myendelm, mx_pixel_size, my_pixel_size, &
       memory_data_type, status) 
! :    X_AXIS, Y_AXIS, DATA, MASK,     :    MX_AXIS, MY_AXIS, MDATA,
!  Description:
!    Interactive definition of an azimuthal and 2-theta region for
!    polar re-binning
!  Keywords:
!    Azimuthal/2-theta.Re-binning, Re-binning.Azimuthal/2-theta
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    27-Nov-2014: V0.24 Account for data type, to get correct D-spacings
!      (Hammersley)
!    26-Nov-2014: V0.23 Changes to "F2D_CLICK" (Hammersley)
!    22-Sep-2012: V0.22 Option to correct parallax effect (Hammersley)
!    17-Mar-2006: V0.21 Support for arbitrary aspect ratio windows (Hammersley)
!    10-Mar-2006: V0.20 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    26-Jul-2004: V0.19 Save cake parameters in internal database
!      (Hammersley)
!    25-Feb-2004: V0.18 Alter menu lay-out for landscape windows (Hammersley)
!    18-Feb-2003: V0.17 Save and recover start / end azimuth and inner and
!      outer radius values in data-base (Hammersley)
!    24-Jul-1998: V0.16 Add "XAXIS" and "YAXIS" argument to
!      "F2D_CAL_CAKE" (Hammersley)
!    28-Jan-1998: V0.15 Add "wavelength" argument for optional
!      Q-space re-binning (Hammersley)
!    10-Mar-1997: V0.14 Change to "F2D_GUI_ZSCALE" to allow
!      masked image support (Hammersley)
!    25-Jan-1997: V0.13 Cater for "CANCEL" buttons (Hammersley)
!    08-Oct-1996: V0.12 Use "GS_INP_UNZOOM" to un-zoom 1-D and/or 2-D regions 
!      (Hammersley)
!    26-Aug-1996: V0.11 Improve help text and treat graphical "CANCEL" input 
!      (Hammersley)
!    24-Aug-1996: V0.10 Add user message if no data is defined and add call to 
!      help subroutine (Hammersley)
!    23-Aug-1996: V0.9 Changes to "GS_INPS_FCOORDINATES" (Hammersley)
!    28-Jun-1996: V0.8 Correct bug to "BEAM CENRE" command, need to
!      convert value to metres before call (Hammersley)
!    29-Mar-1996: V0.7 Rename "CALCULATE" to "INTEGRATE", and don't
!      exchange data arrays after the "CANCEL" button is pressed (Hammersley)
!    08-Mar-1996: V0.6 Try to ensure that transformed data is not
!      masked (Hammersley)
!    15-Feb-1996: V0.5 Changes to "F2D_MASK" (Hammersley)
!    10-Feb-1996: V0.4 Improve plotting logic (Hammersley)
!    04-Feb-1996: V0.3 Program arrays passed through common to allow
!      "EXCHANGE" command to work (Hammersley)
!    02-Feb-1996: V0.2 Add "BEAM CENTRE" comand, etc. (Hammersley)
!    31-Jan-1996: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
     Include 'f2d_fit2d.inc' ! Pointer to program arrays
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
!  Import/Export:
     Logical, Intent(INOUT) :: data_defined ! .True., if the current data
!      arrays are defined to contain data
     Character(Len = *), Intent(INOUT) :: title ! Title for plot
     Character(Len = *), Intent(INOUT) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(INOUT) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(INOUT) :: zlabel ! Z-axis label for plot
     Logical, Intent(INOUT) :: variances_exist ! .True., if error arrays
!      exist
     Integer, Intent(INOUT) :: xnumdat ! Number of data elements defined in
!      X-direction
     Integer, Intent(INOUT) :: ynumdat ! Number of data elements defined in
!      Y-direction
!    Real X_AXIS(xmaxdat) ! X-axis values
!    Real Y_AXIS(ymaxdat) ! Y-axis values
!    Real DATA(xmaxdat, ymaxdat) ! The data values
!    Logical*1 MASK(xmaxdat, ymaxdat) ! The data mask
     Integer, Intent(INOUT) :: xstrelm ! The X-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: ystrelm ! The Y-pixel number for the start of
!      the ROI
     Integer, Intent(INOUT) :: xendelm ! The X-pixel number for the end of
!      the ROI
     Integer, Intent(INOUT) :: yendelm ! The Y-pixel number for the end of
!      the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
     Integer, Intent(INOUT) :: lorentz_geometry ! The Lorentz correction is
!      dependent on the experiment geometry:
!        0 = None
!        1 = Partial correction for an ideal powder to the equivalent of a 
!            2-theta scan i.e. detector at equal distance
     Integer, Intent(INOUT) :: data_type ! Type of data
!      0 = Unknown (No D-spacings)
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
!  Export:
     Logical, Intent(OUT) :: memory_defined ! .True., if the memory arrays
!      are defined to contain data
     Logical, Intent(OUT) :: mask_data ! .True., if the current data is to
!      be masked
     Logical, Intent(OUT) :: mask_memory ! .True., if the memory data is to
!      be masked
     Character(Len = *), Intent(OUT) :: mtitle ! Title label for data
     Character(Len = *), Intent(OUT) :: mxlabel ! X-axis label for data
     Character(Len = *), Intent(OUT) :: mylabel ! Y-axis label for data
     Character(Len = *), Intent(OUT) :: mzlabel ! Z-axis label for data
     Integer, Intent(OUT) :: mxnumdat ! Defines X-extent of data region
     Integer, Intent(OUT) :: mynumdat ! Defines Y-extent of data region
!    Real MX_AXIS(xmaxdat) ! Array containing data X-coordinates
!    Real MY_AXIS(ymaxdat) ! Array containing data Y-coordinates
!    Real MDATA(xmaxdat, ymaxdat) ! Array containing data to be fitted
     Integer, Intent(OUT) :: mxendelm ! End X-element of memory data region
     Integer, Intent(OUT) :: mxstrelm ! Starting X-element of memory data
!      region
     Integer, Intent(OUT) :: myendelm ! End Y-element of memory data region
     Integer, Intent(OUT) :: mystrelm ! Starting Y-element of memory data
!      region
     Real, Intent(OUT) :: mx_pixel_size ! Size of a pixel in the memory data
!      in the X-direction (metres)
     Real, Intent(OUT) :: my_pixel_size ! Size of a pixel in the memory data
!      in the Y-direction (metres)
     Integer, Intent(INOUT) :: memory_data_type ! Type of data
!      0 = Unknown (No D-spacings)
!      1 = 2-D X/Y raw image with beam centre
!      2 = Horizontal 2-theta data
!      3 = 2-D polar image
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.24' ! Version number
     Integer, Parameter :: Max_menu = 16 ! Dimension size of menu
     Integer, Parameter :: Max_message = 10 ! Dimension size of message
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_mode ! Mode for graphical input:
!      2 = Wait for event
!      10 = Return immediately with or without event
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: method_used ! The input method that was used to
!      specify the beam centre:
!        1 = Keyboard,            only "x_beam, y_beam" are defined
!        2 = Single cursor point,   "      "       "     "     "
!        3 = Symmetric points,      "      "       "     "     "
!        4 = Points on circle,  "radius1" and "radial_error" are defined
!        5 = Points on ellipse, "radius1", radius2","angle" and "radial_error" 
!            are defined
     Integer :: num_coordinates ! Number of input coordinates
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! "F2D_CAL_CAKE" status return variable:
!      0 = Good status, transformation output
!      -1 = Operation cancelled, no transformation output
     Logical :: continue ! .True., until user wants to exit
     Logical :: correct_aspect ! .True., if images are to be displayed
!      automatically in the correct aspect ratio
     Logical :: current_aspect ! .True., if images were previously to
!      be displayed automatically in the correct aspect ratio
     Logical :: draw_cake ! .True., if the cake is to be drawn
     Logical, Save :: first = .True. ! .True., if the subroutine is being
!      called for the first time
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: angle ! Angle of "radius1" of ellipse to X-axis
!      (anti-clockwise radians)
     Real, Save :: end_azimuth ! Angle of azimuth of end of region in radians
     Real, Save :: inner_limit ! Inner radius in metres
     Real, Save :: outer_limit ! Outer radius in metres
     Real :: radial_error ! Error in pixels in fitting circle
     Real :: radius1 ! First radius of fitted circle
     Real :: radius2 ! Second radius of fitted circle
     Real, Save :: start_azimuth ! Angle of azimuth of start of region in
!      radians
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: xmax_message ! The maximum X-coordinate for the message region
     Real :: xmin_message ! The minimum X-coordinate for the message region
     Real :: y_coordinate ! Graphical input Y-coordinate position
     Real :: ymax_message ! The maximum Y-coordinate for the message region
     Real :: ymin_message ! The minimum Y-coordinate for the message region
!  Local Arrays:
     Character(Len = 15), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(Max_message) ! User messages
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 16) / 'EXIT', '?', 'HELP', 'BEAM CENTRE', &
       'INTEGRATE', 'END AZIMUTH', 'EXCHANGE', 'FULL', 'INNER RADIUS', &
       'OUTER RADIUS', 'START AZIMUTH', 'UN-ZOOM', 'ZOOM IN', 'Z-SCALING', &
       'MASK', 'ASPECT RATIO' /
     Data (MENUTXT(item), item = 1, 16) / &
       'EXIT: Exit FIT2D', &
       '?: This help on the menu choices', &
       'HELP: Help text on this graphical menu', &
       'BEAM CENTRE: Change beam centre', &
       'INTEGRATE: Calculate "polar" radial/2-theta, azimuth transform', &
       'END AZIMUTH: Change end azimuth', &
       'EXCHANGE: Swap current data with the "memory"', &
       'FULL: View image of full data', &
       'INNER RADIUS: Change inner radius/2-theta angle', &
       'OUTER RADIUS: Change outer radial/2-theta angle', &
       'START AZIMUTH: Change starting azimuthal angle', &
       'UN-ZOOM: Zoom out to see more of the data', &
       'ZOOM IN: Graphical region definition', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'MASK: Mask or Un-mask data (masked pixels are not re-binned)', &
       'ASPECT RATIO: Control automatic correct aspect ratio (or not)' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_CAKE ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (xmaxdat .Le. 0) Then
        status = St_bad_dim1
     Else If (ymaxdat .Le. 0) Then
        status = St_bad_dim2
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_CAKE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire window format
        Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!     Find out message area
        Call GS_INQ_MESSAGE (xmin_message, ymin_message, xmax_message, &
          ymax_message, status)
 
!     Draw masked image or 1-D plot
        Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, %val(pDATA), %val(pMASK), &
          %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, &
          title, xlabel, ylabel, zlabel, status)
 
!     Set up integration region if this is the first time
        If (first) Then
 
!        Try to input default parameters for peak search from the
!        data-base
           Call IO_INQ_RKEYVALUE ('CAKE_START_AZIMUTH', start_azimuth, &
             retstat, status)
           Call IO_INQ_RKEYVALUE ('CAKE_END_AZIMUTH', end_azimuth, retstat, &
             status)
           Call IO_INQ_RKEYVALUE ('CAKE_INNER_LIMIT', inner_limit, retstat, &
             status)
           Call IO_INQ_RKEYVALUE ('CAKE_OUTER_LIMIT', outer_limit, retstat, &
             status)
 
!        Input starting azimuthal and radial limits
           Call F2D_INP_CAKE (xmaxdat, ymaxdat, %val(pDATA), %val(pMASK), &
             %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, xendelm, yendelm, &
             title, xlabel, ylabel, zlabel, experiment, &
             start_azimuth, end_azimuth, inner_limit, outer_limit, status)
 
!        Check for user escape
           If (status .Eq. St_escapevalue) Then
              status = St_goodvalue
              Return
           Else If (status .Ne. St_goodvalue) Then
              Return
           End If
 
           first = .False.
           draw_cake = .True.
 
        Else
 
           If (xendelm - xstrelm + 1 .Gt. 1 .And. &
             yendelm - ystrelm + 1 .Gt. 1) Then
 
!           Draw current cake
              Call F2D_DRAW_CAKE (.False., experiment, &
                start_azimuth, end_azimuth, inner_limit, outer_limit, status)
              draw_cake = .True.
 
           End If
 
        End If
 
!     Set menu layout style
        num_menu = Max_menu
        If (width / height .Gt. 1.2) Then
           Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 12, status)
        Else
           Call GS_SET_MENULAYOUT (gs_vertical, 4, 12, 12, status)
        End If
 
!     Draw menu
        Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
        input_mode = 2
        continue = .True.
        Do While (continue)
 
!        By default update graphics
           update_image = .True.
           update_menu = .True.
           command = 'null'
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, input_mode, &
             input_type, command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_motion) Then
 
!           Pointer motion, don't update
              update_image = .False.
              update_menu = .False.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (data_type, xmaxdat, ymaxdat, &
                xstrelm, ystrelm, xendelm, yendelm, &
                %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, &
                xlabel, ylabel, zlabel, x_coordinate, y_coordinate, &
                experiment, update_image, update_menu, status)
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'BEAM CENTRE') Then
 
!              Allow beam centre to be changed
                 Call F2D_BEAMCENTRE (.True., .True., xmaxdat, ymaxdat, &
                   %val(pDATA), %val(pMASK), %val(pXAXIS), %val(pYAXIS), &
                   xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                   zlabel, .True., method_used, &
                   experiment, radius1, radius2, angle, radial_error, status)
 
!              Redraw current cake
                 Call F2D_DRAW_CAKE (.True., experiment, &
                   start_azimuth, end_azimuth, inner_limit, outer_limit, status)
 
!              Update menu
                 draw_cake = .True.
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'INTEGRATE') Then
 
!              Calculate transformation
                 Call F2D_CAL_CAKE (title, xmaxdat, ymaxdat, xstrelm, ystrelm, &
                   xendelm, yendelm, %val(pDATA), %val(pMASK), %val(pXAXIS), &
                   %val(pYAXIS), experiment, lorentz_geometry, &
                   start_azimuth, end_azimuth, inner_limit, outer_limit, &
                   memory_defined, mxnumdat, mynumdat, %val(pMXAXIS), &
                   %val(pMYAXIS), %val(pMDATA), mxstrelm, mystrelm, mxendelm, &
                   myendelm, mx_pixel_size, my_pixel_size, mtitle, mxlabel, &
                   mylabel, retstat, status)
 
                 memory_data_type = 2

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*, '(''Returned from f2D_CAL_CAKE retstat = '', i6)') &
!                   retstat
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 If (retstat .Eq. 0) Then
 
!                 Swap current data with "memory"
                    Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                      title, xlabel, ylabel, zlabel, variances_exist, &
                      data_defined, &
                      experiment%x_pixel_size, experiment%y_pixel_size, &
                      xstrelm, ystrelm, xendelm, yendelm, &
                      mxnumdat, mynumdat, mxstrelm, &
                      mystrelm, mxendelm, myendelm, mtitle, mxlabel, mylabel, &
                      mzlabel, memory_defined, mx_pixel_size, my_pixel_size, &
                      status)
 

!                 Swap masked drawing variables
                    Call IO_LSWAP (mask_data, mask_memory, status)
                    Call IO_ISWAP (data_type, memory_data_type, status)
 
                    draw_cake = .False.
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                    Write (*, '(''data_type = '', i6)') data_type
!                    Write (*, '(''memory_data_type = '', i6)') memory_data_type
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
                 Else
                    update_image = .True.
                    update_menu = .True.
                 End If
 
              Else If (command .Eq. 'END AZIMUTH') Then
 
!              New end azimuth
 
!              Turn on simple "rubber-band"
                 Call LG_SET_RUBBERBAND ( Gs_wkid_terminal, 1, &
                   experiment%x_beam, experiment%y_beam, status)
 
                 num_coordinates = 0
                 MESSAGE(1) = 'Click on a coordinate to define the end'
                 MESSAGE(2) = 'azimuth of the region to be ' // &
                   'transformed to'
                 MESSAGE(3) = '2-theta or other scans. If you ' // &
                   'click in the'
                 MESSAGE(4) = 'message box (yellow, therefore ' // &
                   'active), then'
                 MESSAGE(5) = 'the complete 360.0 of data will be used.'
                 Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                   ymaxdat, xstrelm, ystrelm, xendelm, yendelm, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                   ylabel, zlabel, &
                   'END AZIMUTH (CLICK HERE FOR START + 360.0)', 5, MESSAGE, &
                   .False., 1, num_coordinates, x_coordinate, y_coordinate, &
                   status)
 
!              Check for user escape
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else If (status .Ne. St_goodvalue) Then
                    Return
                 Else
 
!                 Turn off "rubber-band"
                    Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 0, &
                      experiment%x_beam, experiment%y_beam, status)
 
                    If (num_coordinates .Eq. 1) Then
 
!                    Calculate end azimuth
                       end_azimuth = Atan2 (y_coordinate - experiment%y_beam, &
                         x_coordinate - experiment%x_beam)
                    Else
                       end_azimuth = start_azimuth + 2.0 * Pi
                    End If
 
!                 Redraw current cake
                    Call F2D_DRAW_CAKE (.True., experiment, &
                      start_azimuth, end_azimuth, inner_limit, &
                      outer_limit, status)
 
!                 Update menu
                    draw_cake = .True.
                    update_image = .False.
 
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'EXCHANGE') Then
 
!              Swap current data with "memory"
                 Call F2D_EXCHANGE (xmaxdat, ymaxdat, xnumdat, ynumdat, title, &
                   xlabel, ylabel, zlabel, variances_exist, data_defined, &
                   experiment%x_pixel_size, experiment%y_pixel_size, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, mxnumdat, mynumdat, mxstrelm, mystrelm, mxendelm, &
                   myendelm, mtitle, mxlabel, mylabel, mzlabel, &
                   memory_defined, mx_pixel_size, my_pixel_size, status)
 
!              Swap masked drawing variables
                 Call IO_LSWAP (mask_data, mask_memory, status)
                 Call IO_ISWAP (data_type, memory_data_type, status)
 
                 draw_cake = .Not. draw_cake
 
              Else If (command .Eq. 'FULL') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'HELP') Then
 
!              Interactive help text
                 Call F2D_CAKEHELP (gui, status)
 
              Else If (command .Eq. 'INNER RADIUS') Then
 
!              New inner limit
                 num_coordinates = 0
                 MESSAGE(1) = 'Click on a coordinate to define ' // &
                   'the inner radius'
                 MESSAGE(2) = 'of the "CAKE" region to be ' // &
                   'transformed. By clicking'
                 MESSAGE(3) = 'in the prompt box (yellow ' // &
                   'therefore active) the beam'
                 MESSAGE(4) = 'centre will be used and the ' // &
                   'output scans will be'
                 MESSAGE(5) = 'defined starting from 0.0 angle ' // &
                   'or radius in the'
                 MESSAGE(6) = '2-theta / radial direction.'
                 Call GS_INPS_FCOORDINATES (mask_data, .True., xmaxdat, &
                   ymaxdat, xstrelm, ystrelm, xendelm, yendelm, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                   ylabel, zlabel, 'INNER LIMIT (CLICK HERE FOR BEAM CENTRE)', &
                   6, MESSAGE, .False., 1, num_coordinates, x_coordinate, &
                   y_coordinate, status)
 
!              Check for user escape
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else If (status .Ne. St_goodvalue) Then
                    Return
                 Else
 
                    If (num_coordinates .Eq. 1) Then
 
!                    Calculate inner radius
                       inner_limit = Sqrt(((x_coordinate - experiment%x_beam) &
                         * experiment%x_pixel_size)**2 + &
                         ((y_coordinate - experiment%y_beam) * &
                         experiment%y_pixel_size)**2)
 
                    Else
                       inner_limit = 0.0
                    End If
 
                 End If
 
!              Redraw current cake
                 Call F2D_DRAW_CAKE (.True., experiment, &
                   start_azimuth, end_azimuth, inner_limit, &
                   outer_limit, status)
 
!              Update menu
                 draw_cake = .True.
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'OUTER RADIUS') Then
 
!              New inner limit
                 num_coordinates = 1
                 MESSAGE(1) = 'Click on coordinate to define outer limit'
                 MESSAGE(2) = 'of "cake" region to be transformed.'
                 Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                   ymaxdat, xstrelm, ystrelm, xendelm, yendelm, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                   ylabel, zlabel, 'CLICK TO DEFINE OUTER LIMIT', 2, MESSAGE, &
                   .False., 1, num_coordinates, x_coordinate, y_coordinate, &
                   status)
 
!              Check for user escape
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else If (status .Ne. St_goodvalue) Then
                    Return
                 Else
 
!                 Calculate outer radius
                    outer_limit = Sqrt(((x_coordinate - experiment%x_beam) * &
                      experiment%x_pixel_size)**2 + &
                      ((y_coordinate - experiment%y_beam) * &
                      experiment%y_pixel_size)**2)
 
!                 Redraw current cake
                    Call F2D_DRAW_CAKE (.True., experiment, &
                      start_azimuth, end_azimuth, inner_limit, &
                      outer_limit, status)
 
!                 Update menu
                    draw_cake = .True.
                    update_image = .False.
 
                 End If
                 update_menu = .True.
 
              Else If (command .Eq. 'START AZIMUTH') Then
 
!              Turn on simple "rubber-band"
                 Call LG_SET_RUBBERBAND ( Gs_wkid_terminal, 1, &
                   experiment%x_beam, experiment%y_beam, status)
 
!              New starting azimuth
                 num_coordinates = 0
                 MESSAGE(1) = 'Click on a coordinate to define ' // &
                   'the starting'
                 MESSAGE(2) = 'azimuth of the region to be ' // &
                   'transformed to'
                 MESSAGE(3) = '2-theta or other scans. If you ' // &
                   'click in the'
                 MESSAGE(4) = 'message box (yellow, therefore ' // &
                   'active), then'
                 MESSAGE(5) = 'the X-axis (azimuth 0.0) will be ' // &
                   'used as the'
                 MESSAGE(6) = 'start of the region.'
                 Call GS_INPS_FCOORDINATES ( mask_data, .True., xmaxdat, &
                   ymaxdat, xstrelm, ystrelm, xendelm, yendelm, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                   ylabel, zlabel, 'STARTING AZIMUTH (CLICK HERE FOR 0.0)', 6, &
                   MESSAGE, .False., 1, num_coordinates, x_coordinate, &
                   y_coordinate, status)
 
!              Check for user escape
                 If (status .Eq. St_escapevalue) Then
                    status = St_goodvalue
                 Else If (status .Ne. St_goodvalue) Then
                    Return
                 Else
 
!                 Turn off "rubber-band"
                    Call LG_SET_RUBBERBAND ( Gs_wkid_terminal, 0, &
                      experiment%x_beam, experiment%y_beam, status)
 
                    If (num_coordinates .Eq. 1) Then
 
!                    Calculate starting azimuth
                       start_azimuth = Atan2 (y_coordinate - &
                         experiment%y_beam, x_coordinate - experiment%x_beam)
 
                    Else
                       start_azimuth = 0.0
                    End If
 
!                 Redraw current cake
                    Call F2D_DRAW_CAKE (.True., experiment, &
                      start_azimuth, end_azimuth, inner_limit, &
                      outer_limit, status)
 
!                 Update menu
                    draw_cake = .True.
                    update_image = .False.
 
                 End If
 
                 update_menu = .True.
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pXAXIS), %val(pYAXIS), %val(pDATA), title, xlabel, &
                   ylabel, zlabel, xstrelm, ystrelm, xendelm, yendelm, status)
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 Call F2D_GUI_ZSCALE ( .True., xmaxdat, ymaxdat, xnumdat, &
                   ynumdat, %val(pXAXIS), %val(pYAXIS), %val(pDATA), &
                   %val(pMASK), title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, experiment, &
                   .False., x_coordinate, y_coordinate, status)
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'MASK') Then
 
!              Mask or un-mask data
                 Call F2D_MASK (.True., xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   %val(pDATA), %val(pXAXIS), %val(pYAXIS), title, xlabel, &
                   ylabel, zlabel, experiment, xstrelm, &
                   ystrelm, xendelm, yendelm, %val(pMASK), status)
                 mask_data = .True.
 
!              Draw cake if necessary
                 If (draw_cake) Then
 
!                 Redraw current cake
                    Call F2D_DRAW_CAKE (.False., experiment, &
                      start_azimuth, end_azimuth, inner_limit, &
                      outer_limit, status)
 
                 End If
 
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'ASPECT RATIO') Then
 
!              Inquire current aspect ratio control option
                 Call GS_INQ_IASPECT (current_aspect, status)
 
!              User choice of aspect ratio
                 MESSAGE(1) = 'Enter  "YES"  if  you want image ' // &
                   'display with automatic correct aspect'
                 MESSAGE(2) = 'ratios i.e. the pixels are ' // &
                   'square. Enter "NO" to use all the available'
                 MESSAGE(3) = 'display region.  This  may  ' // &
                   'result in non-square pixels,  but  may  be'
                 MESSAGE(4) = 'preferable for very non-square images.'
                 Call GS_INPL (.True., 0, 1, .True., &
                   'AUTOMATIC CORRECT ASPECT RATIO IMAGE DISPLAY', 4, MESSAGE, &
                   1, 'Enter "YES" on "NO"', correct_aspect, status)
 
                 If (correct_aspect .Neqv. current_aspect) Then
 
!                 Set aspect ratio control option
                    Call GS_SET_IASPECT (correct_aspect, status)
                    update_image = .True.
                    update_menu = .True.
 
                 Else
 
!                 No change
                    update_image = .False.
                    update_menu = .True.
 
                 End If
 
              End If
 
           End If
 
!        Change graphics output if required
           If (update_image) Then
 
              If (data_defined) Then
 
!              Redraw image
                 Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, %val(pDATA), &
                   %val(pMASK), %val(pXAXIS), %val(pYAXIS), xstrelm, ystrelm, &
                   xendelm, yendelm, title, xlabel, ylabel, zlabel, status)
 
                 If (draw_cake) Then
 
!                 Redraw current cake
                    Call F2D_DRAW_CAKE (.False., experiment, &
                      start_azimuth, end_azimuth, inner_limit, &
                      outer_limit, status)
 
                 End If
 
              Else
 
!              Output user warning
                 MESSAGE(1) = ' '
                 MESSAGE(2) = 'THERE IS NO DATA'
                 MESSAGE(3) = ' '
                 MESSAGE(4) = 'IN THE MAIN PROGRAM'
                 MESSAGE(5) = ' '
                 MESSAGE(6) = 'ARRAY. (USE "EXCHANGE"'
                 MESSAGE(7) = ' '
                 MESSAGE(8) = 'OR "EXIT")'
                 MESSAGE(9) = ' '
                 MESSAGE(10) = ' '
                 Call GS_FPROMPT ( Max_message, 10, MESSAGE, status)
 
              End If
 
           End If
 
           If (update_menu) Then
 
!           Set layout
              Call GS_SET_MENULAYOUT (1, 4, 12, 12, status)
 
!           Re-draw menu
              Call GS_FMENU (1, 0, '! CAKE MENU !', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
        If (.Not. gui) Then
 
!        User message
           Call GS_FPROMPT (1, 1, 'CONTROL RETURNED TO TERMINAL WINDOW', &
             status)
           Call GS_UPDATE (status)
 
        End If
 
     End If
 
!  Store cake region parameters in data-base
     Call IO_SET_RKEYVALUE ('CAKE_START_AZIMUTH', start_azimuth, retstat, &
       status)
     Call IO_SET_RKEYVALUE ('CAKE_END_AZIMUTH', end_azimuth, retstat, status)
     Call IO_SET_RKEYVALUE ('CAKE_INNER_LIMIT', inner_limit, retstat, status)
     Call IO_SET_RKEYVALUE ('CAKE_OUTER_LIMIT', outer_limit, retstat, status)
 
     End Subroutine F2D_CAKE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

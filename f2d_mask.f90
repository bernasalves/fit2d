!********1*********2*********3*********4*********5*********6*********7*********8
 
!  ****************
!  *              *
!  * f2d_mask.f90 *
!  *              *
!  ****************
 
!+ F2D_MASK - FIT 2-D data MASK definition
     Subroutine F2D_MASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
       X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, experiment, &
       xstrelm, ystrelm, xendelm, yendelm, MASK, status)
!  Description:
!    Graphical menu for masking/unmasking operations
!  Keywords:
!    Mask.Input, Mask.Definition, Define.Mask, Input.Mask
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.24 Changes to "F2D_CLICK" (Hammersley)
!    16-Oct-2007: V0.23 Add significant pixel masking (Hammersley)
!    17-Mar-2006: V0.22 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.21 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    17-Mar-2005: V0.20 Add "INVERT MASK" option (Hammersley)
!    23-Feb-2005: V0.19 Add "GROW MASK" option (Hammersley)
!    19-Mar-2004: V0.18 Implement "SAVE MASK" (Hammersley)
!    10-Mar-2004: V0.17 Add "SAVE MASK" and "LOAD MASK" commands (Hammersley)
!    25-Feb-2004: V0.16 Alter menu lay-out for landscape windows (Hammersley)
!    04-Feb-1997: V0.15 Add support for 1-D data masking (Hammersley)
!    12-Nov-1996: V0.14 Add option for "THRESHOLD MASK"ing (Hammersley)
!    08-Oct-1996: V0.13 Use "GS_INP_UNZOOM" to un-zoom 1-D and/or 2-D regions 
!      (Hammersley)
!    23-Aug-1996: V0.12 Changes to "F2D_MASKARC" (Hammersley)
!    16-Apr-1996: V0.11 Add extra size for masking peaks (Hammersley)
!    16-Feb-1996: V0.10 Choice of GUI (Hammersley)
!    05-Feb-1996: V0.9 Use "GS_MPLOT" to allow either 2-D or 1-D data to be 
!      output (Hammersley)
!    10-Dec-1995: V0.8 Add arc masking (Hammersley)
!    10-Nov-1995: V0.7 Change to "F2D_CLICK" (Hammersley)
!    26-Oct-1995: V0.6 Change to "F2D_REGION" arguments (Hammersley)
!    12-Sep-1995: V0.5 Add "Z-SCALING" command (Hammersley)
!    06-Sep-1995: V0.4 Allow the user to be able to click on an
!      image pixel and get immediate information (Hammersley)
!    20-Jun-1995: V0.3 Convert to using GS graphics library (Hammersley)
!    05-Mar-1995: V0.2 Add "FULL" choice (Hammersley)
!    03-Mar-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics System constants
!  Import:
     Logical, Intent(IN) :: gui ! .True., if graphical user interface is
!      being used, (image will not be draw)
     Integer, Intent(IN) :: xmaxdat ! Dimension size in X-direction for data
!      arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension size in Y-direction for data
!      arrays
     Integer, Intent(IN) :: xnumdat ! Defines X-extent of data region
     Integer, Intent(IN) :: ynumdat ! Defines Y-extent of data region
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data to display
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! Axis data for the X-axis
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Axis data for the Y-axis
     Character(Len = *), Intent(IN) :: title ! Title label for data
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for data
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for data
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for data
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
!  Import/Export:
     Integer, Intent(INOUT) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(INOUT) :: xendelm ! X-pixel number for the end of the ROI
     Integer, Intent(INOUT) :: yendelm ! Y-pixel number for the end of the ROI
     Logical*1, Intent(INOUT) :: MASK(xmaxdat, ymaxdat) ! The data mask,
!      defining regions not to be fitted, .True. = masked/bad data point, not 
!      to be fitted
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.24' ! Version number
     Integer, Parameter :: Max_menu = 21 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Logical :: continue ! .True., until user wants to exit
     Character(Len = 256), Save :: file_name = 'fit2d.msk' ! Name of mask file
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Return status from "FIO_IN_MASK"
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Integer :: window_format ! Format of graphics window:
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
!  Local Arrays:
     Character(Len = 18), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 10) / &
       'EXIT', &
       '?', &
       'CLEAR MASK', &
       'ZOOM IN', &
       'UN-ZOOM', &
       'FULL UN-ZOOM', &
       'MASK PEAKS (5)', &
       'MASK PEAKS (9)', &
       'MASK PEAKS (15)', &
       'MASK PEAKS (27)' /
     Data (MENU(item), item = 11, 20) / &
       'MASK POLYGON', &
       'UN-MASK POLYGON', &
       'UPDATE DISPLAY', &
       'Z-SCALING', &
       'MASK ARC', &
       'THRESHOLD MASK', &
       'GROW MASK', &
       'INVERT MASK', &
       'LOAD MASK', &
       'SAVE MASK' /
     Data (MENU(item), item = 21, 21) / &
       'SIGNIFICANT PIXELS' /
     Data (MENUTXT(item), item = 1, 10) / 'EXIT: Exit from sub-menu', &
       '?: Display list of commands with short description', &
       'CLEAR MASK: Un-mask all elements', &
       'ZOOM IN: Graphical region definition', &
       'UN-ZOOM: Make region displayed bigger', &
       'FULL UN-ZOOM: View whole of data image', &
       'MASK PEAKS (5): Mask out peaks (Diameter 5 pixels)', &
       'MASK PEAKS (9): Mask out peaks (Diameter 9 pixels)', &
       'MASK PEAKS (15): Mask out peaks (Diameter 15 pixels)', &
       'MASK PEAKS (27): Mask out peaks (Diameter 27 pixels)' /
     Data (MENUTXT(item), item = 11, 20) / &
       'MASK POLYGON: Coordinate definition of region to mask off', &
       'UN-MASK POLYGON: Coordinate definition of region to un-mask', &
       'UPDATE DISPLAY: Redraw image, including masked regions', &
       'Z-SCALING: Automatic or user control of intensity display range', &
       'MASK ARC: Enter three coordinates plus width to define arc mask', &
       'THRESHOLD MASK: Use data values to define masked pixels', &
       'GROW MASK: Mask pixels touching masked pixels', &
       'INVERT MASK: Set masked elements to un-masked and vice versa', &
       'LOAD MASK: Input a previously saved mask from a selected file', &
       'SAVE MASK: Save current mask to a named output file' /
     Data (MENUTXT(item), item = 21, 21) / &
       'SIGNIFICANT PIXELS: Mask pixels significantly above window average' /
!--------1---------2---------3---------4---------5---------6---------7---------8
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_MASK ' // Version)
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
 
!  Re-check status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_MASK ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Special case of 1-D data
        If (xstrelm .Eq. xendelm .Or. ystrelm .Eq. yendelm) Then
 
!        1-D version
           Call F2D_1DMASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, DATA, &
             X_AXIS, Y_AXIS, title, xlabel, ylabel, zlabel, experiment, &
             xstrelm, ystrelm, xendelm, yendelm, MASK, status)
           Return
 
        End If
 
!     Set menu layout style
        num_menu = Max_menu
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
 
        continue = .True.
        update_image = .Not. gui
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Draw image plot of data
              Call GS_MPLOT (.True., xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 12, 17, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 4, 12, 17, status)
              End If
 
!           Draw menu
              Call GS_FMENU (1, 0, 'not used', Max_menu, num_menu, MENU, &
                status)
 
           End If
 
!        By default update
           update_image = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
!           Default is to update
              Continue
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Output the coordinate position and intensity
              Call F2D_CLICK (1, xmaxdat, ymaxdat, xstrelm, ystrelm, xendelm, &
                yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, zlabel, &
                x_coordinate, y_coordinate, experiment, &
                update_image, update_menu, status)
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'CLEAR MASK') Then
 
!              Set all mask elements to un-masked
                 Call MA_L1VALUE (xmaxdat, ymaxdat, 1, 1, xnumdat, ynumdat, &
                   .False., MASK, status)
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'FULL UN-ZOOM') Then
 
!              Set ROI to cover all data
                 xstrelm = 1
                 ystrelm = 1
                 xendelm = xnumdat
                 yendelm = ynumdat
 
              Else If (command .Eq. 'MASK PEAKS (5)') Then
 
!              Click on peaks to mask off (5 pixel diameter masked-off)
                 Call F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, 2.5, &
                   title, xlabel, ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'MASK PEAKS (9)') Then
 
!              Click on peaks to mask off (9 pixel diameter masked-off)
                 Call F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, 4.55, &
                   title, xlabel, ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'MASK PEAKS (15)') Then
 
!              Click on peaks to mask off (15 pixel diameter masked-off)
                 Call F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, 7.55, &
                   title, xlabel, ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'MASK PEAKS (27)') Then
 
!              Click on peaks to mask off (27 pixel diameter masked-off)
                 Call F2D_MASKPEAKS (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, xnumdat, ynumdat, &
                   13.55, title, xlabel, ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'MASK POLYGON') Then
 
!              Enter coordinates to mask off a polygon
                 Call F2D_MASKPOLYGON (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, .True., title, xlabel, &
                   ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'UN-MASK POLYGON') Then
 
!              Enter coordinates of polygon to un-mask
                 Call F2D_MASKPOLYGON (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, .False., title, xlabel, &
                   ylabel, zlabel, MASK, status)
 
              Else If (command .Eq. 'UN-ZOOM') Then
 
!              Increase size of 1-D region or ROI
                 Call GS_INP_UNZOOM (xnumdat, ynumdat, xstrelm, ystrelm, &
                   xendelm, yendelm, status)
 
              Else If (command .Eq. 'UPDATE DISPLAY') Then
 
!              Update is the default
                 Continue
 
              Else If (command .Eq. 'ZOOM IN') Then
 
!              Allow user to define new data region by clicking on two points
                 Call F2D_ZOOMIN ( xmaxdat, ymaxdat, xnumdat, ynumdat, X_AXIS, &
                   Y_AXIS, DATA, title, xlabel, ylabel, zlabel, xstrelm, &
                   ystrelm, xendelm, yendelm, status)
 
              Else If (command .Eq. 'Z-SCALING') Then
 
!              User defined Z-scaling mode
                 Call F2D_GUI_ZSCALE (.True., xmaxdat, ymaxdat, xnumdat, &
                   ynumdat, X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, &
                   zlabel, xstrelm, ystrelm, xendelm, yendelm, experiment, &
                   .False., x_coordinate, y_coordinate, status)
                 update_image = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'MASK ARC') Then
 
!              Enter coordinates and width to mask off an arc
                 Call F2D_MASKARC (xmaxdat, ymaxdat, X_AXIS, Y_AXIS, DATA, &
                   xstrelm, ystrelm, xendelm, yendelm, title, xlabel, ylabel, &
                   zlabel, MASK, status)
 
              Else If (command .Eq. 'THRESHOLD MASK') Then
 
!              Mask pixels which are above (or below) a user input threshold 
!              value
                 Call F2D_MASKTHRESHOLD (.True., xmaxdat, ymaxdat, xstrelm, &
                   ystrelm, xendelm, yendelm, DATA, MASK, status)
 
              Else If (command .Eq. 'GROW MASK') Then
 
                 Call F2D_MASKGROW (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, MASK, status)
 
              Else If (command .Eq. 'INVERT MASK') Then
 
                 Call F2D_MASKINVERT (xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   xstrelm, ystrelm, xendelm, yendelm, MASK, status)
 
              Else If (command .Eq. 'LOAD MASK') Then
 
                 Call FIO_IN_MASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   file_name, retstat, MASK, status)
 
              Else If (command .Eq. 'SAVE MASK') Then
 
                 Call FIO_OUT_MASK (gui, xmaxdat, ymaxdat, xnumdat, ynumdat, &
                   file_name, retstat, MASK, status)
 
              Else If (command .Eq. 'SIGNIFICANT PIXELS') Then

!              Mask pixels above a local window significance level
                 Call F2D_MASKSIGNIFICANT (xmaxdat, ymaxdat, &
                   xnumdat, ynumdat, xstrelm, ystrelm, xendelm, yendelm, &
                   DATA, MASK, status)

              Else
                 update_image = .False.
                 update_menu = .False.
              End If
 
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
 
!        Force output
           Call GS_UPDATE (status)
 
        End If
 
     End If
 
     End Subroutine F2D_MASK
!********1*********2*********3*********4*********5*********6*********7*********8
 
 

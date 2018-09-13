!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_gui_zscale.f90 *
!  *                    *
!  **********************
 
!+ F2D_GUI_ZSCALE: (Graphical User Interface) Z-SCALE requirements
     Subroutine F2D_GUI_ZSCALE (mask_data, xmaxdat, ymaxdat, xnumdat, ynumdat, &
       X_AXIS, Y_AXIS, DATA, MASK, title, xlabel, ylabel, zlabel, xstrelm, &
       ystrelm, xendelm, yendelm, experiment, lut_click, x_pc, y_pc, status)
!  Description:
!    Set intensity scaling for image display
!  Keywords:
!    Image.Z-Scaling, Intensity~Scaling.Images
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    26-Nov-2014: V0.21 Changes to "F2D_CLICK" (Hammersley)
!    17-Mar-2006: V0.20 Support for arbitrary aspect ratio windows (Hammersley)
!    14-Mar-2006: V0.19 Use "EXPERIMENTAL_DETAILS" structure (Hammersley)
!    25-Feb-2004: V0.18 Alter menu lay-out for landscape windows (Hammersley)
!    23-Feb-1999: V0.17 All data-base saving and recovering routines
!      renamed (Hammersley)
!    15-Dec-1998: V0.16 Change to use IO internal database routines (Hammersley)
!    13-Jan-1998: V0.15 Store Z-scaling requirements in data-base (Hammersley)
!    10-Jan-1998: V0.14 Allow clicking on the displayed look-up table
!      to set scaling (Hammersley)
!    10-Mar-1997: V0.13 Add option to mask data (Hammersley)
!    25-Jan-1997: V0.12 Make sure that the menu layout is correct (Hammersley)
!    10-Sep-1996: V0.11 Set X/Y graph Y scale to logarithmic or
!      linear if required (Hammersley)
!    05-Feb-1996: V0.10 Use "GS_PLOT" to allow either 2-d or
!      1-D data to be output (Hammersley)
!    10-Nov-1995: V0.9 Change to "F2D_CLICK" (Hammersley)
!    18-Oct-1995: V0.8 Linear scaling bug corrected (Hammersley)
!    18-Sep-1995: V0.7 Option of logarithmic image intensity scaling 
!      (Hammersley)
!    12-Sep-1995: V0.6 Remove variances array from arguement list (Hammersley)
!    11-Sep-1995: V0.5 Add "EXIT" command and interactive commands (Hammersley)
!    21-Jul-1995: V0.4 "Weak diffraction peak" automatic scaling (Hammersley)
!    20-Jun-1995: V0.3 Change to GS graphics library (Hammersley)
!    09-Dec-1994: V0.2 Set scaling for X/Y graphs (Hammersley)
!    10-Dec-1993: V0.1 Original, based on "F2D_ZSCALE" (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'io.inc'
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
     Include 'gs_constants.inc' ! Graphics system constants
!  Import:
     Logical, Intent(IN) :: mask_data ! .True., if the mask is to be used to
!      mask off pixels
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Integer, Intent(IN) :: xnumdat ! Number of elements defined in X-direction
     Integer, Intent(IN) :: ynumdat ! Number of elements defined in Y-direction
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-axis values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-axis values
     Real, Intent(IN) :: DATA(xmaxdat, ymaxdat) ! The data values
     Logical*1, Intent(IN) :: MASK(xmaxdat, ymaxdat) ! .True., if a pixel is
!      masked-off
     Character(Len = *), Intent(IN) :: title ! Title for plot
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label for plot
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label for plot
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label for plot
     Integer, Intent(IN) :: xstrelm ! X-pixel number for the start of the ROI
     Integer, Intent(IN) :: ystrelm ! Y-pixel number for the start of the ROI
     Integer, Intent(IN) :: xendelm ! The X-pixel number for the end of the ROI
     Integer, Intent(IN) :: yendelm ! The Y-pixel number for the end of the ROI
     Type(EXPERIMENTAL_DETAILS), Intent(IN) :: experiment ! Details of
!      experiment (see "io.inc")
     Logical, Intent(IN) :: lut_click ! .True., if the look-up table was
!      clicked upon to set the scaling
     Real, Intent(IN) :: x_pc ! X-page coordinate of any look-up table clicking
     Real, Intent(IN) :: y_pc ! Y-page coordinate of any look-up table clicking
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.21' ! Version number
     Integer, Parameter :: Max_menu = 12 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable
     Integer :: num_menu ! Number of choices in menu
     Integer :: retstat ! Data base return status variable:
!      0 = Good status
!      1 = No more room (for storage), or no more values
!      2 = Key not found for retrieval, or element doesn't exist
     Integer :: scale_mode ! Mode of scaling (thresholding) used to display 
!      image:
!        0 = Automatic full scaling
!        1 = Linear scaling within set limits (thresholds)
!        2 = Linear scaling with set minimum (threshold), but automatic maximum
!        3 = Linear scaling with set maximum (threshold), but automatic minimum
!        4 = "Diffraction peak" automatic scaling, designed to display weak 
!            diffraction peaks
     Logical :: ask_limits ! .True., if the user is to be asked for
!      the minimum and maximum display limits
     Logical :: continue ! .True., until user wants to exit
     Logical :: logarithmic ! .True., if logarithmic image intensity
!      scaling is being used
     Logical :: local_lut_click ! Local version of "lut_click", which
!      may be changed
     Logical :: lut_drawn ! .True., if a look-up table has been drawn
     Logical :: not_ok ! .True., until scaling input is O.K.
     Logical :: update_image ! .True., if the image needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical :: x_linear ! .True., if the X-axis scale is to be logarithmic
     Logical :: xmaxautoddr ! .True., if the X-maximum of the data
!      display region is to be automatically controlled
     Logical :: xminautoddr ! .True., if the X-minimum of the data
!      display region is to be automatically controlled
     Logical :: y_linear ! .True., if the Y-axis scale is to be logarithmic
     Logical :: ymaxautoddr ! .True., if the Y-maximum of the data
!      display region is to be automatically controlled
     Logical :: yminautoddr ! .True., if the Y-minimum of the data
!      display region is to be automatically controlled
     Real :: height ! Height of window in page coordinates 
     Real :: max_image ! Maximum displayed value in image
     Real :: max_scale ! Maximum scale for log scales
     Real :: maximum ! Maximum page coordinate and corresponding data
!      value for rescaling
     Real :: min_image ! Minimum displayed value in image
     Real :: min_scale ! Minimum scale for log scales
     Real :: minimum ! Minimum page coordinate and corresponding data
!      value for rescaling
     Real :: width ! Width of window in page coordinates 
     Integer :: window_format ! Format of graphics window:
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: x_pc2 ! X-page coordinate of click within LUT bar
     Real :: xmax_lut ! The maximum X-coordinate for displayed look-up table 
!      position
     Real :: xmaxddr ! The maximum X-coordinate of the data display region
     Real :: xmin_lut ! The minimum X-coordinate for displayed look-up table 
!      position
     Real :: xminddr ! The minimum X-coordinate of the data display region
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: y_pc2 ! Y-page coordinate of click within LUT bar
     Real :: ymax_lut ! The maximum Y-coordinate for displayed look-up table 
!      position
     Real :: ymaxddr ! The maximum Y-coordinate of the data display region
     Real :: ymin_lut ! The minimum Y-coordinate for displayed look-up table 
!      position
     Real :: yminddr ! The minimum Y-coordinate of the data display region
!  Local Arrays:
     Character(Len = 15), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command
!      explanation
     Character(Len = 80) :: MESSAGE(2) ! Output text
!  External Functions:
     Real, External :: Lg_pc2wc ! Convert page coordinate to world coordinate
     Real, External :: Lg_wc2pc ! Convert world coordinate to page coordinate
!  Local Data:
     Data (MENU(item), item = 1, 12) / 'EXIT', '?', 'FULLY AUTOMATIC', &
       'WEAK PEAKS', '+ MAXIMUM', '- MAXIMUM', '+ MINIMUM', '- MINIMUM', &
       'USER MIN/MAX', 'USER MINIMUM', 'USER MAXIMUM', 'LOG SCALE' /
     Data (MENUTXT(item), item = 1, 12) / 'EXIT: Exit from this sub-menu', &
       '?: Display list of commands with short description', &
       'FULLY AUTOMATIC: Full range automatic scaling', &
       'WEAK PEAKS: Automatic scaling for weak diffraction peaks', &
       '+ MAXIMUM: Increase maximum display level by 11% of range', &
       '- MAXIMUM: Decrease maximum display level by 10% of range', &
       '+ MINIMUM: Increase minimum display level by 11% of range', &
       '- MINIMUM: Decrease maximum display level by 10% of range', &
       'USER MIN/MAX: User set minimum and maximum limits', &
       'USER MINIMUM: User set minimum, automatic full maximum', &
       'USER MAXIMUM: User set maximum, automatic full minimum', &
       'LOG SCALE: Logarithmic or linear image intensity scaling' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!  Check status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_GUI_ZSCALE ' // Version)
     Else
 
        local_lut_click = lut_click
 
        If (lut_click) Then
 
!        Turn on rubber-banding
           x_coordinate = Lg_pc2wc (.True., x_pc, status)
           y_coordinate = Lg_pc2wc (.False., y_pc, status)
           Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 1, x_coordinate, &
             y_coordinate, status)
 
        End If
 
!     Find out current Z-scaling method
        Call LG_INQ_LOGZSCALING (logarithmic, status)
 
!     Change menu button accordingly
        num_menu = max_menu
        If (logarithmic) Then
           MENU(12) = 'LINEAR SCALE'
           MENUTXT(12) = 'LINEAR SCALE: Linear (or logarithmic) ' // &
             'image intensity scaling'
        Else
           MENU(12) = 'LOG SCALE'
           MENUTXT(12) = 'LOG SCALE: Logarithmic (or ' // &
             'linear) image intensity scaling'
 
        End If
 
!     Inquire current scale mode
        Call GS_INQ_IMAGESCALE (scale_mode, min_image, max_image, status)
 
!     Get current look-up table displayed position
        Call GS_INQ_DLUTPC (lut_drawn, xmin_lut, ymin_lut, xmax_lut, ymax_lut, &
          status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputing menu commands until requested to stop
        continue = .True.
        update_image = .False.
        update_menu = .True.
        Do While (continue)
 
           If (update_image) Then
 
!           Redraw image
              Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              update_menu = .True.
 
!           Inquire possible changes to min/max
              Call GS_INQ_IMAGESCALE (scale_mode, min_image, max_image, &
                status)
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout style
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 19, 15, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 19, 15, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 1, 'INTENSITY SCALING', Max_menu, num_menu, &
                MENU, status)
 
           End If
 
!        By default update
           update_image = .True.
           update_menu = .True.
           ask_limits = .False.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_image = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Convert coordinates to page coordinates
              x_pc2 = Lg_wc2pc (.True., x_coordinate, status)
              y_pc2 = Lg_wc2pc (.False., y_coordinate, status)
 
              If (local_lut_click .And. lut_drawn .And. x_pc2 .Ge. xmin_lut &
                .And. x_pc2 .Le. xmax_lut .And. y_pc2 .Ge. ymin_lut .And. &
                y_pc2 .Le. ymax_lut) Then
 
!              Two clicks within LUT bar
                 minimum = Min(x_pc, x_pc2)
                 maximum = Max(x_pc, x_pc2)
 
!              Convert minimum and maximum page positions to data values
                 If (logarithmic) Then
 
                    min_scale = min_image
                    max_scale = max_image
 
!                 Avoid negatives and zeros, and zero ranges
                    If (max_scale .Gt. 0.0 .And. min_scale .Lt. 0.00001) Then
 
                       If (max_scale .Gt. 10000.0) Then
                          min_scale = 0.1
                       Else If (max_scale .Gt. 1000.0) Then
                          min_scale = 0.01
                       Else If (max_scale .Gt. 100.0) Then
                          min_scale = 0.001
                       Else If (max_scale .Gt. 10.0) Then
                          min_scale = 0.0001
                       Else If (max_scale .Gt. 1.0) Then
                          min_scale = 0.00001
                       Else If (max_scale .Gt. 0.1) Then
                          min_scale = 0.000001
                       Else If (max_scale .Gt. 0.01) Then
                          min_scale = 0.0000001
                       Else If (max_scale .Gt. 0.0) Then
                          min_scale = max_scale / 10000.0
                       End If
 
                    Else If (max_scale .Le. 0.0) Then
                       min_scale = 1.0e-10
                       max_scale = 1.0e-9
                    Else If (min_scale .Eq. max_scale) Then
                       min_scale = min_scale * 0.5
                       max_scale = max_scale * 2.0
                    End If
 
!                 Take logs base 10 of range
                    min_scale = Log10(min_scale)
                    max_scale = Log10(max_scale)
 
                    minimum = min_scale + (max_scale - min_scale) * (minimum - &
                      xmin_lut) / (xmax_lut - xmin_lut)
                    maximum = min_scale + (max_scale - min_scale) * (maximum - &
                      xmin_lut) / (xmax_lut - xmin_lut)
                    minimum = 10**minimum
                    maximum = 10**maximum
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!                 Write (*,
!                 :                   '(''min_image, max_image = '', 2g12.5)')
!                 :                   min_image, max_image
!                 Write (*, '(''minimum, maximum = '', 2g12.5)')
!                 :                   minimum, maximum
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
                 Else
 
                    minimum = min_image + (max_image - min_image) * (minimum - &
                      xmin_lut) / (xmax_lut - xmin_lut)
                    maximum = min_image + (max_image - min_image) * (maximum - &
                      xmin_lut) / (xmax_lut - xmin_lut)
 
                 End If
 
!              Set image scaling
                 scale_mode = 1
                 min_image = minimum
                 max_image = maximum
                 continue = .False.
                 update_image = .True.
                 update_menu = .False.
 
              Else If (local_lut_click) Then
 
!              First click inside LUT and second, outside, set fully
!              automatic scaling
                 scale_mode = 0
                 continue = .False.
                 update_image = .True.
                 update_menu = .False.
 
              Else
 
!              Output the coordinate position and intensity
                 Call F2D_CLICK (1, xmaxdat, ymaxdat, &
                   xstrelm, ystrelm, xendelm, &
                   yendelm, X_AXIS, Y_AXIS, DATA, title, xlabel, ylabel, &
                   zlabel, x_coordinate, y_coordinate, experiment, &
                   update_image, update_menu, status)
 
              End If
 
           Else If (input_type .Eq. Gs_choice) Then
 
!           Carry out menu choices
              If (command .Eq. 'null') Then
                 continue = .True.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_image = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
                 continue = .True.
 
              Else If (command .Eq. 'FULLY AUTOMATIC') Then
 
                 scale_mode = 0
 
              Else If (command .Eq. 'WEAK PEAKS') Then
 
                 scale_mode = 4
 
              Else If (command .Eq. '+ MAXIMUM') Then
 
                 scale_mode = 1
                 max_image = max_image + (max_image - min_image) * 0.11111111
 
              Else If (command .Eq. '- MAXIMUM') Then
 
                 scale_mode = 1
                 max_image = max_image - (max_image - min_image) * 0.1
 
              Else If (command .Eq. '+ MINIMUM') Then
 
                 scale_mode = 1
                 min_image = min_image + (max_image - min_image) * 0.11111111
 
              Else If (command .Eq. '- MINIMUM') Then
 
                 scale_mode = 1
                 min_image = min_image - (max_image - min_image) * 0.1
 
              Else If (command .Eq. 'USER MIN/MAX') Then
 
                 ask_limits = .True.
                 scale_mode = 1
 
              Else If (command .Eq. 'USER MINIMUM') Then
 
                 ask_limits = .True.
                 scale_mode = 2
 
              Else If (command .Eq. 'USER MAXIMUM') Then
 
                 ask_limits = .True.
                 scale_mode = 3
 
              Else If (command .Eq. 'LOG SCALE') Then
 
                 logarithmic = .True.
                 Call LG_SET_LOGZSCALING (.True., status)
                 MENU(12) = 'LINEAR SCALE'
                 MENUTXT(12) = 'LINEAR SCALE: Logarithmic or ' // &
                   'linear image intensity scaling'
 
!              Inquire X/Y graph log/linear requirements
                 Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
 
!              Set X/Y graph log/linear requirements
                 Call GS_SET_DATALOGLIN (x_linear, .False., status)
 
!              Set X/Y graph axes log/linear requirements
!              Call GS_SET_AXESLOGLIN (x_linear, .False.,
!              :               status)
 
              Else If (command .Eq. 'LINEAR SCALE') Then
 
                 logarithmic = .False.
                 Call LG_SET_LOGZSCALING (.False., status)
                 MENU(12) = 'LOG SCALE'
                 MENUTXT(12) = 'LOG SCALE: Logarithmic or ' // &
                   'linear image intensity scaling'
 
!              Inquire X/Y graph log/linear requirements
                 Call GS_INQ_DATALOGLIN (x_linear, y_linear, status)
 
!              Set X/Y graph data log/linear requirements
                 Call GS_SET_DATALOGLIN (x_linear, .True., status)
 
!              Set X/Y graph axes log/linear requirements
!              Call GS_SET_AXESLOGLIN (x_linear, .True., status)
 
              End If
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
           If (ask_limits) Then
 
              not_ok = .True.
              Do While (not_ok)
 
!              Input minimum/maximum limits as required by scaling mode
                 If (scale_mode .Eq. 1 .Or. scale_mode .Eq. 2) Then
 
!                 Set minimum displayed value
                    Call GS_INPR (.False., 0.0, 0.0, .True., &
                      'MINIMUM DISPLAY VALUE', 1, &
                      'Enter lowest data value in display range', 1, &
                      'Enter valid real', min_image, status)
 
                 End If
 
                 If (scale_mode .Eq. 1 .Or. scale_mode .Eq. 3) Then
 
!                 Set maximum displayed value
                    Call GS_INPR (.False., 0.0, 0.0, .True., &
                      'MAXIMUM DISPLAY VALUE', 1, &
                      'Enter lowest data value in display range', 1, &
                      'Enter valid real', max_image, status)
 
                 End If
 
!              Check that the minimum of the display range is less than
!              the maximum
                 If (scale_mode .Eq. 1 .And. min_image .Ge. max_image) Then
 
                    MESSAGE(1) = 'WARNING: The minimum of the ' // &
                      'range has been set greater than or equal to the'
                    MESSAGE(2) = '         maximum of the ' // &
                      'display range. You must reset the values.'
                    Call GS_MESSAGE (2, 2, MESSAGE, status)
 
                 Else
                    not_ok = .False.
                 End If
 
              End Do
 
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Set required scale mode
           Call GS_SET_IMAGESCALE (scale_mode, min_image, max_image, status)
 
!        Inquire current graphics display settings
           Call GS_INQ_AUTODDR (xminautoddr, yminautoddr, xmaxautoddr, &
             ymaxautoddr, status)
           Call GS_INQ_DDR (xminddr, yminddr, xmaxddr, ymaxddr, status)
 
!        Set X/Y graph Y-axis (Z) range
           If (scale_mode .Eq. 0) Then
              Call GS_SET_AUTODDR (.True., .True., .True., .True., status)
           Else If (scale_mode .Eq. 1) Then
              Call GS_SET_AUTODDR (.True., .False., .True., .False., status)
              Call GS_SET_DDR (xminddr, min_image, xmaxddr, max_image, status)
           Else If (scale_mode .Eq. 2) Then
              Call GS_SET_AUTODDR (.True., .False., .True., .True., status)
              Call GS_SET_DDR (xminddr, min_image, xmaxddr, min_image + 1.0, &
                status)
           Else If (scale_mode .Eq. 3) Then
              Call GS_SET_AUTODDR (.True., .True., .True., .False., status)
              Call GS_SET_DDR (xminddr, max_image-1.0, xmaxddr, max_image, &
                status)
           End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!        Check status
           If (status .Eq. St_escapevalue) Then
 
!           Reset status system
              Call ST_DEF_SYSTEM (status)
 
           Else If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
           If (.Not. (input_type .Eq. Gs_resize .Or. (input_type .Eq. &
             Gs_choice .And. command .Eq. '?'))) Then
 
!           Turn off rubber-banding
              Call LG_SET_RUBBERBAND (Gs_wkid_terminal, 0, x_coordinate, &
                y_coordinate, status)
 
              local_lut_click = .False.
 
           End If
 
        End Do
 
!     Update image if required
        If (update_image) Then
 
!        Redraw image
           Call GS_MPLOT (mask_data, xmaxdat, ymaxdat, DATA, MASK, X_AXIS, &
             Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
             ylabel, zlabel, status)
 
        End If
 
!     Store Z-scaling requirements in internal data-base
        Call IO_SET_IKEYVALUE ('Z_SCALING_MODE', scale_mode, retstat, status)
        Call IO_SET_RKEYVALUE ('Z_SCALING_MINIMUM', min_image, retstat, &
          status)
        Call IO_SET_RKEYVALUE ('Z_SCALING_MAXIMUM', max_image, retstat, &
          status)
        Call IO_SET_LKEYVALUE ('Z_SCALING_LOG', logarithmic, retstat, status)
 
     End If
 
     End Subroutine F2D_GUI_ZSCALE
!********1*********2*********3*********4*********5*********6*********7*********8
 

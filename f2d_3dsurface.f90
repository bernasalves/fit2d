!********1*********2*********3*********4*********5*********6*********7*********8
 
!  *********************
!  *                   *
!  * f2d_3dsurface.f90 *
!  *                   *
!  *********************
 
!+ F2D_3DSURFACE - FIT 2-D 3-D SURFACE display (interactive)
     Subroutine F2D_3DSURFACE (print_type, xmaxdat, ymaxdat, DATA_VALUES, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, status)
!  Description:
!    1. Outputs ROI as pixel 3dsurface
!    2. The user can change the 3dsurface with an interactive graphical
!    menu
!  Keywords:
!    3-d~surface~Display, Display~3-d~surface
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Mar-2006: V0.11 Use "LUT_STYLE" structure (Hammersley)
!    17-Mar-2006: V0.10 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.9 Alter menu lay-out for landscape windows (Hammersley)
!    13-Feb-1998: V0.8 Separate style control options into separate
!      "STYLE" menu (Hammersley)
!    12-Feb-1998: V0.7 Add a 360 degree turn option (Hammersley)
!    22-Jan-1998: V0.6 Changes to the argument list of "F2D_PRINT" (Hammersley)
!    20-Jun-1996: V0.5 Add "AXES/NO AXES" commands (Hammersley)
!    22-Nov-1995: V0.4 Add "NO FILL"/"FILL" commands (Hammersley)
!    16-Nov-1995: V0.3 Add style control (Hammersley)
!    15-Nov-1995: V0.2 Automatic default value Z-scaling added (Hammersley)
!    05-Jul-1995: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'gs.inc' ! Graphics system definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Character(Len = *), Intent(INOUT) :: print_type ! Type, of graphics to
!      print. Supported types are:
!        "3-d surface"
!        "banner"
!        "image"
!        "contour"
!        "x/y graph"
     Integer, Intent(IN) :: xmaxdat ! Dimension in X-direction for data arrays
     Integer, Intent(IN) :: ymaxdat ! Dimension in Y-direction for data arrays
     Real, Intent(IN) :: X_AXIS(xmaxdat) ! X-coordinates of grid values
     Real, Intent(IN) :: Y_AXIS(ymaxdat) ! Y-coordinates of grid values
     Real, Intent(IN) :: DATA_VALUES(xmaxdat, ymaxdat)
!      2-D grid of data values to be "contoured"
     Integer, Intent(IN) :: xstrelm ! First element to output in X-direction
     Integer, Intent(IN) :: ystrelm ! First element to output in Y-direction
     Integer, Intent(IN) :: xendelm ! Last element to output in X-direction
     Integer, Intent(IN) :: yendelm ! Last element to output in Y-direction
     Character(Len = *), Intent(IN) :: title ! Title for diagram
     Character(Len = *), Intent(IN) :: xlabel ! X-axis label
     Character(Len = *), Intent(IN) :: ylabel ! Y-axis label
     Character(Len = *), Intent(IN) :: zlabel ! Z-axis label
!  Import/Export:
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.11' ! Version number
     Integer, Parameter :: Max_menu = 21 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: fillcolour ! Colour number to use to fill pixels values if 
!      "fill" is .True., but "zcolour" is .False.
     Integer :: i ! Loop variable
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable for data statements
     Integer :: linecolour ! Colour number for line
     Integer :: max_3d_pixels ! Maximum number of pixels which may
!      be displayed in either direction for a 3-D surface plot (unused)
     Integer :: max_image_pixels ! Maximum number of pixels which may
!      be displayed in either direction for an image
     Integer :: max_index ! Maximum lut index for output display
     Integer :: min_index ! Minimum lut index for output display
     Integer, Save :: num_index_3d = 30 ! Number of LUT colour indices to
!      use for 3-D display
     Integer :: num_menu ! Number of choices in menu
     Integer :: num_rebin ! The number of input pixels to rebin, in
!      each direction, into an output pixel
     Integer :: old_num_levels ! Number of levels in colour table at start
     Integer :: scale_mode ! Mode of scaling (thresholding) used to display 
!      image:
!        0 = Automatic full scaling
!        1 = Linear scaling within set limits (thresholds)
!        2 = Linear scaling with set minimum (threshold), but automatic maximum
!        3 = Linear scaling with set maximum (threshold), but automatic minimum
!        4 = "Diffraction peak" automatic scaling, designed to
!            display weak diffraction peaks
     Logical :: axes ! .True., if axes are to be drawn around the 3-d surface
     Logical :: continue ! .True., until user wants to exit
     Logical :: enumeration ! .True., if enumeration is to be drawn
!      around the 3-d surface
     Logical :: fill ! .True., if the pixels are to be
!      filled-in (this is equivalent to hidden line removal)
     Logical :: frame ! .True., if a frame is to be drawn around the 3-d surface
     Logical :: line ! .True., if a line is to be drawn
!      around the edges of the re-binned pixels
     Logical :: logarithmic ! .True., if logarithmic image intensity
!      scaling is being used
     Logical, Save :: normal_updating = .True. ! .True., if normal
!      resolution and number of colour updating is required
     Logical :: update_3dsurface ! .True., if the 3dsurface needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical :: zcolour ! .True., if a colour table is to be used to represent 
!      the intensity of the drawn pixels
     Real :: current_scaling ! Current scaling multiplication factor to
!      intensity values
     Real :: dummy ! Dummy variable
     Real, Save :: latitude = 1.047197 ! Viewing latitude
     Real :: linewidth ! Line width scale factor
     Real, Save :: longitude = 0.5236 ! Viewing longitude
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: window_size ! Size of half of the data window
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: x_translation ! X-dimension translation of 3-D coordinates
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: y_translation ! Y-dimension translation of 3-D coordinates
     Real :: z_max ! Maximum displayed Z-value
     Real :: z_min ! Minimum displayed Z-value
     Real :: z_scaling ! Scale factor to apply to Z-dimension
     Real :: z_translation ! Z-dimension translation of 3-D coordinates
!  Local Arrays:
     Character(Len = 10), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE_LUT ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
!    Data longitude / 4.712389 /
     Data (MENU(item), item = 1, 7) / 'EXIT', '+ROT.', '+ELEV.', '+ZOOM', &
       'LEFT', 'UP', 'STEEPER' /
     Data (MENU(item), item = 8, 14) / '?', '-ROT.', '-ELEV.', '-ZOOM', &
       'RIGHT', 'DOWN', 'FLATTER' /
     Data (MENU(item), item = 15, 21) / 'PRINT', 'ANGLE', 'DEFAULT', 'LOG', &
       'FAST', 'STYLE', '360' /
     Data (MENUTXT(item), item = 1, 7) / &
       'EXIT: Exit 3-D surface viewing graphical menu', &
       '+ROT.: Increase longitude angle of viewer', &
       '+ELEV.: Increase elevation angle of viewer', &
       '+ZOOM: Increase size of object', &
       'LEFT: View further to the left of the object', &
       'UP: View further above the object', &
       'STEEPER: Increase importance of Z-dimension' /
     Data (MENUTXT(item), item = 8, 14) / &
       '?: View this help text (explanation of options)', &
       '-ROT.: Decrease longitude angle of viewer', &
       '-ELEV.: Decrease elevation angle of viewer', &
       '-ZOOM: Decrease size of object', &
       'RIGHT: View further to the right of the object', &
       'DOWN: View further below the object', &
       'FLATTER: Decrease importance of Z-dimension' /
     Data (MENUTXT(item), item = 15, 21) / &
       'PRINT: Output displayed 3-D surface to graphics file', &
       'ANGLE: Keyboard entry of arbitrary viewing angle', &
       'DEFAULT: Re-set default view', &
       'LOG: Logarithmic (or linear) image intensity scaling', &
       'FAST: Fast mode updating, low resolution, few colours', &
       'STYLE: Control of 3-D surface display diagram style', &
       '360: 360 degree rotation in longitude, in 10 degree steps' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!     Write (*, '(''Entered F2D_3DSURFACE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_3DSURFACE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_3DSURFACE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire current 3-D surface style
        Call GS_INQ_3DSTYLE (line, linecolour, linewidth, fill, zcolour, &
          fillcolour, status)
 
!     Find out current Z-scaling method
        Call LG_INQ_LOGZSCALING (logarithmic, status)
 
!     Change menu button accordingly
        If (logarithmic) Then
           MENU(18) = 'LINEAR'
           MENUTXT(18) = 'LINEAR: Linear (or logarithmic) ' // &
             'image intensity scaling'
        Else
           MENU(18) = 'LOG'
           MENUTXT(18) = 'LOG: Logarithmic (or ' // &
             'linear) image intensity scaling'
 
        End If
 
!     Button for updating mode
        If (normal_updating) Then
           MENU(19) = 'FAST'
           MENUTXT(19) = 'FAST: Fast mode updating, ' // &
             'low resolution, few colours'
        Else
           MENU(19) = 'NORMAL'
           MENUTXT(19) = 'NORMAL: Normal updating, ' // &
             'resolution, and number of colours'
        End If
 
!     Find out current frame/axes output style
        Call GS_INQ_3DAXES (frame, axes, enumeration, status)
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Save colour table set-up
        Call LG_INQ_IMAGERANGE (min_index, max_index, status)
 
!     Set new reduced range of colours
        Call LG_SET_IMAGERANGE (min_index, Min(min_index + num_index_3d - 1, &
          max_index), status)
        
!     Set maximum number of levels in colour table, but remember old value
        Call GS_INQ_LUTSTYLE (STYLE_LUT, status)
        old_num_levels = STYLE_LUT%num_levels
        STYLE_LUT%num_levels = Min(STYLE_LUT%num_levels, num_index_3d)
        Call GS_SET_LUTSTYLE (STYLE_LUT, status)
 
!     Set colour table on "terminal" device
        Call GS_COLOURS (status)
 
!     Set data-window
        x_translation = -(X_AXIS(xstrelm) + X_AXIS(xendelm)) / 2.0
        y_translation = -(Y_AXIS(ystrelm) + Y_AXIS(yendelm)) / 2.0
        z_translation = 0.0
        window_size = Max((X_AXIS(xendelm) - X_AXIS(xstrelm)) / 2.0, &
          (Y_AXIS(yendelm) - Y_AXIS(ystrelm)) / 2.0 )
 
!     Find current Z-scaling requirements
        Call GS_INQ_IMAGESCALE (scale_mode, z_min, z_max, status)
 
!     Find out maximum number of pixels which may be displayed in
!     one direction
        Call GS_INQ_PIXELLIMITS (max_image_pixels, max_3d_pixels, status)
 
!     Calculate necessary rebinning factor to reduce display region
!     to within maximum number of displayed pixels
        num_rebin = ((Max(xendelm - xstrelm + 1, yendelm - ystrelm + 1) - 1) / &
          max_3d_pixels) + 1
 
!     Find minimum and maximum of scaling range
        Call GS_CAL_ZRANGE (xmaxdat, ymaxdat, DATA_VALUES, xstrelm, ystrelm, &
          xendelm, yendelm, num_rebin, scale_mode, z_min, z_max, status)
 
!     Set appropriate default Z-scaling
        If (logarithmic) Then
           z_scaling = 0.5 * (window_size * 2.0) / (Log10(Max(z_max, 1.0)) - &
             Log10(Max(z_min, 0.1)))
        Else
           z_scaling = 0.5 * (window_size * 2.0) / (z_max - z_min)
        End If
 
!     Set menu layout style
        num_menu = Max_menu
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
 
        continue = .True.
        update_3dsurface = .True.
        update_menu = .True.
        Do While (continue)
 
!        Draw graphics output if required
           If (update_3dsurface) Then
 
!           Set new 3-D view
              Call GS_SET_3DVIEW (x_translation, y_translation, z_translation, &
                longitude, latitude, window_size * 30.0, -window_size, &
                -window_size, window_size, window_size, z_scaling, status)
 
!           Re-draw image
              Call GS_3DSURFACE (xmaxdat, ymaxdat, DATA_VALUES, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              print_type = '3-d surface'
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 30, 7, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 7, 30, 7, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 0, 'INTERACTIVE 3-D VIEWER', Max_menu, &
                num_menu, MENU, status)
 
           End If
 
!        By default update graphics
           update_3dsurface = .True.
           update_menu = .True.
 
!        Get user to select between the available menu options
           command = 'null'
           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 2, input_type, &
             command, x_coordinate, y_coordinate, status)
 
           If (input_type .Eq. Gs_resize) Then
 
              update_3dsurface = .True.
              update_menu = .True.
 
           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Eventually the coordinate position can be output
              Continue
              update_3dsurface = .False.
              update_menu = .False.
 
           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_3dsurface = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_3dsurface = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 Call F2D_PRINT (.True., print_type, .False., xmaxdat, &
                   ymaxdat, X_AXIS, Y_AXIS, DATA_VALUES, dummy, dummy, title, &
                   xlabel, ylabel, zlabel, .False., xstrelm, ystrelm, xendelm, &
                   yendelm, status)
                 update_3dsurface = .False.
                 update_menu = .True.
 
              Else If (command .Eq. '360') Then
 
                 Do i = 1, 36
                    longitude = longitude + 10.0 * Pi / 180.0
                    If (longitude .Ge. 2.0 * Pi) Then
                       longitude = longitude - 2.0 * Pi
                    End If
 
!                 Set new 3-D view
                    Call GS_SET_3DVIEW ( x_translation, y_translation, &
                      z_translation, longitude, latitude, window_size*30.0, &
                      -window_size, -window_size, window_size, window_size, &
                      z_scaling, status)
 
!                 Re-draw image
                    Call GS_3DSURFACE (xmaxdat, ymaxdat, DATA_VALUES, X_AXIS, &
                      Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, &
                      xlabel, ylabel, zlabel, status)
                    print_type = '3-d surface'
                    update_menu = .True.
 
                 End Do
 
              Else If (command .Eq. 'DEFAULT') Then
 
!              Default view position
                 x_translation = -(X_AXIS(xstrelm) + X_AXIS(xendelm)) / 2.0
                 y_translation = -(Y_AXIS(ystrelm) + Y_AXIS(yendelm)) / 2.0
                 z_translation = 0.0
                 window_size = Max((X_AXIS(xendelm) - X_AXIS(xstrelm)) / 2.0, &
                   (Y_AXIS(yendelm) - Y_AXIS(ystrelm)) / 2.0 )
                 z_scaling = 0.5 * (window_size * 2.0) / (z_max - z_min)
 
                 longitude = 30.0 * Pi / 180.0
                 latitude = 60.0 * Pi /180.0
 
              Else If (command .Eq. '+ROT.') Then
 
                 longitude = longitude + 10.0 * Pi / 180.0
                 If (longitude .Ge. 2.0 * Pi) Then
                    longitude = longitude - 2.0 * Pi
                 End If
 
              Else If (command .Eq. '-ROT.') Then
 
                 longitude = longitude - 10.0 * Pi / 180.0
                 If (longitude .Lt. 0.0) Then
                    longitude = longitude + 2.0 * Pi
                 End If
 
              Else If (command .Eq. '+ELEV.') Then
 
                 latitude = latitude + 10.0 * Pi / 180.0
                 If (latitude .Gt. Pi/2.0) Then
                    latitude = Pi / 2.0
                 End If
 
              Else If (command .Eq. '-ELEV.') Then
 
                 latitude = latitude - 10.0 * Pi /180.0
                 If (latitude .Lt. -Pi/2.0) Then
                    latitude = -Pi/2.0
                 End If
 
              Else If (command .Eq. '+ZOOM') Then
 
                 window_size = window_size / 1.2
 
              Else If (command .Eq. '-ZOOM') Then
 
                 window_size = window_size * 1.2
 
              Else If (command .Eq. 'LEFT') Then
 
                 x_translation = x_translation + window_size * 0.2
 
              Else If (command .Eq. 'RIGHT') Then
 
                 x_translation = x_translation - window_size * 0.2
 
              Else If (command .Eq. 'UP') Then
 
                 z_translation = z_translation - window_size * 0.2
 
              Else If (command .Eq. 'DOWN') Then
 
                 z_translation = z_translation + window_size * 0.2
 
              Else If (command .Eq. 'STEEPER') Then
 
                 z_scaling = z_scaling * 1.414214
 
              Else If (command .Eq. 'FLATTER') Then
 
                 z_scaling = z_scaling / 1.414214
 
              Else If (command .Eq. 'ANGLE') Then
 
!              Graphical input of rotation angle
                 longitude = longitude * 180.0 / Pi
                 Call GS_INPR (.True., -360.0, 360.0, .True., &
                   'VIEW ROTATION ANGLE', 1, &
                   'Enter view rotation angle, around image', 1, &
                   'Enter valid real', longitude, status)
                 longitude = longitude * Pi / 180.0
 
!              Graphical input of elevation angle
                 latitude = latitude * 180.0 / Pi
                 Call GS_INPR (.True., -90.0, 90.0, .True., &
                   'VIEW ELEVATION ANGLE', 1, 'Enter view elevation angle', 1, &
                   'Enter valid real', latitude, status)
                 latitude = latitude * Pi / 180.0
 
              Else If (command .Eq. 'LOG') Then
 
!              Set Z-scaling method to logarithmic
                 Call LG_SET_LOGZSCALING (.True., status)
                 MENU(18) = 'LINEAR'
                 MENUTXT(18) = 'LINEAR: Linear (or logarithmic) ' // &
                   'intensity scaling'
                 current_scaling = z_scaling / (0.5 * (window_size * 2.0) / &
                   (z_max - z_min))
                 z_scaling = current_scaling * 0.5 * (window_size * 2.0) / &
                   (Log10(Max(z_max, 1.0)) - Log10(Max(z_min,0.1)))
 
              Else If (command .Eq. 'LINEAR') Then
 
!              Set Z-scaling method to logarithmic
                 Call LG_SET_LOGZSCALING (.False., status)
                 MENU(18) = 'LOG'
                 MENUTXT(18) = 'LOG: Logarithmic (or ' // &
                   'linear) intensity scaling'
                 current_scaling = z_scaling / (0.5 * (window_size * 2.0) / &
                   (Log10(Max(z_max, 1.0)) - Log10(Max(z_min,0.1))))
                 z_scaling = current_scaling * 0.5 * (window_size * 2.0) / &
                   (z_max - z_min)
 
              Else If (command .Eq. 'FAST') Then
 
!              Fast mode updating
                 normal_updating = .False.
                 Call GS_SET_PIXELLIMITS (max_image_pixels, 50, status)
 
!              Set new reduced range of colours
                 Call LG_SET_IMAGERANGE (min_index, Min(min_index + 10 - 1, &
                   max_index), status)
                 STYLE_LUT%num_levels = Min(STYLE_LUT%num_levels, 10)
                 Call GS_SET_LUTSTYLE (STYLE_LUT, status)

!              Set colour table on "terminal" device
                 Call GS_COLOURS (status)
 
                 MENU(19) = 'NORMAL'
                 MENUTXT(19) = 'NORMAL: Normal updating, ' // &
                   'resolution, and number of colours'
 
              Else If (command .Eq. 'NORMAL') Then
 
!              Normal mode updating
                 normal_updating = .True.
                 Call GS_SET_PIXELLIMITS (max_image_pixels, max_3d_pixels, &
                   status)
 
!              Set new reduced range of colours
                 Call LG_SET_IMAGERANGE (min_index, &
                   Min(min_index+num_index_3d-1, max_index), status)
                 STYLE_LUT%num_levels = Min(STYLE_LUT%num_levels, num_index_3d)
                 Call GS_SET_LUTSTYLE (STYLE_LUT, status)
 
!              Set colour table on "terminal" device
                 Call GS_COLOURS (status)
 
!              Reset colour translation table
                 Call GS_CAL_COLOURS (status)
 
                 MENU(19) = 'FAST'
                 MENUTXT(19) = 'FAST: Fast mode updating, ' // &
                   'low resolution, few colours'
 
              Else If (command .Eq. 'STYLE') Then
 
!              Interactive menu for controlling display style
                 Call F2D_3DSTYLE (normal_updating, xmaxdat, ymaxdat, &
                   DATA_VALUES, X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, &
                   yendelm, title, xlabel, ylabel, zlabel, status)
                 update_3dsurface = .False.
                 update_menu = .True.
 
              End If
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Before updating of graphics'')')
!        Write (*, '(''update_3dsurface = '', l1)') update_3dsurface
!        Write (*, '(''update_menu = '', l1)') update_menu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Reset colour table set-up
        Call LG_SET_IMAGERANGE (min_index, max_index, status)
        STYLE_LUT%num_levels = old_num_levels
        Call GS_SET_LUTSTYLE (STYLE_LUT, status)
 
!     Reset colour table
        Call GS_COLOURS (status)
 
!     Reset colour translation table
        Call GS_CAL_COLOURS (status)
 
     End If
 
     End Subroutine F2D_3DSURFACE
!********1*********2*********3*********4*********5*********6*********7*********8

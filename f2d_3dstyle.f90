!********1*********2*********3*********4*********5*********6*********7*********8

!  *******************
!  *                 *
!  * f2d_3dstyle.f90 *
!  *                 *
!  *******************
 
!+ F2D_3DSTYLE - FIT 2-D 3-D surface STYLE
     Subroutine F2D_3DSTYLE (normal_updating, xmaxdat, ymaxdat, DATA_VALUES, &
       X_AXIS, Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
       ylabel, zlabel, status)
!  Description:
!  Keywords:
!    3-d~surface~Display.Style, Style.Display~3-d~surface
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.eu)
!  History:
!    23-Mar-2006: V0.5 Use "LUT_STYLE" structure (Hammersley)
!    17-Mar-2006: V0.4 Support for arbitrary aspect ratio windows (Hammersley)
!    25-Feb-2004: V0.3 Alter menu lay-out for landscape windows (Hammersley)
!    03-Jun-2003: V0.2 Tidy-up code (Hammersley)
!    13-Feb-1998: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'gs.inc' ! Graphics system definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic 'status' constants
!  Import:
     Logical, Intent(IN) :: normal_updating ! .True., if normal resolution
!      and number of colour updating is required
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
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.5' ! Version number
     Integer, Parameter :: Max_menu = 9 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: fillcolour ! Colour number to use to fill pixels values if 
!      "fill" is .True., but "zcolour" is .False.
!    Integer i ! Loop variable
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable for data statements
     Integer :: linecolour ! Colour number for line
     Integer :: lut_choice ! Number corresponding to LUT choice
     Integer :: max_3d_pixels ! Maximum number of pixels which may
!      be displayed in either direction for a 3-D surface plot (unused)
     Integer :: max_image_pixels ! Maximum number of pixels which may
!      be displayed in either direction for an image
     Integer :: max_index ! Maximum lut index for output display
     Integer :: min_index ! Minimum lut index for output display
     Integer :: num_index_3d ! Number of LUT colour indices for 3-D display
     Integer :: num_levels ! Number of levels to display
     Integer :: num_menu ! Number of choices in menu
!    Integer num_rebin ! The number of input pixels to rebin, in
!    each direction, into an output pixel
!    Integer scale_mode ! Mode of scaling (thresholding) used to
!    display image:
!    0 = Automatic full scaling
!    1 = Linear scaling within set limits (thresholds)
!    2 = Linear scaling with set minimum (threshold), but
!    automatic maximum
!    3 = Linear scaling with set maximum (threshold), but
!    automatic minimum
!    4 = "Diffraction peak" automatic scaling, designed to
!    display weak diffraction peaks
     Logical :: axes ! .True., if axes are to be drawn around the 3-d surface
     Logical :: continue ! .True., until user wants to exit
     Logical :: enumeration ! .True., if enumeration is to be drawn
!      around the 3-d surface
     Logical :: fill ! .True., if the pixels are to be
!      filled-in (this is equivalent to hidden line removal)
     Logical :: frame ! .True., if a frame is to be drawn around the 3-d surface
     Logical :: line ! .True., if a line is to be drawn around the edges of the 
!      re-binned pixels
     Logical :: logarithmic ! .True., if logarithmic image intensity
!      scaling is being used
     Logical :: lower_image ! .True., if an image is to be displayed
!      below the 3-D surface
     Logical :: update_3dsurface ! .True., if the 3dsurface needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Logical, Save :: upper_image = .True. ! .True., if an image is to be
!      displayed above the 3-D surface
     Logical :: zcolour ! .True., if a colour table is to be
!      used to represent the intensity of the drawn pixels
!    Real current_scaling ! Current scaling multiplication factor to
!      intensity values
!    Real dummy ! Dummy variable
!    Real latitude ! Viewing latitude
     Real :: linewidth ! Line width scale factor
!    Real longitude ! Viewing longitude
!    Real window_size ! Size of half of the data window
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Real :: width ! Width of window in page coordinates 
     Real :: x_coordinate ! Graphical input X-coordinate
!    Real x_translation ! X-dimension translation of 3-D coordinates
     Real :: y_coordinate ! Graphical input Y-coordinate
!    Real y_translation ! Y-dimension translation of 3-D coordinates
!    Real z_translation ! Z-dimension translation of 3-D coordinates
!  Local Arrays:
     Character(Len = 10), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE_LUT ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 9) / 'EXIT', '?', 'HELP', 'LIMITS', 'LINES', &
       'FILL', 'AXES', 'TOP IMAGE', 'LOW IMAGE' /
     Data (MENUTXT(item), item = 1, 9) / &
       'EXIT: Exit 3-D surface viewing graphical menu', &
       '?: View this help text (explanation of options)', 'HELP: Help text', &
       'LIMITS: Set output pixel resolution limit and number of colours', &
       'LINES: Draw lines around the re-binned pixels', &
       'FILL: Fill areas of surface with colour or Z-colours', &
       'AXES: Draw enumerated axis around the surface', &
       'TOP IMAGE: Add 3-D projected image above 3-D surface', &
       'LOW IMAGE: Add 3-D projected image below 3-D surface' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_3DSTYLE'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_3DSTYLE ' // Version)
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
        Call ST_SAVE ('Subroutine F2D_3DSTYLE ' // Version)
     Else
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead.
 
!     Inquire current 3-D surface style
        Call GS_INQ_3DSTYLE (line, linecolour, linewidth, fill, zcolour, &
          fillcolour, status)
 
!     Change default buttons, depending on style
        If (line) Then
           MENU(5) = 'NO LINES'
           MENUTXT(5) = 'NO LINES: No lines drawn around the re-binned pixels'
 
        Else
           MENU(5) = 'LINES'
           MENUTXT(5) = 'LINES: Draw lines around the re-binned pixels'
        End If
 
        If (fill) Then
           MENU(6) = 'NO FILL'
           MENUTXT(6) = 'NO FILL: No area filling of coloured polygons'
        Else
           MENU(6) = 'FILL'
           MENUTXT(6) = 'FILL: Fill areas of surface with colour or Z-colours'
        End If
 
!     Find out current Z-scaling method
        Call LG_INQ_LOGZSCALING (logarithmic, status)
 
!     Find out current frame/axes output style
        Call GS_INQ_3DAXES (frame, axes, enumeration, status)
 
        If (axes) Then
           MENU(7) = 'NO AXES'
           MENUTXT(7) = 'NO AXES: Don''t draw enumerated axes around the ' // &
             'surface'
        Else
           MENU(7) = 'AXES'
           MENUTXT(7) = 'AXES: Draw enumerated axis around the surface'
        End If
 
        Call GS_INQ_3DSIMAGE (lower_image, upper_image, status)
 
        If (upper_image) Then
           MENU(8) = 'NO TOP'
           MENUTXT(8) = 'NO TOP: Don''t display upper 3-D projected image'
        Else
           MENU(8) = 'TOP IMAGE'
           MENUTXT(8) = 'TOP IMAGE: Add 3-D projected image above 3-D surface'
        End If
 
        If (lower_image) Then
           MENU(9) = 'NO LOWER'
           MENUTXT(9) = 'NO LOWER: Don''t display lower 3-D projected image'
        Else
           MENU(9) = 'LOW IMAGE'
           MENUTXT(9) = 'LOW IMAGE: Add 3-D projected image below 3-D surface'
        End If
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Inquire colour table set-up
        Call LG_INQ_IMAGERANGE (min_index, max_index, status)
        Call GS_INQ_LUTSTYLE (STYLE_LUT, status)
 
!     Find out maximum number of pixels which may be displayed in
!     one direction
        Call GS_INQ_PIXELLIMITS (max_image_pixels, max_3d_pixels, status)
 
!     Set menu layout style
        num_menu = Max_menu
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Loop inputting menu commands until requested to stop
 
        continue = .True.
        update_3dsurface = .False.
        update_menu = .True.
        Do While (continue)
 
!        Draw graphics output if required
           If (update_3dsurface) Then
 
!           Re-draw image
              Call GS_3DSURFACE (xmaxdat, ymaxdat, DATA_VALUES, X_AXIS, &
                Y_AXIS, xstrelm, ystrelm, xendelm, yendelm, title, xlabel, &
                ylabel, zlabel, status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (gs_vertical, 1, 19, 7, status)
              Else
                 Call GS_SET_MENULAYOUT (gs_horizontal, 3, 19, 7, status)
              End If
 
!           Re-draw menu
              Call GS_FMENU (1, 1, '3-D SURFACE STYLE', Max_menu, num_menu, &
                MENU, status)
 
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
 
              Else If (command .Eq. 'HELP') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'LIMITS') Then
 
                 Call GS_INPI (.True., 10, 1000, .True., &
                   'MAXIMUM OUTPUT DIMENSION', 1, &
                   'Enter maximum number of pixels', 1, 'Enter valid integer', &
                   max_3d_pixels, status)
 
                 Call GS_INPI (.True., 1, max_index - min_index + 1, .True., &
                   'COLOUR LEVELS', 1, 'Enter number of colour levels', 1, &
                   'Enter valid integer', num_index_3d, status)
 
                 If (normal_updating) Then
 
                    Call GS_SET_PIXELLIMITS (max_image_pixels, max_3d_pixels, &
                      status)
 
!                 Set new reduced range of colours
                    Call LG_SET_IMAGERANGE (min_index, Min(min_index + &
                      num_index_3d - 1, max_index), status)
                    STYLE_LUT%num_levels = Min(STYLE_LUT%num_levels, &
                      num_index_3d)
                    Call GS_SET_LUTSTYLE (STYLE_LUT, status)
 
!                 Set colour table on "terminal" device
                    Call GS_COLOURS (status)
 
                 End If
 
              Else If (command .Eq. 'LINES') Then
 
!              Set lines to be output
                 line = .True.
                 Call GS_SET_3DSTYLE (line, linecolour, linewidth, fill, &
                   zcolour, fillcolour, status)
                 MENU(5) = 'NO LINES'
                 MENUTXT(5) = 'NO LINES: No lines drawn ' // &
                   'around the re-binned pixels'
 
              Else If (command .Eq. 'NO LINES') Then
 
!              Set no lines to be output
                 line = .False.
                 Call GS_SET_3DSTYLE (line, linecolour, linewidth, fill, &
                   zcolour, fillcolour, status)
                 MENU(5) = 'LINES'
                 MENUTXT(5) = 'LINES: Draw lines around the re-binned pixels'
 
              Else If (command .Eq. 'FILL') Then
 
!              Set fill
                 fill = .True.
                 Call GS_SET_3DSTYLE (line, linecolour, linewidth, fill, &
                   zcolour, fillcolour, status)
                 MENU(6) = 'NO FILL'
                 MENUTXT(6) = 'NO FILL: No area filling of coloured polygons'
 
              Else If (command .Eq. 'NO FILL') Then
 
!              Set no fill for output
                 fill = .False.
                 Call GS_SET_3DSTYLE (line, linecolour, linewidth, fill, &
                   zcolour, fillcolour, status)
                 MENU(6) = 'FILL'
                 MENUTXT(6) = 'FILL: Fill areas of surface ' // &
                   'with colour or Z-colours'
 
              Else If (command .Eq. 'AXES') Then
 
!              Set axes to be output
                 Call GS_SET_3DAXES (.True., .True., .True., status)
                 MENU(7) = 'NO AXES'
                 MENUTXT(7) = 'NO AXES: Don''t draw ' // &
                   'enumerated axes around the surface'
 
              Else If (command .Eq. 'NO AXES') Then
 
!              Set no axes for output
                 Call GS_SET_3DAXES (.False., .False., .False., status)
                 MENU(7) = 'AXES'
                 MENUTXT(7) = 'AXES: Draw enumerated axis around the surface'
 
              Else If (command .Eq. 'TOP IMAGE') Then
 
                 MENU(8) = 'NO TOP'
                 MENUTXT(8) = 'NO TOP: Don''t ' // &
                   'display lower 3-D projected image'
                 upper_image = .True.
                 Call GS_SET_3DSIMAGE (lower_image, upper_image, status)
 
              Else If (command .Eq. 'NO TOP') Then
 
                 MENU(8) = 'TOP IMAGE'
                 MENUTXT(8) = 'TOP IMAGE: Add 3-D projected ' // &
                   'image above 3-D surface'
                 upper_image = .False.
                 Call GS_SET_3DSIMAGE (lower_image, upper_image, status)
                 Call GS_SET_3DSIMAGE (lower_image, upper_image, status)
 
              Else If (command .Eq. 'LOW IMAGE') Then
 
                 MENU(9) = 'NO LOWER'
                 MENUTXT(9) = 'NO LOWER: Don''t ' // &
                   'display lower 3-D projected image'
                 lower_image = .True.
                 Call GS_SET_3DSIMAGE (lower_image, upper_image, status)
 
              Else If (command .Eq. 'NO LOWER') Then
 
                 MENU(9) = 'LOW IMAGE'
                 MENUTXT(9) = 'LOW IMAGE: Add 3-D projected ' // &
                   'image below 3-D surface'
                 lower_image = .False.
                 Call GS_SET_3DSIMAGE (lower_image, upper_image, status)
 
              End If
 
           End If
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
     End If
 
     End Subroutine F2D_3DSTYLE
!********1*********2*********3*********4*********5*********6*********7*********8
 
 
 
 
 

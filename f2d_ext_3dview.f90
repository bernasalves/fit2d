!********1*********2*********3*********4*********5*********6*********7*********8
 
!  **********************
!  *                    *
!  * f2d_ext_3dview.f90 *
!  *                    *
!  **********************
 
!+ F2D_EXT_3DVIEW - FIT 2-D 3-D peak Viewer
     Subroutine F2D_EXT_3DVIEW (max_peaks, num_peaks, PEAKS, experiment, status)
!  Description:
!    1. Outputs peaks as 3-d projection
!    2. The user can change the view with an interactive graphical menu
!  Keywords:
!    3-d~Peaks, Peak.3-d~view
!  Method:
!  Deficiencies:
!  Bugs:
!  Authors:
!    A P Hammersley (hammersley@esrf.fr)
!  History:
!    07-May-2008: V0.8 Add lattice drawing (Hammersley)
!    02-May-2008: V0.7 Option to define unit cell (Hammersley)
!    16-Apr-2008: V0.6 Testing rotation (Hammersley)
!    15-Apr-2008: V0.5 Rotation implementated (Hammersley)
!    14-Apr-2008: V0.4 Implementing rotation (Hammersley)
!    09-Apr-2008: V0.3 Debugging "GS_INP_MENUCHOICE" (Hammersley)
!    08-Apr-2008: V0.2 Add left button continuous rotation (Hammersley)
!    16-Nov-2006: V0.1 Original (Hammersley)
!  Type Definitions:
     Implicit None
     Include 'gs.inc' ! Graphics system definitions
!  Global Constants:
     Include 'st_symbols.inc' ! Symbolic "status" constants
!  Import:
     Integer, Intent(IN) :: max_peaks ! Dimension of "PEAKS"
     Integer, Intent(IN) :: num_peaks ! Number of defined peaks
     Type(PEAK_STRUCTURE), Intent(IN) :: PEAKS(max_peaks) ! Peak information
!  Import/Export:
     Type(EXPERIMENTAL_DETAILS), Intent(INOUT) :: experiment ! Details of
!      experiment (see "io.inc")
!  Export:
!  Status:
     Integer, Intent(INOUT) :: status ! Status return variable
!  Local Constants:
     Character(Len = 5), Parameter :: Version = 'V0.8' ! Version number
     Integer, Parameter :: Max_menu = 16 ! Dimension size of menu
!  Local Variables:
     Character(Len = 80) :: command ! User entered command
     Integer :: i ! Loop variable
     Integer :: input_type ! Type of graphical input
     Integer :: item ! Loop variable for data statements
     Integer :: peak ! Loop variable for peaks
     Integer :: num_menu ! Number of choices in menu
     Logical :: continue ! .True., until user wants to exit
     Logical :: left_button_down ! .True., whilst left mouse button is down
     Logical :: update_view ! .True., if the view needs to be redrawn
     Logical :: update_menu ! .True., if the menu needs to be redrawn
     Real :: dummy ! Dummy variable
     Real, Save :: latitude = 1.047197 ! Viewing latitude
     Real :: linewidth ! Line width scale factor
     Real, Save :: longitude = 0.5236 ! Viewing longitude
     Integer :: window_format ! Format of graphics window
     Real :: height ! Height of window in page coordinates 
     Integer :: stat ! Memory allocation status return variable
     Real :: start_latitude ! Latitude when left mouse button pressed
     Real :: start_longitude ! Longitude when left mouse button pressed
     Real :: width ! Width of window in page coordinates 
     Real :: window_size ! Size of half of the data window
     Real :: x_change ! Change in X-coordinate during left button down movement
     Real :: x_coordinate ! Graphical input X-coordinate
     Real :: y_coordinate ! Graphical input Y-coordinate
     Real :: x_start_co ! X-coordinate when left mouse button pressed
     Real :: x_max ! Maximum X-coordinate of peaks
     Real :: x_min ! Minimum X-coordinate of peaks
     Real :: x_translation ! X-translation of view
     Real :: xmaxgpp ! X-maximum of graph page position
     Real :: xmingpp ! X-minimum of graph page position
     Real :: y_change ! Change in Y-coordinate during left button down movement
     Real :: y_max ! Maximum Y-coordinate of peaks
     Real :: y_min ! Minimum Y-coordinate of peaks
     Real :: y_start_co ! Y-coordinate when left mouse button pressed
     Real :: y_translation ! Y-translation of view
     Real :: ymaxgpp ! Y-maximum of graph page position
     Real :: ymingpp ! Y-minimum of graph page position
     Real :: z_scaling ! Scaling applied to Z-direction
     Real :: z_max ! Maximum Z-coordinate of peaks
     Real :: z_min ! Minimum Z-coordinate of peaks
     Real :: z_translation ! Z-translation of view
!  Local Arrays:
     Character(Len = 10), Save :: MENU(Max_menu) ! Available menu commands
     Character(Len = 80), Save :: MENUTXT(Max_menu) ! Menu command explanations
     Real, Allocatable :: X_COORDINATES(:) ! Peak X-coordinates
     Real, Allocatable :: Y_COORDINATES(:) ! Peak Y-coordinates
     Real, Allocatable :: Z_COORDINATES(:) ! Peak Z-coordinates
!  Local Data Structures:
     Type(LUT_STYLE) :: STYLE_LUT ! LUT Style (see "gs.inc")
!  External Functions:
!  Local Data:
     Data (MENU(item), item = 1, 16) / &
       'EXIT', &
       '+ROT.', &
       '+ELEV.', &
       '+ZOOM', &
       'LEFT', &
       'UP', &
       '?', &
       '-ROT.', &
       '-ELEV.', &
       '-ZOOM', &
       'RIGHT', &
       'DOWN', &
       'PRINT', &
       'ANGLE', &
       'DEFAULT', &
       'CELL' /
     Data (MENUTXT(item), item = 1, 16) / &
       'EXIT: Exit 3-D surface viewing graphical menu', &
       '+ROT.: Increase longitude angle of viewer', &
       '+ELEV.: Increase elevation angle of viewer', &
       '+ZOOM: Increase size of object', &
       'LEFT: View further to the left of the object', &
       'UP: View further above the object', &
       '?: View this help text (explanation of options)', &
       '-ROT.: Decrease longitude angle of viewer', &
       '-ELEV.: Decrease elevation angle of viewer', &
       '-ZOOM: Decrease size of object', &
       'RIGHT: View further to the right of the object', &
       'DOWN: View further below the object', &
       'PRINT: Output displayed 3-D surface to graphics file', &
       'ANGLE: Keyboard entry of arbitrary viewing angle', &
       'DEFAULT: Re-set default view', &
       'CELL: Define unit cell parameters' /
!--------1---------2---------3---------4---------5---------6---------7---------8
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!  Write (*, '(''Entered F2D_EXT_3DVIEW'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!  Check input status
     If (status .Ne. St_goodvalue) Then
        Call ST_SAVE ('Subroutine F2D_EXT_3DVIEW ' // Version)
        Return
     End If
 
!  Check that the region to be added to is reasonably defined
     If (max_peaks .Le. 0) Then
        status = St_bad_dim1
     Else If (num_peaks .Lt. 0 .Or. num_peaks .Gt. max_peaks) Then
        status = St_bad_adr1
     End If
 
!  Recheck status
     If (status .Ne. St_goodvalue) Then
        status = St_mod_fit2d + status
        Call ST_SAVE ('Subroutine F2D_EXT_3DVIEW ' // Version)
     Else

        Allocate (X_COORDINATES(num_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_EXT_3DVIEW ' // Version)
           Return
        End If
        Allocate (Y_COORDINATES(num_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_EXT_3DVIEW ' // Version)
           Return
        End If
        Allocate (Z_COORDINATES(num_peaks), Stat = stat)
        If (stat .Ne. 0) Then
           status = St_mod_fit2d + St_bad_malloc
           Call ST_SAVE ('Subroutine F2D_EXT_3DVIEW ' // Version)
           Return
        End If

!     Transfer peak positions and calculate min/max 
        x_min = 1.0e38; y_min = 1.0e38; z_min = 1.0e38
        x_max = -1.0e38; y_max = -1.0e38; z_max = -1.0e38

        Do peak = 1, num_peaks

           X_COORDINATES(peak) = PEAKS(peak)%a_star
           Y_COORDINATES(peak) = PEAKS(peak)%b_star
           Z_COORDINATES(peak) = PEAKS(peak)%c_star
           
           x_min = Min(x_min, X_COORDINATES(peak))
           x_max = Max(x_max, X_COORDINATES(peak))
           y_min = Min(y_min, Y_COORDINATES(peak))
           y_max = Max(y_max, Y_COORDINATES(peak))
           z_min = Min(z_min, Z_COORDINATES(peak))
           z_max = Max(z_max, Z_COORDINATES(peak))

        End Do

!     Set initial distance and translation
        x_translation = -(x_min + x_max) / 2.0
        y_translation = -(y_min + y_max) / 2.0
        z_translation = -(z_min + z_max) / 2.0
        z_scaling = 1.0
        window_size = Max((x_max - x_min) / 2.0, (y_max - y_min) / 2.0, &
          (z_max - z_min) / 2.0)
        longitude = 0.0 * Pi / 180.0
        latitude = 0.0 * Pi / 180.0

        Call LG_MARKERCOLOUR (Gs_blue, status)
        Call LG_MARKERTYPE (Lg_cross, status) ! Diagonal cross

! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!     Arguments would appear to be reasonable, go ahead
        num_menu = max_menu
        continue = .True.
        update_view = .True.
        update_menu = .True.
        left_button_down = .False.
        Do While (continue)
 
!        Draw graphics output if required
           If (update_view) Then

!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''window_size = '', f12.5)') window_size
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

!           Set new 3-D view
              Call GS_SET_3DVIEW (x_translation, y_translation, z_translation, &
                longitude, latitude, window_size * 500.0, &
                -window_size, -window_size, &
                window_size, window_size, z_scaling, status)

              Call LG_DATAWINDOW (-window_size, -window_size, &
                window_size, window_size, status)
              Call GS_INQ_GPP (xmingpp, ymingpp, xmaxgpp, ymaxgpp, status)
              Call LG_VIEWPORT (xmingpp, ymingpp, xmaxgpp, ymaxgpp, status)
 
!           Clear background
              Call GS_BACKGROUND (status)

!           Draw unit cell
              Call F2D_DRAW_LATTICE (experiment, status)

!           Draw markers
              Call GS_3MARKERS (num_peaks, num_peaks, &
                X_COORDINATES, Y_COORDINATES, Z_COORDINATES, status)
              
              Call GS_UPDATE (status)
              update_menu = .True.
 
           End If
 
           If (update_menu) Then
 
!           Inquire window format
              Call GS_INQ_WINDOWFORMAT (window_format, width, height, status)
 
!           Set menu layout
              If (width / height .Gt. 1.2) Then
                 Call GS_SET_MENULAYOUT (Gs_vertical, 1, 30, 7, status)
              Else
                 Call GS_SET_MENULAYOUT (Gs_horizontal, 6, 30, 7, status)
              End If
 
!           Re-draw menu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!              Write (*, '(''F2D_EXT_3DVIEW: Before GS_FMENU'')')
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
              Call GS_FMENU (1, 0, 'INTERACTIVE 3-D VIEWER', Max_menu, &
                num_menu, MENU, status)
 
           End If
 
!        By default don't update graphics
           update_view = .False.
           update_menu = .False.
 

!        Get user to select between the available menu options
           command = 'null'

           Call GS_INP_MENUCHOICE (Max_menu, num_menu, MENU, 100, input_type, &
             command, x_coordinate, y_coordinate, status)
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!           Write (*, '(''F2D_EXT_3DVIEW: input_type = '', i6)') input_type
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG

           If (input_type .Eq. Gs_resize) Then
 
              update_view = .True.
              update_menu = .True.
 
           Else If (left_button_down .And. input_type .Eq. Gs_motion) Then

!           Rotate peak view
              x_change = x_coordinate - x_start_co
              y_change = y_coordinate - y_start_co
              longitude = start_longitude + x_change / window_size * Pi
              latitude = start_latitude + y_change / window_size * Pi


           Else If (input_type .Eq. Gs_coordinate) Then
 
!           Eventually the coordinate position can be output
              Continue
              update_view = .False.
              update_menu = .False.
 
            Else If (input_type .Eq. Gs_leftbuttondown) Then

!           The left mouse button has been depressed
              left_button_down = .True.
              x_start_co = x_coordinate
              y_start_co = y_coordinate
              start_longitude = longitude
              start_latitude = latitude

!              Write (*, '(''Left button down'')')

           Else If (input_type .Eq. Gs_leftbuttonup) Then

              left_button_down = .False.

!              Write (*, '(''Left button up'')')

           Else
 
!           Menu choice input
 
! - - - -1- - - - -2- - - - -3- - - - -4- - - - -5- - - - -6- - - - -7- - - - -8
!           Carry out menu choices
              If (command .Eq. 'null') Then
 
                 update_view = .False.
                 update_menu = .False.
 
              Else If (command .Eq. 'CELL') Then
 
!              Input new parameters for unit cell
                 Call F2D_GUI_UNITCELL (experiment, status)
                 update_view = .True.
 
              Else If (command .Eq. 'EXIT') Then
 
                 continue = .False.
                 update_view = .False.
                 update_menu = .False.
 
              Else If (command .Eq. '?') Then
 
!              Display list of available commands
                 Call GS_MESSAGE (Max_menu, num_menu, MENUTXT, status)
 
              Else If (command .Eq. 'PRINT') Then
 
!              Output current graphics to file, prompting for file
!              name if necessary
                 update_view = .False.
                 update_menu = .True.
 
              Else If (command .Eq. 'DEFAULT') Then
 
!              Default view position
                 x_translation = -(x_min + x_max) / 2.0
                 y_translation = -(y_min + y_max) / 2.0
                 z_translation = -(z_min + z_max) / 2.0
                 z_scaling = 1.0
                 window_size = Max((x_min + x_max) / 2.0, &
                   (y_min + y_max) / 2.0, (z_min + z_max) / 2.0)
                 longitude = 0.0 * Pi / 180.0
                 latitude = 0.0 * Pi / 180.0
 
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
                 If (latitude .Gt. Pi / 2.0) Then
                    latitude = Pi / 2.0
                 End If
 
              Else If (command .Eq. '-ELEV.') Then
 
                 latitude = latitude - 10.0 * Pi /180.0
                 If (latitude .Lt. -Pi / 2.0) Then
                    latitude = -Pi / 2.0
                 End If 
 
              Else If (command .Eq. '+ZOOM') Then
 
                 window_size = window_size / 1.4
 
              Else If (command .Eq. '-ZOOM') Then
 
                 window_size = window_size * 1.4
                 
              Else If (command .Eq. 'LEFT') Then
 
                 x_translation = x_translation + window_size * 0.1
 
              Else If (command .Eq. 'RIGHT') Then
 
                 x_translation = x_translation - window_size * 0.1
 
              Else If (command .Eq. 'UP') Then
 
                 z_translation = z_translation - window_size * 0.1
 
              Else If (command .Eq. 'DOWN') Then
 
                 z_translation = z_translation + window_size * 0.1
 
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
 
              End If
 
           End If
 
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
!        Write (*, '(''Before updating of graphics'')')
!        Write (*, '(''update_view = '', l1)') update_view
!        Write (*, '(''update_menu = '', l1)') update_menu
!**DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG***DEBUG
 
!        Check status
           Call ST_OUT (status)
           If (status .Ne. St_goodvalue) Then
              continue = .False.
           End If
 
        End Do
 
!     Free memory
        Deallocate (X_COORDINATES)
        Deallocate (Y_COORDINATES)
        Deallocate (Z_COORDINATES)

     End If
 
     End Subroutine F2D_EXT_3DVIEW
!********1*********2*********3*********4*********5*********6*********7*********8
